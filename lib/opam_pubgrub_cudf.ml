(* A package is a name whose versions are its CUDF versions and 0, which is the
   package not being installed. A disjunction of several packages is a name of
   its own, whose versions are the places of its alternatives. *)

module Name = struct
  type t = Real of string | Or of int * string

  let compare a b =
    match (a, b) with
    | Real a, Real b -> String.compare a b
    | Or (a, _), Or (b, _) -> Int.compare a b
    | Real _, Or _ -> -1
    | Or _, Real _ -> 1

  let pp f = function
    | Real n -> Format.pp_print_string f n
    | Or (_, label) -> Format.fprintf f "(%s)" label
end

module Version = struct
  type t = int

  let compare = Int.compare
  let pp = Format.pp_print_int
end

module Solver = Pubgrub.Make (Name) (Version)
module Ranges = Solver.Ranges

type constr = Cudf_types.relop * Cudf_types.version

type t = {
  universe : Cudf.universe;
  constraints : (string, constr) Hashtbl.t;
  prefer_oldest : bool;
  handle_avoid_version : bool;
  prefer_installed : bool;
  versions : (string, int list * bool) Hashtbl.t;
      (** by package: 0 and its versions, oldest first, and whether the newest is always
          the one to prefer *)
  ors : (Cudf_types.vpkg list, Name.t) Hashtbl.t;
  alternatives : (int, (string * Ranges.t) array) Hashtbl.t;
  recommended : (string, unit) Hashtbl.t;
}

type selections = (Cudf_types.pkgname * Cudf_types.version) list
type diagnostics = Solver.incompatibility

let fop : Cudf_types.relop -> int -> int -> bool = function
  | `Eq -> ( = )
  | `Neq -> ( <> )
  | `Geq -> ( >= )
  | `Gt -> ( > )
  | `Leq -> ( <= )
  | `Lt -> ( < )

let tagged_with_avoid_version pkg =
  List.exists
    (function "avoid-version", (`Int 1 | `Bool true) -> true | _ -> false)
    pkg.Cudf.pkg_extra

let create ?(prefer_oldest = false) ?(handle_avoid_version = true)
    ?(prefer_installed = false) ~constraints universe =
  let table = Hashtbl.create 16 in
  List.iter (fun (name, c) -> Hashtbl.add table name c) constraints;
  {
    universe;
    constraints = table;
    prefer_oldest;
    handle_avoid_version;
    prefer_installed;
    versions = Hashtbl.create 1024;
    ors = Hashtbl.create 64;
    alternatives = Hashtbl.create 64;
    recommended = Hashtbl.create 64;
  }

let present = Ranges.strictly_higher_than 0

(* the versions a constraint admits, which are never 0 *)
let range : constr option -> Ranges.t = function
  | None -> present
  | Some (op, v) ->
      Ranges.intersection present
        (match op with
        | `Eq -> Ranges.singleton v
        | `Neq -> Ranges.complement (Ranges.singleton v)
        | `Geq -> Ranges.higher_than v
        | `Gt -> Ranges.strictly_higher_than v
        | `Leq -> Ranges.lower_than v
        | `Lt -> Ranges.strictly_lower_than v)

let versions t name =
  match Hashtbl.find_opt t.versions name with
  | Some known -> known
  | None ->
      let wanted = Hashtbl.find_all t.constraints name in
      let pkgs =
        List.filter
          (fun pkg -> List.for_all (fun (op, v) -> fop op pkg.Cudf.version v) wanted)
          (Cudf.lookup_packages t.universe name)
      in
      let plain =
        (not t.prefer_oldest)
        && List.for_all
             (fun pkg ->
               not
                 ((t.prefer_installed && pkg.Cudf.installed)
                 || (t.handle_avoid_version && tagged_with_avoid_version pkg)))
             pkgs
      in
      let known =
        (0 :: List.sort Int.compare (List.map (fun pkg -> pkg.Cudf.version) pkgs), plain)
      in
      Hashtbl.add t.versions name known;
      known

let vers t : Name.t -> int list = function
  | Real name -> fst (versions t name)
  | Or (i, _) -> List.init (Array.length (Hashtbl.find t.alternatives i)) succ

(* less is better *)
let preference t pkg1 pkg2 =
  let by_version () =
    if t.prefer_oldest then Int.compare pkg1.Cudf.version pkg2.Cudf.version
    else Int.compare pkg2.Cudf.version pkg1.Cudf.version
  in
  let by_avoid () =
    if not t.handle_avoid_version then by_version ()
    else
      match (tagged_with_avoid_version pkg1, tagged_with_avoid_version pkg2) with
      | true, false when not pkg1.Cudf.installed -> 1
      | false, true when not pkg2.Cudf.installed -> -1
      | _ -> by_version ()
  in
  if not t.prefer_installed then by_avoid ()
  else
    match (pkg1.Cudf.installed, pkg2.Cudf.installed) with
    | true, false -> -1
    | false, true -> 1
    | _ -> by_avoid ()

let best t name candidates =
  match List.filter (fun v -> v > 0) candidates with
  | [] -> 0
  | v :: rest when snd (versions t name) -> List.fold_left max v rest
  | v :: rest ->
      let pkg v = Cudf.lookup_package t.universe (name, v) in
      let better a v =
        let b = pkg v in
        if preference t b a < 0 then b else a
      in
      (List.fold_left better (pkg v) rest).Cudf.version

let choose t ~(assigned : Name.t -> Solver.selection) (name : Name.t) candidates =
  match name with
  | Real name ->
      (* not installed where nothing asks for it *)
      if List.mem 0 candidates && not (Hashtbl.mem t.recommended name) then 0
      else best t name candidates
  | Or (i, _) -> (
      (* an alternative already installed, or else the first *)
      let alternatives = Hashtbl.find t.alternatives i in
      let installed k =
        let name, r = alternatives.(k - 1) in
        match assigned (Real name) with Decided v -> Ranges.contains v r | _ -> false
      in
      match List.find_opt installed candidates with
      | Some k -> k
      | None -> List.fold_left min (List.hd candidates) candidates)

let add (name : Name.t) r deps =
  let rec go = function
    | [] -> [ (name, r) ]
    | (n, r') :: rest when Name.compare n name = 0 -> (n, Ranges.intersection r r') :: rest
    | d :: rest -> d :: go rest
  in
  go deps

(* nothing has this name, so that to need it is to be uninstallable *)
let nothing = Name.Real "="

let pp_vpkg f ((name, c) : Cudf_types.vpkg) =
  match c with
  | None -> Format.pp_print_string f name
  | Some (op, v) ->
      let op =
        match op with
        | `Eq -> "="
        | `Neq -> "!="
        | `Geq -> ">="
        | `Gt -> ">"
        | `Leq -> "<="
        | `Lt -> "<"
      in
      Format.fprintf f "%s %s %d" name op v

(* One clause of a package's dependencies, which the package's own version may
   already meet. *)
let clause t pkg (alternatives : Cudf_types.vpkg list) deps =
  let self = pkg.Cudf.package in
  match alternatives with
  | [] -> add nothing present deps
  | [ (name, _) ] when String.equal name self -> deps
  | [ (name, c) ] -> add (Real name) (range c) deps
  | _ ->
      if
        List.exists
          (fun (name, c) ->
            String.equal name self && Ranges.contains pkg.Cudf.version (range c))
          alternatives
      then deps
      else
        let grouped =
          List.fold_left
            (fun acc (name, c) ->
              if String.equal name self then acc
              else
                match List.assoc_opt name acc with
                | Some r ->
                    (name, Ranges.union r (range c)) :: List.remove_assoc name acc
                | None -> (name, range c) :: acc)
            [] alternatives
          |> List.rev
        in
        (match grouped with
        | [] -> add nothing present deps
        | [ (name, r) ] -> add (Real name) r deps
        | _ ->
            let name =
              match Hashtbl.find_opt t.ors alternatives with
              | Some name -> name
              | None ->
                  let i = Hashtbl.length t.alternatives in
                  let label =
                    Format.asprintf "%a"
                      (Format.pp_print_list
                         ~pp_sep:(fun f () -> Format.pp_print_string f " | ")
                         pp_vpkg)
                      alternatives
                  in
                  let name = Name.Or (i, label) in
                  Hashtbl.add t.alternatives i (Array.of_list grouped);
                  Hashtbl.add t.ors alternatives name;
                  name
            in
            add name Ranges.full deps)

let deps t (name : Name.t) v =
  match name with
  | Or (i, _) ->
      let name, r = (Hashtbl.find t.alternatives i).(v - 1) in
      [ (Name.Real name, r) ]
  | Real _ when v = 0 -> []
  | Real name ->
      let pkg = Cudf.lookup_package t.universe (name, v) in
      let deps = List.fold_left (fun deps c -> clause t pkg c deps) [] pkg.Cudf.depends in
      List.fold_left
        (fun deps (other, c) ->
          (* a package conflicts with its other versions as it is *)
          if String.equal other name then deps
          else add (Real other) (Ranges.complement (range c)) deps)
        deps pkg.Cudf.conflicts

let solve t pkgs =
  let query =
    List.fold_left
      (fun query (name, importance) ->
        match importance with
        | `Essential ->
            Hashtbl.remove t.recommended name;
            add (Real name) present query
        | `Recommended ->
            if not (List.exists (fun (n, _) -> Name.compare n (Real name) = 0) query) then
              Hashtbl.replace t.recommended name ();
            add (Real name) Ranges.full query)
      [] pkgs
  in
  Solver.solve ~choose:(choose t) ~vers:(vers t) ~deps:(deps t) query
  |> Result.map
       (List.filter_map (function
         | Name.Real name, v when v > 0 -> Some (name, v)
         | _ -> None))

let packages_of_result selections = selections

let diagnostics ?verbose:_ incompatibility =
  Format.asprintf "%a" Solver.explain_incompatibility incompatibility
