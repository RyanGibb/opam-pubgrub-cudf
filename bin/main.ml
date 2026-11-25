(* Usage: opam-pubgrub-cudf INPUT OUTPUT [CRITERIA]

   An external solver for opam. INPUT is the request as opam writes it. OUTPUT is
   written with the packages to have installed, or with FAIL where there is no solution,
   and the reason is then printed. *)

let not_relop = function
  | `Eq -> `Neq
  | `Neq -> `Eq
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Leq -> `Gt
  | `Lt -> `Geq

let contains s sub =
  let n = String.length sub in
  let rec from i = i + n <= String.length s && (String.sub s i n = sub || from (i + 1)) in
  from 0

(* What a CUDF request asks of the solver: the packages that must be installed, those
   already installed that should stay if they can, and the versions that are ruled out. *)
let create_spec ~keep_installed universe request =
  let add pkg req c (pkgs, constraints) =
    ((pkg, req) :: pkgs, match c with None -> constraints | Some c -> (pkg, c) :: constraints)
  in
  let essential spec (pkg, c) = add pkg `Essential c spec in
  let restricts (pkgs, constraints) (pkg, c) =
    ( pkgs,
      match c with
      | None -> (pkg, (`Lt, 1)) :: (pkg, (`Gt, 1)) :: constraints
      | Some (relop, v) -> (pkg, (not_relop relop, v)) :: constraints )
  in
  let asked name =
    List.exists
      (fun (pkg, _) -> String.equal pkg name)
      (request.Cudf.install @ request.Cudf.upgrade @ request.Cudf.remove)
  in
  let spec = List.fold_left essential ([], []) request.Cudf.install in
  let spec = List.fold_left essential spec request.Cudf.upgrade in
  let spec = List.fold_left restricts spec request.Cudf.remove in
  Cudf.fold_packages_by_name
    (fun spec name pkgs ->
      match List.find_opt (fun pkg -> pkg.Cudf.installed) pkgs with
      | Some { Cudf.keep = `Keep_version; version; _ } ->
          essential spec (name, Some (`Eq, version))
      | Some { Cudf.keep = `Keep_package; _ } -> essential spec (name, None)
      | Some { Cudf.keep = `Keep_feature; _ } -> assert false
      | Some { Cudf.keep = `Keep_none; _ } ->
          if keep_installed && not (asked name) then add name `Recommended None spec
          else spec
      | None -> spec)
    spec universe

let () =
  let input, output, criteria =
    match Sys.argv with
    | [| _; input; output |] -> (input, output, "")
    | [| _; input; output; criteria |] -> (input, output, criteria)
    | _ ->
        prerr_endline "usage: opam-pubgrub-cudf INPUT OUTPUT [CRITERIA]";
        exit 2
  in
  match Cudf_parser.load_from_file input with
  | _, _, None ->
      prerr_endline "no request";
      exit 2
  | preamble, universe, Some request -> (
      let pkgs, constraints =
        create_spec ~keep_installed:(not (contains criteria "+removed")) universe request
      in
      let t =
        Opam_pubgrub_cudf.create
          ~prefer_oldest:(contains criteria "+count[version-lag")
          ~handle_avoid_version:true
          ~prefer_installed:(contains criteria "-changed")
          ~constraints universe
      in
      let oc = open_out output in
      (match Opam_pubgrub_cudf.solve t pkgs with
      | Ok selections ->
          let installed =
            List.map
              (fun nv ->
                let pkg = Cudf.lookup_package universe nv in
                { pkg with was_installed = pkg.installed; installed = true })
              (Opam_pubgrub_cudf.packages_of_result selections)
          in
          Cudf_printer.pp_solution oc
            ( (match preamble with Some p -> p | None -> Cudf.default_preamble),
              Cudf.load_universe installed )
      | Error diagnostics ->
          output_string oc "FAIL\n";
          prerr_endline (Opam_pubgrub_cudf.diagnostics diagnostics));
      close_out oc)
