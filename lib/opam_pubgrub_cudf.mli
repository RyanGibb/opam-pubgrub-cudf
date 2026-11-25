(** PubGrub over a CUDF universe. *)

type t
type selections
type diagnostics

val create :
  ?prefer_oldest:bool ->
  ?handle_avoid_version:bool ->
  ?prefer_installed:bool ->
  constraints:(Cudf_types.pkgname * (Cudf_types.relop * Cudf_types.version)) list ->
  Cudf.universe ->
  t
(** [create ~constraints universe] is a solver that gets candidates from [universe],
    leaving out the versions that fail any of [constraints].

    @param prefer_oldest prefer each package's oldest version. [false] by default.
    @param handle_avoid_version
      take a version with the [("avoid-version", `Int 1)] or [("avoid-version", `Bool true)]
      extra property only where no other will do, unless it is installed. [true] by
      default.
    @param prefer_installed
      prefer the installed version of a package to any other. [false] by default. *)

val solve :
  t ->
  (Cudf_types.pkgname * [ `Essential | `Recommended ]) list ->
  (selections, diagnostics) result
(** [solve t packages] finds a compatible set of package versions that includes every
    [`Essential] package and what they need, and each [`Recommended] one that it can. *)

val packages_of_result : selections -> (Cudf_types.pkgname * Cudf_types.version) list

val diagnostics : ?verbose:bool -> diagnostics -> string
(** Why there is no solution, as PubGrub derived it. *)
