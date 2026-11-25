(lang dune 3.18)

(generate_opam_files)

(name opam-pubgrub-cudf)

(license MIT)

(maintainers "Ryan Gibb <ryan@freumh.org>")

(source
 (github RyanGibb/opam-pubgrub-cudf))

(package
 (name opam-pubgrub-cudf)
 (synopsis "PubGrub over a CUDF universe")
 (depends
  (ocaml (>= 4.14))
  (cudf (>= 0.10))
  pubgrub))
