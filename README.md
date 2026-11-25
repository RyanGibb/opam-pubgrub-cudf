# opam-pubgrub-cudf

[PubGrub](https://github.com/RyanGibb/ocaml-pubgrub) over a CUDF universe.

    opam switch create . --deps-only
    dune build
    dune install

It is an external solver for opam:

    opam install foo --solver='opam-pubgrub-cudf %{input}% %{output}% %{criteria}%'
