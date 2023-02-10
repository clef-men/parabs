(lang dune 3.0)
(name parabstr)

(generate_opam_files true)

(authors "Clément Allain" "François Pottier")
(maintainers "clement.allain@inria.fr")

(package
  (name parabstr)
  (synopsis "Parallel abstractions for OCaml5")
  (depends
    (ocaml (>= 5.0.0))
    lockfree
  )
)
