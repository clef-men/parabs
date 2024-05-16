(lang dune 3.0)
(name parabs)

(generate_opam_files true)

(authors "Clément Allain")
(maintainers "clement.allain@inria.fr")

(package
  (name parabs)
  (synopsis "Parallel abstractions for OCaml5")
  (depends
    (ocaml (>= 5.0.0))
    (saturn (= 0.4.0))
  )
)
