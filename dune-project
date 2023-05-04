(lang dune 3.7)

(authors "Antonin Décimo <antonin.decimo@gmail.com>")
(maintainers "Antonin Décimo <antonin.decimo@gmail.com>")
(license MIT)

(generate_opam_files true)

(package
 (name camlwalk)
 (synopsis "The camel walks, OCaml runs")
 (depends
  (tsdl (>= 1.0.0))
  (tsdl-image (>= 0.5))))
