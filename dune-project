(lang dune 3.0)

;; Project configuration
(name yourbones)
(version dev)
(generate_opam_files true)

;; Project description
(source (github funkywork/yourbones))
(license MIT)
(authors "Funkywork")
(maintainers "Funkywork")

;; Packages description

(package
 (name yourbones)
 (synopsis "A companion library for writing applications on Tezos")
 (description "A companion library for writing dApps in OCaml that should fit with Nightmare")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0.0))
  (data-encoding (>= 0.7.1))
  (odoc :with-doc)
  (alcotest :with-test)))
