(lang dune 3.2)
(using mdx 0.3)

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
  (ocaml (>= 5.0.0))
  (dune (>= 3.0.0))
  (data-encoding (>= 0.7.1))
  preface
  (odoc :with-doc)
  (alcotest :with-test)))

(package
 (name yourbones_ppx)
 (synopsis "A ppx companion for Yourbones")
 (description "Some syntax extension to deal with Tezos values")
 (depends
  (ocaml (>= 5.0.0))
  (dune (>= 3.0.0))
  (ppxlib (>= 0.29.1))
  (yourbones (= :version))
  (mdx (and :with-test (>= 2.0.0)))))
