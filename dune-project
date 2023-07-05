(lang dune 3.8)
(using mdx 0.4)

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
  (digestif (>= 1.1.0))
  preface
  zarith
  tezos-base58
  (nightmare :pinned)
  (nightmare-test (and :pinned :with-test))
  (odoc :with-doc)
  (alcotest :with-test)))

(package
 (name yourbones-ppx)
 (synopsis "A ppx companion for Yourbones")
 (description "Some syntax extension to deal with Tezos values")
 (depends
  (ocaml (>= 5.0.0))
  (dune (>= 3.0.0))
  (ppxlib (>= 0.29.1))
  (yourbones (= :version))
  (mdx (and :with-test (>= 2.0.0)))))

(package
  (name yourbones_js)
  (synopsis "The JavaScript runtime for interaction with Tezos")
  (description "JavaScript runtime to exploit data described by the Yourbones library and interact with Tezos RPCs")
  (depends
   (ocaml (>= 5.0.0))
   (dune (>= 3.0.0))
   (lwt (>= 5.6.1))
   (preface (>= 1.0.0))
   (js_of_ocaml-compiler (>= 5.3.0))
   (js_of_ocaml-ppx (>= 5.3.0))
   (js_of_ocaml-lwt (>= 5.3.0))
   zarith_stubs_js
   (nightmare_js :pinned)
   (yourbones (= :version))))

(package
  (name yourbones_js-beacon)
  (synopsis "A very opinionated binding of Beacon-SDK in Js_of_OCaml")
  (description "An attempt to progressively link the Beacon SDK (TZIP-10) to interact with Wallets in a dApp")
  (depends
   (ocaml (>= 5.0.0))
   (dune (>= 3.0.0))
   (lwt (>= 5.6.1))
   (preface (>= 1.0.0))
   (js_of_ocaml-compiler (>= 5.3.0))
   (js_of_ocaml-ppx (>= 5.3.0))
   (js_of_ocaml-lwt (>= 5.3.0))
   zarith_stubs_js
   (nightmare_js :pinned)
   (yourbones (= :version))))
