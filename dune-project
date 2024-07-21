(lang dune 3.15)
(using melange 0.1)

(name fortune)

(generate_opam_files true)

(source
 (github theteachr/fortune))

(authors "theteachr")

(maintainers "theteachr")

(license LICENSE)

(package
 (name fortune)
 (synopsis "Monopoly Deal in OCaml")
 (description "An implementation of Monopoly Deal in OCaml")
 (depends ocaml dune)
 (tags
  (card-game "monopoly deal")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
