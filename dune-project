(lang dune 3.16)

(name microtree)

(generate_opam_files true)

(source
 (github lessej/microtree))

(authors "lessej")

(maintainers "lessej")

(license MIT)

(documentation https://github.com/lessej/microtree)

(package
 (name microtree)
 (synopsis "A small pretty printer for trees.")
 (description "A minimal tool to pretty print trees which are defined in a single line.")
 (depends ocaml dune)
 (tags
  (topics "trees")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
