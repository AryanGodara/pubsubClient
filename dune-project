(lang dune 3.9)

(name midiClient)

(generate_opam_files true)

(source
 (github Aryan/midiClient))

(authors "Aryan Godara")

(maintainers "Aryan Godara")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name midiClient)
 (synopsis "A short synopsis will be added here")
 (description "A longer description will be added here")
(depends 
  (ocaml
    (>= 5.0.0))
  dune
  (portmidi
    (>= 0.1))
  (cmdliner
    (>= 1.1.1))
)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
