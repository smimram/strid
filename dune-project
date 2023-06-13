(lang dune 2.0)
(name strid)
(license GPL-2.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(homepage "http://smimram.github.io/strid/")
(bug_reports "https://github.com/smimram/strid/issues")

(generate_opam_files true)
(using menhir 2.0)

(package
 (name strid)
 (depends
   graphics
 )
 (synopsis "String diagram generator for LaTeX")
 (description "Strid is a tool to generate string diagrams for inclusion in LaTeX documents.")
)
