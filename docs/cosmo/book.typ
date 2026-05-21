
#import "@preview/shiroa:0.3.1": *

#show: book

#book-meta(
  title: "cosmo",
  description: "The documentation for cosmo language design and implementation notes",
  authors: ("Myriad-Dreamin",),
  repository: "https://github.com/Myriad-Dreamin/cosmo",
  repository-edit: "https://github.com/Myriad-Dreamin/cosmo/edit/main/{path}",
  language: "en",
  summary: [
    #prefix-chapter("intro.typ")[Introduction]
    #prefix-chapter("syntax.typ")[Syntax]
    #prefix-chapter("syntax-bnf.typ")[BNF Specification]
    #prefix-chapter("trait.typ")[Trait Design]
    #prefix-chapter("ufcs.typ")[Method Call Design]
  ],
)

#build-meta(dest-dir: "../../dist/docs")

// re-export page template
#import "/typ/templates/page.typ": project
#let book-page = project
