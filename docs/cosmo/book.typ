
#import "@preview/shiroa:0.1.1": *

#show: book

#book-meta(
  title: "cosmo",
  summary: [
    #prefix-chapter("intro.typ")[Introduction]
    #prefix-chapter("syntax.typ")[Syntax]
    #prefix-chapter("syntax-bnf.typ")[BNF Specification]
    #prefix-chapter("class.typ")[Class Design]
    #prefix-chapter("trait.typ")[Trait Design]
    #prefix-chapter("ufcs.typ")[Method Call Design]
  ],
)

#build-meta(dest-dir: "../../dist/docs")

// re-export page template
#import "/typ/templates/page.typ": project
#let book-page = project
