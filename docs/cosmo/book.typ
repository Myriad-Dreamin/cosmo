
#import "@preview/shiroa:0.1.0": *

#show: book

#book-meta(
  title: "cosmo",
  summary: [
    #prefix-chapter("intro.typ")[Introduction]
    #prefix-chapter("syntax.typ")[Syntax]
  ],
)

#build-meta(dest-dir: "../../dist/docs")

// re-export page template
#import "/typ/templates/page.typ": project
#let book-page = project
