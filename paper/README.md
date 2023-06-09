Notes on the manuscript and supplement PDF files

Both files are built from `paper.Rmd` using the `rticles` package.  Specifically, they use a template for Science articles that — as of 2023-06-09 — has not yet been pulled into the main `rticles` package (<https://github.com/rstudio/rticles/pull/486>).  To install the appropriate version of `rticles` use `devtools::install_github("christopherkenny/rticles")`.  

References must be handled using LaTeX `\cite`, and the logic for handling the manuscript vs. supplement requires keeping references in `references.bib`.  

`make` will (1) build and typeset both PDF files and (2) generate a word count of the manuscript.  
