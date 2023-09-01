The paper PDF includes both the main text and the supplement.  This file is built from `paper.Rmd` using the `rticles` package.  Specifically, they use a template for Science articles that — as of 2023-06-09 — has not yet been pulled into the main `rticles` package (<https://github.com/rstudio/rticles/pull/486>).  To install the appropriate version of `rticles` use `devtools::install_github("christopherkenny/rticles")`.  

References must be handled using LaTeX `\cite` commands.  For unclear reasons, references are unstable, and sometimes bibtex seems to "forget" about the `bib` files.  Something like the following steps may fix this issue: 

1. Run `make clean` to remove everything downstream from `paper.tex`. 
2. Delete `paper.tex`
3. Run `make pdf` to recreate `paper.tex`
4. If references do not appear correctly, run `make aux` to force rebuilding the `aux` file

When everything is working correctly, `make` will (1) build and typeset both the PDF file and (2) generate a word count of the manuscript.  
