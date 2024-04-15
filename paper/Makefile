all: pdf wc

pdf: paper.pdf
paper.pdf: paper.qmd \
           references.bib \
           ../scripts/11-proof-of-concept.qmd
# 	Rscript -e "rmarkdown::render('paper.Rmd')"
	quarto render paper.qmd

.PHONY: wc
wc: 
	pdftotext paper.pdf - | wc -w

.PHONY: clean
clean:
	latexmk -C

.PHONY: aux
aux: 
	latexmk -pdf
