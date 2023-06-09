all: pdf wc

pdf: paper.pdf
paper.pdf: paper.Rmd \
           references.bib
	Rscript -e "rmarkdown::render('paper.Rmd')"

.PHONY: wc
wc: 
	pdftotext paper.pdf - | wc -w