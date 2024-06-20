all: pdf wc

pdf: paper.pdf
paper.pdf: paper.qmd \
           references.bib \
           ../scripts/11-proof-of-concept.qmd
# 	Rscript -e "rmarkdown::render('paper.Rmd')"
	quarto render paper.qmd

.PHONY: images
images:
	cp ../out/08_focal_topics.png img/08_focal_topics.img
	cp ../out/02_count.png img/02_count.png
	cp ../out/12-stm.png img/12-stm.png
	cp ../out/05_lg_gamma.png img/05_lg_gamma.png
	cp ../out/05_md_gamma.png img/05_md_gamma.png
	cp ../out/05_sm_gamma.png img/05_sm_gamma.png
	cp ../out/06-lg-silge.pdf img/06-lg-silge.pdf
	cp ../out/06-md-silge.pdf img/06-md-silge.pdf
	cp ../out/06-sm-silge.pdf img/06-sm-silge.pdf
	cp ../out/08_grid_lg.pdf img/08_grid_lg.pdf
	cp ../out/08_grid_md.pdf img/08_grid_md.pdf
	cp ../out/08_grid_sm.pdf img/08_grid_sm.pdf

.PHONY: recache
recache: 
	quarto render paper.qmd --cache-refresh

.PHONY: wc
wc: 
	pdftotext paper.pdf - | wc -w

.PHONY: clean
clean:
	latexmk -C

.PHONY: aux
aux: 
	latexmk -pdf
