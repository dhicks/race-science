all: pdf tex wc

split = 31

pdf: paper.pdf
paper.pdf: paper.qmd \
           references.yaml \
           refs-manual.bib \
           ../scripts/11-proof-of-concept.qmd \
           img/*
# 	Rscript -e "rmarkdown::render('paper.Rmd')"
	quarto render paper.qmd
	
tex: paper-noapdx.tex
paper-noapdx.tex: paper.tex
	sed '/\\appendix/,$$d' $< > $@
	echo '' >> $@
	echo '\end{document}' >> $@

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
	cp ../out/15_pf_count.png img/15_pf_count.png
	cp ../out/15_presence.png img/15_presence.png

.PHONY: recache
recache: 
	quarto render paper.qmd --cache-refresh

.PHONY: wc
wc: 
	echo "splitting at page $(split)"
	qpdf paper.pdf --pages . 1-$(split) -- main.pdf
	pdftotext main.pdf - | wc -w

.PHONY: clean
clean:
	latexmk -C

.PHONY: aux
aux: 
	latexmk -pdf

.PHONY: diff
diff:
	Rscript -e "latexdiffr::git_latexdiff('paper.qmd', '334e16388c27d542644b50c8feae36362a2eac74')"
