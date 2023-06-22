all: pipe



## Locations
out := ../out
data := ../data
out := ../out

## Metadata and phrase counts generated in non-public section of project
phrases := $(data)/00_phrases/prefix=MQ/year=2004/part-0.parquet \
           $(data)/00_phrases/prefix=10.2466/part-0.parquet
           
meta00 := $(data)/00_meta_mq.csv \
        $(data)/00_meta_mainstr/publisher=Springer\ Science\ and\ Business\ Media\ LLC/container.title=Behavior\ Genetics/year=2010/part-0.parquet


## Analysis pipeline ---------------------------------------------------------------
## Script 01: Combine MQ and mainstream datasets
target01 := $(data)/01_metadata/container.title=Psychological\ Reports/year=2010/part-0.parquet
meta := $(target01)

$(target01): 01_combine_datasets.R \
             $(phrases) \
             $(meta00)
	rscript 01_combine_datasets.R

## Script 02: Describe corpus 
target02 := 02_describe_corpus.html
$(target02): 02_describe_corpus.R \
                         $(meta) \
                         $(phrases)
	rscript -e "rmarkdown::render('$<')"

## Script 03: Vocabulary selection
target03 := $(data)/03-vocab-lg.Rds
$(target03): 03_vocab.R \
             $(meta) \
             $(phrases) \
             ../R/phrases.R
	rscript 03_vocab.R

## Script 04: Topic modeling
.PHONY: models
target04 := $(data)/04_tm/04_md_tmfast.Rds
models := $(target04)
$(target04): 04_topic_models.R \
             $(meta) \
             $(phrases) \
             $(target03)
# 	rscript 04_topic_models.R --force
	@echo "Models may need to be refit.  Run rscript 04_topic_models.R --force."; exit 1

## Script 05: Descriptive plots and renormalization exponents for topic-doc distributions
.PHONY: exponents
target05 := $(out)/05_sm_gamma.png
exponents := $(target05)
$(target05): 05_all_gamma.R \
             $(models) \
             $(meta)
	rscript 05_all_gamma.R

## Script 06: Hellinger distances for word-topic distributions between values of k
target06 := $(out)/06_sm_mereogram.png
$(target06): 06_hellinger_betas.R \
             $(models)
	rscript 06_hellinger_betas.R

## Script 07: Interactive UMAP visualization, plus UMAP embeddings and QC spreadsheet
.PHONY: umap
target07 := $(out)/07_lg_40.html
umap := $(target07)
$(target07): 07_umap_interactive.R \
             $(meta) \
             $(phrases) \
             $(models) \
             $(exponents)
# 	rscript 07_umap_interactive.R --force
	@echo "UMAP embeddings may need to be recalculated. Run rscript 07_umap_interactive.R --force"; exit 1

## Script 08: Occurrence of topics over time by journal
target08 := $(out)/08_presentation.png
$(target08): 08_topics_over_time.R \
             $(meta) \
             $(phrases) \
             $(models) \
			 $(exponents)
	rscript 08_topics_over_time.R

## Script 09: Static UMAP visualization
target09 := $(out)/09_lg_40_umap.png
$(target09): 09_umap_static.R \
             $(meta) \
             $(models) \
             $(exponents) \
             $(umap)
	rscript 09_umap_static.R


pipe: $(target02) $(target05) $(target06) $(target07) $(target08) $(target09)
