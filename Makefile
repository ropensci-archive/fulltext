all: move rmd2md

vignettes:
		cd inst/vign;\
		Rscript --vanilla -e 'library(knitr); knit("fulltext_vignette.Rmd")'

move:
		cp inst/vign/fulltext_vignette.md vignettes
		cp -rf inst/vign/img/* vignettes/img/

rmd2md:
		cd vignettes;\
		mv fulltext_vignette.md fulltext_vignette.Rmd
