all: move pandoc rmd2md

vignettes:
		cd inst/vign;\
		Rscript --vanilla -e 'library(knitr); knit("fulltext_vignette.Rmd")'

move:
		cp inst/vign/fulltext_vignette.md vignettes
		cp -rf inst/vign/img/* vignettes/img/

pandoc:
		cd vignettes;\
		pandoc -H margins.sty fulltext_vignette.md -o fulltext_vignette.pdf --highlight-style=tango;\
		pandoc -H margins.sty fulltext_vignette.md -o fulltext_vignette.html --highlight-style=tango

rmd2md:
		cd vignettes;\
		cp fulltext_vignette.md fulltext_vignette.Rmd;\
