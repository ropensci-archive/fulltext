all: move rmd2md

move:
		cp inst/vign/fulltext_vignette.md vignettes;\
		cp inst/vign/getting_fulltext.md vignettes;\
		cp inst/vign/formats.md vignettes

rmd2md:
		cd vignettes;\
		mv fulltext_vignette.md fulltext_vignette.Rmd;\
		mv getting_fulltext.md getting_fulltext.Rmd;\
		mv formats.md formats.Rmd
