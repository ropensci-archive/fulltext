RSCRIPT = Rscript --no-init-file

all: move rmd2md

move:
		cp inst/vign/fulltext_vignette.md vignettes;\
		cp inst/vign/getting_fulltext.md vignettes

rmd2md:
		cd vignettes;\
		mv fulltext_vignette.md fulltext_vignette.Rmd;\
		mv getting_fulltext.md getting_fulltext.Rmd

install: doc build
		R CMD INSTALL . && rm *.tar.gz

build:
		R CMD build .

doc:
		${RSCRIPT} -e "devtools::document()"

eg:
		${RSCRIPT} -e "devtools::run_examples()"
			
check:
		${RSCRIPT} -e "devtools::check(document = FALSE, cran = TRUE)"
