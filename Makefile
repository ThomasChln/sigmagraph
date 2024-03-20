# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean devtools_check

doc.pdf:
	R CMD Rd2pdf -o doc.pdf .

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

roxygenise:
	R -e "roxygen2::roxygenise()"

devtools_test:
	R -e "devtools::test()"

devtools_check:
	R -e "devtools::check()"

vignette:
	cd vignettes;\
	R -e "rmarkdown::render('sigmagraph.Rmd')"
	#R -e "Sweave('sigmagraph.Rnw');tools::texi2pdf('sigmagraph.tex')"

clean:
	$(RM) doc.pdf
	cd vignettes;\
	$(RM) *.pdf *.aux *.bbl *.blg *.out *.tex *.log

download_js:
	wget https://cdn.bootcdn.net/ajax/libs/sigma.js/2.4.0/sigma.min.js
	mv sigma.min.js inst/htmlwidgets/lib/sigma-2.4.0
	wget https://cdn.jsdelivr.net/npm/graphology@0.25.4/dist/graphology.umd.min.js
	mv graphology.umd.min.js inst/htmlwidgets/lib/graphology-0.25.4
