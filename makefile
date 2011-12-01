include ~/.Renviron

Pkg_dir=pkg/tea

all:  pkg
	make docs
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD build tea
	cd $(Pkg_dir)/..; R CMD INSTALL -l ~/.Rlibs tea*.tar.gz
	#cp tea*.tar.gz ~/.
	#rm -f tea*.tar.gz

nodoc:  pkg
	#R_LIBS=$(R_LIBS) R CMD check tea
	R_LIBS=$(R_LIBS) R CMD build tea
	R CMD INSTALL -l ~/.Rlibs tea*.tar.gz

docs:
	cd $(Pkg_dir) && R CMD roxygen .
	mkdir -p $(Pkg_dir)/man
	mv $(Pkg_dir)/..roxygen/man/* $(Pkg_dir)/man/
	rm $(Pkg_dir)/..roxygen -r
	cd doc; make;
	mkdir -p $(Pkg_dir)/inst/doc
	cp doc/tea.pdf $(Pkg_dir)/inst/doc/tea-overview.pdf

talk:
	cd c/peptalk && bison --verbose -d -t peptalk.y 
	cd c/peptalk && flex peptalk.l
	mkdir -p $(Pkg_dir)/src
	mv c/peptalk/*.c c/peptalk/*.h $(Pkg_dir)/src/

pkg: talk
	mkdir -p $(Pkg_dir)/src #a reminder that make talk does this
	mkdir -p tests
	cp -r data R tests $(Pkg_dir) 
	cp `find c -type f` $(Pkg_dir)/src #flattens to a one-level directory
	cp -r pkging/* $(Pkg_dir)
	cd $(Pkg_dir); autoconf; rm configure.ac; rm -r autom4te.cache

clean:
	rm -fr $(Pkg_dir)/* tea.Rcheck
	rm -f c/peptalk/peptalk.output
