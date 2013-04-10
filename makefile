include ~/.Renviron

Pkg_dir=pkg/tea

all:  pkg
	make docs
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD build tea
	cd $(Pkg_dir)/..; R CMD INSTALL -l ~/.Rlibs tea*.tar.gz
	#cp tea*.tar.gz ~/.
	#rm -f tea*.tar.gz

test:  pkg
	#R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD build tea | sed '/-fpic/d'
	#cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD check tea*.tar.gz
	cd $(Pkg_dir)/..; R CMD INSTALL -l ~/.Rlibs tea*.tar.gz

nodoc:  pkg
	#R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD build tea | sed '/-fpic/d'
	cd $(Pkg_dir)/..; R CMD INSTALL -l ~/.Rlibs tea*.tar.gz

docs:	#also set up the demo data.
	cd $(Pkg_dir) && R CMD roxygen .
	mkdir -p $(Pkg_dir)/man
	mv $(Pkg_dir)/..roxygen/man/* $(Pkg_dir)/man/
	rm $(Pkg_dir)/..roxygen -r
	cd doc; make;
	mkdir -p $(Pkg_dir)/inst/doc
	cp doc/tea.pdf $(Pkg_dir)/inst/doc/tea-overview.pdf
	cp -r demo $(Pkg_dir)
	cd $(Pkg_dir)/demo; . insert_bugs; rm ss08pdc.csv insert_bugs

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
	make keys
	cd $(Pkg_dir); autoconf; rm configure.ac; rm -r autom4te.cache

keys:
	cd doc; m4 -P make_key_docs.m4 `find ../pkg/tea/ -name '*.c' -or -name '*.h' -or -name '*.R'` | sed 's/^#//' > keys.tex
	cd doc; m4 -P make_key_prints.m4 `find ../pkg/tea/ -name '*.c' -or -name '*.h' -or -name '*.R'` |sort > key_prints.tex
	cd doc; m4 -P make_key_list.m4 `find ../pkg/tea/ -name '*.c' -or -name '*.h' -or -name '*.R'` > ../$(Pkg_dir)/src/keylist


clean:
	rm -fr $(Pkg_dir)/* $(Pkg_dir)/../tea.Rcheck $(Pkg_dir)/../tea*.tar.gz
	rm -f c/peptalk/peptalk.output
	rm -f tests/edit-rel_age/graph/*
	rm -f demo/bigdemo/*.png

push:
	@if [ "x$(MSG)" = 'x' ] ; then echo "MSG='whatever, dude.'" make push; fi
	@test "x$(MSG)" != 'x'
	git commit -a  -m "$(MSG)"
	git svn fetch
	git svn rebase
	git svn dcommit

get:
	git svn fetch
	git svn rebase
	#R_LIBS=$(R_LIBS) R CMD check tea
	cd $(Pkg_dir)/..; R_LIBS=$(R_LIBS) R CMD build tea | sed '/-fpic/d'
	cd $(Pkg_dir)/..; R CMD INSTALL -l ~/.Rlibs tea*.tar.gz
