
DOC=paper
LD_LIBRARY_PATH:=/apps/graphviz/usr/lib64:$(LD_LIBRARY_PATH) #for dot; research2 specific.


all: tea.bbl keys.tex
	sed -i 's/\\imputationtrue/\\imputationfalse/' $(DOC).tex
	-pdflatex $(DOC)
	scp $(DOC).pdf $(KLEMENS)/www/asst/d.pdf

keys:
	find ../pkg/tea/ -name '*.y' -or -name '*.c' -or -name '*.h' -or -name '*.R' | xargs cat | ./make_key_docs.awk  > keys.tex

imputation: tea.bbl
	sed -i 's/\\imputationfalse/\\imputationtrue/' $(DOC).tex
	pdflatex $(DOC)

DOT=/apps/graphviz/usr/bin/dot

diagram:
	if [ -e $(DOT) ] ; then \
		cd dots&& $(DOT) -Tpng < pepflow.dot > pepflow.png; \
	else	\
		cd dots&& dot -Tpng < pepflow.dot > pepflow.png; \
	fi
	#cd dots&& /apps/graphviz/usr/bin/dot -Teps < pepflow.dot > pepflow.eps

tea.bbl: tea.bib keys diagram
	pdflatex $(DOC).tex

PROG=read_spec

keyspec:
	gcc -std=gnu99 -g $(PROG).c ~/.Rlibs/tea/libs/tea.so `pkg-config --libs apophenia` -L/usr/lib64/R/lib -lR -o $(PROG)
	./read_spec
