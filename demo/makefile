
PROG=demo

all:
	gcc -std=gnu99 -g $(PROG).c ~/.Rlibs/tea/libs/tea.so `pkg-config --libs apophenia` \
		-L/usr/lib64/R/lib -I/usr/lib64/R/include  -I/usr/share/R/include  -L/usr/share/R/lib  -lR -o $(PROG)

db:
	apop_text_to_db testdata dt test.db
