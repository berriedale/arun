
GPRBUILD:=$(shell which gprbuild)
GPRCLEAN:=$(shell which gprclean)
EXE=obj/arun

all: $(EXE)

$(EXE): prepare
	$(GPRBUILD) -Parun.gpr -cargs:c $(shell pkg-config --cflags gio-2.0)

prepare: src/arun-resources.c
	mkdir -p obj

src/arun-resources.c: arun.gresource.xml arun.glade
	glib-compile-resources --generate-source --target=$@ arun.gresource.xml

run: all
	./$(EXE)

clean:
	$(GPRCLEAN) -Parun.gpr
	rm -f src/arun-resources.c

.PHONY: all clean prepare run
