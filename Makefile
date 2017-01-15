
GPRBUILD:=$(shell which gprbuild)
GPRCLEAN:=$(shell which gprclean)
EXE=obj/arun
GPRFILE=arun.gpr

all: $(EXE)

$(EXE): prepare
	$(GPRBUILD) -P$(GPRFILE) -cargs:c $(shell pkg-config --cflags gio-2.0)

prepare: src/arun-resources.c
	mkdir -p obj

src/arun-resources.c: arun.gresource.xml arun.glade
	glib-compile-resources --generate-source --target=$@ arun.gresource.xml

run: all
	./$(EXE)

doc:
	gnatdoc -P$(GPRFILE) --no-subprojects

clean:
	$(GPRCLEAN) -p$(GPRFILE)
	rm -f src/arun-resources.c

.PHONY: all clean prepare run doc
