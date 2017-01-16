
GPRBUILD:=$(shell which gprbuild)
GPRCLEAN:=$(shell which gprclean)
EXE=obj/arun
GPRFILE=arun.gpr
RESOURCES=resources

all: $(EXE)

$(EXE): prepare
	$(GPRBUILD) -P$(GPRFILE) -cargs:c $(shell pkg-config --cflags gio-2.0)

prepare: src/arun-resources.c
	mkdir -p obj

src/arun-resources.c: $(RESOURCES)/arun.gresource.xml $(RESOURCES)/arun.glade
	glib-compile-resources --generate-source --target=$@ $(RESOURCES)/arun.gresource.xml

run: all
	./$(EXE)

doc:
	gnatdoc -P$(GPRFILE) --no-subprojects

clean:
	$(GPRCLEAN) -P$(GPRFILE)
	rm -f src/arun-resources.c

.PHONY: all clean prepare run doc
