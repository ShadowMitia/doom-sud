OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind

OPTIONS = -g -annot

PACKAGES = # sdl

COMPILE = $(OCAMLFIND) $(OCAMLC) # -package $(PACKAGES)
COMPILEOPT = $(OCAMLFIND) $(OCAMLOPT) # -package $(PACKAGES)

LIBS = str.cma graphics.cma
# bigarray.cma sdl.cma
LIBOPTS = $(LIBS:.cma=.cmxa)

EXEC = bsp

OBJS = 	trigo.cmo options.cmo point.cmo \
	segment.cmo bsp.cmo physic.cmo \
	player.cmo parse_lab.cmo render.cmo main.cmo

OPTOBJS = $(OBJS:.cmo=.cmx)
FILESMLI = $(OBJS:.cmo=.mli)
FILESMLS = $(OBJS:.cmo=.ml)

all: depend $(EXEC)

opt: depend $(EXEC).opt

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

$(EXEC): $(OBJS)
	$(COMPILE) $(OPTIONS) -o $(EXEC) $(LIBS) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(COMPILEOPT) $(OPTIONS) -o $(EXEC) $(LIBOPTS) $(OPTOBJS)

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.annot .depend
	rm -f $(EXEC) 
	rm -f $(EXEC).opt

.SUFFIXES: .ml .mli .cmo .cmi .mll

.ml.cmo:
	$(COMPILE) $(OPTIONS) -c $<

.ml.cmx:
	$(COMPILEOPT) $(OPTIONS) -c $<

.cmo.cmi:
	$(COMPILE) $(OPTIONS) -c $<

.mli.cmi:
	$(COMPILE) $(OPTIONS) -c $<

.mll.ml:
	ocamllex $<

.depend:
	$(OCAMLDEP) $(FILESMLI) $(FILESMLS) > .depend

depend:
	$(OCAMLDEP) $(FILESMLI) $(FILESMLS) > .depend

-include .depend
