.SUFFIXES : .ml .cmo .mli .cmi

CMOS = \
    labels.cmo \
    vars.cmo \
    gencpp.cmo \
    semantic.cmo \
    lamescan.cmo \
    lamepar.cmo \
    lame.cmo

CMIS = \
    ast.cmi \
    labels.cmi \
    vars.cmi \
    icode.cmi \
    gencpp.cmi \
    semantic.cmi \
    lamepar.cmi

all : lame.exe

.ml.cmo :
    ocamlc -c $<

.mli.cmi :
    ocamlc -c $<

lamepar.mli : lamepar.mly
    ocamlyacc lamepar.mly

lamescan.ml : lamescan.mll
    ocamllex lamescan.mll

lame.exe : $(CMIS) $(CMOS)
    ocamlc -o lame.exe $(CMOS)
