OBJS = ast.cmo scanner.cmo parser.cmo compile.cmo execute.cmo matcab.cmo

matcab : $(OBJS)
	ocamlc -o matcab $(OBJS)

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

scanner.ml : scanner.mll
	ocamllex scanner.mll

%.cmo : %.ml
	ocamlc -c $<

%.cmi :  %.mli
	ocamlc -c $<

.PHONY : clean
clean: 
	rm -f *.cmo parser.ml parser.mli scanner.ml *.cmi *.output matcab *.class

ast.cmo :
ast.cmx :
compile.cmo : ast.cmo
compile.cmx : ast.cmx
matcab.cmo : scanner.cmo parser.cmi compile.cmo ast.cmo
matcab.cmx : scanner.cmx parser.cmx compile.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
parser.cmi : ast.cmo
execute.cmo :
execute.cmx : 
