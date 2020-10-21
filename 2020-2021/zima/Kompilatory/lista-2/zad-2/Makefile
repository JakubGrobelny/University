TARGET = zad2

zad2: zad2.cmo
	ocamlc -o $@ zad2.cmo

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo $(TARGET)
	rm -rf zad2.ml zad2.mli

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend