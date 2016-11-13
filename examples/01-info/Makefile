all: info.byte

info.byte: info.ml
	ocamlfind ocamlc -package unix,bitcoin,bitcoin.ocamlnet -linkpkg -o $@ $<

clean:
	rm -f *.cm[iot] *.byte
