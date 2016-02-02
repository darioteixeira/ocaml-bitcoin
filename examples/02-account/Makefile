all: account.byte

account.byte: account.ml
	ocamlfind ocamlc -package unix,lwt,lwt.ppx,bitcoin,bitcoin.cohttp -linkpkg -o $@ $<

clean:
	rm -f *.cm[io] *.byte

