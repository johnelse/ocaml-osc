dist/build/lib-osc/osc.cmxa:
	obuild configure
	obuild build

.PHONY: clean install uninstall

clean:
	rm -rf dist

install:
	ocamlfind install osc lib/META $(wildcard dist/build/lib-osc/*)

uninstall:
	ocamlfind remove osc
