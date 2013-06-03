dist/build/lib-osc/osc.cmxa:
	obuild configure
	obuild build

install:
	ocamlfind install osc lib/META $(wildcard dist/build/lib-osc/*)

uninstall:
	ocamlfind remove osc

.PHONY: clean
clean:
	rm -rf dist
