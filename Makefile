dist/build/lib-osc/osc.cmxa:
	obuild configure
	obuild build

.PHONY: clean install uninstall

clean:
	rm -rf dist

install:
	ocamlfind install osc lib/META \
		$(wildcard dist/build/lib-osc/*) \
		$(wildcard dist/build/lib-osc_unix/*)

uninstall:
	ocamlfind remove osc
