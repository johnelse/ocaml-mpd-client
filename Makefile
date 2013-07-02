dist/build/lib-mpd-client/mpd-client.cmxa:
	obuild configure --enable-tests
	obuild build

install:
	ocamlfind install mpd-client lib/META \
		$(wildcard dist/build/lib-mpd_client/*) \
		$(wildcard dist/build/lib-mpd_client.lwt/*) \
		$(wildcard dist/build/lib-mpd_client.unix/*)

uninstall:
	ocamlfind remove mpd-client

.PHONY: clean test
clean:
	rm -rf dist

test:
	obuild test --output
