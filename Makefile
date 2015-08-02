# Copyright (c) 2015 Richard Mortier <mort@cantab.net>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-include Makefile.config

NAME = $(shell oasis query name)
VERSION = $(shell oasis query version)

all byte native build: configure
	ocaml setup.ml -build

lib: build reinstall
	[ ! -r bin/Makefile ] && mirage configure --no-depext bin/config.ml || true
	mirage clean bin/config.ml

tftpd: bin/tftpd
bin/tftpd: bin/config.ml bin/tftpd.ml
	[ ! -r bin/Makefile ] && \
		mirage configure --unix bin/config.ml || true
	mirage build bin/config.ml
	cp bin/_build/main.native bin/tftpd

configure setup.data: setup.ml
	ocaml $< -configure --enable-tests

setup: setup.ml
setup.ml: _oasis
	oasis setup

test doc install uninstall reinstall: all
	ocaml setup.ml -$@

## Documentation

doc/html/.git:
	mkdir -p doc/html
	cd doc/html && ( \
		git init && \
		git remote add origin git@github.com:mor1/ocaml-tftp.git && \
		git checkout -B gh-pages \
	)

gh-pages: doc/html/.git
	cd doc/html && git checkout -B gh-pages
	rm -f doc/html/*
	$(MAKE) doc && cp tftp.docdir/* doc/html/
	cd doc/html && ( \
		git add * && \
		git commit -a -m "Documentation updates" && \
		git push origin gh-pages \
	)

## Publish

ARCHIVE = https://github.com/mor1/ocaml-tftp/archive/$(VERSION).tar.gz

opam: release publish pr

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push upstream $(VERSION)

publish:
	OPAMYES=1 opam pin add -n .
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)

pr:
	OPAMYES=1 opam pin add -n $(NAME) $(NAME).$(VERSION)
	opam publish submit $(NAME).$(VERSION) && $(RM) -r $(NAME).$(VERSION)


## Clean

clean:
	ocaml setup.ml -clean
	[ -r bin/Makefile ] && mirage clean bin/config.ml || true
	$(RM) log bin/tftpd

distclean:
	ocaml setup.ml -distclean
	oasis setup-clean
	$(RM) setup.* _tags configure myocamlbuild.ml
	$(RM) lib/META lib/*.ml*lib lib/*.mlpack

.PHONY:  all byte native build doc test install uninstall reinstall \
  clean distclean configure setup
