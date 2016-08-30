emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el

elpa: update compile test

update:
	$(emacs) -batch -l test/make-update.el

compile:
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/run-tests

clean:
	rm -f *.elc

.PHONY: all update compile test clean
