EMACS ?= emacs
EMACSFLAGS =
CASK = cask

OBJECTS = projectile.elc

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

.PHONY: build
build : elpa $(OBJECTS)

.PHONY: byte-compile-strict
byte-compile-strict : elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		--directory "."                          \
		$(EMACSFLAGS)                            \
		--eval "(progn                           \
			(setq byte-compile-error-on-warn t)  \
			(batch-byte-compile))" projectile.el

.PHONY: test
test : byte-compile-strict
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l test/run-tests

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -f elpa
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
