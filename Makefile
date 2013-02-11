EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACSFLAGS =
CARTON = carton
VAGRANT = vagrant

OBJECTS = projectile.elc

.PHONY: deps
deps :
	$(CARTON) install
	$(CARTON) update

.PHONY: build
build : deps $(OBJECTS)

.PHONY: test
test : build
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l test/test

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -rf elpa # Clean packages installed for development

%.elc : %.el
	$(CARTON) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
