EMACS ?= emacs
EMACSFLAGS =
CASK = cask
CASKPROXY ?=
VAGRANT = vagrant
FIG = fig
DOCKER = docker

OBJECTS = projectile.elc

elpa:
	$(CASK) install $(CASKPROXY)
	$(CASK) update $(CASKPROXY)
	touch $@

.PHONY: build
build : elpa $(OBJECTS)

.PHONY: test
test : build
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l test/run-tests

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: fig-up
fig-up :
	$(DOCKER) build -t projectile-testdata test/docker/testdata
	$(DOCKER) build -t projectile-testrunner test/docker/testrunner
	$(FIG) build
	$(FIG) up -d sshd polipo

.PHONY: fig-bench
fig-bench :
	$(FIG) build projectile
	$(FIG) up --no-deps -d projectile
	$(FIG) logs

.PHONY: clean
clean :
	rm -f $(OBJECTS)
	rm -f elpa
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
