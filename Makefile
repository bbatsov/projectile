CASK = cask
export EMACS ?= emacs
EMACSFLAGS =
TESTFLAGS =

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

elpaclean:
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

compile: $(OBJS)

clean:
	rm -f $(OBJS)

test: $(PGKDIR)
	$(CASK) exec buttercup -L .
