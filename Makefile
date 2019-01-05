CASK = cask
export EMACS ?= emacs
EMACSFLAGS =
TESTFLAGS =

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean elpa

all: compile

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

elpaclean:
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

compile: elpa
	$(CASK) build

clean:
	rm -f $(OBJS)

test: $(PGKDIR)
	$(CASK) exec buttercup -L .
