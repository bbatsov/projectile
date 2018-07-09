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

compile: $(OBJS)

clean:
	rm -f $(OBJS)

test: $(PKGDIR)
	$(CASK) exec ert-runner $(TESTFLAGS)
