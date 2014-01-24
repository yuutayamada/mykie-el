CASK ?= cask
EMACS ?= emacs

all: test

test:
	${MAKE} unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

clean:
	rm -f mykie.elc

.PHONY:	all test unit install
