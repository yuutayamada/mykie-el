CASK ?= cask
EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile lisp/mykie.el

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

clean-elc:
	rm -f lisp/*.elc

.PHONY:	all test unit install
