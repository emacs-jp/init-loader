.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . -l test-init-loader.el -f ert-run-tests-batch-and-exit
