.PHONY : test

test:
	emacs -Q -batch -L . -l test-init-loader.el -f ert-run-tests-batch-and-exit
