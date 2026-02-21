##
# ron.el
#
# @file
# @version 0.1

TESTS = test-ron.el
EMACS =	emacs -batch -l ron.el -l ert

test: test-ron.el
	$(EMACS) -l $(TESTS) -f ert-run-tests-batch-and-exit

# end
