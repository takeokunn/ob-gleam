EMACS ?= emacs

BATCH = $(EMACS) -Q --batch

.PHONY: all compile test lint package-lint clean

all: compile

compile:
	$(BATCH) \
	  --eval "(require 'org)" \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile ob-gleam.el

test:
	$(BATCH) \
	  --eval "(require 'org)" \
	  -l ob-gleam.el \
	  -l test/ob-gleam-test.el \
	  -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) \
	  --eval "(require 'org)" \
	  -l ob-gleam.el \
	  --eval "(checkdoc-file \"ob-gleam.el\")"

package-lint:
	$(BATCH) \
	  --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (package-refresh-contents) (package-install 'package-lint))" \
	  -l package-lint \
	  -f package-lint-batch-and-exit ob-gleam.el

clean:
	rm -f *.elc
