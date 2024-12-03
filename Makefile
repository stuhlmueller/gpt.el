export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: lint
lint: cask
	cask exec emacs --quick --batch --directory . --eval "(require 'package-lint)" --eval '(setq package-lint-main-file "le-gpt.el")' -f package-lint-batch-and-exit *.el


.PHONY: checkdoc
checkdoc: cask
	cask exec emacs -Q -batch --eval '(dolist (file (directory-files "." t "\\.el$$")) (checkdoc-file file))'

.PHONY: compile
compile: clean cask
	cask exec emacs -Q -L . -batch -f batch-byte-compile *.el

.PHONY: clean
clean:
	rm -f *.elc

