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

.PHONY: compile
compile: cask
	! (cask eval "(let ((byte-compile-error-on-warn t)) \
	                 (cask-cli/build))" 2>&1 \
	   | egrep -a "(Warning|Error):") ; \
	  (ret=$$? ; cask clean-elc && exit $$ret)
