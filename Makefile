EMACS ?= emacs

# Directories containing Emacs Lisp source
LISP_DIRS := .

# Gather .el files at the project root (ignoring common generated files)
EL_FILES := $(shell find $(LISP_DIRS) -maxdepth 1 -type f -name '*.el' -not -name 'custom.el' -print)

.PHONY: all check help check-parens check-load byte-compile byte-compile-strict lint lint-verbose test format clean clean-elc clean-eln

# Default target
all: check

# Run the main quality gates
check: check-parens check-load byte-compile lint
	@echo "✓ All checks passed"

help:
	@echo "Available targets:"
	@echo "  all              - Run all checks (default)"
	@echo "  check            - Run parentheses, load, compile, and lint checks"
	@echo "  check-parens     - Ensure all Emacs Lisp files have balanced parentheses"
	@echo "  check-load       - Load each file in batch mode to catch syntax errors"
	@echo "  byte-compile     - Byte-compile all Emacs Lisp files"
	@echo "  byte-compile-strict - Byte-compile with warnings treated as errors"
	@echo "  lint             - Run package-lint (important errors only; skipped if unavailable)"
	@echo "  lint-verbose     - Run package-lint with all warnings"
	@echo "  test             - Run ERT tests if present"
	@echo "  format           - Alias for check-parens"
	@echo "  clean            - Remove compiled artefacts (.elc, .eln)"
	@echo "  clean-elc        - Remove .elc files"
	@echo "  clean-eln        - Remove .eln files"

check-parens:
	@echo "Checking for unbalanced parentheses..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  for file in $(EL_FILES); do \
	    echo "  Checking $$file..."; \
	    $(EMACS) -Q --batch --eval "(let ((f \"$$file\")) (find-file f) (check-parens))"; \
	  done; \
	  echo "✓ Parentheses check passed"; \
	fi

check-load:
	@echo "Checking if files load cleanly..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  for file in $(EL_FILES); do \
	    echo "  Loading $$file..."; \
	    $(EMACS) -Q --batch -L . \
	      --eval "(condition-case err (load-file \"$$file\") (error (progn (princ (format \"Error loading %s: %s\\n\" \"$$file\" err)) (kill-emacs 1))))"; \
	  done; \
	  echo "✓ Load check passed"; \
	fi

format: check-parens

byte-compile:
	@echo "Byte-compiling Emacs Lisp files..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  $(EMACS) -Q --batch -L . -f batch-byte-compile $(EL_FILES); \
	  echo "✓ Byte-compilation completed"; \
	fi

byte-compile-strict:
	@echo "Byte-compiling Emacs Lisp files (warnings treated as errors)..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  $(EMACS) -Q --batch -L . \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    -f batch-byte-compile $(EL_FILES); \
	fi

lint:
	@echo "Running package-lint (important errors only)..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  $(EMACS) -Q --batch \
	    --eval "(require 'package)" \
	    --eval "(setq package-user-dir (expand-file-name \".emacs-packages\"))" \
	    --eval "(package-initialize)" \
	    --eval "(if (require 'package-lint nil t) nil (princ \"package-lint not installed; skipping lint.\n\") (kill-emacs 0))" \
	    -L . \
	    -f package-lint-batch-and-exit $(EL_FILES) && echo "Linting passed"; \
	fi

lint-verbose:
	@echo "Running package-lint (all warnings)..."
	@if [ -z "$(strip $(EL_FILES))" ]; then \
	  echo "  No Emacs Lisp files found."; \
	else \
	  $(EMACS) -Q --batch \
	    --eval "(require 'package)" \
	    --eval "(setq package-user-dir (expand-file-name \".emacs-packages\"))" \
	    --eval "(package-initialize)" \
	    --eval "(if (require 'package-lint nil t) nil (princ \"package-lint not installed; skipping lint.\n\") (kill-emacs 0))" \
	    -L . \
	    --eval "(setq package-lint-batch-fatal nil)" \
	    -f package-lint-batch-and-exit $(EL_FILES); \
	fi

test:
	@if [ -d test ]; then \
	  echo "Running ERT tests..."; \
	  $(EMACS) -Q --batch -L . -L test -l ert \
	    --eval "(mapc (lambda (f) (load f)) (directory-files \"test\" t \".*\\\\.el\"))" \
	    -f ert-run-tests-batch-and-exit; \
	else \
	  echo "No ERT tests found; skipping."; \
	fi

clean-elc:
	@echo "Removing .elc files..."
	@find $(LISP_DIRS) -type f -name '*.elc' -delete || true

clean-eln:
	@echo "Removing .eln files..."
	@find . -type f -name '*.eln' -delete || true

clean: clean-elc clean-eln
