elisp_files := $(wildcard *.el)
linting_files := $(filter-out %-autoloads.el,$(elisp_files))

install:
	@echo "Installing project dependencies..."
	@cask install
	@echo "Done"

lint:
	@echo "Linting elisp files with elisp-lint..."
	@cask exec emacs -Q \
		--batch -l elisp-lint.el \
		-f elisp-lint-files-batch \
		$(linting_files)
	@echo "Done"
