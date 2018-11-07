project = emidje
elisp_files = $(wildcard *.el)
linting_files = $(filter-out %-autoloads.el,$(elisp_files))
autoload_files = $(wildcard *autoloads.el*)
objects = $(wildcard *elc)

.cask:
	@echo "Installing project dependencies..."
	@cask install
	@echo "Done"

install: .cask

lint: install
	@echo "Linting elisp files with elisp-lint..."
	@cask exec emacs -Q \
		--batch -l elisp-lint.el \
		-f elisp-lint-files-batch \
		$(linting_files)
	@echo "Done"

autoloads:
	@echo "Generating autoloads for $(project)..."
	@cask exec emacs -Q --batch \
		-l autoload.el \
		--eval "(let ((generated-autoload-file (expand-file-name \"$(project)-autoloads.el\"))) \
		  (update-directory-autoloads (expand-file-name \".\")))"
	@echo "Done"

package: autoloads
	@echo "Packaging $(project) $(shell cask version)..."
	@cask package
	@echo "Done"

clean:
	@echo "Cleaning project directory..."
	@rm -f $(autoload_files) $(objects)
	@echo "Done"

elpa-clean: clean
	@echo "Cleaning .cask directory..."
	@rm -rf .cask
	@echo "Done"
