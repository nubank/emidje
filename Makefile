project = emidje
elisp_files = $(wildcard *.el)
linting_files = $(filter-out %-autoloads.el,$(elisp_files))
autoload_files = $(wildcard *autoloads.el*)
objects = $(wildcard *elc)
version = $(shell cask version)
dist = dist/emidje-$(version)
package = $(wildcard $(dist)/emidje-*.tar)
deployable := $(wildcard dist/emidje-*.tar.gz)

.cask:
	@echo "Installing project dependencies..."
	@cask install
	@echo "Done"

install: .cask

lint: install
	@echo "Linting elisp files with elisp-lint..."
	@cask exec emacs -Q --batch \
		-l elisp-lint.el \
		-f elisp-lint-files-batch \
		$(linting_files)
	@echo "Linting elisp files with package-lint..."
	@cask exec emacs -Q -batch \
		--eval "(progn (require 'package) \
			(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives) \
			(package-initialize))" \
	-l package-lint.el \
	-f package-lint-batch-and-exit \
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
	@mkdir -p $(dist)
	@cask package $(dist)
	@echo "Done"

install-package: package
	@echo "Installing $(package)..."
	@cask exec emacs -Q --batch \
		-l package.el \
		--eval "(prog2 (package-initialize) \
			(package-install-file \"$(shell pwd)/$(package)\"))"
	@echo "Done"

release: lint package
	@echo "Releasing Emidje version $(version)..."
	@./bin/release.sh $(version) && \
		cd dist; tar -zcvf emidje-$(version).tar.gz emidje-$(version) && \
		hub release create -a $(deployable) -m "Release version $(version). An extensive changelog is available at https://github.com/nubank/emidje/blob/master/CHANGELOG.md." $(version)
	@echo "Done"

clean:
	@echo "Cleaning project directory..."
	@rm -f $(autoload_files) $(objects)
	@echo "Done"

clean-all: clean
	@echo "Cleaning .cask directory..."
	@rm -rf .cask
	@echo "Cleaning dist directory..."
	@rm -rf dist
	@echo "Done"
