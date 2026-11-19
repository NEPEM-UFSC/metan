R ?= Rscript

.PHONY: help setup check-env document test check site

help:
	@echo "Targets:"
	@echo "  setup      Install development dependencies"
	@echo "  check-env  Verify the R environment"
	@echo "  document   Regenerate man/ and NAMESPACE with roxygen2"
	@echo "  test       Run the testthat suite"
	@echo "  check      Run R CMD check"
	@echo "  site       Build the pkgdown site into docs/"

setup:
	$(R) tools/setup.R

check-env:
	$(R) tools/check-env.R

document:
	$(R) -e 'devtools::document()'

test:
	$(R) -e 'devtools::test()'

check:
	$(R) -e 'devtools::check()'

site:
	$(R) -e 'pkgdown::build_site()'
