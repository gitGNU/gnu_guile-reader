TESTS = scheme-reader.scm position.scm library.scm confinement.scm
EXTRA_DIST = $(TESTS) benchmark.scm

if HAVE_GUILE2

TESTS += unicode.scm

endif HAVE_GUILE2

EXTRA_DIST += unicode.scm


TESTS_ENVIRONMENT = top_builddir=$(top_builddir)

CLEANFILES = ,,confined-module.scm

### arch-tag: 5b846980-7663-4fa8-bf89-3041747c738b
