mkdir tests; cd tests
git clone git@github.com:c-testsuite/c-testsuite --depth 1
cd c-testsuite/tests/single-exec
for f in "./"*.tags; do if grep -q c89 $f; then cp ${f%.tags} ../../..; fi; done
