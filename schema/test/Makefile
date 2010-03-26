.PHONY: tests old new force

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

old:
	@./testsuite

# Do not hide Passed tests, since we want to compare error messages
# The W3C testsuite is meant to test XSD 1.0, so we have to configure the parser
# in the appropriate mode.
new:
	-@./testsuite -a > testsuite.tmp.out 2>&1
	-@tr -d '\r' < testsuite.tmp.out > testsuite.tmp
	-@diff -c testsuite.out testsuite.tmp
	-@./schematest --feature xsd_1_0 > schematest.tmp.out
	-@tr -d '\r' < schematest.tmp.out > schematest.tmp
	-@diff -c schematest.out schematest.tmp
force:
