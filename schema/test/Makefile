.PHONY: tests new force

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

# Do not hide Passed tests, since we want to compare error messages
# The W3C testsuite is meant to test XSD 1.0, so we have to configure the parser
# in the appropriate mode.
new:
	-@./testsuite > testsuite.tmp.out 2>&1
	-@tr -d '\r' < testsuite.tmp.out > testsuite.tmp
	-@diff -c testsuite.out testsuite.tmp
	#-@./schematest --xsd10 > schematest.tmp.out
	-@./schematest --xsd10 -f
	-@tr -d '\r' < schematest.tmp.out > schematest.tmp
	-@diff -c schematest.out schematest.tmp
force:
