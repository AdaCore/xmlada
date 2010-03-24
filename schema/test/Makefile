.PHONY: tests old new force

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

old:
	@./testsuite

new:
	-@./testsuite -a > testsuite.tmp.out 2>&1
	-@tr -d '\r' < testsuite.tmp.out > testsuite.tmp
	-@diff -c testsuite.out testsuite.tmp
	-@./schematest --hide PA > schematest.tmp.out
	-@tr -d '\r' < schematest.tmp.out > schematest.tmp
	-@diff -c schematest.out schematest.tmp
force:
