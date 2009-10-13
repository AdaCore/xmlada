.PHONY: tests old new force

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

old:
	@./testsuite

new:
	@./testsuite -a
	@./schematest > schematest.tmp.out
	@tr -d '\r' < schematest.tmp.out > schematest.tmp
	@diff -c schematest.out schematest.tmp
force:
