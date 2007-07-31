.PHONY: tests old new force

tests: all old new

all:
	-@./test_date_time
	-@./testnumbers	

old:
	@./testsuite

new:
	@./schematest > schematest.tmp.out
	-@diff -c schematest.out schematest.tmp.out
	@tail -1 schematest.tmp.out
force:
