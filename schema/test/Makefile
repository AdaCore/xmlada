.PHONY: tests old new force

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

old:
	@./testsuite

new:
	@./schematest > schematest.tmp.out
	-@dos2unix schematest.tmp.out
	@diff -c schematest.out schematest.tmp.out
force:
