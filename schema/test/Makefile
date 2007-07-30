.PHONY: tests old new force

tests: old new

old:
	@./testsuite

new:
	@./schematest > schematest.tmp.out
	diff schematest.out schematest.tmp.out
	# Always print the number of tests
	tail -1 schematest.tmp.out

force:
