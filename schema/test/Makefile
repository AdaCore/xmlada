.PHONY: tests old new force

tests: old new

old:
	@./testsuite

new:
	@./schematest > schematest.tmp.out
	-@diff schematest.out schematest.tmp.out
	@tail -1 schematest.tmp.out

force:
