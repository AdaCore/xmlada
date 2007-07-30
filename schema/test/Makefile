.PHONY: tests old new force

tests: new old

old:
	./testsuite

new:
	./schematest > schematest.tmp.out && diff schematest.out schematest.tmp.out

force:
