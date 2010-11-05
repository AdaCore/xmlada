tests: force
	@./testxml -auto > testxml.tmp.out
	-@tr -d '\r' < testxml.tmp.out > testxml.tmp
	@ diff testxml.out testxml.tmp
	@cd act_testsuite; ./run

force:
