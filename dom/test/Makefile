
ifeq (${USE_VALGRIND},1)
VALGRIND=valgrind --leak-check=full 
else
VALGRIND=
endif

tests: force
	@${VALGRIND} ./testxml -auto -silent
	@cd act_testsuite; ./run

force:
