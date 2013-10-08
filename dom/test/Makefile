tests: force
	@./testxml -auto > test_xml.tmp_out
	-@tr -d '\r' < test_xml.tmp_out > test_xml.out
	@diff test_xml.expected test_xml.out > test_xml.diff
	@cat test_xml.diff
	@if [ -d act_testsuite ]; then cd act_testsuite; ./run > test_act.out; else echo "DOM act_test_suite directory not found (expected)"; fi

force:
