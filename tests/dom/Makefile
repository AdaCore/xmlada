tests: force
	@./obj/testxml -auto > obj/test_xml.tmp_out
	-@tr -d '\r' < obj/test_xml.tmp_out > obj/test_xml.out
	@diff test_xml.expected obj/test_xml.out > obj/test_xml.diff
	@cat obj/test_xml.diff
	@if [ -d ../adacore/dom ]; then cd ../adacore/dom; sh ./run.sh; else echo "DOM adacore directory not found (ignored)"; fi

force:
