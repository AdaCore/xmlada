.PHONY: tests new force

# Set this to "acroread" if you have it installed on your machine
PDFVIEWER=okular

tests: all new

all:
	-@./obj/test_date_time
	-@./obj/testnumbers	

# Check whether the W3C tests are available
check_w3c_tests:
	@test -d xmlschema2006-11-06

# Do not hide Passed tests, since we want to compare error messages
# The W3C testsuite is meant to test XSD 1.0, so we have to configure the parser
# in the appropriate mode.
new: check_w3c_tests
	@if [ -d ../adacore/schema ]; then cd ../adacore/schema; sh ./run.sh; else echo "Schema adacore directory not found (ignored)"; fi
	-@./obj/schematest --xsd10 > obj/test_schema.tmp_out
	-@tr -d '\r' < obj/test_schema.tmp_out > obj/test_schema.out
	-@diff -u test_schema.expected obj/test_schema.out > obj/test_schema.diff
	@cat obj/test_schema.diff

# Explain how the various errors are split:
errors:
	@egrep "^  -" test_schema.expected | perl -pi -e  's/\S+:\d+:\d+://g; s/^\s+//' | sort | uniq -c | sort -n

# Display the NFA used for validation. This requires the external tool
# "graphviz" to be installed on your machine.
# To use:
#     ./testschema -xsd <xsdfile> -debug > foo
#     make dot_schema
# Note that this does not include the XSD metaschema (which is a big NFA, that
# pdf viewers have trouble to display). Substitute "tail" for "head" if you
# still want to see it

LOGFILE=foo

dot_schema:
	grep digraph ${LOGFILE} | tail -1 > ${LOGFILE}.dot
	dot -O -Tpdf ${LOGFILE}.dot
	${PDFVIEWER} ${LOGFILE}.dot.pdf

clean_dot:
	${RM} ${LOGFILE} ${LOGFILE}.dot ${LOGFILE}.dot.pdf

force:
