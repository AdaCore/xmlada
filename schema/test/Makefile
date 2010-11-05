.PHONY: tests new force

# Set this to "acroread" if you have it installed on your machine
PDFVIEWER=okular

tests: all new

all:
	-@./test_date_time
	-@./testnumbers	

# Do not hide Passed tests, since we want to compare error messages
# The W3C testsuite is meant to test XSD 1.0, so we have to configure the parser
# in the appropriate mode.
new:
	-@./testsuite > testsuite.tmp.out 2>&1
	-@tr -d '\r' < testsuite.tmp.out > testsuite.tmp
	-@diff -c testsuite.out testsuite.tmp
	-@./schematest --xsd10 > schematest.tmp.out
	-@tr -d '\r' < schematest.tmp.out > schematest.tmp
	-@diff -u schematest.out schematest.tmp

# Explain how the various errors are split:
errors:
	@egrep "^  -" schematest.out | perl -pi -e  's/\S+:\d+:\d+://g; s/^\s+//' | sort | uniq -c | sort -n

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
