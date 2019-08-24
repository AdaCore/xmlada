TESTS= test_unicode \
		 test_state \
		 test_xml \
		 test_adacore_dom \
		 test_schema \
		 test_date_time \
		 test_numbers \
		 test_adacore_schema \
		 test_multiple_xsd \
		 test_examples_dom \
		 test_examples_sax \
		 test_examples_schema \
		 test_base64


# Force running all the tests
.PHONY: ${TESTS}

tests: ${TESTS}

EXECDIR=./obj/
RESULTS=$(shell pwd)/results
EXAMPLE_EXPECTED=$(shell pwd)/examples

test_unicode: EXEC=unicode/test_unicode
test_unicode: generic_test_unicode

test_base64: EXEC=base64/test_base64
test_base64: generic_test_base64

test_state: EXEC=sax/teststate
test_state: generic_test_state

test_xml: EXEC=dom/testxml
test_xml: ARGS=-auto
test_xml: generic_test_xml

test_schema: EXEC=schema/schematest
test_schema: ARGS=--xsd10
test_schema: generic_test_schema

test_date_time: EXEC=schema/test_date_time
test_date_time: generic_test_date_time

test_numbers: EXEC=schema/testnumbers
test_numbers: generic_test_numbers

test_adacore_dom: EXEC=adacore/dom/adacore_dom.sh
test_adacore_dom: EXECDIR=./

test_adacore_schema: EXEC=adacore/schema/adacore_schema.sh
test_adacore_schema: EXECDIR=./

ifneq ($(wildcard adacore/.*),)
test_adacore_dom: generic_test_adacore_dom
test_adacore_schema: generic_test_adacore_schema
else
test_adacore_dom: generic_not_run_adacore_dom
test_adacore_schema: generic_not_run_adacore_schema
endif

generic_test_%: BASE=$(notdir $(EXEC))
generic_test_%: DIR=$(dir $(EXEC))
generic_test_%:
	@echo "$(EXEC)"
	@cd $(DIR); \
		mkdir -p obj; \
		$(EXECDIR)$(BASE) $(ARGS) > obj/$(BASE).tmp_out; \
		tr -d '\r' < obj/$(BASE).tmp_out > $(RESULTS)/$(BASE).out ; \
		diff $(BASE).expected $(RESULTS)/$(BASE).out > $(RESULTS)/$(BASE).diff; \
	  	if test -s $(RESULTS)/$(BASE).diff; then \
		   echo "DIFF"; \
			cat $(RESULTS)/$(BASE).diff ; \
		else \
		   echo "OK"; \
		fi

generic_not_run_%:
	@echo "$(EXEC) skipped"

test_multiple_xsd:
	@echo "multiple xsd"
	@cd schema/multiple_xsd; \
		gprbuild -q -Pdefault.gpr; \
		./validate > $(RESULTS)/multiple_xsd.out; \
		cp $(RESULTS)/multiple_xsd.out $(RESULTS)/multiple_xsd.diff; \
		cat $(RESULTS)/multiple_xsd.diff

test_examples_dom:
	@echo "test examples dom"
	@cd ../docs/dom; \
		gprbuild -q -p -Pdefault.gpr 2> $(RESULTS)/domexample.out; \
		./domexample >> $(RESULTS)/domexample.out; \
		cp $(RESULTS)/domexample.out $(RESULTS)/domexample.diff; \
		cat $(RESULTS)/domexample.diff; \
		./domexample2 > $(RESULTS)/domexample2.out; \
		diff -b $(EXAMPLE_EXPECTED)/domexample2.expected $(RESULTS)/domexample2.out > $(RESULTS)/domexample2.diff; \
		if test -s $(RESULTS)/domexample2.diff; then \
			cat $(RESULTS)/domexample2.diff; fi; \
		./domschemaexample > $(RESULTS)/domschemaexample.out; \
		cp $(RESULTS)/domschemaexample.out $(RESULTS)/domschemaexample.diff; \
		cat $(RESULTS)/domschemaexample.diff

test_examples_sax:
	@echo "test examples sax"
	@cd ../docs/sax; \
		gprbuild -q -p -Pdefault.gpr 2> $(RESULTS)/saxexample_main.out; \
		./saxexample_main >> $(RESULTS)/saxexample_main.out; \
		diff -b $(EXAMPLE_EXPECTED)/saxexample_main.expected $(RESULTS)/saxexample_main.out > $(RESULTS)/saxexample_main.diff; \
		if test -s $(RESULTS)/saxexample_main.diff; then \
			cat $(RESULTS)/saxexample_main.diff; \
		fi

test_examples_schema:
	@echo "test examples schema"
	@cd ../docs/schema; \
		gprbuild -q -p -Pdefault.gpr 2> $(RESULTS)/schemaexample.out; \
		./schemaexample >> $(RESULTS)/schemaexample.out; \
		cp $(RESULTS)/schemaexample.out $(RESULTS)/schemaexample.diff; \
		cat $(RESULTS)/schemaexample.diff
