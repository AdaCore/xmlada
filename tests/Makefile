TESTS= test_unicode \
		 test_state \
		 test_xml \
		 test_adacore_dom \
		 test_schema \
		 test_date_time \
		 test_numbers \
		 test_adacore_schema

# Force running all the tests
.PHONY: ${TESTS}

tests: ${TESTS}

EXECDIR=./obj/
RESULTS=$(shell pwd)/results

test_unicode: EXEC=unicode/test_unicode
test_unicode: generic_test_unicode

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
