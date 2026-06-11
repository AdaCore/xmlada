TESTS= test_unicode \
		 test_state \
		 test_sax_noextgen \
		 test_sax_noextpar \
		 test_sax_noextdtd \
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

tests: build ${TESTS}

build:
	make -C .. test

EXECDIR=./obj/
RESULTS=$(shell pwd)/results
EXAMPLE_EXPECTED=$(shell pwd)/examples

test_unicode: EXEC=unicode/test_unicode
test_unicode: generic_test_unicode

test_base64: EXEC=base64/test_base64
test_base64: generic_test_base64

test_state: EXEC=sax/teststate
test_state: generic_test_state

test_sax_noextgen:
	@echo "sax/testsax noextgenent.xml --noextgen -nocolor"
	@cd sax; \
		mkdir -p obj; \
		./obj/testsax noextent.xml --noextgen -nocolor > obj/testsax_noextgenent.tmp_out; \
		tr -d '\r' < obj/testsax_noextgenent.tmp_out > $(RESULTS)/testsax_noextgenent.out ; \
		diff noextgenent.expected $(RESULTS)/testsax_noextgenent.out > $(RESULTS)/testsax_noextgenent.diff; \
		if test -s $(RESULTS)/testsax_noextgenent.diff; then \
			echo "DIFF"; \
			cat $(RESULTS)/testsax_noextgenent.diff ; \
		else \
			echo "OK"; \
		fi

test_sax_noextpar:
	@echo "sax/testsax noextent.xml --noextpar -nocolor"
	@cd sax; \
		mkdir -p obj; \
		./obj/testsax noextent.xml --noextpar -nocolor > obj/testsax_noextpar.tmp_out; \
		tr -d '\r' < obj/testsax_noextpar.tmp_out > $(RESULTS)/testsax_noextpar.out ; \
		diff noextparent.expected $(RESULTS)/testsax_noextpar.out > $(RESULTS)/testsax_noextpar.diff; \
		if test -s $(RESULTS)/testsax_noextpar.diff; then \
			echo "DIFF"; \
			cat $(RESULTS)/testsax_noextpar.diff ; \
		else \
			echo "OK"; \
		fi

test_sax_noextdtd:
	@echo "sax/testsax noextdtd.xml --noextdtd -nocolor"
	@cd sax; \
		mkdir -p obj; \
		./obj/testsax noextdtd.xml --noextdtd -nocolor > obj/testsax_noextdtd.tmp_out; \
		tr -d '\r' < obj/testsax_noextdtd.tmp_out > $(RESULTS)/testsax_noextdtd.out ; \
		diff noextdtd.expected $(RESULTS)/testsax_noextdtd.out > $(RESULTS)/testsax_noextdtd.diff; \
		if test -s $(RESULTS)/testsax_noextdtd.diff; then \
			echo "DIFF"; \
			cat $(RESULTS)/testsax_noextdtd.diff ; \
		else \
			echo "OK"; \
		fi

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
		./validate > $(RESULTS)/multiple_xsd.out; \
		cp $(RESULTS)/multiple_xsd.out $(RESULTS)/multiple_xsd.diff; \
		cat $(RESULTS)/multiple_xsd.diff

test_examples_dom:
	@echo "test examples dom"
	@cd ../docs/dom; \
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
		./saxexample_main >> $(RESULTS)/saxexample_main.out; \
		diff -b $(EXAMPLE_EXPECTED)/saxexample_main.expected $(RESULTS)/saxexample_main.out > $(RESULTS)/saxexample_main.diff; \
		if test -s $(RESULTS)/saxexample_main.diff; then \
			cat $(RESULTS)/saxexample_main.diff; \
		fi

test_examples_schema:
	@echo "test examples schema"
	@cd ../docs/schema; \
		./schemaexample >> $(RESULTS)/schemaexample.out; \
		cp $(RESULTS)/schemaexample.out $(RESULTS)/schemaexample.diff; \
		cat $(RESULTS)/schemaexample.diff
