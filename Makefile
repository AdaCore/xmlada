
# Modify the line below to reflect the location where
# you want to install the library

PREFIX=


# List of all modules to compile
# You can remove "dom" if you don't intend to use the DOM interface
MODULES=unicode input_sources state_machine sax dom

##############################
##  Main Makefile: three targets are available:
##    obj:   build all objects (no executable)
##    test:  build the test programs found in the modules
##    clean: remove all object files and temporary files
##    install: install all the files under the PREFIX hierarchy, as
##           defined in Makefile.module
##############################

MODULE_OBJ=${MODULES:%=-aO%/obj}
MODULE_SRC=${MODULES:%=-aI%}
MODULE_CLEAN=${MODULES:%=%_clean}
MODULE_TEST=${MODULES:%=%_test}

RANLIB=ranlib
CHMOD=chmod
ARFLAGS=cr
CP=cp -p -f
MKDIR=mkdir -p

.PHONY: all obj test clean install

all: obj test docs
obj: ${MODULES}

docs: force
	${MAKE} -C docs

${MODULES}: force
	${MAKE} -C $@ all_obj

${MODULE_CLEAN}: force
	${MAKE} -C ${@:%_clean=%} clean

${MODULE_TEST}: force
	${MAKE} -C ${@:%_test=%} test

${MODULE_INSTALL}: force
	${MAKE} -C ${@:%_install=%} install

install: ${MODULES}
	@${MKDIR} ${PREFIX}/lib
	@${MKDIR} ${PREFIX}/include/xmlada
	# Building the libraries
	${AR} ${ARFLAGS} ${PREFIX}/lib/libxml.a ${MODULES:%=%/obj/*.o}
	@if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  ${RANLIB} ${PREFIX}/lib/libxml.a; \
        fi
	gcc -shared -o ${PREFIX}/lib/libxml.so ${MODULES:%=%/obj/*.o}
	# Installing the files
	${CP} ${MODULES:%=%/obj/*.ali} ${PREFIX}/lib/
	cd ${PREFIX}/lib; ${CHMOD} -w *.ali
	${CP} ${MODULES:%=%/*.ad[bs]} ${PREFIX}/include/xmlada/

test: force
	${MAKE} ${MODULE_TEST}

clean: force
	${RM} *.o *.ali testxml b~*
	${MAKE} ${MODULE_CLEAN}
	cd docs; make clean

force:
