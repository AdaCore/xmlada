
FORMATS=${addprefix xml.,ps html info txt}

all: ${FORMATS}

%.dvi: %.texi
	echo x | tex $<
	texindex ${<:%.texi=%.cp}
	echo x | tex $<

%.ps: %.dvi
	dvips -o $@ $<

%.html: %.texi
	texi2html -glossary -menu -split_chapter -number $<

%.info: %.texi
	makeinfo $<

%.txt: %.texi
	makeinfo -o $@ --number-sections $<

clean: force
	@${RM} *.cp
	@${RM} *.aux
	@${RM} *.cps
	@${RM} *.fn
	@${RM} *.ky
	@${RM} *.log
	@${RM} *.pg
	@${RM} *.toc
	@${RM} *.tp
	@${RM} *.vr

clean_all: clean
	@${RM} ${FORMATS} *.html

force:
