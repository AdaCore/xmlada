MAIN := convert
UNICODE_FILES := Blocks.txt NameAliases.txt UnicodeData.txt

.PHONY: all clean ${MAIN}

all: $(MAIN) $(UNICODE_FILES)
	mkdir -p generated
	./$^

$(MAIN):
	gprbuild -p -Pconvert

$(filter-out NameAliases.txt,$(UNICODE_FILES)):
	wget ftp://ftp.unicode.org/Public/8.0.0/ucd/$@
NameAliases.txt:
	wget ftp://ftp.unicode.org/Public/8.0.0/ucd/$@
	patch -p0 < NameAliases.diff

clean::
	rm -f $(UNICODE_FILES)
	gprclean -Pconvert
	rm -f unicode-names-*.ads
