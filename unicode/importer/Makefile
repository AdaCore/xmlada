MAIN := convert
UNICODE_VERSION := 13.0.0
UNICODE_URL := ftp://ftp.unicode.org/Public/$(UNICODE_VERSION)/ucd
UNICODE_FILES := Blocks.txt NameAliases.txt UnicodeData.txt

.PHONY: all clean ${MAIN}

all: $(MAIN) $(UNICODE_FILES)
	mkdir -p generated
	./$^

$(MAIN):
	gprbuild -p -Pconvert

$(filter-out NameAliases.txt,$(UNICODE_FILES)):
	wget $(UNICODE_URL)/$@
NameAliases.txt:
	wget $(UNICODE_URL)/$@
	patch -p0 < NameAliases.diff

clean:
	rm -f $(UNICODE_FILES)
	gprclean -Pconvert
	rm -rf generated/
