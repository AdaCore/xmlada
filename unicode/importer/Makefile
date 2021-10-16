MAIN := convert
UNICODE_VERSION := 14.0.0
UNICODE_URL := ftp://ftp.unicode.org/Public/$(UNICODE_VERSION)/ucd
UNICODE_FILES := Blocks.txt NameAliases.txt UnicodeData.txt

.PHONY: all clean ${MAIN}

all: $(MAIN) $(UNICODE_FILES)
	mkdir -p generated
	./$^

$(MAIN):
	gprbuild -p -Pconvert

$(UNICODE_FILES):
	wget $(UNICODE_URL)/$@

clean:
	rm -f $(UNICODE_FILES)
	gprclean -Pconvert
	rm -rf generated/
