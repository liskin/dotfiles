.PHONY: all
all: cv.selfcont.html cv.print.html cv.pdf

PUBLIC ?=

PANDOC ?= pandoc
PANDOC_FLAGS ?=

PANDOC_FLAGS_COMMON=\
	--standalone \
	--from=markdown \
	--to=html \
	--strip-comments \
	--lua-filter=pandoc-meta-variables.lua \
	--metadata-file=meta-defaults.yaml \
	--metadata-file=meta-$(if $(PUBLIC),public,private).yaml \
	$(PANDOC_FLAGS)
PANDOC_INPUTS_COMMON=\
	pandoc-meta-variables.lua \
	meta-public.yaml \
	meta-private.yaml

%.html: %.md Makefile $(PANDOC_INPUTS_COMMON)
	$(PANDOC) $(PANDOC_FLAGS_COMMON) --output=$@ $<

PANDOC_FLAGS_SELFCONT=\
	$(PANDOC_FLAGS_COMMON) \
	--embed-resources
PANDOC_INPUTS_SELFCONT=\
	$(PANDOC_INPUTS_COMMON) \
	style.css

%.selfcont.html: %.md Makefile $(PANDOC_INPUTS_SELFCONT)
	$(PANDOC) $(PANDOC_FLAGS_SELFCONT) --output=$@ $<

PANDOC_FLAGS_PRINT=\
	$(PANDOC_FLAGS_COMMON) \
	--embed-resources \
	--metadata-file=meta-print.yaml
PANDOC_INPUTS_PRINT=\
	$(PANDOC_INPUTS_COMMON) \
	meta-print.yaml \
	style-print.css \
	style.css

%.print.html: %.md Makefile $(PANDOC_INPUTS_PRINT)
	$(PANDOC) $(PANDOC_FLAGS_PRINT) --output=$@ $<

# needs:
#   apt install node-playwright
%.pdf: %.print.html Makefile
	$(RM) $@
	-bwrap-home --tmpfs -- \
		./playwright/pdf.mjs file://$(CURDIR)/$< $@
	pdfinfo $@ > /dev/null

.PHONY: clean
clean:
	$(RM) -r $(foreach md,$(basename $(wildcard *.md)),$(wildcard $(md).html $(md).selfcont.html $(md).print.html $(md).pdf))
