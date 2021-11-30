all: cv.html

PUBLIC ?=

private.yaml: | public.yaml
	cat $| >>$@

PANDOC ?= pandoc
PANDOC_FLAGS ?=

PANDOC_FLAGS_COMMON=\
	--standalone \
	--from=markdown \
	--to=html \
	--strip-comments \
	--lua-filter=pandoc-meta-variables.lua \
	--metadata-file=meta-footer.yaml \
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
	--self-contained
PANDOC_INPUTS_SELFCONT=\
	$(PANDOC_INPUTS_COMMON) \
	style.css

%.selfcont.html: %.md Makefile $(PANDOC_INPUTS_SELFCONT)
	$(PANDOC) $(PANDOC_FLAGS_SELFCONT) --output=$@ $<

PANDOC_FLAGS_PRINT=\
	$(PANDOC_FLAGS_COMMON) \
	--self-contained \
	--metadata-file=meta-print.yaml
PANDOC_INPUTS_PRINT=\
	$(PANDOC_INPUTS_COMMON) \
	meta-print.yaml \
	style.css

%.print.html: %.md Makefile $(PANDOC_INPUTS_PRINT)
	$(PANDOC) $(PANDOC_FLAGS_PRINT) --output=$@ $<

CHROME=/usr/bin/google-chrome
CHROME_DIR=./.chrometmp

%.pdf: %.print.html Makefile
	$(CHROME) \
		--user-data-dir=$(CHROME_DIR) \
		--crash-dumps-dir=$(CHROME_DIR) \
		--enable-logging=stderr \
		--log-level=2 \
		--virtual-time-budget=10000 \
		--headless \
		--print-to-pdf-no-header \
		--print-to-pdf=$@ \
		$<
	pdf-drop-blank-pages $@
