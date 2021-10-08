man_subdir = man$(patsubst .%,%,$(suffix $(basename $(1))))
man_filename = $(call man_subdir,$(1))/$(basename $(1))

MANPAGES = $(foreach MD,$(wildcard *.*.md),$(call man_filename,$(MD)))

.PHONY: all
all: $(MANPAGES)

define MANPAGE_TEMPLATE
$(1): | .man/$(notdir $(1)) $(dir $(1))
	ln -sfr .man/$(notdir $(1)) $$@
endef

$(foreach MANPAGE,$(MANPAGES),$(eval $(call MANPAGE_TEMPLATE,$(MANPAGE))))

.man/%: %.md
	pandoc --standalone --to man $< -o $@

man%:
	mkdir $@

.DELETE_ON_ERROR:
