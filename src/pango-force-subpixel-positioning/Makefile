BUILD := _build

.PHONY: build
build: $(BUILD)/build.ninja
	ninja -C _build all patchelf

$(BUILD)/build.ninja: | meson.build
	meson $(BUILD)

.PHONY: clean
clean:
	$(RM) -r $(BUILD)
