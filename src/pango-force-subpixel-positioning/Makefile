BUILD := _build

.PHONY: build
build: $(BUILD)/build.ninja
	env -u LD_LIBRARY_PATH \
		ninja -C _build all patchelf

$(BUILD)/build.ninja: | meson.build
	env -u LD_LIBRARY_PATH \
		meson setup $(BUILD)

.PHONY: clean
clean:
	$(RM) -r $(BUILD)
