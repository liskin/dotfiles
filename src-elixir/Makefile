.PHONY: all
all: elixir elixir-ls

.PHONY: elixir
elixir:
	$(MAKE) -C $@

.PHONY: elixir-ls
elixir-ls: VER=$(shell unset GIT_DIR && git -C elixir-ls describe --tags)
elixir-ls:
	[ "$(VER)" ]
	[ "$$(readlink elixir-ls/rel)" = rel-$(VER) ] || { \
		cd elixir-ls && \
		mix elixir_ls.release -o rel-$(VER) && \
		ln -fs rel-$(VER) rel; \
	}
