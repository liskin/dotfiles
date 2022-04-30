.PHONY: all
all: elixir elixir-ls

.PHONY: elixir
elixir:
	$(MAKE) -C $@

.PHONY: elixir-ls
elixir-ls: VER=$(shell unset GIT_DIR && git -C elixir-ls describe --tags)
elixir-ls: elixir
	[ "$(VER)" ]
	[ "$$(readlink elixir-ls-rel)" = elixir-ls-rel-$(VER) ] || { \
		cd elixir-ls && \
		mix deps.get && \
		mix elixir_ls.release -o ../elixir-ls-rel-$(VER) && \
		ln -fsT elixir-ls-rel-$(VER) ../elixir-ls-rel; \
	}
