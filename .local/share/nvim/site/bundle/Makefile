.PHONY: all
all:

all: CopilotChat.nvim/lua/tiktoken_core.so
CopilotChat.nvim/lua/tiktoken_core.so: CopilotChat.nvim/build/tiktoken_core.so
	ln -sfr $< $@

CopilotChat.nvim/build/tiktoken_core.so:
	$(MAKE) -C CopilotChat.nvim build/tiktoken_core.so
