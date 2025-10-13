; fix for:
; https://github.com/nvim-treesitter/nvim-treesitter/issues/6530
; https://github.com/nvim-treesitter/nvim-treesitter/pull/6574

((comment) @injection.content
  (#set! injection.language "comment"))

((shell_command) @injection.content
  (#set! injection.language "bash")
  (#set! injection.include-children))

((run_instruction
  (heredoc_block) @injection.content)
  (#set! injection.language "bash")
  (#set! injection.include-children))
