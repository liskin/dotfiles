-- vim:set path^=./lua:

vim.loader.enable()

-- use 'gf' to jump between files
require 'init.vimrc' -- ~/.vimrc
require 'init.diagnostic'
require 'init.diagnostic_delay'
require 'init.lsp_config' -- see also ~/.vim/projectrc
require 'init.lsp'
require 'init.plug_treesitter'
require 'init.plug_treesitter_textobjects'
require 'init.plug_treesitter_context'
require 'init.plug_trouble'
require 'init.plug_lsp_format'
require 'init.plug_fidget'
require 'init.plug_cmp'
require 'init.plug_hop'
require 'init.plug_fzf'
require 'init.plug_indent_blankline'
require 'init.plug_presenting'
require 'init.plug_iron'
require 'init.plug_dropbar'
require 'init.plug_copilot_lazy' -- see also 'init.plug_copilot'
require 'init.plug_copilotchat'
require 'init.plug_codecompanion'
require 'init.plug_ferris'
require 'init.plug_lazydev'
require 'init.plug_roslyn'
require 'init.plug_incr'
require 'init.plug_neotest'

-- see also 'init.after'
