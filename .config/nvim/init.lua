-- vim:set path^=./lua:

vim.loader.enable()

-- use 'gf' to jump between files
require 'init.vimrc' -- ~/.vimrc
require 'init.diagnostic'
require 'init.diagnostic_delay'
require 'init.lsp' -- see also 'init.after_lsp'
require 'init.plug_treesitter'
require 'init.plug_trouble'
require 'init.plug_lsp_lines'
require 'init.plug_fidget'
require 'init.plug_cmp'
require 'init.plug_hop'
require 'init.plug_fzf'
require 'init.plug_indent_blankline'
require 'init.plug_presenting'
require 'init.plug_iron'

-- see also 'init/after'
