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
require 'init.plug_conform'
require 'init.plug_fidget'
require 'init.plug_cmp'
require 'init.plug_snippy'
require 'init.plug_hop'
require 'init.plug_fzf'
require 'init.plug_indent_blankline'
require 'init.plug_presenting'
require 'init.plug_ferris'
require 'init.plug_lazydev'
require 'init.plug_zenmode'
require 'init.plug_zeal'
require 'lz.n'.load 'init.lazy'

-- see also 'init.after'
