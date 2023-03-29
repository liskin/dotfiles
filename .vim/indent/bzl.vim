if exists('b:did_indent') | finish | endif

" fix https://github.com/bazelbuild/vim-ft-bzl/issues/16
runtime! indent/python.vim
