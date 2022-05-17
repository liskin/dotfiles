" Set a global project root for rust-analyzer to avoid starting a separate
" instance for dependencies. Assumes a single project, but that's how I use
" vim anyway.
let g:ale_root = {'analyzer': fnamemodify(findfile('Cargo.toml', fnameescape(getcwd()) . ';') ?? './Cargo.toml', ':p:h')}

AleAddLinter rust analyzer
AleAddFixer rust rustfmt
