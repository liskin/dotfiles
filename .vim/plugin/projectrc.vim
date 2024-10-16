if &cp || exists('g:loaded_projectrc') | finish | endif
let g:loaded_projectrc = 1

let s:projectrc_dir = expand("~/.vim/projectrc/")
if !isdirectory(s:projectrc_dir)
	call mkdir(s:projectrc_dir, "p")
endif

let s:projectrc_untracked_dir = s:projectrc_dir .. ".untracked/"
if !isdirectory(s:projectrc_untracked_dir)
	call mkdir(s:projectrc_untracked_dir, "p")
endif

function! s:path_upwards(path) abort
	let parts = split(a:path, '\v/+')
	let path_list = []

	while !empty(parts)
		call add(path_list, join(parts, '/'))
		let parts = parts[:-2]
	endwhile

	if a:path[0] is# '/'
		call map(path_list, {_, p -> '/' .. p})
		call add(path_list, '/')
	endif

	return path_list
endfunc

function! s:projectrc_filenames() abort
	let paths = s:path_upwards(getcwd())
	let filenames = []
	for path in paths
		let filename = fnamemodify(path, ":p:gs%[^A-Za-z0-9]%_%")
		let filenames += [s:projectrc_untracked_dir .. filename, s:projectrc_dir .. filename]
	endfor
	return filenames
endfunc

function! s:load_projectrc() abort
	augroup ProjectRCLocal
		autocmd!
		for projectrc in reverse(s:projectrc_filenames())
			if file_readable(projectrc)
				execute "source" fnameescape(projectrc)
			endif
		endfor
	augroup END
endfunc

function! s:edit_projectrc(untracked) abort
	let projectrc = s:projectrc_filenames()[a:untracked ? 0 : 1]
	execute "split" fnameescape(projectrc)
endfunc

command! LoadProjectRC call s:load_projectrc()
command! EditProjectRC call s:edit_projectrc(0)
command! EditProjectRCUntracked call s:edit_projectrc(1)

augroup ProjectRC
	autocmd!
	autocmd BufNewFile,BufRead ~/.vim/projectrc/* setf vim
augroup END

" utils for ProjectRCs:
command! -nargs=1 SourceRelative execute "source" (fnameescape(expand('<sfile>:p:h')) .. "/" .. <q-args>)

LoadProjectRC
