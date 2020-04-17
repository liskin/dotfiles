if &cp || exists('g:loaded_projectrc')
	finish
endif

let g:loaded_projectrc = 1

let s:projectrc_dir = expand("~/.vim/projectrc/")
if !isdirectory(s:projectrc_dir)
	call mkdir(s:projectrc_dir, "p")
endif

function! s:projectrc_filenames() abort
	let paths = ale#path#Upwards(getcwd())
	return map(paths, {_, d -> s:projectrc_dir . fnamemodify(d, ":p:gs%[^A-Za-z0-9]%_%")})
endfunc

function! s:load_projectrc() abort
	for projectrc in reverse(s:projectrc_filenames())
		if file_readable(projectrc)
			execute "source" fnameescape(projectrc)
		endif
	endfor
endfunc

function! s:edit_projectrc() abort
	let projectrc = s:projectrc_filenames()[0]
	execute "split" fnameescape(projectrc)
endfunc

command! LoadProjectRC call s:load_projectrc()
command! EditProjectRC call s:edit_projectrc()

augroup ProjectRC
	autocmd BufNewFile,BufRead ~/.vim/projectrc/* setf vim
augroup END

LoadProjectRC
