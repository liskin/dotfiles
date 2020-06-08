if exists("b:did_ftplugin_taskwiki_after") | finish | endif
let b:did_ftplugin_taskwiki_after = 1

nnoremap <silent><buffer> <C-]> :VimwikiFollowLink<CR>
nnoremap <silent><buffer> <C-W><C-]> :VimwikiSplitLink<CR>

function! s:refresh() abort
	let data_location = py3eval("cache().get_relevant_tw().config.get('data.location')")
	let task_data_mod = map(glob(data_location . "/*.data", 1, 1), {_, f -> getftime(f)})
	let file_mod = getftime(expand("%"))
	let older_than_task_data = !empty(filter(task_data_mod, {_, data_mod -> data_mod > file_mod}))
	let older_than_today = strptime("%F", strftime("%F")) > file_mod
	if older_than_task_data || older_than_today
		TaskWikiBufferLoad
		write
	endif
endfunction

function! s:refresh_if_safe() abort
	if mode(1) ==# 'n' && !&modified
		call s:refresh()
	endif
endfunction

augroup taskwikiRefresh
	autocmd! * <buffer>
	autocmd FocusGained <buffer> call s:refresh_if_safe()
	autocmd BufEnter <buffer> call timer_start(0, {-> s:refresh_if_safe()})
augroup END

call timer_start(0, {-> s:refresh()})
