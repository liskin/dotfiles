if exists("b:loaded_vimwiki_liskin_auto") | finish | endif
let b:loaded_vimwiki_liskin_auto = 1

" taskwiki only follows wikilinks by default, not URLs; this is a workaround
function! vimwiki#liskin#follow_link()
	let syn_stack = map(synstack(line('.'), col('.')), {_, s -> synIDattr(s, "name")})
	let inside_link = index(syn_stack, 'VimwikiLink') != -1
	if inside_link
		VimwikiFollowLink
	else
		py3 Mappings.task_info_or_vimwiki_follow_link()
	endif
endfunction

function! vimwiki#liskin#refresh() abort
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

function! vimwiki#liskin#refresh_if_safe() abort
	if mode(1) ==# 'n' && !&modified
		call vimwiki#liskin#refresh()
	endif
endfunction
