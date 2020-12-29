if exists("g:loaded_vimwiki_liskin_auto") | finish | endif
let g:loaded_vimwiki_liskin_auto = 1

" taskwiki only follows wikilinks by default, not URLs; this is a workaround
function! vimwiki#liskin#follow_link() abort
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

function! vimwiki#liskin#selected_tasks() abort
	silent return py3eval("[{k:t[k] for k in ['uuid','description']} for t in SelectedTasks().tasks]")
endfunction

function! vimwiki#liskin#task_note_ini_filename(task) abort
	let desc = a:task['description']
	let desc = iconv(desc, "utf-8", "ascii//translit")
	let desc = substitute(desc, "[^A-Za-z0-9]", "-", "g")
	let desc = substitute(desc, "---*", "-", "g")
	return desc
endfunction

function! vimwiki#liskin#strip_ext(file) abort
	if !empty(a:file) && "." . fnamemodify(a:file, ":e") ==# vimwiki#vars#get_wikilocal("ext")
		return fnamemodify(a:file, ":r")
	else
		return a:file
	endif
endfunction

function! vimwiki#liskin#add_ext(file) abort
	return empty(a:file) ? '' : vimwiki#liskin#strip_ext(a:file) . vimwiki#vars#get_wikilocal("ext")
endfunction

function! vimwiki#liskin#note_wikifile(ini) abort
	let file = "notes/" . strftime("%F") . "-" . a:ini

	while 1
		" completion needs g:vimwiki_auto_chdir = 1
		let file = input("Wiki file: ", file, "file")
		if empty(file) || !filereadable(vimwiki#liskin#add_ext(file))
			break
		elseif input("Wiki file " . file . " exists, use anyway? (y/N) ") =~# '^y'
			break
		endif
	endwhile

	return vimwiki#liskin#strip_ext(file)
endfunc

function! vimwiki#liskin#task_note_wikifile(task) abort
	let link = ".notes/" . a:task['uuid']

	let file = resolve(link)
	if filereadable(file)
		return vimwiki#liskin#strip_ext(file)
	endif

	let ini = vimwiki#liskin#task_note_ini_filename(a:task)
	let file = vimwiki#liskin#note_wikifile(ini)
	if !empty(file)
		call mkdir(fnamemodify(link, ":h"), "p")
		" needs g:vimwiki_auto_chdir = 1
		silent call system("ln -f -s -r " . shellescape(vimwiki#liskin#add_ext(file)) . " " . shellescape(link))
	endif

	return file
endfunc

function! vimwiki#liskin#TaskWikiNote() abort
	let tasks = vimwiki#liskin#selected_tasks()
	if empty(tasks)
		" create new note
		let wikifile = vimwiki#liskin#note_wikifile('')
	elseif len(tasks) is 1
		" open or create/associate note
		let wikifile = vimwiki#liskin#task_note_wikifile(tasks[0])
	else
		echoerr "vimwiki#liskin#TaskWikiNote: len(tasks) is " . len(tasks)
		return
	endif

	if !empty(wikifile)
		call vimwiki#base#goto(wikifile)
	endif
endfunction
