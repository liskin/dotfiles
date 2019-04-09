" Vim syntax support file
" VERSION: 0.11 
" Maintainer: Francois Rigault <francois.rigault@free.fr>
" Last Change: 2004 Mar 26
" Transform a file into TEX file, using the current syntax highlighting.
"
" this file is based on the script 2html.vim
" see: 
" Maintainer: Bram Moolenaar <Bram@vim.org>
" Last Change: 2001 Sep 02
"	       (modified by David Ne\v{c}as (Yeti) <yeti@physics.muni.cz>)
" Transform a file into HTML, using the current syntax highlighting.
"
" BUG: you may run this script 2 times to get it work
"
" FIXME:words REVERSEME BACKSLASH and ACCOLADE are not allowed in the text
"
" TODO 	clean the code
"
" LASTLOG: 2004 Mar 26
" 	add support for non-gui vim
 

" init the list of colors used in the text
let s:colorlist = ""

" When not in gui we can only guess the colors.
if has("gui_running")
  let s:whatterm = "gui"
else
  let s:whatterm = "cterm"
  if &t_Co == 8
    let s:cterm_color0  = "#808080"
    let s:cterm_color1  = "#ff6060"
    let s:cterm_color2  = "#00ff00"
    let s:cterm_color3  = "#ffff00"
    let s:cterm_color4  = "#8080ff"
    let s:cterm_color5  = "#ff40ff"
    let s:cterm_color6  = "#00ffff"
    let s:cterm_color7  = "#ffffff"
  else
    let s:cterm_color0  = "#000000"
    let s:cterm_color1  = "#c00000"
    let s:cterm_color2  = "#008000"
    let s:cterm_color3  = "#804000"
    let s:cterm_color4  = "#0000c0"
    let s:cterm_color5  = "#c000c0"
    let s:cterm_color6  = "#008080"
    let s:cterm_color7  = "#c0c0c0"
    let s:cterm_color8  = "#808080"
    let s:cterm_color9  = "#ff6060"
    let s:cterm_color10 = "#00ff00"
    let s:cterm_color11 = "#ffff00"
    let s:cterm_color12 = "#8080ff"
    let s:cterm_color13 = "#ff40ff"
    let s:cterm_color14 = "#00ffff"
    let s:cterm_color15 = "#ffffff"
  endif
endif

" transform a 2 digits hex number into a dec number in [0,1]
function! s:HexToDec(nombre)
	let nhigh = substitute(a:nombre, '^\(.\).*', '\1', 'g')
	let nlow  = substitute(a:nombre, '^.\(.\).*', '\1', 'g')
	let nhigh = substitute(nhigh, 'a', '10', 'g')
	let nhigh = substitute(nhigh, 'b', '11', 'g')
	let nhigh = substitute(nhigh, 'c', '12', 'g')
	let nhigh = substitute(nhigh, 'd', '13', 'g')
	let nhigh = substitute(nhigh, 'e', '14', 'g')
	let nhigh = substitute(nhigh, 'f', '15', 'g')
	let nlow = substitute(nlow, 'a', '10', 'g')
	let nlow = substitute(nlow, 'b', '11', 'g')
	let nlow = substitute(nlow, 'c', '12', 'g')
	let nlow = substitute(nlow, 'd', '13', 'g')
	let nlow = substitute(nlow, 'e', '14', 'g')
	let nlow = substitute(nlow, 'f', '15', 'g')
	let ntot = nhigh * 16 + nlow
	"return "0." . ntot*100 / 256
	return printf('0.%02d', ntot * 50 / 255)
endfun

" extract red, green and blue parts from a color and return
" it with the latexstyle 
function! s:FormatColor(couleur)
	let red= substitute(a:couleur, '\(..\).*', '\1', 'g')
	let green= substitute(a:couleur, '..\(..\).*', '\1', 'g')
	let blue= substitute(a:couleur, '....\(..\).*', '\1', 'g')
	return s:HexToDec(red) . "," .  s:HexToDec(green) . "," .  s:HexToDec(blue)	
endfun

" Return good color specification: in GUI no transformation is done, in
" terminal return RGB values of known colors and empty string on unknown
if s:whatterm == "gui"
  function! s:TexColor(color)
	  let s:colorlist = s:colorlist . a:color
	  if (a:color == "")
		  return ""
	  else
		  return 'color' . substitute(a:color, '#', '', 'g')
	  endif
  endfun
else
  function! s:TexColor(color)
    if exists("s:cterm_color" . a:color)
      let tmp = "s:cterm_color" . a:color
      let tmp2 = ""
      exe "let tmp2 =" . tmp
      let s:colorlist = s:colorlist . tmp2
      return 'color' . substitute(tmp2, '#', '', 'g')
    else
      return ""
    endif
  endfun
endif

"if !exists("html_use_css")
  " Return opening HTML tag for given highlight id
  function! s:HtmlOpening(id)
    let a = ""
"    if synIDattr(a:id, "inverse")
"      " For inverse, we always must set both colors (and exchange them)
"      let x = s:TexColor(synIDattr(a:id, "fg#", s:whatterm))
"      let a = a . '<span style="background-color: ' . ( x != "" ? x : s:fgc ) . '">'
"      let x = s:TexColor(synIDattr(a:id, "bg#", s:whatterm))
"      let a = a . '<font color="' . ( if x != "" ? x : s:bgc ) . '">'
"    else
"      let x = s:TexColor(synIDattr(a:id, "bg#", s:whatterm))
"      if x != "" | let a = a . '<span style="background-color: ' . x . '">' | endif
      let x = s:TexColor(synIDattr(a:id, "fg#", s:whatterm))
      if x != "" | let a = a . '\\textcolor{' . x . '}{' | endif
"    endif
"    if synIDattr(a:id, "bold") | let a = a . '\\bf{' | endif
    if synIDattr(a:id, "italic") | let a = a . '\\textit{' | endif
    if synIDattr(a:id, "underline") | let a = a . '\\underline{' | endif
    return a
  endfun

  " Return closing HTML tag for given highlight id
  function s:HtmlClosing(id)
    let a = ""
    if synIDattr(a:id, "underline") | let a = a . "}" | endif
    if synIDattr(a:id, "italic") | let a = a . "}" | endif
"    if synIDattr(a:id, "bold") | let a = a . "}" | endif
"    if synIDattr(a:id, "inverse")
"      let a = a . '</font></span>'
"    else
      let x = s:TexColor(synIDattr(a:id, "fg#", s:whatterm))
      if x != "" | let a = a . '}' | endif
"      let x = s:TexColor(synIDattr(a:id, "bg#", s:whatterm))
"      if x != "" | let a = a . '</span>' | endif
"    endif
    return a
  endfun
"endif

" Return CSS style describing given highlight id (can be empty)
function! s:CSS1(id)
  let a = ""
"  if synIDattr(a:id, "inverse")
"    " For inverse, we always must set both colors (and exchange them)
"    let x = s:TexColor(synIDattr(a:id, "bg#", s:whatterm))
"    let a = a . "color: " . ( x != "" ? x : s:bgc ) . "; "
"    let x = s:TexColor(synIDattr(a:id, "fg#", s:whatterm))
"    let a = a . "background-color: " . ( x != "" ? x : s:fgc ) . "; "
"  else
    let x = s:TexColor(synIDattr(a:id, "fg#", s:whatterm))
    if x != "" | let a = a . "color: " . x . "; " | endif
    let x = s:TexColor(synIDattr(a:id, "bg#", s:whatterm))
    if x != "" | let a = a . "background-color: " . x . "; " | endif
"  endif
  if synIDattr(a:id, "bold") | let a = a . "font-weight: bold; " | endif
  if synIDattr(a:id, "italic") | let a = a . "font-style: italic; " | endif
  if synIDattr(a:id, "underline") | let a = a . "text-decoration: underline; " | endif
  return a
endfun

" Set some options to make it work faster.
" Expand tabs in original buffer to get 'tabstop' correctly used.
" Don't report changes for :substitute, there will be many of them.
let s:old_title = &title
let s:old_icon = &icon
let s:old_et = &l:et
let s:old_report = &report
set notitle noicon
setlocal et
set report=1000000

" Split window to create a buffer with the TEX file.
if expand("%") == ""
  new Untitled.tex
else
  new %.tex
endif
set modifiable
%d
let s:old_paste = &paste
set paste

exe "normal \<C-W>p"

" List of all id's
let s:idlist = ","

" Loop over all lines in the original text
let s:end = line("$")
let s:lnum = 1
while s:lnum <= s:end

  " Get the current line, with tabs expanded to spaces when needed
  " FIXME: What if it changes syntax highlighting?
  let s:line = getline(s:lnum)
  if stridx(s:line, "\t") >= 0
    exe s:lnum . "retab!"
    let s:did_retab = 1
    let s:line = getline(s:lnum)
  else
    let s:did_retab = 0
  endif
  let s:len = strlen(s:line)
  let s:new = ""

"   if s:numblines
"     let s:new = '<span class=lnr>' . strpart('        ', 0, strlen(line("$")) - strlen(s:lnum)) . s:lnum . '</span>  '
"   endif

  " Loop over each character in the line
  let s:col = 1
  while s:col <= s:len
    let s:startcol = s:col " The start column for processing text
    let s:id = synID(s:lnum, s:col, 1)
    let s:col = s:col + 1
    " Speed loop (it's small - that's the trick)
    " Go along till we find a change in synID
    while s:col <= s:len && s:id == synID(s:lnum, s:col, 1) | let s:col = s:col + 1 | endwhile

    " Output the text with the same synID, with class set to c{s:id}
    let s:id = synIDtrans(s:id)
    let s:new = s:new . 'REPLACEME{' . s:id . '}{' . substitute(substitute(substitute(substitute(strpart(s:line, s:startcol - 1, s:col - s:startcol),'\\', 'BACKSLASH' , 'g'), '#', '\\#', 'g'), '}', 'ACCOLADE', 'g'), '{', '\\{', 'g') . '}'
    " Add the class to class list if it's not there yet
    if stridx(s:idlist, "," . s:id . ",") == -1
      let s:idlist = s:idlist . s:id . ","
    endif

    if s:col > s:len
      break
    endif
  endwhile
  if s:did_retab
    undo
  endif

  exe "normal \<C-W>pa" . strtrans(s:new) . "\n\e\<C-W>p"
  let s:lnum = s:lnum + 1
  +
endwhile
" Finish with the last line
 exe "normal \<C-W>pa\n\e"

" Now, when we finally know which, we define the colors and styles
" if exists("html_use_css")
"   8
" endif

" Find out the background and foreground color.
let s:fgc = s:TexColor(synIDattr(highlightID("Normal"), "fg#", s:whatterm))
let s:bgc = s:TexColor(synIDattr(highlightID("Normal"), "bg#", s:whatterm))
if s:fgc == ""
  let s:fgc = ( &background == "dark" ? "#ffffff" : "#000000" )
endif
if s:bgc == ""
  let s:bgc = ( &background == "dark" ? "#000000" : "#ffffff" )
endif


" Gather attributes for all other classes
let s:idlist = strpart(s:idlist, 1)
while s:idlist != ""
  let s:attr = ""
  let s:col = stridx(s:idlist, ",")
  let s:id = strpart(s:idlist, 0, s:col)
  let s:idlist = strpart(s:idlist, s:col + 1)
  let s:attr = s:CSS1(s:id)
  " If the class has some attributes, export the style, otherwise DELETE all
  " its occurences to make the HTML shorter
  if s:attr != ""
    if exists("html_use_css")
      execute "normal A\n.c" . s:id . " { " . s:attr . "}"
    else
      execute '%s+REPLACEME{' . s:id . '}{\([^}]*\)}+' . s:HtmlOpening(s:id) . '\1' . s:HtmlClosing(s:id) . '+g'
    endif
  else
      execute '%s+REPLACEME{' . s:id . '}{\([^}]*\)}+\1+g'
    8
  endif
endwhile

"""""" 

" latex header with color definitions
exe "normal ggO\\begin{alltt}\<ESC>"
exe "normal ggO\\begin{document}\<ESC>"
let s:temp = ""
while s:colorlist != ""
	let s:temp=substitute(s:colorlist, '^#\(......\).*', '\1', 'g') 
	exe "normal ggO\\definecolor{color" . s:temp . "}{rgb}{" . s:FormatColor(s:temp) . "}\<ESC>"
	let s:colorlist=substitute(s:colorlist, '#' . s:temp, '', 'g')
endwhile
exe "normal Go\\end{alltt}\<ESC>"
" exe "normal ggO\\usepackage[T1]{fontenc}\<ESC>"
exe "normal ggO\\usepackage{fullpage,upquote}\<ESC>"
exe "normal ggO\\usepackage{color}\<ESC>"
exe "normal ggO\\usepackage{alltt}\<ESC>"
exe "normal ggO\\documentclass[10pt,a4paper]{article}\<ESC>"
exe "normal Go\\end{document}\<ESC>"

" Substitute the replaced characters
execute '%s/ACCOLADE/\\}/ge'
execute '%s/BACKSLASH/\\begin{math}\\backslash\\end{math}/ge'

" Cleanup (we've already lost last user's pattern match highlighting)
%s:\s\+$::e
if has("extra_search")
  nohlsearch
endif

" Restore old settings
let &report = s:old_report
let &title = s:old_title
let &icon = s:old_icon
let &paste = s:old_paste
exe "normal \<C-W>p"
let &l:et = s:old_et
exe "normal \<C-W>p"

" Save a little bit of memory (worths doing?)
unlet s:old_et s:old_paste s:old_icon s:old_report s:old_title
unlet s:whatterm s:idlist s:lnum s:end s:fgc s:bgc
unlet! s:col s:id s:attr s:len s:line s:new s:did_retab s:numblines
unlet s:colorlist s:temp
delfunc s:TexColor
delfunc s:FormatColor
delfunc s:HexToDec
delfunc s:CSS1
if !exists("html_use_css")
  delfunc s:HtmlOpening
  delfunc s:HtmlClosing
endif
