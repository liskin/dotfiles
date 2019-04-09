" Vim syntax file
" Language:     NetLogo 5
" Maintainer:   Alan G. Isaac <aisaac@american.edu>
"   Tries to improve on Steven Stoddard's syntax file 
"   at http://voo-du.net/media/dump/nlogo.vim
" Copyright: 2014 Alan G. Isaac
" License: MIT http://opensource.org/licenses/MIT
" Last Change:  2014-07-29
" Filenames:    *.nlogo,*.nlogo~,*.nls

" TODO
" finish handling breeds (better approaches?)
" handle indenting
" handle (nested) brackets and parentheses
" make more NetLogo distinctions
" error detection (nlError)

if exists("b:current_syntax")
  finish
endif

syn case ignore

" Comment
syn keyword nlTodo contained TODO FIXME XXX
syn region nlComment start=";" end="$" keepend contains=nlTodo

" Constant
" a string is enclosed in double quotes
syn region nlString start=+"+ skip=+\\\\\|\\"+ end=+"+
" NetLogo "integer" (legal to start with 0!)

" all NetLogo numbers are floating point but have a variety of representations
" number: optional minus, integer part, no fractional part, optional separator, optional exponent
syn match nlNumber /\<-\=\d\+\.\=\([Ee][-+]\=\d\+\)\=/
" number: optional minus, integer part, separator, fractional part, optional exponent
syn match nlNumber /\<-\=\d\+\.\d\+\([Ee][-+]\=\d\+\)\=/
" number: optional minus, no integer part, separator, fractional part, optional exponent
syn match nlNumber /\<-\=\.\d\+\([Ee][-+]\=\d\+\)\=/

"TODO: necessary?
setlocal iskeyword+=-
setlocal iskeyword+=?
setlocal iskeyword+=#

" Constants
syn keyword nlArithmeticConstant e pi

syn keyword nlColorConstant black gray white red orange brown yellow green lime
    \ turquoise cyan sky blue violet magenta pink

syn keyword nlBooleanConstant false true

syn keyword nlSpecial startup resize-world set-patch-size 
        \ follow ride ride-me watch

syn keyword nlStatement carefully error-message report run runresult stop wait without-interruption error
syn keyword nlConditional if ifelse ifelse-value
syn keyword nlLogicalOperator and not or xor
syn keyword nlArithmeticOperator + - * /
syn keyword nlRepeat ask ask-concurrent repeat loop while foreach map
syn match nlRepeat /\<?\d*\>/

" Declarations
syn keyword nlDeclare
        \ __includes extensions globals
        \ patches-own turtles-own links-own 
        \ breed undirected-link-breed directed-link-breed

"note: the first three are "special" (i.e., can grow)
syn keyword nlSets patches turtles links
        \ no-patches no-turtles no-links
        \ turtles-at turtles-here turtles-on other-turtles-here
        \ neighbors neighbors4
        \ my-in-links my-links my-out-links

"TODO:  breeds-own, BREED-at BREED-here BREED-on other-BREED-here
"removed: pen-down? turtles-from

syn keyword nlTypeQuery
        \ is-boolean? is-number? is-string? is-list?
        \ is-command-task? is-reporter-task?
        \ is-agent? is-agentset?
        \ is-patch? is-patch-set?
        \ is-turtle? is-turtle-set?
        \ is-link? is-undirected-link? is-directed-link? is-link-set?

"TODO: is-<breed>? (too likely to overlap user defs?)


" assignment
syn keyword nlDefine end let set to to-report task

"note: don't include `breed` despite two uses: declaring breeds, and attr access
syn keyword nlTurtleAttr color heading hidden? label label-color
        \ pen-mode pen-size shape size who xcor ycor
        \ patch-here patch-ahead patch-at-heading-and-distance
        \ patch-left-and-ahead patch-right-and-ahead

syn keyword nlPatchAttr pcolor plabel plabel-color pxcor pycor

"note: don't include `breed` despite two uses: declaring breeds, and attr access
syn keyword nlLinkAttr color label label-color shape thickness
        \ end1 end2 hidden? tie-mode

syn keyword nlSelect self myself of
        \ turtle patch link nobody
        \ one-of n-of max-one-of max-n-of min-one-of min-n-of
        \ with with-max with-min
        \ bf butfirst but-first bl butlast but-last first last empty? any? all?
        \ with-local-randomness

"removed: random-one-of random-n-of 

syn keyword nlKeyword jump left lt pen-erase pe pen-up pu right rt showturtle st clear-patches cp
        \ back bk
        \ clear-ticks clear-turtles ct die
        \ distance distancexy downhill downhill4 dx dy forward fd
        \ hideturtle ht home inspect pen-down pd set-default-shape
        \ setxy setxyz shapes stamp subtract-headings towards towardsxy uphill diffuse diffuse4
        \ distance inspect nsum nsum4 at-points count histogram-from in-radius
        \ extract-hsb extract-rgb hsb rgb scale-color shade-of? wrap-color
        \ clear-all ca clear-drawing clear-patches cp display no-display beep clear-output
        \ export-view export-interface export-output export-plot export-all-plots export-world
        \ get-date-and-time import-world mouse-down? mouse-xcor mouse-ycor
        \ output-print output-show output-type output-write print read-from-string
        \ reset-perspective rp reset-ticks reset-timer set-current-directory
        \ show show-turtle st timer type
        \ user-choice user-choose-directory user-choose-file user-choose-new-file
        \ user-input user-message user-one-of user-yes-or-no?
        \ write file-at-end? file-close file-close-all file-delete file-exists?
        \ file-open file-print file-read file-read-characters file-read-line
        \ file-show file-type file-write
        \ user-directory user-file user-new-file filter
        \ fput item length list lput member? modes n-values position reduce remove
        \ remove-duplicates remove-item replace-item reverse sentence se shuffle
        \ sort sort-by sort-on sublist item length member? position remove remove-item
        \ read-from-string replace-item reverse substring word
        \ autoplot? auto-plot-off auto-plot-on clear-all-plots clear-plot
        \ export-plot export-all-plots histogram-from histogram-list
        \ plot plot-name plot-pen-down ppd plot-pen-reset plot-pen-up ppu
        \ plot-x-max plot-x-min plot-y-max plot-y-min plotxy ppd ppu
        \ set-current-plot set-current-plot-pen set-histogram-num-bars
        \ set-plot-pen-color set-plot-pen-interval set-plot-pen-mode
        \ set-plot-x-range set-plot-y-range setup-plots
        \ movie-cancel movie-close movie-grab-view movie-grab-interface movie-set-frame-rate movie-start movie-status

"removed: distance-nowrap distancexy-nowrap no-label towards-nowrap towardsxy-nowrap
" in-radius-nowrap random-int-or-float

syn keyword nlMath
        \ abs acos asin atan ceiling cos exp floor int ln log
        \ max mean median min mod modes new-seed precision
        \ random random-exponential random-float random-gamma random-normal random-poisson random-seed
        \ remainder round sin sqrt standard-deviation subtract-headings sum tan variance

syn keyword nlLinks
        \ both-ends other-end clear-links
        \ in-link-from
        \ layout-radial layout-spring layout-tutte link-heading link-length
        \ link-with
        \ out-link-to hide-link show-link tie untie 


"TODO: in-BREED-from BREED-with <link-breeds>-own my-BREEDS my-in-BREEDS my-out-BREEDS out-BREED-to

"keywords get priority over matches, so we still list them
"(plus this allows easily turning off breed highlights)
syn keyword nlCreate
        \ create-turtles crt create-ordered-turtles cro create-temporary-plot-pen 
        \ create-link-from create-links-from create-link-to create-links-to create-link-with create-links-with
        \ hatch sprout

syn match nlCreate /\<create-\S\+/
syn match nlCreate /\<hatch-\S\+/
syn match nlCreate /\<sprout-\S\+/

" nlCreate handles these breed cases:
" hatch-BREED sprout-BREED 
" create-BREED-from create-BREEDS-from
" create-BREED-to create-BREEDS-to
" create-BREED-with create-BREEDS-with

syn keyword nlLinkNeighbor
        \ link-neighbor? link-neighbors
        \ in-link-neighbor? in-link-neighbors
        \ out-link-neighbor? out-link-neighbors

"handle breeds
syn match nlLinkNeighbor /\<\(in-\|out-\)\?\S\+-neighbor[?s]\>/

"       \ BREED-neighbor? BREED-neighbors
"       \ in-BREED-neighbor? in-BREED-neighbors
"       \ out-BREED-neighbor? out-BREED-neighbors

hi link nlComment                Comment
hi link nlArithmeticConstant     Constant
hi link nlColorConstant          Constant
hi link nlString                 String
hi link nlNumber                 Number
hi link nlBooleanConstant        Boolean
hi link nlStatement              Statement
hi link nlConditional            Conditional
hi link nlLogicalOperator        Operator
hi link nlArithmeticOperator     Operator
hi link nlMath                   Function
hi link nlRepeat                 Repeat
hi link nlKeyword                Keyword
hi link nlLinks                  Keyword
hi link nlCreate                 Keyword
hi link nlLinkNeighbor           Keyword
hi link nlSets                   Type
hi link nlTurtleAttr             Keyword
hi link nlPatchAttr              Keyword
hi link nlLinkAttr               Keyword
hi link nlTypeQuery              Keyword
hi link nlDefine                 Define
hi link nlDeclare                Define
hi link nlSelect                 Keyword
hi link nlSpecial                Special
hi link nlTodo                   Todo
hi link nlError                  Error

let b:current_syntax="nlogo"
