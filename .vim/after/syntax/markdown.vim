" jekyll/liquid directives
syn region markdownRule start="{%" end="%}"

" kramdown abbr syntax
syn region markdownIdDeclaration matchgroup=markdownLinkDelimiter start="^ \{0,3\}\*\[" end="\]:" oneline keepend
