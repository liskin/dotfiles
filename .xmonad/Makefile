xmonad-x86_64-linux: stack
	stack exec -- ghc --make xmonad.hs -fforce-recomp -rtsopts -threaded -with-rtsopts="-V0" -o xmonad-x86_64-linux.tmp
	mv -f xmonad-x86_64-linux.tmp xmonad-x86_64-linux

ghcid: stack
	ghcid -c 'stack exec -- ghci xmonad.hs'

stack:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin" \
		xmobar \
		xmonad \
		xmonad-contrib \
		split
