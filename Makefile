xmonad-i386-linux: stack
	stack exec -- ghc --make xmonad.hs -i -ilib -fforce-recomp -rtsopts -threaded -with-rtsopts="-V0" -o xmonad-i386-linux.tmp
	mv -f xmonad-i386-linux.tmp xmonad-i386-linux

stack:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin" \
		gsmenu \
		xmobar \
		xmonad \
		xmonad-contrib \
		split
