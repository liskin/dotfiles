all:
	#ghc --make xmonad.hs ghcflags.c -i -ilib -fforce-recomp -rtsopts -o xmonad-i386-linux.tmp
	#ghc --make xmonad.hs ghcflags.c -i -ilib -prof -auto-all -osuf p_o -hisuf p_hi -fforce-recomp -rtsopts -o xmonad-i386-linux.tmp
	#ghc --make xmonad.hs -i -ilib -fforce-recomp -rtsopts -with-rtsopts="-V0" -o xmonad-i386-linux.tmp
	#ghc --make xmonad.hs -i -ilib -fforce-recomp -rtsopts -threaded -o xmonad-i386-linux.tmp
	ghc --make xmonad.hs -i -ilib -fforce-recomp -rtsopts -threaded -with-rtsopts="-V0" -o xmonad-i386-linux.tmp
	mv -f xmonad-i386-linux.tmp xmonad-i386-linux
