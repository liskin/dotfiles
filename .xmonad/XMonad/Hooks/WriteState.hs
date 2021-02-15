-- |
-- Module      :  XMonad.Hooks.WriteState
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- Periodic backup of xmonad state (needs patched xmonad that stores the state
-- in per-session files and doesn't delete it on restore).
--
module XMonad.Hooks.WriteState (writeStateHook) where

import Control.Monad
import System.Posix.Time
import System.Posix.Types

import XMonad

import qualified XMonad.Util.ExtensibleState as XS

data LastWriteState = LastWriteState EpochTime deriving (Show, Read)
instance ExtensionClass LastWriteState where initialValue = LastWriteState 0

writeStateLogHook :: X ()
writeStateLogHook = do
    LastWriteState lastWrite <- XS.get
    now <- io $ epochTime
    when (lastWrite + 60 < now) $ do
        writeStateToFile
        XS.put $ LastWriteState now

writeStateHook :: XConfig a -> XConfig a
writeStateHook cfg = cfg{ logHook = logHook cfg <> writeStateLogHook }
