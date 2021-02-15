-- |
-- Module      :  XMonad.Actions.DoNotDisturb
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- Helpers and hooks for blocking of distractions and procrastination at the
-- window manager level.
--
module XMonad.Actions.DoNotDisturb (
    -- * DoNotDisturb toggle
    DoNotDisturb(..),
    getDoNotDisturb,
    setDoNotDisturb,
    toggleDoNotDisturb,

    -- * Deferring urgency
    deferUrgencyHook,
    replayDeferredUrgents,
    isDND,
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Util.ExtensibleState as XS

data DoNotDisturb = Disturb | DoNotDisturb
    deriving (Eq, Read, Show)

instance ExtensionClass DoNotDisturb where
    initialValue = Disturb
    extensionType = PersistentExtension

-- | Get the current state of 'DoNotDisturb'.
--
-- Useful wherever disturbing behaviour needs to be disabled, for example in
-- 'logHook' to disable highlighting urgent workspaces/windows.
getDoNotDisturb :: X DoNotDisturb
getDoNotDisturb = XS.get

-- | Set 'DoNotDisturb' state and refresh if necessary.
setDoNotDisturb :: DoNotDisturb -> X ()
setDoNotDisturb dnd = whenX (XS.modified (const dnd)) refresh

-- | Toggle 'DoNotDisturb' state and refresh.
toggleDoNotDisturb :: X ()
toggleDoNotDisturb = setDoNotDisturb . toggle =<< getDoNotDisturb
    where toggle Disturb = DoNotDisturb; toggle DoNotDisturb = Disturb

data DeferredUrgents = DeferredUrgents{ fromDeferredUrgents :: [Window] }
    deriving (Read, Show)

instance ExtensionClass DeferredUrgents where
    initialValue = DeferredUrgents []
    extensionType = PersistentExtension

-- | Mark windows whose urgency was deferred as urgent now.
replayDeferredUrgents :: X ()
replayDeferredUrgents = withWindowSet $ \ws -> do
    urgents <- XS.gets fromDeferredUrgents
    XS.put $ DeferredUrgents []
    mapM_ askUrgent $ filter (`W.member` ws) urgents

-- | 'UrgencyHook' that defers marking the window urgent until
-- 'replayDeferredUrgents' (done in 'setDoNotDisturb' 'Disturb').
-- Use as an argument to 'withUrgencyHook' or 'withUrgencyHookC'.
--
-- Example to defer urgents on workspace 1 when DND:
--
-- > deferUrgencyHook $ isDND <&&> windowTag =? Just "1"
deferUrgencyHook :: Query Bool -> Window -> X ()
deferUrgencyHook q w = whenX (runQuery q w) $ do
    let ins ws = if elem w ws then ws else w : ws
    XS.modify (DeferredUrgents . ins . fromDeferredUrgents)
    clearUrgents' [w]

-- | 'Query' for 'deferUrgencyHook', True if 'DoNotDisturb' is on.
isDND :: Query Bool
isDND = liftX getDoNotDisturb =? DoNotDisturb
