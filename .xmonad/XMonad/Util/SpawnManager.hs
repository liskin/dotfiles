{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  XMonad.Util.SpawnManager
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
--
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
-- Stability   :  experimental
-- Portability :  unknown
--
-- More powerful alternative to "XMonad.Util.SpawnOnce": with support for
-- stopping and restarting of commands as well.
--
module XMonad.Util.SpawnManager (
    spawnManaged,
    spawnOnlyManaged,
    respawnManaged,
    respawnOnlyManaged,
    respawnAllManaged,
    killManaged,
    killAllManaged,
    ) where

import Data.Functor
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, mapConcurrently_)
import Control.Exception
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import qualified Data.Map as M
import qualified Data.Set as S

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

newtype SpawnManager = SpawnManager { getSpawnManager :: M.Map String ProcessID }
    deriving (Read, Show)

instance ExtensionClass SpawnManager where
    initialValue = SpawnManager mempty
    extensionType = PersistentExtension

-- | TODO (spawn given, leave other)
spawnManaged :: [String] -> X ()
spawnManaged = mapM_ spawnManaged'

spawnManaged' :: String -> X ()
spawnManaged' cmd = do
    pidOld <- XS.gets $ M.lookup cmd . getSpawnManager
    alive <- maybe (pure False) (io . isPidAlive) pidOld
    when (not alive) $ do
        pid <- spawnPID cmd
        XS.modify $ SpawnManager . M.insert cmd pid . getSpawnManager

-- | TODO (spawn given, kill other)
spawnOnlyManaged :: [String] -> X ()
spawnOnlyManaged cmds = killExceptManaged cmds >> spawnManaged cmds

-- | TODO (respawn given, leave other)
respawnManaged :: [String] -> X ()
respawnManaged cmds = killManaged cmds >> spawnManaged cmds

-- | TODO (respawn given, kill other)
respawnOnlyManaged :: [String] -> X ()
respawnOnlyManaged cmds = killAllManaged >> spawnManaged cmds

-- | TODO (respawn all)
respawnAllManaged :: X ()
respawnAllManaged = respawnManaged =<< XS.gets (M.keys . getSpawnManager)

-- | TODO (kill given)
killManaged :: [String] -> X ()
killManaged cmds = do
    sm <- XS.gets getSpawnManager
    let cmds' = S.fromList cmds
        (smKill, smLeave) = (M.restrictKeys sm cmds', M.withoutKeys sm cmds')
    io $ killPids (M.elems smKill)
    XS.put $ SpawnManager smLeave

-- | TODO (kill except given)
killExceptManaged :: [String] -> X ()
killExceptManaged cmds = do
    sm <- XS.gets getSpawnManager
    let cmds' = S.fromList cmds
        (smLeave, smKill) = (M.restrictKeys sm cmds', M.withoutKeys sm cmds')
    io $ killPids (M.elems smKill)
    XS.put $ SpawnManager smLeave

-- | TODO (kill all)
killAllManaged :: X ()
killAllManaged = killManaged =<< XS.gets (M.keys . getSpawnManager)

isPidAlive :: ProcessID -> IO Bool
isPidAlive pid = try (signalProcess nullSignal pid) <&> \case
    Right () -> True
    Left e | isDoesNotExistError e -> False
           | otherwise -> True -- some other error, assume it's alive

killPids :: [ProcessID] -> IO ()
killPids = mapConcurrently_ killPid

-- TODO: version that works without threaded RTS
killPid :: ProcessID -> IO ()
killPid pid = try_ $ race_ (killer 50000) waiter
    where
        try_ = handle (\(_ :: SomeException) -> pure ())
        waiter = getProcessStatus True False pid
        killer delay = do
            signalProcess (if delay < 1000000 then sigTERM else sigKILL) pid
            threadDelay delay
            killer (delay * 2)
