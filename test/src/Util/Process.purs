{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Util.Process (spawnMongo, spawnQuasar, spawnQuasarInit) where

import Prelude

import Control.Monad.Aff (Aff, launchAff, delay, forkAff, apathize)
import Control.Monad.Aff.AVar (AVAR, makeVar, takeVar, putVar)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), isJust)
import Data.Posix.Signal (Signal(SIGTERM))
import Data.String as Str
import Data.Time.Duration (Milliseconds(..))
import Node.Buffer (BUFFER)
import Node.ChildProcess as CP
import Node.Encoding as Enc
import Node.FS (FS)
import Node.Stream as Stream

spawnMongo ∷ ∀ eff. Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, fs ∷ FS, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) CP.ChildProcess
spawnMongo = do
  spawn "MongoDB" "[initandlisten] waiting for connections" $ liftEff $
    CP.spawn
      "mongod"
      (Str.split (Str.Pattern " ") "--port 63174 --dbpath db")
      (CP.defaultSpawnOptions { cwd = Just "test/tmp" })

spawnQuasar ∷ ∀ eff. Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, fs ∷ FS, buffer ∷ BUFFER, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) CP.ChildProcess
spawnQuasar = do
  spawn "Quasar" "Press Enter to stop" $ liftEff $
    CP.spawn
      "java"
      (Str.split (Str.Pattern " ") "-jar ../../../jars/quasar.jar -c config.json")
      (CP.defaultSpawnOptions { cwd = Just "test/tmp/quasar" })

spawnQuasarInit ∷ ∀ eff. Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, fs ∷ FS, buffer ∷ BUFFER, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) Unit
spawnQuasarInit = do
  log "Starting Quasar initUpdateMetaStore..."
  var ← makeVar
  _ ← liftEff do
    cp ← CP.spawn
      "java"
      (Str.split (Str.Pattern " ") "-jar ../../../jars/quasar.jar initUpdateMetaStore -c config.json")
      (CP.defaultSpawnOptions { cwd = Just "test/tmp/quasar" })
    CP.onExit cp case _ of
      CP.Normally _ →
        void $ launchAff $ apathize $ putVar var (Right unit)
      _ →
        void $ launchAff $ apathize $ putVar var (Left unit)
  either (const (throwError (error "Process exited abnormally"))) (const (pure unit)) =<< takeVar var

spawn
  ∷ ∀ eff
  . String
  → String
  → Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) CP.ChildProcess
  → Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, console ∷ CONSOLE, exception ∷ EXCEPTION | eff) CP.ChildProcess
spawn name startLine spawnProc = do
  log $ "Starting " <> name <> "..."
  var ← makeVar
  proc ← spawnProc
  liftEff $ Stream.onDataString (CP.stderr proc) Enc.UTF8 \s →
    void $ launchAff $ putVar var $ Just $ error $ "An error occurred: " <> s
  liftEff $ Stream.onDataString (CP.stdout proc) Enc.UTF8 \s →
    void $ launchAff
      if isJust (Str.indexOf (Str.Pattern startLine) s)
      then putVar var Nothing
      else pure unit
  _ ← forkAff do
    delay (Milliseconds 10000.0)
    putVar var $ Just (error "Timed out")
  v ← takeVar var
  case v of
    Nothing → log "Started" $> proc
    Just err → do
      _ ← liftEff $ CP.kill SIGTERM proc
      throwError err
