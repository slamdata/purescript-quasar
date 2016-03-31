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

module Test.Util.Process where

import Prelude

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR, makeVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console (log)

import Data.String as Str
import Data.Maybe (Maybe(..), isJust)

import Node.Buffer (BUFFER)
import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff as FSA
import Node.Process as Proc
import Node.Stream as Stream
import Node.Encoding as Enc

import Test.Util.FS as FS

spawnMongo ∷ ∀ eff. Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, fs ∷ FS, console ∷ CONSOLE, err ∷ EXCEPTION | eff) CP.ChildProcess
spawnMongo = do
  FS.rmRec "test/tmp/db"
  FS.mkdirRec "test/tmp/db"
  var ← makeVar
  log "Starting mongod..."
  proc ← liftEff $ do --pipeOutputs =<<
    cp ← CP.spawn
      "mongod"
      ["--port", "63174", "--dbpath", "db"]
      (CP.defaultSpawnOptions { cwd = Just "test/tmp" })
    Stream.onDataString (CP.stdout cp) Enc.UTF8 \s -> launchAff do
      if isJust (Str.indexOf "[initandlisten] waiting for connections on port" s)
        then putVar var unit
        else pure unit
    pure cp
  takeVar var *> log "Started"
  pure proc

spawnQuasar ∷ ∀ eff. Aff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, fs ∷ FS, buffer ∷ BUFFER, console ∷ CONSOLE, err ∷ EXCEPTION | eff) CP.ChildProcess
spawnQuasar = do
  FS.rmRec "test/tmp/quasar"
  FS.mkdirRec "test/tmp/quasar"
  buf ← FSA.readFile "test/quasar/config.json"
  FSA.writeFile "test/tmp/quasar/config.json" buf
  var ← makeVar
  log "Starting Quasar..."
  proc ← liftEff $ do
    cp ←
      CP.spawn
        "java"
        ["-jar", "../../quasar/quasar.jar", "-c", "config.json"]
        (CP.defaultSpawnOptions { cwd = Just "test/tmp/quasar" })
    Stream.onDataString (CP.stdout cp) Enc.UTF8 \s -> launchAff do
      if isJust (Str.indexOf "Press Enter to stop" s)
        then putVar var unit
        else pure unit
    pure cp
  takeVar var *> log "Started"
  pure proc

pipeOutputs ∷ ∀ eff. CP.ChildProcess → Eff (cp ∷ CP.CHILD_PROCESS, console ∷ CONSOLE, err ∷ EXCEPTION | eff) CP.ChildProcess
pipeOutputs cp = do
  Stream.pipe (CP.stdout cp) Proc.stdout
  Stream.pipe (CP.stderr cp) Proc.stderr
  CP.onExit cp Debug.Trace.traceAnyA
  pure cp
