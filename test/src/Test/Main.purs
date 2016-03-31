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

module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, runAff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, Error, throwException)
import Control.Monad.Reader.Trans (runReaderT)

import Data.Argonaut (jsonEmptyObject, (~>), (:=))
import Data.Either (Either(..), isRight)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Posix.Signal (Signal(SIGTERM))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Network.HTTP.Affjax (AJAX)

import Node.Buffer (BUFFER)
import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff as FSA
import Node.Process (PROCESS)
import Node.Process as Proc

import Test.Assert (ASSERT, assert')
import Test.Util.Process (spawnMongo, spawnQuasar)

import Quasar.QuasarF (QuasarF(..))
import Quasar.QuasarF.Interpreter.Aff (eval)

-- | Evaluates and runs a `QuasarF` value, throwing an assertion error if the
-- | query fails.
run
  ∷ ∀ eff a
  . Show a
  ⇒ QuasarF (Either Error a)
  → Aff (ajax ∷ AJAX, console ∷ CONSOLE, assert ∷ ASSERT | eff) (Either Error a)
run qf = do
  x ← runReaderT (eval qf) ({ basePath: "http://localhost:53174" })
  log (show x)
  liftEff $ assert' "Query errored" (isRight x)
  pure x

-- | Used to catch Aff exceptions that don't get caught in main due to them
-- | being raised asynchronously.
jumpOutOnError
  ∷ ∀ eff a
  . Aff (console ∷ CONSOLE, process ∷ PROCESS | eff) a
  → Aff (console ∷ CONSOLE, process ∷ PROCESS | eff) a
jumpOutOnError aff = do
  x ← attempt aff
  case x of
    Left err → liftEff do
      Console.error (show err)
      Proc.exit 1
    Right x' → pure x'

main ∷ Eff (avar ∷ AVAR, cp ∷ CP.CHILD_PROCESS, process ∷ PROCESS, err ∷ EXCEPTION, fs ∷ FS, buffer ∷ BUFFER, console ∷ CONSOLE, ajax ∷ AJAX, assert ∷ ASSERT) Unit
main = runAff throwException (const (pure unit)) $ jumpOutOnError do

  mongod ← spawnMongo
  quasar ← spawnQuasar

  dataFiles ← FSA.readdir "test/data"
  for_ dataFiles \file ->
    liftEff $ CP.spawn
      "mongoimport"
      ["--port", "63174", "--file", file]
      (CP.defaultSpawnOptions { cwd = Just "test/data" })

  log "\nGetMetadata:"
  run $ GetMetadata testDbAnyDir id

  log "\nReadQuery:"
  run $ ReadQuery testDbAnyDir "SELECT * FROM `/test/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) (Just { offset: 0, limit: 1 }) id

  log "\nWriteQuery:"
  run $ WriteQuery testDbAnyDir testFile1 "SELECT * FROM `/test/smallZips` WHERE city IS NOT NULL" SM.empty id

  log "\nCompileQuery:"
  run $ CompileQuery testDbAnyDir "SELECT * FROM `/test/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) id

  log "\nMoveFile:"
  run $ MoveFile testFile1 testFile2 id

  log "\nWriteFile:"
  run $ WriteFile testFile1 (Right [content]) id

  log "\nAppendFile:"
  run $ AppendFile testFile1 (Right [content]) id

  log "\nReadFile:"
  run $ ReadFile testFile1 (Just { offset: 0, limit: 100 }) id
  run $ ReadFile testFile2 (Just { offset: 0, limit: 1 }) id

  log "\nDeleteFile:"
  run $ DeleteFile testFile1 id
  run $ DeleteFile testFile2 id

  log "\nCreateMount:"
  run $ CreateMount testMount mountConfig1 id

  log "\nUpdateMount:"
  run $ UpdateMount testMount mountConfig2 id

  log "\nGetMount:"
  run $ GetMount testMount id

  log "\nDeleteMount:"
  run $ DeleteMount testMount id

  liftEff do
    CP.kill SIGTERM mongod
    CP.kill SIGTERM quasar

  where
  testDbAnyDir = Left (rootDir </> dir "test")
  testFile1 = rootDir </> dir "test" </> file "zzz"
  testFile2 = rootDir </> dir "test" </> file "aaa"
  testMount = Right (rootDir </> file "testMount")

  content =
    "foo" := "bar"
    ~> jsonEmptyObject

  mountConfig1 =
    "view" :=
      ( "connectionUri" := "sql2:///?q=select+*+from+%60/test/smallZips%60"
      ~> jsonEmptyObject
      )
    ~> jsonEmptyObject

  mountConfig2 =
    "view" :=
      ( "connectionUri" := "sql2:///?q=select+*+from+%60/test/slamengine_commits%60"
      ~> jsonEmptyObject
      )
    ~> jsonEmptyObject
