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
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Error.Class (throwError)
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

import Test.Assert (ASSERT, assert)
import Test.Util.Process (spawnMongo, spawnQuasar)

import Quasar.QuasarF (QuasarF(..), QError(..))
import Quasar.QuasarF.Interpreter.Aff (eval)

-- | Evaluates and runs a `QuasarF` value, throwing an assertion error if the
-- | query fails.
run
  ∷ ∀ eff a
  . Show a
  ⇒ (Either QError a → Boolean)
  → QuasarF (Either QError a)
  → Aff (ajax ∷ AJAX, console ∷ CONSOLE, assert ∷ ASSERT | eff) (Either QError a)
run pred qf = do
  x ← runReaderT (eval qf) ({ basePath: "http://localhost:53174" })
  log (show x)
  liftEff $ assert (pred x)
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

  result ← attempt do

    log "\nServerInfo:"
    run isRight $ ServerInfo id

    log "\nGetMetadata:"
    run isRight $ GetMetadata (Left testDbAnyDir) id
    run isNotFound $ GetMetadata (Right nonexistant) id

    log "\nReadQuery:"
    run isRight $ ReadQuery (Left testDbAnyDir) "SELECT * FROM `/test/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) (Just { offset: 0, limit: 1 }) id

    log "\nWriteQuery:"
    run isRight $ WriteQuery (Left testDbAnyDir) testFile1 "SELECT * FROM `/test/smallZips` WHERE city IS NOT NULL" SM.empty id

    log "\nCompileQuery:"
    run isRight $ CompileQuery (Left testDbAnyDir) "SELECT * FROM `/test/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) id

    log "\nMoveData:"
    run isRight $ MoveData (Right testFile1) (Right testFile2) id
    run isRight $ MoveData (Left testFile2Dir) (Left testFile3Dir) id

    log "\nWriteFile:"
    run isRight $ WriteFile testFile1 (Right [content]) id

    log "\nAppendFile:"
    run isRight $ AppendFile testFile1 (Right [content]) id

    log "\nReadFile:"
    run isRight $ ReadFile testFile1 (Just { offset: 0, limit: 100 }) id
    run isRight $ ReadFile testFile3 (Just { offset: 0, limit: 1 }) id

    log "\nReadFile NotFound:"
    run isNotFound $ ReadFile nonexistant Nothing id

    log "\nDeleteData:"
    run isRight $ DeleteData (Right testFile1) id
    run isRight $ DeleteData (Left testFile3Dir) id

    log "\nCreateMount:"
    run isRight $ CreateMount (Right testMount) mountConfig1 id

    log "\nUpdateMount:"
    run isRight $ UpdateMount (Right testMount) mountConfig2 id

    log "\nGetMount:"
    run isRight $ GetMount (Right testMount) id

    log "\nMoveMount:"
    run isRight $ MoveMount (Right testMount) (Right testMount2) id

    log "\nDeleteMount:"
    run isRight $ DeleteMount (Right testMount2) id

  liftEff do
    CP.kill SIGTERM mongod
    CP.kill SIGTERM quasar

  case result of
    Left err → throwError err
    Right _ → pure unit

  where
  testDbAnyDir = rootDir </> dir "test"
  nonexistant = rootDir </> dir "test" </> file "nonexistant"
  testFile1 = rootDir </> dir "test" </> file "zzz"
  testFile2Dir = rootDir </> dir "test" </> dir "subdir"
  testFile2 = testFile2Dir </> file "aaa"
  testFile3Dir = rootDir </> dir "test" </> dir "what"
  testFile3 = testFile3Dir </> file "aaa"
  testMount = rootDir </> file "testMount"
  testMount2 = rootDir </> file "testMount2"

  isNotFound ∷ ∀ a. Either QError a → Boolean
  isNotFound e = case e of
    Left NotFound → true
    _ → false

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
