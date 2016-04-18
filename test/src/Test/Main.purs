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
import Data.Functor.Coproduct (left)
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

import Quasar.Advanced.QuasarAF.Interpreter.Aff (Config, eval)
import Quasar.Data (QData(..), JSONMode(..))
import Quasar.Mount (MountConfig(..))
import Quasar.QuasarF (QuasarF, QError(..))
import Quasar.QuasarF as QF

-- | Evaluates and runs a `QuasarF` value, throwing an assertion error if the
-- | query fails.
run
  ∷ ∀ eff a
  . Show a
  ⇒ (Either QError a → Boolean)
  → QuasarF (Either QError a)
  → Aff (ajax ∷ AJAX, console ∷ CONSOLE, assert ∷ ASSERT | eff) (Either QError a)
run pred qf = do
  x ← runReaderT (eval (left qf)) config
  log (show x)
  liftEff $ assert (pred x)
  pure x

config ∷ Config ()
config =
  { basePath: "http://localhost:53174"
  , idToken: Nothing
  , permissions: []
  }

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
  for_ dataFiles \file →
    liftEff $ CP.spawn
      "mongoimport"
      ["--port", "63174", "--file", file]
      (CP.defaultSpawnOptions { cwd = Just "test/data" })

  result ← attempt do

    log "\nServerInfo:"
    run isRight $ map (\{ name, version } → name <> " " <> version) <$> QF.serverInfo

    log "\nReadQuery:"
    run isRight $ QF.readQuery Readable (Left testDbAnyDir) "SELECT _id as obj FROM `/test/slamengine_commits`" (SM.fromFoldable [Tuple "foo" "bar"]) (Just { offset: 0, limit: 1 })
    run isRight $ QF.readQuery Precise (Left testDbAnyDir) "SELECT _id as obj FROM `/test/slamengine_commits`" (SM.fromFoldable [Tuple "foo" "bar"]) (Just { offset: 0, limit: 1 })

    log "\nWriteQuery:"
    run isRight $ map _.out <$> QF.writeQuery (Left testDbAnyDir) testFile1 "SELECT * FROM `/test/smallZips` WHERE city IS NOT NULL" SM.empty

    log "\nCompileQuery:"
    run isRight $ QF.compileQuery (Left testDbAnyDir) "SELECT * FROM `/test/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"])

    log "\nGetMetadata:"
    run isRight $ QF.dirMetadata testDbAnyDir
    run isRight $ QF.fileMetadata testFile1
    run isNotFound $ QF.fileMetadata nonexistant

    log "\nMoveData:"
    run isRight $ QF.moveData (Right testFile1) (Right testFile2)
    run isRight $ QF.moveData (Left testFile2Dir) (Left testFile3Dir)

    log "\nWriteFile:"
    run isRight $ QF.writeFile testFile1 content

    log "\nAppendFile:"
    run isRight $ QF.appendFile testFile1 content

    log "\nReadFile:"
    run isRight $ QF.readFile Precise testFile1 (Just { offset: 0, limit: 100 })
    run isRight $ QF.readFile Readable testFile3 (Just { offset: 0, limit: 1 })
    run isNotFound $ QF.readFile Readable nonexistant Nothing

    log "\nDeleteData:"
    run isRight $ QF.deleteData (Right testFile1)
    run isRight $ QF.deleteData (Left testFile3Dir)

    log "\nCreateMount:"
    run isRight $ QF.createMount (Right testMount) mountConfig1

    log "\nUpdateMount:"
    run isRight $ QF.updateMount (Right testMount) mountConfig2

    log "\nGetMount:"
    run isRight $ QF.getMount (Left rootDir)
    run isRight $ QF.getMount (Right testMount)

    log "\nMoveMount:"
    run isRight $ QF.moveMount (Right testMount) (Right testMount2)

    log "\nDeleteMount:"
    run isRight $ QF.deleteMount (Right testMount2)

  liftEff do
    CP.kill SIGTERM mongod
    CP.kill SIGTERM quasar

  case result of
    Left err → throwError err
    Right _ → pure unit

  where
  testDbAnyDir = rootDir </> dir "test"
  nonexistant = rootDir </> dir "test" </> file "nonexistant"
  testFile1 = rootDir </> dir "test" </> file "Пациенты# #"
  testFile2Dir = rootDir </> dir "test" </> dir "subdir"
  testFile2 = testFile2Dir </> file "Ϡ⨁⟶≣ΜϞ"
  testFile3Dir = rootDir </> dir "test" </> dir "what"
  testFile3 = testFile3Dir </> file "Ϡ⨁⟶≣ΜϞ"
  testMount = rootDir </> file "testMount"
  testMount2 = rootDir </> file "testMount2"

  isNotFound ∷ ∀ a. Either QError a → Boolean
  isNotFound e = case e of
    Left NotFound → true
    _ → false

  content = JSON Readable
    [ "foo" := "bar" ~> jsonEmptyObject
    , "foo" := "baz" ~> jsonEmptyObject
    ]

  mountConfig1 = ViewConfig
    { query: "select * from `/test/smallZips`"
    , vars: SM.empty
    }

  mountConfig2 = ViewConfig
    { query: "select * from `/test/slamengine_commits`"
    , vars: SM.empty
    }
