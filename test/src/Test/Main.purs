{-
Copyright 2017 SlamData, Inc.

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
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Argonaut ((:=), (~>))
import Data.Argonaut as J
import Data.Argonaut.JCursor as JC
import Data.Either (Either(..), isRight)
import Data.Foldable (traverse_)
import Data.Functor.Coproduct (left)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Posix.Signal (Signal(SIGTERM))
import Data.StrMap as SM
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Network.HTTP.Affjax (AJAX)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.Process (PROCESS)
import Node.Process as Proc
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Quasar.Advanced.QuasarAF.Interpreter.Aff (Config, eval)
import Quasar.Data (QData(..))
import Quasar.Data.Json as Json
import Quasar.Mount (MountConfig(..))
import Quasar.QuasarF (ExpiredContent(..), QError(..), QuasarF)
import Quasar.QuasarF as QF
import Quasar.Spawn.Util.Process (spawnQuasar, spawnQuasarInit)
import SqlSquared as Sql
import Test.Assert (ASSERT, assert)
import Test.Util.Effect (Effects)
import Test.Util.FS as FS

-- | Evaluates and runs a `QuasarF` value, throwing an assertion error if the
-- | query fails.
run
  ∷ ∀ eff a
  . Show a
  ⇒ (Either QError a → Boolean)
  → QuasarF (Either QError a)
  → Aff (ajax ∷ AJAX, console ∷ CONSOLE, assert ∷ ASSERT | eff) Unit
run pred qf = do
  x ← runReaderT (eval (left qf)) config
  log (show x)
  liftEff $ assert (pred x)

config ∷ Config ()
config =
  { basePath: Left
    { scheme: URI.Scheme "http"
    , authority: Just (URI.Authority Nothing [Tuple (URI.NameAddress "localhost") (Just (URI.Port 53174))])
    , path: rootDir
    }
  , idToken: Nothing
  , permissions: []
  }

showContentExpired ∷ forall a. Show a => ExpiredContent a -> String
showContentExpired =
  \(ExpiredContent {content, expired}) → "content: " <> show content <> " expired: " <> show expired

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

main ∷ Eff Effects Unit
main = void $ runAff throwException (const (pure unit)) $ jumpOutOnError do
  FS.rmRec "test/tmp/db"
  FS.rmRec "test/tmp/quasar"
  FS.mkdirRec "test/tmp/db"
  FS.mkdirRec "test/tmp/quasar"

  FSA.readdir "test/data" >>= traverse_ \path →
    FSA.readFile ("test/data/" <> path)
      >>= FSA.writeFile ("test/tmp/db/" <> path)

  qconfig ← FSA.readTextFile UTF8 "test/quasar/config.json"
  case J.jsonParser qconfig of
    Left err → throwError (error "Could not parse default quasar config.json")
    Right conf → do
      cwd ← liftEff Proc.cwd
      let
        cwd' = case Str.indexOf (Str.Pattern ":\\") cwd of
          Just ix → Str.replaceAll (Str.Pattern "\\") (Str.Replacement "/") (Str.drop (ix + 1) cwd)
          Nothing → cwd
      let path = cwd' <> "/test/tmp/db/"
      let cur = JC.insideOut $ JC.downField "mountings" $ JC.downField "/" $ JC.downField "spark-local" $ JC.downField "connectionUri" JC.JCursorTop
      case JC.cursorSet cur (J.fromString path) conf of
        Nothing → throwError (error "Could not set path in quasar config.json")
        Just qconfig' → FSA.writeTextFile UTF8 "test/tmp/quasar/config.json" (J.stringify qconfig')

  spawnQuasarInit "test/tmp/quasar/config.json" "jars/quasar.jar"
  quasar ← spawnQuasar "test/tmp/quasar/config.json" "jars/quasar.jar" "-C slamdata"

  result ← attempt do

    log "\nServerInfo:"
    run isRight $ map (\{ name, version } → name <> " " <> version) <$> QF.serverInfo

    log "\nGetMetastore:"
    run isRight $ map show <$> QF.getMetastore

    log "\nReadQuery:"
    run isRight $ QF.readQuery Json.Readable testDbAnyDir
      (unsafeQuery "SELECT sha as obj FROM `/slamengine_commits.json`")
      (SM.fromFoldable [Tuple "foo" "bar"])
      (Just { offset: 0, limit: 1 })
    run isRight $ QF.readQuery Json.Precise testDbAnyDir
      (unsafeQuery "SELECT sha as obj FROM `/slamengine_commits.json`")
      (SM.fromFoldable [Tuple "foo" "bar"])
      (Just { offset: 0, limit: 1 })

    log "\nWriteQuery:"
    run isRight $ map _.out <$> QF.writeQuery testDbAnyDir testFile1
      (unsafeQuery "SELECT * FROM `/smallZips.json` WHERE city IS NOT NULL")
      SM.empty

    log "\nCompileQuery:"
    run isRight $ map _.physicalPlan <$> QF.compileQuery testDbAnyDir
      (unsafeQuery "SELECT * FROM `/smallZips.json`")
      (SM.fromFoldable [Tuple "foo" "bar"])

    log "\nGetMetadata:"
    run isRight $ QF.dirMetadata testDbAnyDir Nothing
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
    run isRight $ QF.readFile Json.Precise testFile1 (Just { offset: 0, limit: 100 })
    run isRight $ QF.readFileCache Json.Precise testFile1 (Just { offset: 0, limit: 100 }) <#> map showContentExpired
    run isRight $ QF.readFile Json.Readable testFile3 (Just { offset: 0, limit: 1 })
    run isRight $ QF.readFileCache Json.Readable testFile3 (Just { offset: 0, limit: 1 }) <#> map showContentExpired

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

    log "\nInvokeFile:"
    run isRight $ QF.createMount (Left testMount3) mountConfig3
    run isRight $ QF.invokeFile Json.Precise testProcess (SM.fromFoldable [Tuple "a" "4", Tuple "b" "2"]) Nothing
    run isRight $ QF.invokeFileDetail Json.Precise testProcess (SM.fromFoldable [Tuple "a" "4", Tuple "b" "2"]) Nothing <#> map showContentExpired

    log "\nDone!"

  liftEff do
    void $ CP.kill SIGTERM quasar

  case result of
    Left err → throwError err
    Right _ → pure unit

  where
  testDbAnyDir = rootDir
  nonexistant = rootDir </> file "nonexistant"
  testFile1 = rootDir </> file "test1"
  testFile2Dir = rootDir </> dir "subdir"
  testFile2 = testFile2Dir </> file "test2"
  testFile3Dir = rootDir </> dir "what"
  testFile3 = testFile3Dir </> file "test3"
  testMount = rootDir </> file "testMount"
  testMount2 = rootDir </> file "testMount2"
  testMount3 = rootDir </> dir "testMount3" </> dir ""
  testProcess = rootDir </> dir "testMount3" </> file "test"

  isNotFound ∷ ∀ a. Either QError a → Boolean
  isNotFound e = case e of
    Left NotFound → true
    _ → false

  content =
    QData
      (Left (Json.Options { encoding: Json.Array, precision: Json.Readable }))
      $ J.stringify
      $ J.fromArray
          [ "foo" := "bar" ~> J.jsonEmptyObject
          , "foo" := "baz" ~> J.jsonEmptyObject
          ]

  mountConfig1 = ViewConfig
    { query: unsafeQuery "select * from `/smallZips.json`"
    , vars: SM.empty
    }

  mountConfig2 = ViewConfig
    { query: unsafeQuery "select * from `/slamengine_commits.json`"
    , vars: SM.empty
    }

  mountConfig3 = ModuleConfig
    { "module": unsafeModule "create function test(:a, :b) begin :a + :b end"
    }

unsafeQuery ∷ String → Sql.SqlQuery
unsafeQuery q = unsafePartial $ fromParsed q $ Sql.parseQuery q

unsafeModule ∷ String → Sql.SqlModule
unsafeModule q = unsafePartial $ fromParsed q $ Sql.parseModule q

fromParsed ∷ ∀ a b. Show a ⇒ Partial ⇒ String → Either a b → b
fromParsed q (Left err) = crashWith $ "for query `" <> q <> "` got error: " <> show err
fromParsed _ (Right x) = x
