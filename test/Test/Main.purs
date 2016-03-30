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

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throwException)
import Control.Monad.Reader.Trans (runReaderT)

import Data.Argonaut (jsonEmptyObject, (~>), (:=))
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (Natural())
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Network.HTTP.Affjax (AJAX)

import Test.Assert (ASSERT, assert')

import Quasar.QuasarF (QuasarF(..))
import Quasar.QuasarF.Interpreter.Aff (eval)

run
  ∷ ∀ eff a
  . Show a
  ⇒ QuasarF (Either Error a)
  → Aff (ajax ∷ AJAX, console ∷ CONSOLE, assert ∷ ASSERT | eff) (Either Error a)
run qf = do
  x ← runReaderT (eval qf) ({ basePath: "http://localhost" })
  liftEff $ assert' "Query errored" (isRight x)
  log (show x)
  pure x

main ∷ Eff (ajax ∷ AJAX, console ∷ CONSOLE, err ∷ EXCEPTION, assert ∷ ASSERT) Unit
main = runAff throwException (const (pure unit)) do

  log "\nGetMetadata:"
  run $ GetMetadata localAnyDir id

  log "\nReadQuery:"
  run $ ReadQuery testDbAnyDir "SELECT * FROM `/testDb/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) (Just { offset: 0, limit: 1 }) id

  log "\nWriteQuery:"
  run $ WriteQuery testDbAnyDir testFile1 "SELECT * FROM `/testDb/smallZips` WHERE city IS NOT NULL" SM.empty id

  log "\nCompileQuery:"
  run $ CompileQuery testDbAnyDir "SELECT * FROM `/testDb/smallZips`" (SM.fromFoldable [Tuple "foo" "bar"]) id

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

  where
  localAnyDir = Left (rootDir </> dir "local")
  testDbAnyDir = Left (rootDir </> dir "testDb")
  testFile1 = rootDir </> dir "testDb" </> file "zzz"
  testFile2 = rootDir </> dir "testDb" </> file "aaa"
  testMount = Right (rootDir </> file "testMount")

  content =
    "foo" := "bar"
    ~> jsonEmptyObject

  mountConfig1 =
    "view" :=
      ( "connectionUri" := "sql2:///?q=select+*+from+%60/testDb/smallZips%60"
      ~> jsonEmptyObject
      )
    ~> jsonEmptyObject

  mountConfig2 =
    "view" :=
      ( "connectionUri" := "sql2:///?q=select+*+from+%60/testDb/cities%60"
      ~> jsonEmptyObject
      )
    ~> jsonEmptyObject
