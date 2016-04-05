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

module Quasar.QuasarF where

import Prelude

import Control.Monad.Eff.Exception (Error)

import Data.Argonaut (Json, JArray, JObject)
import Data.Either (Either)
import Data.Either.Nested (Either3)
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, AbsDir, Sandboxed)
import Data.StrMap (StrMap)

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either DirPath FilePath

type SQL = String
type MountConfig = JObject
type Metadata = Json

newtype LDJSON = LDJSON String

runLDJSON ∷ LDJSON → String
runLDJSON (LDJSON s) = s

newtype CSV = CSV String

runCSV ∷ CSV → String
runCSV (CSV s) = s

type Content = Either3 LDJSON CSV JArray
type Vars = StrMap String

type Pagination = { offset ∷ Int, limit ∷ Int }

-- TODO: distinguish 404 errors (for move source missing, at least)

data QError
  = NotFound
  | Error Error

instance showQError ∷ Show QError where
  show NotFound = "NotFound"
  show (Error err) = "(Error " <> show err <> ")"

data QuasarF a
  = ServerInfo (Either QError JObject → a)
  | ReadQuery AnyPath SQL Vars (Maybe Pagination) (Either QError JArray → a)
  | WriteQuery AnyPath FilePath SQL Vars (Either QError JObject → a)
  | CompileQuery AnyPath SQL Vars (Either QError String → a)
  | GetMetadata AnyPath (Either QError Metadata → a)
  | ReadFile FilePath (Maybe Pagination) (Either QError JArray → a)
  | WriteFile FilePath Content (Either QError Unit → a)
  | AppendFile FilePath Content (Either QError Unit → a)
  | DeleteData AnyPath (Either QError Unit → a)
  | MoveData AnyPath AnyPath (Either QError Unit → a)
  | GetMount AnyPath (Either QError MountConfig → a)
  | CreateMount AnyPath Json (Either QError Unit → a)
  | UpdateMount AnyPath Json (Either QError Unit → a)
  | MoveMount AnyPath AnyPath (Either QError Unit → a)
  | DeleteMount AnyPath (Either QError Unit → a)
