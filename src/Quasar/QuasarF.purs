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

import Data.Argonaut (Json, JArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, AbsDir, Sandboxed)
import Data.StrMap (StrMap)

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either DirPath FilePath

type SQL = String
type MountConfig = Json
type Metadata = Json

newtype LDJSON = LDJSON String

runLDJSON ∷ LDJSON → String
runLDJSON (LDJSON s) = s

type Content = Either LDJSON JArray
type Vars = StrMap String

type Pagination = { offset ∷ Int, limit ∷ Int }

data QuasarF a
  = ReadQuery AnyPath SQL Vars (Maybe Pagination) (Either Error Json → a)
  | WriteQuery AnyPath FilePath SQL Vars (Either Error Json → a)
  | CompileQuery AnyPath SQL Vars (Either Error String → a)
  | GetMetadata AnyPath (Either Error Metadata → a)
  | ReadFile FilePath (Maybe Pagination) (Either Error Json → a)
  | WriteFile FilePath Content (Either Error Unit → a)
  | AppendFile FilePath Content (Either Error Unit → a)
  | DeleteFile FilePath (Either Error Unit → a)
  | MoveFile FilePath FilePath (Either Error Unit → a)
  | GetMount AnyPath (Either Error MountConfig → a)
  | CreateMount AnyPath MountConfig (Either Error Unit → a)
  | UpdateMount AnyPath MountConfig (Either Error Unit → a)
  | DeleteMount AnyPath (Either Error Unit → a)
