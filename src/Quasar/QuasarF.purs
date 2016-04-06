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

import Control.Monad.Eff.Exception (Error, message)

import Data.Argonaut (Json, JArray, JObject)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Path.Pathy (AbsFile, AbsDir, Sandboxed)
import Data.StrMap (StrMap)

import Quasar.Data (QData)

type FilePath = AbsFile Sandboxed
type DirPath = AbsDir Sandboxed
type AnyPath = Either DirPath FilePath

type SQL = String
type MountConfig = JObject
type Metadata = Json
type Vars = StrMap String

type Pagination = { offset ∷ Int, limit ∷ Int }

data QError
  = NotFound
  | Error Error

instance showQError ∷ Show QError where
  show NotFound = "NotFound"
  show (Error err) = "(Error " <> show err <> ")"

printQError ∷ QError → String
printQError NotFound = "Resource not found"
printQError (Error err) = message err

data QuasarF a
  = ServerInfo (Either QError JObject → a)
  | ReadQuery AnyPath SQL Vars (Maybe Pagination) (Either QError JArray → a)
  | WriteQuery AnyPath FilePath SQL Vars (Either QError JObject → a)
  | CompileQuery AnyPath SQL Vars (Either QError String → a)
  | GetMetadata AnyPath (Either QError Metadata → a)
  | ReadFile FilePath (Maybe Pagination) (Either QError JArray → a)
  | WriteFile FilePath QData (Either QError Unit → a)
  | AppendFile FilePath QData (Either QError Unit → a)
  | DeleteData AnyPath (Either QError Unit → a)
  | MoveData AnyPath AnyPath (Either QError Unit → a)
  | GetMount AnyPath (Either QError MountConfig → a)
  | CreateMount AnyPath Json (Either QError Unit → a)
  | UpdateMount AnyPath Json (Either QError Unit → a)
  | MoveMount AnyPath AnyPath (Either QError Unit → a)
  | DeleteMount AnyPath (Either QError Unit → a)

serverInfo ∷ QuasarF (Either QError JObject)
serverInfo = ServerInfo id

readQuery ∷ AnyPath → SQL → Vars → Maybe Pagination → QuasarF (Either QError JArray)
readQuery path sql vars pagination = ReadQuery path sql vars pagination id

writeQuery ∷ AnyPath → FilePath → SQL → Vars → QuasarF (Either QError JObject)
writeQuery path file sql vars = WriteQuery path file sql vars id

compileQuery ∷ AnyPath → SQL → Vars → QuasarF (Either QError String)
compileQuery path sql vars = CompileQuery path sql vars id

getMetadata ∷ AnyPath → QuasarF (Either QError Metadata)
getMetadata path = GetMetadata path id

readFile ∷ FilePath → Maybe Pagination → QuasarF (Either QError JArray)
readFile path pagination = ReadFile path pagination id

writeFile ∷ FilePath → QData → QuasarF (Either QError Unit)
writeFile path content = WriteFile path content id

appendFile ∷ FilePath → QData → QuasarF (Either QError Unit)
appendFile path content = AppendFile path content id

deleteData ∷ AnyPath → QuasarF (Either QError Unit)
deleteData path = DeleteData path id

moveData ∷ AnyPath → AnyPath → QuasarF (Either QError Unit)
moveData from to = MoveData from to id

getMount ∷ AnyPath → QuasarF (Either QError MountConfig)
getMount path = GetMount path id

createMount ∷ AnyPath → Json → QuasarF (Either QError Unit)
createMount path config = CreateMount path config id

updateMount ∷ AnyPath → Json → QuasarF (Either QError Unit)
updateMount path config = UpdateMount path config id

moveMount ∷ AnyPath → AnyPath → QuasarF (Either QError Unit)
moveMount from to = MoveMount from to id

deleteMount ∷ AnyPath → QuasarF (Either QError Unit)
deleteMount path = DeleteMount path id
