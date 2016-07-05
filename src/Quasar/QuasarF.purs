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

module Quasar.QuasarF
  ( module Quasar.QuasarF
  , module Quasar.Error
  , module Quasar.Types
  ) where

import Prelude

import Data.Argonaut (JArray)
import Data.Maybe (Maybe)

import Quasar.Data (QData, JSONMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson)
import Quasar.Error (type (:~>), QResponse, QError(..), lowerQError, printQError)
import Quasar.FS (Resource)
import Quasar.Mount (MountConfig)
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (AnyPath, FilePath, DirPath, Pagination, Vars, SQL, CompileResultR)

data QuasarF a
  = ServerInfo (ServerInfo :~> a)
  | ReadQuery JSONMode DirPath SQL Vars (Maybe Pagination) (JArray :~> a)
  | WriteQuery DirPath FilePath SQL Vars (OutputMeta :~> a)
  | CompileQuery DirPath SQL Vars (CompileResultR :~> a)
  | FileMetadata FilePath (Unit :~> a)
  | DirMetadata DirPath ((Array Resource) :~> a)
  | ReadFile JSONMode FilePath (Maybe Pagination) (JArray :~> a)
  | WriteFile FilePath QData (Unit :~> a)
  | AppendFile FilePath QData (Unit :~> a)
  | DeleteData AnyPath (Unit :~> a)
  | MoveData AnyPath AnyPath (Unit :~> a)
  | GetMount AnyPath (MountConfig :~> a)
  | CreateMount AnyPath MountConfig (Unit :~> a)
  | UpdateMount AnyPath MountConfig (Unit :~> a)
  | MoveMount AnyPath AnyPath (Unit :~> a)
  | DeleteMount AnyPath (Unit :~> a)

instance functorQuasarF ∷ Functor QuasarF where
  map f (ServerInfo g) = ServerInfo (f <<< g)
  map f (ReadQuery mode path sql vars pagination g) = ReadQuery mode path sql vars pagination (f <<< g)
  map f (WriteQuery path file sql vars g) = WriteQuery path file sql vars (f <<< g)
  map f (CompileQuery path sql vars g) = CompileQuery path sql vars (f <<< g)
  map f (FileMetadata path g) = FileMetadata path (f <<< g)
  map f (DirMetadata path g) = DirMetadata path (f <<< g)
  map f (ReadFile mode path pagination g) = ReadFile mode path pagination (f <<< g)
  map f (WriteFile path content g) = WriteFile path content (f <<< g)
  map f (AppendFile path content g) = AppendFile path content (f <<< g)
  map f (DeleteData path g) = DeleteData path (f <<< g)
  map f (MoveData from to g) = MoveData from to (f <<< g)
  map f (GetMount path g) = GetMount path (f <<< g)
  map f (CreateMount path config g) = CreateMount path config (f <<< g)
  map f (UpdateMount path config g) = UpdateMount path config (f <<< g)
  map f (MoveMount from to g) = MoveMount from to (f <<< g)
  map f (DeleteMount path g) = DeleteMount path (f <<< g)

-- `E` for `either error`
type QuasarFE res = QuasarF (QResponse res)

serverInfo
  ∷ QuasarFE ServerInfo
serverInfo =
  ServerInfo id

readQuery
  ∷ JSONMode
  → DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarFE JArray
readQuery mode path sql vars pagination =
  ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarFE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ DirPath
  → FilePath
  → SQL
  → Vars
  → QuasarFE OutputMeta
writeQuery path file sql vars =
  WriteQuery path file sql vars id

compileQuery
  ∷ DirPath
  → SQL
  → Vars
  → QuasarFE CompileResultR
compileQuery path sql vars =
  CompileQuery path sql vars id

fileMetadata
  ∷ FilePath
  → QuasarFE Unit
fileMetadata path =
  FileMetadata path id

dirMetadata
  ∷ DirPath
  → QuasarFE (Array Resource)
dirMetadata path =
  DirMetadata path id

readFile
  ∷ JSONMode
  → FilePath
  → Maybe Pagination
  → QuasarFE JArray
readFile mode path pagination =
  ReadFile mode path pagination id

readFileEJson
  ∷ FilePath
  → Maybe Pagination
  → QuasarFE (Array EJson)
readFileEJson path pagination =
  readFile Precise path pagination <#> resultsAsEJson

writeFile
  ∷ FilePath
  → QData
  → QuasarFE Unit
writeFile path content =
  WriteFile path content id

appendFile
  ∷ FilePath
  → QData
  → QuasarFE Unit
appendFile path content =
  AppendFile path content id

deleteData
  ∷ AnyPath
  → QuasarFE Unit
deleteData path =
  DeleteData path id

moveData
  ∷ AnyPath
  → AnyPath
  → QuasarFE Unit
moveData from to =
  MoveData from to id

getMount
  ∷ AnyPath
  → QuasarFE MountConfig
getMount path =
  GetMount path id

createMount
  ∷ AnyPath
  → MountConfig
  → QuasarFE Unit
createMount path config =
  CreateMount path config id

updateMount
  ∷ AnyPath
  → MountConfig
  → QuasarFE Unit
updateMount path config =
  UpdateMount path config id

moveMount
  ∷ AnyPath
  → AnyPath
  → QuasarFE Unit
moveMount from to =
  MoveMount from to id

deleteMount
  ∷ AnyPath
  → QuasarFE Unit
deleteMount path =
  DeleteMount path id
