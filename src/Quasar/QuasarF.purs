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
  , module Quasar.Types
  ) where

import Prelude

import Data.Argonaut (JArray)
import Data.Either (Either)
import Data.Maybe (Maybe)

import Quasar.Data (QData, JSONMode)
import Quasar.FS (Resource)
import Quasar.Mount (MountConfig)
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (QError(..), AnyPath, FilePath, DirPath, Pagination, Vars, SQL, printQError)

data QuasarF a
  = ServerInfo (Either QError ServerInfo → a)
  | ReadQuery JSONMode AnyPath SQL Vars (Maybe Pagination) (Either QError JArray → a)
  | WriteQuery AnyPath FilePath SQL Vars (Either QError OutputMeta → a)
  | CompileQuery AnyPath SQL Vars (Either QError String → a)
  | FileMetadata FilePath (Either QError Unit → a)
  | DirMetadata DirPath (Either QError (Array Resource) → a)
  | ReadFile JSONMode FilePath (Maybe Pagination) (Either QError JArray → a)
  | WriteFile FilePath QData (Either QError Unit → a)
  | AppendFile FilePath QData (Either QError Unit → a)
  | DeleteData AnyPath (Either QError Unit → a)
  | MoveData AnyPath AnyPath (Either QError Unit → a)
  | GetMount AnyPath (Either QError MountConfig → a)
  | CreateMount AnyPath MountConfig (Either QError Unit → a)
  | UpdateMount AnyPath MountConfig (Either QError Unit → a)
  | MoveMount AnyPath AnyPath (Either QError Unit → a)
  | DeleteMount AnyPath (Either QError Unit → a)

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

serverInfo ∷ QuasarF (Either QError ServerInfo)
serverInfo = ServerInfo id

readQuery ∷ JSONMode → AnyPath → SQL → Vars → Maybe Pagination → QuasarF (Either QError JArray)
readQuery mode path sql vars pagination = ReadQuery mode path sql vars pagination id

writeQuery ∷ AnyPath → FilePath → SQL → Vars → QuasarF (Either QError OutputMeta)
writeQuery path file sql vars = WriteQuery path file sql vars id

compileQuery ∷ AnyPath → SQL → Vars → QuasarF (Either QError String)
compileQuery path sql vars = CompileQuery path sql vars id

fileMetadata ∷ FilePath → QuasarF (Either QError Unit)
fileMetadata path = FileMetadata path id

dirMetadata ∷ DirPath → QuasarF (Either QError (Array Resource))
dirMetadata path = DirMetadata path id

readFile ∷ JSONMode → FilePath → Maybe Pagination → QuasarF (Either QError JArray)
readFile mode path pagination = ReadFile mode path pagination id

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

createMount ∷ AnyPath → MountConfig → QuasarF (Either QError Unit)
createMount path config = CreateMount path config id

updateMount ∷ AnyPath → MountConfig → QuasarF (Either QError Unit)
updateMount path config = UpdateMount path config id

moveMount ∷ AnyPath → AnyPath → QuasarF (Either QError Unit)
moveMount from to = MoveMount from to id

deleteMount ∷ AnyPath → QuasarF (Either QError Unit)
deleteMount path = DeleteMount path id
