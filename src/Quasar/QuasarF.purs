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

module Quasar.QuasarF
  ( module Quasar.QuasarF
  , module Quasar.Error
  , module Quasar.Types
  ) where

import Prelude

import DOM.File.Types (Blob)
import Data.Argonaut (JArray)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Pathy (AbsDir, AbsFile, AbsPath)
import Quasar.Data (QData)
import Quasar.Data.Json (PrecisionMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson)
import Quasar.Error (type (:~>), QResponse, QError(..), PDFError(..), UnauthorizedDetails(..), lowerQError, printQError)
import Quasar.FS (QResource)
import Quasar.Metastore (Metastore)
import Quasar.Mount (MountConfig(..))
import Quasar.Mount.View as View
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (Pagination, Vars, CompileResultR)
import SqlSquared (SqlQuery)

data QuasarF a
  = ServerInfo (ServerInfo :~> a)
  | ReadQuery PrecisionMode AbsDir SqlQuery Vars (Maybe Pagination) (JArray :~> a)
  | WriteQuery AbsDir AbsFile SqlQuery Vars (OutputMeta :~> a)
  | CompileQuery AbsDir SqlQuery Vars (CompileResultR :~> a)
  | FileMetadata AbsFile (Unit :~> a)
  | DirMetadata AbsDir (Maybe Pagination) ((Array QResource) :~> a)
  | ReadFile PrecisionMode AbsFile (Maybe Pagination) (JArray :~> a)
  | WriteFile AbsFile QData (Unit :~> a)
  | WriteDir AbsDir Blob (Unit :~> a)
  | AppendFile AbsFile QData (Unit :~> a)
  | InvokeFile PrecisionMode AbsFile Vars (Maybe Pagination) (JArray :~> a)
  | DeleteData AbsPath (Unit :~> a)
  | MoveData AbsPath AbsPath (Unit :~> a)
  | GetMount AbsPath (MountConfig :~> a)
  | MoveMount AbsPath AbsPath (Unit :~> a)
  | DeleteMount AbsPath (Unit :~> a)
  | CreateMount AbsPath MountConfig (Maybe Seconds) (Unit :~> a)
  | UpdateMount AbsPath MountConfig (Maybe Seconds) (Unit :~> a)
  | GetMetastore (Metastore () :~> a)
  | PutMetastore { initialize ∷ Boolean, metastore ∷ Metastore (password ∷ String) } (Unit :~> a)

derive instance functorQuasarF ∷ Functor QuasarF

-- `E` for `either error`
type QuasarFE res = QuasarF (QResponse res)

serverInfo
  ∷ QuasarFE ServerInfo
serverInfo =
  ServerInfo id

readQuery
  ∷ PrecisionMode
  → AbsDir
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarFE JArray
readQuery mode path sql vars pagination =
  ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ AbsDir
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarFE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ AbsDir
  → AbsFile
  → SqlQuery
  → Vars
  → QuasarFE OutputMeta
writeQuery path file sql vars =
  WriteQuery path file sql vars id

compileQuery
  ∷ AbsDir
  → SqlQuery
  → Vars
  → QuasarFE CompileResultR
compileQuery path sql vars =
  CompileQuery path sql vars id

fileMetadata
  ∷ AbsFile
  → QuasarFE Unit
fileMetadata path =
  FileMetadata path id

dirMetadata
  ∷ AbsDir
  → Maybe Pagination
  → QuasarFE (Array QResource)
dirMetadata path pagination =
  DirMetadata path pagination id

readFile
  ∷ PrecisionMode
  → AbsFile
  → Maybe Pagination
  → QuasarFE JArray
readFile mode path pagination =
  ReadFile mode path pagination id

readFileEJson
  ∷ AbsFile
  → Maybe Pagination
  → QuasarFE (Array EJson)
readFileEJson path pagination =
  readFile Precise path pagination <#> resultsAsEJson

writeFile
  ∷ AbsFile
  → QData
  → QuasarFE Unit
writeFile path content =
  WriteFile path content id

writeDir
  ∷ AbsDir
  → Blob
  → QuasarFE Unit
writeDir path content =
  WriteDir path content id

appendFile
  ∷ AbsFile
  → QData
  → QuasarFE Unit
appendFile path content =
  AppendFile path content id

invokeFile
  ∷ PrecisionMode
  → AbsFile
  → Vars
  → Maybe Pagination
  → QuasarFE JArray
invokeFile mode path vars pagination =
  InvokeFile mode path vars pagination id

invokeFileEJson
  ∷ AbsFile
  → Vars
  → Maybe Pagination
  → QuasarFE (Array EJson)
invokeFileEJson path vars pagination =
  invokeFile Precise path vars pagination <#> resultsAsEJson

deleteData
  ∷ AbsPath
  → QuasarFE Unit
deleteData path =
  DeleteData path id

moveData
  ∷ AbsPath
  → AbsPath
  → QuasarFE Unit
moveData from to =
  MoveData from to id

getMount
  ∷ AbsPath
  → QuasarFE MountConfig
getMount path =
  GetMount path id

createMount
  ∷ AbsPath
  → MountConfig
  → QuasarFE Unit
createMount path config =
  CreateMount path config Nothing id

updateMount
  ∷ AbsPath
  → MountConfig
  → QuasarFE Unit
updateMount path config =
  UpdateMount path config Nothing id

createCachedView
  ∷ AbsPath
  → View.Config
  → Seconds
  → QuasarFE Unit
createCachedView path config maxAge =
  CreateMount path (ViewConfig config) (Just maxAge) id

updateCachedView
  ∷ AbsPath
  → View.Config
  → Seconds
  → QuasarFE Unit
updateCachedView path config maxAge =
  UpdateMount path (ViewConfig config) (Just maxAge) id

moveMount
  ∷ AbsPath
  → AbsPath
  → QuasarFE Unit
moveMount from to =
  MoveMount from to id

deleteMount
  ∷ AbsPath
  → QuasarFE Unit
deleteMount path =
  DeleteMount path id

getMetastore ∷ QuasarFE (Metastore ())
getMetastore = GetMetastore id

putMetastore
  ∷ { initialize ∷ Boolean, metastore ∷ Metastore (password ∷ String) }
  → QuasarFE Unit
putMetastore ms = PutMetastore ms id
