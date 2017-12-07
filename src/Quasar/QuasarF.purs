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
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Seconds)
import Data.Traversable (class Foldable, class Traversable, sequenceDefault)
import Quasar.Data (QData)
import Quasar.Data.Json (PrecisionMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson', resultsAsEJson)
import Quasar.Error (type (:~>), QResponse, QError(..), PDFError(..), UnauthorizedDetails(..), lowerQError, printQError)
import Quasar.FS (QResource)
import Quasar.Metastore (Metastore)
import Quasar.Mount (MountConfig(..))
import Quasar.Mount.View as View
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (AnyPath, FilePath, DirPath, Pagination, Vars, CompileResultR)
import SqlSquared (SqlQuery)

newtype ExpiredContent a = ExpiredContent
  { content ∷ a
  , expired ∷ Boolean
  }

derive instance functorExpiredContent ∷ Functor ExpiredContent
derive instance newtypeExpiredContent ∷ Newtype (ExpiredContent a) _

instance foldableExpiredContent ∷ Foldable ExpiredContent where
  foldr f a ec = f (unwrap ec).content a
  foldl f a ec = f a (unwrap ec).content
  foldMap f ec = f (unwrap ec).content

instance traversableExpiredContent ∷ Traversable ExpiredContent where
  traverse f (ExpiredContent {content, expired}) =
    map (ExpiredContent <<< {expired, content: _}) (f content)
  sequence ec = sequenceDefault ec

data QuasarF a
  = ServerInfo (ServerInfo :~> a)
  | ReadQuery PrecisionMode DirPath SqlQuery Vars (Maybe Pagination) (JArray :~> a)
  | WriteQuery DirPath FilePath SqlQuery Vars (OutputMeta :~> a)
  | CompileQuery DirPath SqlQuery Vars (CompileResultR :~> a)
  | FileMetadata FilePath (Unit :~> a)
  | DirMetadata DirPath (Maybe Pagination) ((Array QResource) :~> a)
  | ReadFile PrecisionMode FilePath (Maybe Pagination) (JArray :~> a)
  | ReadFileCache PrecisionMode FilePath (Maybe Pagination) (ExpiredContent JArray :~> a)
  | WriteFile FilePath QData (Unit :~> a)
  | WriteDir DirPath Blob (Unit :~> a)
  | AppendFile FilePath QData (Unit :~> a)
  | InvokeFile PrecisionMode FilePath Vars (Maybe Pagination) (ExpiredContent JArray :~> a)
  | DeleteData AnyPath (Unit :~> a)
  | MoveData AnyPath AnyPath (Unit :~> a)
  | GetMount AnyPath (MountConfig :~> a)
  | CreateMount AnyPath MountConfig (Maybe Seconds) (Unit :~> a)
  | UpdateMount AnyPath MountConfig (Maybe Seconds) (Unit :~> a)
  | MoveMount AnyPath AnyPath (Unit :~> a)
  | DeleteMount AnyPath (Unit :~> a)
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
  → DirPath
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarFE JArray
readQuery mode path sql vars pagination =
  ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ DirPath
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarFE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ DirPath
  → FilePath
  → SqlQuery
  → Vars
  → QuasarFE OutputMeta
writeQuery path file sql vars =
  WriteQuery path file sql vars id

compileQuery
  ∷ DirPath
  → SqlQuery
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
  → Maybe Pagination
  → QuasarFE (Array QResource)
dirMetadata path pagination =
  DirMetadata path pagination id

readFile
  ∷ PrecisionMode
  → FilePath
  → Maybe Pagination
  → QuasarFE JArray
readFile mode path pagination =
  ReadFile mode path pagination id

readFileCache
  ∷ PrecisionMode
  → FilePath
  → Maybe Pagination
  → QuasarFE (ExpiredContent JArray)
readFileCache mode path pagination =
  ReadFileCache mode path pagination id

readFileEJson
  ∷ FilePath
  → Maybe Pagination
  → QuasarFE (Array EJson)
readFileEJson path pagination =
  map (_.content <<< unwrap) <$> readFileEJsonDetail path pagination

readFileEJsonDetail
  ∷ FilePath
  → Maybe Pagination
  → QuasarFE (ExpiredContent (Array EJson))
readFileEJsonDetail path pagination =
  readFileCache Precise path pagination <#> resultsAsEJson'

writeFile
  ∷ FilePath
  → QData
  → QuasarFE Unit
writeFile path content =
  WriteFile path content id

writeDir
  ∷ DirPath
  → Blob
  → QuasarFE Unit
writeDir path content =
  WriteDir path content id

appendFile
  ∷ FilePath
  → QData
  → QuasarFE Unit
appendFile path content =
  AppendFile path content id

invokeFile
  ∷ PrecisionMode
  → FilePath
  → Vars
  → Maybe Pagination
  → QuasarFE JArray
invokeFile mode path vars pagination =
  map (_.content <<< unwrap) <$> invokeFileDetail mode path vars pagination

invokeFileDetail
  ∷ PrecisionMode
  → FilePath
  → Vars
  → Maybe Pagination
  → QuasarFE (ExpiredContent JArray)
invokeFileDetail mode path vars pagination =
  InvokeFile mode path vars pagination id

invokeFileEJson
  ∷ FilePath
  → Vars
  → Maybe Pagination
  → QuasarFE (Array EJson)
invokeFileEJson path vars pagination =
  map (_.content <<< unwrap) <$> invokeFileEJsonDetail path vars pagination

invokeFileEJsonDetail
  ∷ FilePath
  → Vars
  → Maybe Pagination
  → QuasarFE (ExpiredContent (Array EJson))
invokeFileEJsonDetail path vars pagination =
  invokeFileDetail Precise path vars pagination <#> resultsAsEJson'

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
  CreateMount path config Nothing id

updateMount
  ∷ AnyPath
  → MountConfig
  → QuasarFE Unit
updateMount path config =
  UpdateMount path config Nothing id

createCachedView
  ∷ AnyPath
  → View.Config
  → Seconds
  → QuasarFE Unit
createCachedView path config maxAge =
  CreateMount path (ViewConfig config) (Just maxAge) id

updateCachedView
  ∷ AnyPath
  → View.Config
  → Seconds
  → QuasarFE Unit
updateCachedView path config maxAge =
  UpdateMount path (ViewConfig config) (Just maxAge) id

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

getMetastore ∷ QuasarFE (Metastore ())
getMetastore = GetMetastore id

putMetastore
  ∷ { initialize ∷ Boolean, metastore ∷ Metastore (password ∷ String) }
  → QuasarFE Unit
putMetastore ms = PutMetastore ms id
