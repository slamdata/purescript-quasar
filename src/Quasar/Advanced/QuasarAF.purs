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

module Quasar.Advanced.QuasarAF
  ( module Quasar.Advanced.QuasarAF
  , module Quasar.QuasarF
  , module Quasar.Error
  , module Quasar.Types
  ) where

import Prelude

import Data.Argonaut (JArray)
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe)

import Quasar.Advanced.OIDC as OIDC
import Quasar.Advanced.PermissionToken as PToken
import Quasar.Advanced.Permission as P
import Quasar.Advanced.Group as Gr
import Quasar.Advanced.User as U
import Quasar.Data (QData, JSONMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson)
import Quasar.Error (type (:~>), QError, QResponse, lowerQError, printQError)
import Quasar.FS (Resource)
import Quasar.Mount (MountConfig)
import Quasar.QuasarF (QuasarF(..))
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (AnyPath, FilePath, DirPath, Pagination, Vars, SQL)

type QuasarAFP = Coproduct QuasarF QuasarAF
-- | `E` stands for `either error`
type QuasarAFPE res = QuasarAFP (QResponse res)


data QuasarAF a
  = AuthProviders ((Array OIDC.Provider) :~> a)
  | ListTokens ((Array PToken.PermissionTokenResponse) :~> a)
  | GetToken PToken.PermissionTokenId (PToken.PermissionTokenResponse :~> a)
  | NewToken PToken.NewPermissionTokenRequest (PToken.PermissionTokenResponse :~> a)
  | DeleteToken PToken.PermissionTokenId (Unit :~> a)
  | GroupInfo Gr.Group (Gr.GroupResponse :~> a)
  | DeleteGroup Gr.Group (Unit :~> a)
  | ModifyGroup Gr.Group Gr.ModifyGroupRequest (Unit :~> a)
  | Share (Array (Either Gr.Group U.UserId)) (Array P.Permission) (Unit :~> a)

instance functorQuasarAF ∷ Functor QuasarAF where
  map f (AuthProviders g) = AuthProviders (f <<< g)
  map f (ListTokens g) = ListTokens (f <<< g)
  map f (GetToken pid g) = GetToken pid (f <<< g)
  map f (NewToken req g) = NewToken req (f <<< g)
  map f (DeleteToken pid g) = DeleteToken pid (f <<< g)
  map f (GroupInfo gr g) = GroupInfo gr (f <<< g)
  map f (DeleteGroup gr g) = DeleteGroup gr (f <<< g)
  map f (ModifyGroup gr req g) = ModifyGroup gr req (f <<< g)
  map f (Share with perms g) = Share with perms (f <<< g)

serverInfo ∷ QuasarAFP (Either QError ServerInfo)
serverInfo = left $ ServerInfo id

readQuery
  ∷ JSONMode
  → DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarAFPE JArray
readQuery mode path sql vars pagination =
  left $ ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarAFPE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ DirPath
  → FilePath
  → SQL
  → Vars
  → QuasarAFPE OutputMeta
writeQuery path file sql vars =
  left $ WriteQuery path file sql vars id

compileQuery
  ∷ DirPath
  → SQL
  → Vars
  → QuasarAFPE String
compileQuery path sql vars =
  left $ CompileQuery path sql vars id

fileMetadata
  ∷ FilePath
  → QuasarAFPE Unit
fileMetadata path =
  left $ FileMetadata path id

dirMetadata
  ∷ DirPath
  → QuasarAFPE (Array Resource)
dirMetadata path =
  left $ DirMetadata path id

readFile
  ∷ JSONMode
  → FilePath
  → Maybe Pagination
  → QuasarAFPE JArray
readFile mode path pagination =
  left $ ReadFile mode path pagination id

readFileEJson
  ∷ FilePath
  → Maybe Pagination
  → QuasarAFPE (Array EJson)
readFileEJson path pagination =
  readFile Precise path pagination <#> resultsAsEJson

writeFile
  ∷ FilePath
  → QData
  → QuasarAFPE Unit
writeFile path content =
  left $ WriteFile path content id

appendFile
  ∷ FilePath
  → QData
  → QuasarAFPE Unit
appendFile path content =
  left $ AppendFile path content id

deleteData
  ∷ AnyPath
  → QuasarAFPE Unit
deleteData path =
  left $ DeleteData path id

moveData
  ∷ AnyPath
  → AnyPath
  → QuasarAFPE Unit
moveData from to =
  left $ MoveData from to id

getMount
  ∷ AnyPath
  → QuasarAFPE MountConfig
getMount path =
  left $ GetMount path id

createMount
  ∷ AnyPath
  → MountConfig
  → QuasarAFPE Unit
createMount path config =
  left $ CreateMount path config id

updateMount
  ∷ AnyPath
  → MountConfig
  → QuasarAFPE Unit
updateMount path config =
  left $ UpdateMount path config id

moveMount
  ∷ AnyPath
  → AnyPath
  → QuasarAFPE Unit
moveMount from to =
  left $ MoveMount from to id

deleteMount
  ∷ AnyPath
  → QuasarAFPE Unit
deleteMount path =
  left $ DeleteMount path id

authProviders
  ∷ QuasarAFPE (Array OIDC.Provider)
authProviders =
  right $ AuthProviders id

listPermissionTokens
  ∷ QuasarAFPE (Array PToken.PermissionTokenResponse)
listPermissionTokens =
  right $ ListTokens id

getPermissionToken
  ∷ PToken.PermissionTokenId
  → QuasarAFPE PToken.PermissionTokenResponse
getPermissionToken pid =
  right $ GetToken pid id

deletePermissionToken
  ∷ PToken.PermissionTokenId
  → QuasarAFPE Unit
deletePermissionToken pid =
  right $ DeleteToken pid id

newPermissionToken
  ∷ PToken.NewPermissionTokenRequest
  → QuasarAFPE PToken.PermissionTokenResponse
newPermissionToken req =
  right $ NewToken req id

groupInfo
  ∷ Gr.Group
  → QuasarAFPE Gr.GroupResponse
groupInfo gr =
  right $ GroupInfo gr id

deleteGroup
  ∷ Gr.Group
  → QuasarAFPE Unit
deleteGroup gr =
  right $ DeleteGroup gr id

modifyGroup
  ∷ Gr.Group
  → Gr.ModifyGroupRequest
  → QuasarAFPE Unit
modifyGroup gr req =
  right $ ModifyGroup gr req id

share
  ∷ Array (Either Gr.Group U.UserId)
  → Array P.Permission
  → QuasarAFPE Unit
share with perms =
  right $ Share with perms id
