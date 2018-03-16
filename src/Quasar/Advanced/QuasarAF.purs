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

module Quasar.Advanced.QuasarAF
  ( module Quasar.Advanced.QuasarAF
  , module Quasar.QuasarF
  , module Quasar.Error
  , module Quasar.Types
  , module QA
  ) where

import Prelude

import DOM.File.Types (Blob)
import Data.Argonaut (JArray)
import Data.Foldable (class Foldable, foldMap)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds)
import Quasar.Advanced.Types as QA
import Quasar.Data (QData)
import Quasar.Data.Json (PrecisionMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson)
import Quasar.Error (type (:~>), QError(..), QResponse, lowerQError, printQError)
import Quasar.FS (QResource)
import Quasar.FS.Mount (Mount, MountF, Move)
import Quasar.Metastore (Metastore)
import Quasar.Mount (MountConfig(..))
import Quasar.Mount.View as View
import Quasar.QuasarF (QuasarF(..))
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (AnyPath, FilePath, DirPath, Pagination, Vars, CompileResultR)
import SqlSquared (SqlQuery)

data QuasarAF a
  = GroupInfo QA.GroupPath (QA.GroupInfoR :~> a)
  | CreateGroup QA.GroupPath (Unit :~> a)
  | ModifyGroup QA.GroupPath QA.GroupPatchR (Unit :~> a)
  | DeleteGroup QA.GroupPath (Unit :~> a)
  | AuthorityList ((Array QA.PermissionR) :~> a)
  | PermissionList Boolean ((Array QA.PermissionR) :~> a)
  | PermissionInfo QA.PermissionId (QA.PermissionR :~> a)
  | PermissionChildren QA.PermissionId Boolean ((Array QA.PermissionR) :~> a)
  | SharePermission QA.ShareRequestR ((Array QA.PermissionR) :~> a)
  | DeletePermission QA.PermissionId (Unit :~> a)
  | TokenList ((Array QA.TokenR) :~> a)
  | TokenInfo QA.TokenId (QA.TokenR :~> a)
  | CreateToken (Maybe QA.TokenName) (Array QA.ActionR) (QA.TokenR :~> a)
  | UpdateToken QA.TokenId (Array QA.ActionR) (QA.TokenR :~> a)
  | DeleteToken QA.TokenId (Unit :~> a)
  | AuthProviders ((Array QA.ProviderR) :~> a)
  | Licensee (QA.Licensee :~> a)
  | LicenseInfo (QA.LicenseInfo :~> a)
  | PDFInfo (Unit :~> a)

-- | `C` for coproduct
type QuasarAFC = Coproduct QuasarF QuasarAF

-- | `E` for `either error` (`QResponse` is `Either QError` already)
type QuasarAFCE res = QuasarAFC (QResponse res)

derive instance functorQuasarAF ∷ Functor QuasarAF

serverInfo
  ∷ QuasarAFCE ServerInfo
serverInfo =
  left $ ServerInfo id

readQuery
  ∷ PrecisionMode
  → DirPath
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarAFCE JArray
readQuery mode path sql vars pagination =
  left $ ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ DirPath
  → SqlQuery
  → Vars
  → Maybe Pagination
  → QuasarAFCE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ DirPath
  → FilePath
  → SqlQuery
  → Vars
  → QuasarAFCE OutputMeta
writeQuery path file sql vars =
  left $ WriteQuery path file sql vars id

compileQuery
  ∷ DirPath
  → SqlQuery
  → Vars
  → QuasarAFCE CompileResultR
compileQuery path sql vars =
  left $ CompileQuery path sql vars id

fileMetadata
  ∷ FilePath
  → QuasarAFCE Unit
fileMetadata path =
  left $ FileMetadata path id

dirMetadata
  ∷ DirPath
  → Maybe Pagination
  → QuasarAFCE (Array QResource)
dirMetadata path pagination =
  left $ DirMetadata path pagination id

readFile
  ∷ PrecisionMode
  → FilePath
  → Maybe Pagination
  → QuasarAFCE JArray
readFile mode path pagination =
  left $ ReadFile mode path pagination id

readFileEJson
  ∷ FilePath
  → Maybe Pagination
  → QuasarAFCE (Array EJson)
readFileEJson path pagination =
  readFile Precise path pagination <#> resultsAsEJson

writeFile
  ∷ FilePath
  → QData
  → QuasarAFCE Unit
writeFile path content =
  left $ WriteFile path content id

writeDir
  ∷ DirPath
  → Blob
  → QuasarAFCE Unit
writeDir path content =
  left $ WriteDir path content id

appendFile
  ∷ FilePath
  → QData
  → QuasarAFCE Unit
appendFile path content =
  left $ AppendFile path content id

deleteData
  ∷ AnyPath
  → QuasarAFCE Unit
deleteData path =
  left $ DeleteData path id

moveData
  ∷ AnyPath
  → AnyPath
  → QuasarAFCE Unit
moveData from to =
  left $ MoveData from to id

getMount
  ∷ AnyPath
  → QuasarAFCE MountConfig
getMount path =
  left $ GetMount path id

createMount
  ∷ AnyPath
  → MountConfig
  → QuasarAFCE Unit
createMount path config =
  left $ CreateMount path config Nothing id

updateMount
  ∷ AnyPath
  → MountConfig
  → QuasarAFCE Unit
updateMount path config =
  left $ UpdateMount path config Nothing id

createCachedView
  ∷ AnyPath
  → View.Config
  → Seconds
  → QuasarAFCE Unit
createCachedView path config maxAge =
  left $ CreateMount path (ViewConfig config) (Just maxAge) id

updateCachedView
  ∷ AnyPath
  → View.Config
  → Seconds
  → QuasarAFCE Unit
updateCachedView path config maxAge =
  left $ UpdateMount path (ViewConfig config) (Just maxAge) id

moveMount
  ∷ MountF Move
  → QuasarAFCE Unit
moveMount move =
  left $ MoveMount move id

deleteMount
  ∷ Mount
  → QuasarAFCE Unit
deleteMount mount =
  left $ DeleteMount mount id

getMetastore ∷ QuasarAFCE (Metastore ())
getMetastore = left $ GetMetastore id

putMetastore
  ∷ { initialize ∷ Boolean, metastore ∷ Metastore (password ∷ String) }
  → QuasarAFCE Unit
putMetastore ms = left $ PutMetastore ms id

groupInfo
  ∷ QA.GroupPath
  → QuasarAFCE QA.GroupInfoR
groupInfo pt =
  right $ GroupInfo pt id

createGroup
  ∷ QA.GroupPath
  → QuasarAFCE Unit
createGroup pt =
  right $ CreateGroup pt id

modifyGroup
  ∷ QA.GroupPath
  → QA.GroupPatchR
  → QuasarAFCE Unit
modifyGroup pt ptch =
  right $ ModifyGroup pt ptch id

addUsersToGroup
  ∷ ∀ f
  . Foldable f
  ⇒ QA.GroupPath
  → f QA.UserId
  → QuasarAFCE Unit
addUsersToGroup pt us =
  modifyGroup pt { addUsers: foldMap pure us, removeUsers: [] }

removeUsersFromGroup
  ∷ ∀ f
  . Foldable f
  ⇒ QA.GroupPath
  → f QA.UserId
  → QuasarAFCE Unit
removeUsersFromGroup pt us =
  modifyGroup pt { addUsers: [], removeUsers: foldMap pure us }

deleteGroup
  ∷ QA.GroupPath
  → QuasarAFCE Unit
deleteGroup pt =
  right $ DeleteGroup pt id

authorityList
  ∷ QuasarAFCE (Array QA.PermissionR)
authorityList =
  right $ AuthorityList id

permissionList
  ∷ Boolean
  → QuasarAFCE (Array QA.PermissionR)
permissionList isTransitive =
  right $ PermissionList isTransitive id

permissionInfo
  ∷ QA.PermissionId
  → QuasarAFCE QA.PermissionR
permissionInfo pid =
  right $ PermissionInfo pid id

permissionChildren
  ∷ QA.PermissionId
  → Boolean
  → QuasarAFCE (Array QA.PermissionR)
permissionChildren pid isTransitive =
  right $ PermissionChildren pid isTransitive id

sharePermission
  ∷ QA.ShareRequestR
  → QuasarAFCE (Array QA.PermissionR)
sharePermission req =
  right $ SharePermission req id

deletePermission
  ∷ QA.PermissionId
  → QuasarAFCE Unit
deletePermission pid =
  right $ DeletePermission pid id

tokenList
  ∷ QuasarAFCE (Array QA.TokenR)
tokenList =
  right $ TokenList id

tokenInfo
  ∷ QA.TokenId
  → QuasarAFCE QA.TokenR
tokenInfo tid =
  right $ TokenInfo tid id

createToken
  ∷ Maybe QA.TokenName
  → Array QA.ActionR
  → QuasarAFCE QA.TokenR
createToken mbName actions =
  right $ CreateToken mbName actions id

updateToken
  ∷ QA.TokenId
  → Array QA.ActionR
  → QuasarAFCE QA.TokenR
updateToken tid actions =
  right $ UpdateToken tid actions id

deleteToken
  ∷ QA.TokenId
  → QuasarAFCE Unit
deleteToken tid =
  right $ DeleteToken tid id

authProviders
  ∷ QuasarAFCE (Array QA.ProviderR)
authProviders =
  right $ AuthProviders id

licensee
  ∷ QuasarAFCE QA.Licensee
licensee =
  right $ Licensee id

licenseInfo
  ∷ QuasarAFCE QA.LicenseInfo
licenseInfo =
  right $ LicenseInfo id

pdfInfo
  ∷ QuasarAFCE Unit
pdfInfo =
  right $ PDFInfo id
