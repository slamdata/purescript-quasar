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
  , module QA
  ) where

import Prelude

import Data.Argonaut (JArray)
import Data.Foldable (class Foldable, foldMap)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe)
import Data.Path.Pathy as Pt

import Quasar.Data (QData, JSONMode(..))
import Quasar.Data.Json.Extended (EJson, resultsAsEJson)
import Quasar.Error (type (:~>), QError(..), QResponse, lowerQError, printQError)
import Quasar.FS (Resource)
import Quasar.Mount (MountConfig)
import Quasar.QuasarF (QuasarF(..))
import Quasar.Query.OutputMeta (OutputMeta)
import Quasar.ServerInfo (ServerInfo)
import Quasar.Types (AnyPath, FilePath, DirPath, Pagination, Vars, SQL, CompileResultR)
import Quasar.Advanced.Types as QA

data QuasarAF a
  = GroupInfo (Pt.AbsFile Pt.Sandboxed) (QA.GroupInfoR :~> a)
  | CreateGroup (Pt.AbsFile Pt.Sandboxed) (Unit :~> a)
  | ModifyGroup (Pt.AbsFile Pt.Sandboxed) QA.GroupPatchR (Unit :~> a)
  | DeleteGroup (Pt.AbsFile Pt.Sandboxed) (Unit :~> a)
  | PermissionList Boolean ((Array QA.PermissionR) :~> a)
  | PermissionInfo QA.PermissionId (QA.PermissionR :~> a)
  | PermissionChildren QA.PermissionId Boolean ((Array QA.PermissionR) :~> a)
  | SharePermission QA.ShareRequestR ((Array QA.PermissionR) :~> a)
  | DeletePermission QA.PermissionId (Unit :~> a)
  | TokenList ((Array QA.TokenR) :~> a)
  | TokenInfo QA.TokenId (QA.TokenR :~> a)
  | CreateToken (Maybe QA.TokenName) (Array QA.ActionR) (QA.TokenR :~> a)
  | DeleteToken QA.TokenId (Unit :~> a)
  | AuthProviders ((Array QA.ProviderR) :~> a)
  | Licensee (QA.Licensee :~> a)

-- | `C` for coproduct
type QuasarAFC = Coproduct QuasarF QuasarAF

-- | `E` for `either error` (`QResponse` is `Either QError` already)
type QuasarAFCE res = QuasarAFC (QResponse res)

instance functorQuasarAF ∷ Functor QuasarAF where
  map f (GroupInfo pt g) = GroupInfo pt (f <<< g)
  map f (CreateGroup pt g) = CreateGroup pt (f <<< g)
  map f (ModifyGroup pt p g) = ModifyGroup pt p (f <<< g)
  map f (DeleteGroup pt g) = DeleteGroup pt (f <<< g)
  map f (PermissionList tr g) = PermissionList tr (f <<< g)
  map f (PermissionInfo pid g) = PermissionInfo pid (f <<< g)
  map f (PermissionChildren pid tr g) = PermissionChildren pid tr (f <<< g)
  map f (SharePermission req g) = SharePermission req (f <<< g)
  map f (DeletePermission pid g) = DeletePermission pid (f <<< g)
  map f (TokenList g) = TokenList (f <<< g)
  map f (TokenInfo tid g) = TokenInfo tid (f <<< g)
  map f (CreateToken mbName actions g) = CreateToken mbName actions (f <<< g)
  map f (DeleteToken tid g) = DeleteToken tid (f <<< g)
  map f (AuthProviders g) = AuthProviders (f <<< g)
  map f (Licensee g) = Licensee (f <<< g)


serverInfo
  ∷ QuasarAFCE ServerInfo
serverInfo =
  left $ ServerInfo id

readQuery
  ∷ JSONMode
  → DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarAFCE JArray
readQuery mode path sql vars pagination =
  left $ ReadQuery mode path sql vars pagination id

readQueryEJson
  ∷ DirPath
  → SQL
  → Vars
  → Maybe Pagination
  → QuasarAFCE (Array EJson)
readQueryEJson path sql vars pagination =
  readQuery Precise path sql vars pagination <#> resultsAsEJson

writeQuery
  ∷ DirPath
  → FilePath
  → SQL
  → Vars
  → QuasarAFCE OutputMeta
writeQuery path file sql vars =
  left $ WriteQuery path file sql vars id

compileQuery
  ∷ DirPath
  → SQL
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
  → QuasarAFCE (Array Resource)
dirMetadata path =
  left $ DirMetadata path id

readFile
  ∷ JSONMode
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
  left $ CreateMount path config id

updateMount
  ∷ AnyPath
  → MountConfig
  → QuasarAFCE Unit
updateMount path config =
  left $ UpdateMount path config id

moveMount
  ∷ AnyPath
  → AnyPath
  → QuasarAFCE Unit
moveMount from to =
  left $ MoveMount from to id

deleteMount
  ∷ AnyPath
  → QuasarAFCE Unit
deleteMount path =
  left $ DeleteMount path id

groupInfo
  ∷ Pt.AbsFile Pt.Sandboxed
  → QuasarAFCE QA.GroupInfoR
groupInfo pt =
  right $ GroupInfo pt id

createGroup
  ∷ Pt.AbsFile Pt.Sandboxed
  → QuasarAFCE Unit
createGroup pt =
  right $ CreateGroup pt id

modifyGroup
  ∷ Pt.AbsFile Pt.Sandboxed
  → QA.GroupPatchR
  → QuasarAFCE Unit
modifyGroup pt ptch =
  right $ ModifyGroup pt ptch id

addUsersToGroup
  ∷ ∀ f
  . Foldable f
  ⇒ Pt.AbsFile Pt.Sandboxed
  → f QA.UserId
  → QuasarAFCE Unit
addUsersToGroup pt us =
  modifyGroup pt { addUsers: foldMap pure us, removeUsers: [] }

removeUsersFromGroup
  ∷ ∀ f
  . Foldable f
  ⇒ Pt.AbsFile Pt.Sandboxed
  → f QA.UserId
  → QuasarAFCE Unit
removeUsersFromGroup pt us =
  modifyGroup pt { addUsers: [], removeUsers: foldMap pure us }


deleteGroup
  ∷ Pt.AbsFile Pt.Sandboxed
  → QuasarAFCE Unit
deleteGroup pt =
  right $ DeleteGroup pt id

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
