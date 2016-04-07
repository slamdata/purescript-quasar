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
  ) where

import Prelude

import Data.Argonaut (Json, JArray, JObject)
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe)

import Quasar.Advanced.Auth.Provider as Auth
import Quasar.Data (QData)
import Quasar.QuasarF (QuasarF(..), QError(..), AnyPath, MountConfig, FilePath, Pagination, Metadata, Vars, SQL, printQError)

type QuasarAFP = Coproduct QuasarF QuasarAF

data QuasarAF a
  = AuthProviders (Either QError (Array Auth.Provider) → a)

serverInfo ∷ QuasarAFP (Either QError JObject)
serverInfo = left $ ServerInfo id

readQuery ∷ AnyPath → SQL → Vars → Maybe Pagination → QuasarAFP (Either QError JArray)
readQuery path sql vars pagination = left $ ReadQuery path sql vars pagination id

writeQuery ∷ AnyPath → FilePath → SQL → Vars → QuasarAFP (Either QError JObject)
writeQuery path file sql vars = left $ WriteQuery path file sql vars id

compileQuery ∷ AnyPath → SQL → Vars → QuasarAFP (Either QError String)
compileQuery path sql vars = left $ CompileQuery path sql vars id

getMetadata ∷ AnyPath → QuasarAFP (Either QError Metadata)
getMetadata path = left $ GetMetadata path id

readFile ∷ FilePath → Maybe Pagination → QuasarAFP (Either QError JArray)
readFile path pagination = left $ ReadFile path pagination id

writeFile ∷ FilePath → QData → QuasarAFP (Either QError Unit)
writeFile path content = left $ WriteFile path content id

appendFile ∷ FilePath → QData → QuasarAFP (Either QError Unit)
appendFile path content = left $ AppendFile path content id

deleteData ∷ AnyPath → QuasarAFP (Either QError Unit)
deleteData path = left $ DeleteData path id

moveData ∷ AnyPath → AnyPath → QuasarAFP (Either QError Unit)
moveData from to = left $ MoveData from to id

getMount ∷ AnyPath → QuasarAFP (Either QError MountConfig)
getMount path = left $ GetMount path id

createMount ∷ AnyPath → Json → QuasarAFP (Either QError Unit)
createMount path config = left $ CreateMount path config id

updateMount ∷ AnyPath → Json → QuasarAFP (Either QError Unit)
updateMount path config = left $ UpdateMount path config id

moveMount ∷ AnyPath → AnyPath → QuasarAFP (Either QError Unit)
moveMount from to = left $ MoveMount from to id

deleteMount ∷ AnyPath → QuasarAFP (Either QError Unit)
deleteMount path = left $ DeleteMount path id

authProviders ∷ QuasarAFP (Either QError (Array Auth.Provider))
authProviders = right $ AuthProviders id
