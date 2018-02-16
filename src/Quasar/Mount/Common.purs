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

module Quasar.Mount.Common where

import Prelude

import Data.Lens (view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Quasar.Data.URI as URI
import Data.URI.Authority (_userInfo)
import Data.URI.Extra.UserPassInfo (UserPassInfo(..))

newtype Credentials = Credentials { user ∷ String, password ∷ String }

derive instance newtypeCredentials ∷ Newtype Credentials _
derive instance eqCredentials ∷ Eq Credentials
derive instance ordCredentials ∷ Ord Credentials

instance showCredentials ∷ Show Credentials where
  show (Credentials { user, password }) =
    "(Credentials { user: " <> show user <> ", password: " <> show password <> " })"

combineCredentials ∷ Credentials → UserPassInfo
combineCredentials (Credentials { user, password }) = UserPassInfo { user, password: Just password }

extractCredentials ∷ ∀ hosts. Maybe (URI.Authority UserPassInfo hosts) → Maybe Credentials
extractCredentials mbAuth = 
  let 
    mbUI = mbAuth >>= view _userInfo
  in
    mbUI <#> (\(UserPassInfo u) -> Credentials u{ password = fromMaybe "" u.password})
