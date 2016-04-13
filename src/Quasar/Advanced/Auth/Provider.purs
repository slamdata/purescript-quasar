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

module Quasar.Advanced.Auth.Provider where

import Prelude

import Control.Bind ((=<<), (>=>))

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)

import OIDCCryptUtils.Types (ClientID(..))

import Quasar.Advanced.Auth.OpenIDConfiguration as OIDC

type Provider =
  { displayName ∷ String
  , clientID ∷ ClientID
  , openIDConfiguration ∷ OIDC.OpenIDConfiguration
  }

fromJSON ∷ Json → Either String Provider
fromJSON = decodeJson >=> \obj → do
  displayName ← obj .? "display_name"
  clientID ← ClientID <$> obj .? "client_id"
  openIDConfiguration ← OIDC.fromJSON =<< obj .? "openid_configuration"
  pure { displayName, clientID, openIDConfiguration }