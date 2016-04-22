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

module Quasar.Advanced.Auth.OpenIDConfiguration where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)

import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils.Types (Issuer(..))

type OpenIDConfiguration =
  { issuer ∷ Issuer
  , authorizationEndpoint ∷ String
  , tokenEndpoint ∷ String
  , userinfoEndpoint ∷ String
  , jwks ∷ Array JSONWebKey
  }

fromJSON ∷ Json → Either String OpenIDConfiguration
fromJSON = decodeJson >=> \obj → do
    issuer ← Issuer <$> obj .? "issuer"
    authorizationEndpoint ← obj .? "authorization_endpoint"
    tokenEndpoint ← obj .? "token_endpoint"
    userinfoEndpoint ← obj .? "userinfo_endpoint"
    jwks ← obj .? "jwks"
    pure { issuer, authorizationEndpoint, tokenEndpoint, userinfoEndpoint, jwks }
