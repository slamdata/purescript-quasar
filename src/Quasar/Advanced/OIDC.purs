module Quasar.Advanced.OIDC where

import Prelude

import Control.Bind ((>=>), (=<<))

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Either (Either)

import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils.Types (Issuer(..), ClientID(..))

type OpenIDConfiguration =
  { issuer ∷ Issuer
  , authorizationEndpoint ∷ String
  , tokenEndpoint ∷ String
  , userinfoEndpoint ∷ String
  , jwks ∷ Array JSONWebKey
  }

oidcFromJSON ∷ Json → Either String OpenIDConfiguration
oidcFromJSON = decodeJson >=> \obj → do
    issuer ← Issuer <$> obj .? "issuer"
    authorizationEndpoint ← obj .? "authorization_endpoint"
    tokenEndpoint ← obj .? "token_endpoint"
    userinfoEndpoint ← obj .? "userinfo_endpoint"
    jwks ← obj .? "jwks"
    pure { issuer, authorizationEndpoint, tokenEndpoint, userinfoEndpoint, jwks }

type Provider =
  { displayName ∷ String
  , clientID ∷ ClientID
  , openIDConfiguration ∷ OpenIDConfiguration
  }

fromJSON ∷ Json → Either String Provider
fromJSON = decodeJson >=> \obj → do
  displayName ← obj .? "display_name"
  clientID ← ClientID <$> obj .? "client_id"
  openIDConfiguration ← oidcFromJSON =<< obj .? "openid_configuration"
  pure { displayName, clientID, openIDConfiguration }
