module Quasar.Advanced.OIDC where

import Prelude

import Control.Bind ((>=>))

import Data.Argonaut (class DecodeJson, decodeJson, (.?))

import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils.Types (Issuer(..), ClientID(..))

newtype OpenIDConfiguration =
  OpenIDConfiguration
    { issuer ∷ Issuer
    , authorizationEndpoint ∷ String
    , tokenEndpoint ∷ String
    , userinfoEndpoint ∷ String
    , jwks ∷ Array JSONWebKey
    }

instance decodeJSONOIDC ∷ DecodeJson OpenIDConfiguration where
  decodeJson = decodeJson >=> \obj → do
    issuer ← Issuer <$> obj .? "issuer"
    authorizationEndpoint ← obj .? "authorization_endpoint"
    tokenEndpoint ← obj .? "token_endpoint"
    userinfoEndpoint ← obj .? "userinfo_endpoint"
    jwks ← obj .? "jwks"
    pure
      $ OpenIDConfiguration
          { issuer, authorizationEndpoint, tokenEndpoint, userinfoEndpoint, jwks }

newtype Provider =
  Provider
    { displayName ∷ String
    , clientID ∷ ClientID
    , openIDConfiguration ∷ OpenIDConfiguration
    }

instance decodeJsonProvider ∷ DecodeJson Provider where
  decodeJson = decodeJson >=> \obj → do
    displayName ← obj .? "display_name"
    clientID ← ClientID <$> obj .? "client_id"
    openIDConfiguration ← obj .? "openid_configuration"
    pure $ Provider { displayName, clientID, openIDConfiguration }
