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

module Quasar.Advanced.PermissionToken where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)

import Quasar.Advanced.Permission (Permission)

newtype PermissionToken = PermissionToken String
runPermissionToken ∷ PermissionToken → String
runPermissionToken (PermissionToken s) = s

instance encodeJsonPermissionToken ∷ EncodeJson PermissionToken where
  encodeJson (PermissionToken s) = encodeJson s

instance decodeJsonPermissionToken ∷ DecodeJson PermissionToken where
  decodeJson s = PermissionToken <$> decodeJson s


newtype PermissionTokenId = PermissionTokenId String
runPermissionTokenId ∷ PermissionTokenId → String
runPermissionTokenId (PermissionTokenId s) = s


instance encodeJsonPermissionTokenId ∷ EncodeJson PermissionTokenId where
  encodeJson (PermissionTokenId s) = encodeJson s

instance decodeJsonPermissionTokenId ∷ DecodeJson PermissionTokenId where
  decodeJson s = PermissionTokenId <$> decodeJson s


newtype PermissionTokenName = PermissionTokenName String
runPermissionTokenName ∷ PermissionTokenName → String
runPermissionTokenName (PermissionTokenName s) = s

instance encodeJsonPermissionTokenName ∷ EncodeJson PermissionTokenName where
  encodeJson (PermissionTokenName s) = encodeJson s

instance decodeJsonPermissionTokenName ∷ DecodeJson PermissionTokenName where
  decodeJson s = PermissionTokenName <$> decodeJson s

newtype PermissionTokenResponse =
  PermissionTokenResponse
    { id ∷ PermissionTokenId
    , name ∷ PermissionTokenName
    , hash ∷ PermissionToken
    }

newtype NewPermissionTokenRequest =
  NewPermissionTokenRequest
    { name ∷ PermissionTokenName
    , permissions ∷ Array Permission
    }
