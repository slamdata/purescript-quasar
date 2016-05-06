module Quasar.Advanced.Permission where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Either (Either(..))

data Permission
  = Add
  | Read
  | Modify
  | Delete

instance encodeJsonPermission ∷ EncodeJson Permission where
  encodeJson Add = encodeJson "Add"
  encodeJson Read = encodeJson "Read"
  encodeJson Modify = encodeJson "Modify"
  encodeJson Delete = encodeJson "Delete"

instance decodeJsonPermission ∷ DecodeJson Permission where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "Add" → pure Add
      "Read" → pure Read
      "Modify" → pure Modify
      "Delete" → pure Delete
      _ → Left "Incorrect permission"
