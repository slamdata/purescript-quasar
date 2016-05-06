-- Most of this module is how I imagine it should work
module Quasar.Advanced.Share where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Either (Either)

import Quasar.Advanced.Group as G
import Quasar.Advanced.User as U
import Quasar.Advanced.Permission as P

newtype ShareId = ShareId String
runShareId ∷ ShareId → String
runShareId (ShareId sid) = sid

instance encodeJsonShareId ∷ EncodeJson ShareId where
  encodeJson (ShareId sid) = encodeJson sid

instance decodeJsonShareId ∷ DecodeJson ShareId where
  decodeJson s = ShareId <$> decodeJson s

newtype Share =
  Share
    { id ∷ ShareId
    , with ∷ Array (Either G.Group U.UserId)
    , permissions ∷ Array P.Permission
    }
