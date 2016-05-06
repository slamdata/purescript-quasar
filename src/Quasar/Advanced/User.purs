module Quasar.Advanced.User where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)

newtype UserId = UserId String
runUserId ∷ UserId → String
runUserId (UserId uid) = uid

instance encodeJsonUserId ∷ EncodeJson UserId where
  encodeJson (UserId uid) = encodeJson uid

instance decodeJsonUserId ∷ DecodeJson UserId where
  decodeJson js = UserId <$> decodeJson js
