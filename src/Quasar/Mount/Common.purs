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

module Quasar.Mount.Common where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple)
import Data.URI as URI

type Host = Tuple URI.Host (Maybe URI.Port)

credentials ∷ Maybe String → Maybe String → Maybe String
credentials user password = do
  u ← user
  p ← password
  pure (u <> ":" <> p)

extractHost ∷ Maybe URI.Authority → Either String Host
extractHost (Just (URI.Authority _ hs)) =
  case hs of
    [h] → Right h
    [] → Left "No host specified"
    _ → Left "Multiple hosts specified"
extractHost _ = Left "No host specified"

extractCredentials ∷ Maybe URI.Authority → { user ∷ Maybe String, password ∷ Maybe String }
extractCredentials auth =
  case auth >>= (\(URI.Authority userInfo _) → userInfo) of
    Nothing → { user: Nothing, password: Nothing }
    Just userInfo →
      case Str.indexOf (Str.Pattern ":") userInfo of
        Nothing → { user: Just userInfo, password: Nothing }
        Just ix →
          { user: Just (Str.take ix userInfo)
          , password: Just (Str.drop (ix + 1) userInfo)
          }
