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

module Quasar.Mount.SparkFTP
  ( Config
  , toJSON
  , fromJSON
  , toURI
  , fromURI
  , module Exports
  ) where

import Prelude

import Control.MonadPlus (guard)


import Data.Argonaut (Json, (.?), (:=), (~>))
import Data.Argonaut as J
import Data.Array ((!!))
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.Path (printPath, parseURIPathAbs)

import Global (decodeURIComponent)

import Quasar.Mount.Common (Host, extractHost, credentials)
import Quasar.Mount.Common (Host) as Exports
import Quasar.Types (AnyPath)

import Text.Parsing.StringParser (runParser)

type Config =
  { sparkHost ∷ Host
  , ftpHost ∷ Host
  , path ∷ AnyPath
  , user ∷ String
  , password ∷ String
  , props ∷ SM.StrMap (Maybe String)
   }

toJSON ∷ Config → Json
toJSON config =
  let uri = URI.printAbsoluteURI (toURI config)
  in "spark-hdfs" := ("connectionUri" := uri ~> J.jsonEmptyObject) ~> J.jsonEmptyObject

fromJSON ∷ Json → Either String Config
fromJSON
  = fromURI
  <=< lmap show <<< URI.runParseAbsoluteURI
  <=< (_ .? "connectionUri")
  <=< (_ .? "spark-hdfs")
  <=< J.decodeJson

toURI ∷ Config → URI.AbsoluteURI
toURI { sparkHost, ftpHost, path, user, password, props } =
  mkURI sparkURIScheme sparkHost
    (Just (URI.Query $ requiredProps <> optionalProps))
    user
    password
  where
  requiredProps ∷ L.List (Tuple String (Maybe String))
  requiredProps = L.fromFoldable
    [ Tuple "hdfsUrl"
      $ Just $ id
        $ URI.printAbsoluteURI
        $ mkURI ftpURIScheme ftpHost Nothing user password
    , Tuple "rootPath" $ Just $ printPath path
    ]
  optionalProps ∷ L.List (Tuple String (Maybe String))
  optionalProps = SM.toUnfoldable props

extractCredentials ∷ Maybe URI.Authority → Either String { user ∷ String, password ∷ String }
extractCredentials auth = maybe (Left "Failed to extract credentials from URI") Right do
  URI.Authority userInfo _ ← auth
  ui ← userInfo
  let substrs = Str.split (Str.Pattern ":") ui
  guard $ Arr.length substrs == 2
  user ← substrs !! 0
  password ← substrs !! 1
  pure { user, password }


mkURI ∷ URI.URIScheme → Host → Maybe URI.Query → String → String → URI.AbsoluteURI
mkURI scheme host params user password =
  URI.AbsoluteURI
    (Just scheme)
    (URI.HierarchicalPart (Just (URI.Authority (credentials (Just user) (Just password)) (pure host))) Nothing)
    params

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth _) query) = do
  unless (scheme == Just sparkURIScheme) $ Left "Expected `spark` URL scheme"
  sparkHost ← extractHost auth
  creds ← extractCredentials auth
  let props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query

  Tuple ftpHost props' ← case SM.pop "hdfsUrl" props of
    Just (Tuple (Just value) rest) → do
      value' ← extractHost' ftpURIScheme $ decodeURIComponent value
      pure (Tuple value' rest)
    _ → Left "Expected `ftpUrl` query parameter"

  Tuple path props'' ← case SM.pop "rootPath" props' of
    Just (Tuple (Just value) rest) → do
      value' ← lmap show $ runParser parseURIPathAbs value
      pure (Tuple value' rest)
    _ → Left "Expected `rootPath` query parameter"
  pure { sparkHost, ftpHost, path, props: props'', user: creds.user, password: creds.password }

extractHost' ∷ URI.URIScheme → String → Either String Host
extractHost' scheme@(URI.URIScheme name) uri = do
  URI.AbsoluteURI scheme' (URI.HierarchicalPart auth _) _ ←
    lmap show $ URI.runParseAbsoluteURI uri
  unless (scheme' == Just scheme) $ Left $ "Expected '" <> name <> "' URL scheme"
  extractHost auth

sparkURIScheme ∷ URI.URIScheme
sparkURIScheme = URI.URIScheme "spark"

ftpURIScheme ∷ URI.URIScheme
ftpURIScheme = URI.URIScheme "ftp"

