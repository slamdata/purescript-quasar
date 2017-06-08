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

import Data.Argonaut (Json, (.?), (:=), (~>))
import Data.Argonaut as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.URI as URI
import Data.URI.Path (printPath, parseURIPathAbs)
import Global (decodeURIComponent)
import Quasar.Mount.Common (Host, Credentials(..)) as Exports
import Quasar.Mount.Common (Host, Credentials, combineCredentials, extractCredentials, extractHost)
import Quasar.Types (DirPath)
import Text.Parsing.StringParser (runParser)

type Config =
  { sparkHost ∷ Host
  , ftpHost ∷ Host
  , path ∷ DirPath
  , credentials ∷ Credentials
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
toURI { sparkHost, ftpHost, path, credentials, props } =
  mkURI sparkURIScheme sparkHost
    (Just (URI.Query $ requiredProps <> optionalProps))
    credentials
  where
  requiredProps ∷ L.List (Tuple String (Maybe String))
  requiredProps = L.fromFoldable
    [ Tuple "hdfsUrl"
      $ Just $ id
        $ URI.printAbsoluteURI
        $ mkURI ftpURIScheme ftpHost Nothing credentials
    , Tuple "rootPath" $ Just $ printPath (Left path)
    ]
  optionalProps ∷ L.List (Tuple String (Maybe String))
  optionalProps = SM.toUnfoldable props

mkURI ∷ URI.URIScheme → Host → Maybe URI.Query → Credentials → URI.AbsoluteURI
mkURI scheme host params credentials =
  URI.AbsoluteURI
    (Just scheme)
    (URI.HierarchicalPart (Just (URI.Authority (Just (combineCredentials credentials)) (pure host))) Nothing)
    params

note ∷ ∀ a b. a → Maybe b → Either a b
note a mb = case mb of
  Just b → Right b
  Nothing → Left a

fromURI ∷ URI.AbsoluteURI → Either String Config
fromURI (URI.AbsoluteURI scheme (URI.HierarchicalPart auth _) query) = do
  unless (scheme == Just sparkURIScheme) $ Left "Expected `spark` URL scheme"
  sparkHost ← extractHost auth
  credentials ← note "Failed to extract credentials" (extractCredentials auth)
  let props = maybe SM.empty (\(URI.Query qs) → SM.fromFoldable qs) query

  Tuple ftpHost props' ← case SM.pop "hdfsUrl" props of
    Just (Tuple (Just value) rest) → do
      value' ← extractHost' ftpURIScheme $ decodeURIComponent value
      pure (Tuple value' rest)
    _ → Left "Expected `ftpUrl` query parameter"

  Tuple path props'' ← case SM.pop "rootPath" props' of
    Just (Tuple (Just value) rest) → do
      value' ← lmap show $ runParser parseURIPathAbs value
      dirPath ← case value' of
        Left dp → pure dp
        Right _ → Left "Expected `rootPath` to be a directory path"
      pure (Tuple dirPath rest)
    _ → Left "Expected `rootPath` query parameter"

  pure { sparkHost, ftpHost, path, props: props'', credentials }

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
