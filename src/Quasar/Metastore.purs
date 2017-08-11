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

module Quasar.Metastore where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either)

data Metastore
  = H2Metastore String
  | PostgresMetastore { host ∷ String, port ∷ Int, database ∷ String, password ∷ String }

fromJSON ∷ Json → Either String Metastore
fromJSON = decodeJson >=> \obj → decodeH2 obj <|> decodePostgres obj
  where
    decodeH2 obj = do
      conf ← obj .? "h2"
      H2Metastore <$> conf .? "location"
    decodePostgres obj = do
      conf ← obj .? "postgresql"
      map PostgresMetastore $
        { host: _, port: _, database: _, password: _ }
          <$> conf .? "host"
          <*> conf .? "port"
          <*> conf .? "database"
          <*> conf .? "password"

toJSON ∷ Metastore → Json
toJSON = case _ of
  H2Metastore s →
    "h2" := ("location" := s ~> jsonEmptyObject )
    ~> jsonEmptyObject
  PostgresMetastore { host, port, database, password } →
    "postgresql" :=
      ("host" := host
       ~> "port" := port
       ~> "database" := database
       ~> "password" := password
       ~> jsonEmptyObject
      )
    ~> jsonEmptyObject
