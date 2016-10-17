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

module Quasar.Mount where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut (Json)
import Data.Either (Either(..))

import Quasar.Mount.Couchbase as Couchbase
import Quasar.Mount.MongoDB as MongoDB
import Quasar.Mount.View as View

data MountConfig
  = ViewConfig View.Config
  | MongoDBConfig MongoDB.Config
  | CouchbaseConfig Couchbase.Config

instance showMountConfig ∷ Show MountConfig where
  show (ViewConfig { query, vars })
    = "(ViewConfig { query: " <> show query
    <> ", vars: " <> show vars <> " })"
  show (MongoDBConfig { hosts, path, user, password, props })
    = "(MongoDBConfig { hosts: " <> show hosts
    <> ", path: " <> show path
    <> ", user: " <> show user
    <> ", password: " <> show password
    <> ", props: " <> show props <> " })"
  show (CouchbaseConfig { host, user, password })
    = "(CouchbaseConfig { host: " <> show host
    <> ", user: " <> show user
    <> ", password: " <> show password <> " })"

fromJSON ∷ Json → Either String MountConfig
fromJSON json
  = ViewConfig <$> View.fromJSON json
  <|> MongoDBConfig <$> MongoDB.fromJSON json
  <|> Left "Could not decode mount config"

toJSON ∷ MountConfig → Json
toJSON (ViewConfig config) = View.toJSON config
toJSON (MongoDBConfig config) = MongoDB.toJSON config
toJSON (CouchbaseConfig config) = Couchbase.toJSON config
