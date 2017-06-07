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

module Quasar.Mount where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut (Json)
import Data.Either (Either(..))

import Quasar.Mount.Couchbase as Couchbase
import Quasar.Mount.MarkLogic as MarkLogic
import Quasar.Mount.Module as Module
import Quasar.Mount.MongoDB as MongoDB
import Quasar.Mount.SparkLocal as SparkLocal
import Quasar.Mount.SparkHDFS as SparkHDFS
import Quasar.Mount.SparkFTP as SparkFTP
import Quasar.Mount.View as View

data MountConfig
  = ViewConfig View.Config
  | ModuleConfig Module.Config
  | MongoDBConfig MongoDB.Config
  | CouchbaseConfig Couchbase.Config
  | MarkLogicConfig MarkLogic.Config
  | SparkHDFSConfig SparkHDFS.Config
  | SparkFTPConfig SparkFTP.Config
  | SparkLocalConfig SparkLocal.Config

instance showMountConfig ∷ Show MountConfig where
  show (ViewConfig { query, vars })
    = "(ViewConfig { query: " <> show query
    <> ", vars: " <> show vars <> " })"
  show (ModuleConfig config)
    = "(ModuleConfig { module: " <> config."module" <> " })"
  show (MongoDBConfig { hosts, auth, props })
    = "(MongoDBConfig { hosts: " <> show hosts
    <> ", auth: " <> show auth
    <> ", props: " <> show props <> " })"
  show (CouchbaseConfig { host, user, password })
    = "(CouchbaseConfig { host: " <> show host
    <> ", user: " <> show user
    <> ", password: " <> show password <> " })"
  show (MarkLogicConfig { host, path, credentials, format })
    = "(MarkLogicConfig { host: " <> show host
    <> ", path: " <> show path
    <> ", credentials: " <> show credentials
    <> ", format: " <> show format <> " })"
  show (SparkHDFSConfig { sparkHost, hdfsHost, path })
    = "(SparkHDFSConfig { sparkHost: " <> show sparkHost
    <> ", hdfsHost: " <> show hdfsHost
    <> ", path: " <> show path <> " })"
  show (SparkFTPConfig { sparkHost, ftpHost, path, user, password })
    = "(SparkFTPConfig { sparkHost: " <> show sparkHost
    <> ", ftpHost: " <> show ftpHost
    <> ", path: " <> show path
    <> ", user: " <> show user
    <> ", password: " <> show password <> " })"
  show (SparkLocalConfig path )
    = "(SparkLocalConfig { path: " <> show path <> " })"

fromJSON ∷ Json → Either String MountConfig
fromJSON json
  = ViewConfig <$> View.fromJSON json
  <|> ModuleConfig <$> Module.fromJSON json
  <|> MongoDBConfig <$> MongoDB.fromJSON json
  <|> CouchbaseConfig <$> Couchbase.fromJSON json
  <|> MarkLogicConfig <$> MarkLogic.fromJSON json
  <|> SparkHDFSConfig <$> SparkHDFS.fromJSON json
  <|> SparkFTPConfig <$> SparkFTP.fromJSON json
  <|> SparkLocalConfig <$> SparkLocal.fromJSON json
  <|> Left "Could not decode mount config"

toJSON ∷ MountConfig → Json
toJSON (ViewConfig config) = View.toJSON config
toJSON (ModuleConfig config) = Module.toJSON config
toJSON (MongoDBConfig config) = MongoDB.toJSON config
toJSON (CouchbaseConfig config) = Couchbase.toJSON config
toJSON (MarkLogicConfig config) = MarkLogic.toJSON config
toJSON (SparkHDFSConfig config) = SparkHDFS.toJSON config
toJSON (SparkFTPConfig config) = SparkFTP.toJSON config
toJSON (SparkLocalConfig config) = SparkLocal.toJSON config

