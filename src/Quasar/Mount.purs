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
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Quasar.Mount.Couchbase as Couchbase
import Quasar.Mount.MarkLogic as MarkLogic
import Quasar.Mount.Mimir as Mimir
import Quasar.Mount.Module as Module
import Quasar.Mount.MongoDB as MongoDB
import Quasar.Mount.SparkFTP as SparkFTP
import Quasar.Mount.SparkHDFS as SparkHDFS
import Quasar.Mount.SparkLocal as SparkLocal
import Quasar.Mount.Unknown as Unknown
import Quasar.Mount.View as View
import Quasar.Mount.Type (MountType(..))

data MountConfig
  = ViewConfig View.Config
  | ModuleConfig Module.Config
  | MongoDBConfig MongoDB.Config
  | CouchbaseConfig Couchbase.Config
  | MarkLogicConfig MarkLogic.Config
  | SparkHDFSConfig SparkHDFS.Config
  | SparkFTPConfig SparkFTP.Config
  | SparkLocalConfig SparkLocal.Config
  | MimirConfig Mimir.Config
  | UnknownConfig Unknown.Config

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
  show (CouchbaseConfig { host, bucketName, password, docTypeKey, queryTimeout })
    = "(CouchbaseConfig { host: " <> show host
    <> ", bucketName: " <> show bucketName
    <> ", password: " <> show password
    <> ", docTypeKey: " <> show docTypeKey
    <> ", queryTimeout: " <> show queryTimeout <> " })"
  show (MarkLogicConfig { host, path, credentials, format })
    = "(MarkLogicConfig { host: " <> show host
    <> ", path: " <> show path
    <> ", credentials: " <> show credentials
    <> ", format: " <> show format <> " })"
  show (SparkHDFSConfig { sparkHost, hdfsHost, path })
    = "(SparkHDFSConfig { sparkHost: " <> show sparkHost
    <> ", hdfsHost: " <> show hdfsHost
    <> ", path: " <> show path <> " })"
  show (SparkFTPConfig { sparkHost, ftpHost, path, credentials })
    = "(SparkFTPConfig { sparkHost: " <> show sparkHost
    <> ", ftpHost: " <> show ftpHost
    <> ", path: " <> show path
    <> ", credentials: " <> show credentials <> " })"
  show (SparkLocalConfig path)
    = "(SparkLocalConfig " <> show path <> ")"
  show (MimirConfig path)
    = "(MimirConfig " <> show path <> ")"
  show (UnknownConfig { mountType, connectionUri })
    = "(UnknownConfig { mountType: " <> show mountType
    <> ", connectionUri: " <> show connectionUri <> " })"

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
  <|> MimirConfig <$> Mimir.fromJSON json
  <|> UnknownConfig <$> Unknown.fromJSON json
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
toJSON (MimirConfig config) = Mimir.toJSON config
toJSON (UnknownConfig config) = Unknown.toJSON config

getType ∷ MountConfig → MountType
getType (ViewConfig _) = View
getType (ModuleConfig _) = Module
getType (MongoDBConfig _) = MongoDB
getType (CouchbaseConfig _) = Couchbase
getType (MarkLogicConfig _) = MarkLogic
getType (SparkHDFSConfig _) = SparkHDFS
getType (SparkFTPConfig _) = SparkFTP
getType (SparkLocalConfig _) = SparkLocal
getType (MimirConfig _) = Mimir
getType (UnknownConfig { mountType }) = Unknown (Just mountType)

_View ∷ Prism' MountConfig View.Config
_View = prism' ViewConfig case _ of
  ViewConfig config → Just config
  _ → Nothing

_Module ∷ Prism' MountConfig Module.Config
_Module = prism' ModuleConfig case _ of
  ModuleConfig config → Just config
  _ → Nothing

_MongoDB ∷ Prism' MountConfig MongoDB.Config
_MongoDB = prism' MongoDBConfig case _ of
  MongoDBConfig config → Just config
  _ → Nothing

_Couchbase ∷ Prism' MountConfig Couchbase.Config
_Couchbase = prism' CouchbaseConfig case _ of
  CouchbaseConfig config → Just config
  _ → Nothing

_MarkLogic ∷ Prism' MountConfig MarkLogic.Config
_MarkLogic = prism' MarkLogicConfig case _ of
  MarkLogicConfig config → Just config
  _ → Nothing

_SparkHDFS ∷ Prism' MountConfig SparkHDFS.Config
_SparkHDFS = prism' SparkHDFSConfig case _ of
  SparkHDFSConfig config → Just config
  _ → Nothing

_SparkFTP ∷ Prism' MountConfig SparkFTP.Config
_SparkFTP = prism' SparkFTPConfig case _ of
  SparkFTPConfig config → Just config
  _ → Nothing

_SparkLocal ∷ Prism' MountConfig SparkLocal.Config
_SparkLocal = prism' SparkLocalConfig case _ of
  SparkLocalConfig config → Just config
  _ → Nothing

_Mimir ∷ Prism' MountConfig Mimir.Config
_Mimir = prism' MimirConfig case _ of
  MimirConfig config → Just config
  _ → Nothing

_Unknown ∷ Prism' MountConfig Unknown.Config
_Unknown = prism' UnknownConfig case _ of
  UnknownConfig config → Just config
  _ → Nothing
