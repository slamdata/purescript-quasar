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

module Quasar.Mount.Type where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

data MountType
  = View
  | Module
  | MongoDB
  | Couchbase
  | MarkLogic
  | SparkHDFS
  | SparkFTP
  | SparkLocal
  | Mimir
  | Unknown (Maybe String)

derive instance eqMountType ∷ Eq MountType
derive instance ordMountType ∷ Ord MountType
derive instance genericMountType ∷ Generic MountType _
instance showMountType ∷ Show MountType where show = genericShow

fromName ∷ String → MountType
fromName = case _ of
  "view" → View
  "module" → Module
  "mongodb" → MongoDB
  "couchbase" → Couchbase
  "marklogic" → MarkLogic
  "spark-hdfs" → SparkHDFS
  "spark-ftp" → SparkFTP
  "spark-local" → SparkLocal
  "mimir" → Mimir
  other → Unknown (Just other)
