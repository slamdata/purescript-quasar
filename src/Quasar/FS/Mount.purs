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

module Quasar.FS.Mount where

import Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Path.Pathy (DirName, FileName, dir, file, pathName, (</>))
import Data.TacitString as TS
import Quasar.Types (AnyPath, DirPath, FilePath)

data MountF f
  = View (f FilePath)
  | Module (f DirPath)
  | MongoDB (f DirPath)
  | Couchbase (f DirPath)
  | MarkLogic (f DirPath)
  | SparkHDFS (f DirPath)
  | SparkLocal (f DirPath)
  | Mimir (f DirPath)
  | Unknown String (f AnyPath)

type Mount = MountF Identity
type MountType = MountF (Const Unit)


instance eqMount ∷ (Eq1 f) => Eq (MountF f) where
  eq inX inY =  case inX of
    View x
      | View y <- inY -> eq1 x y
      | otherwise -> false
    Module x
      | Module y <- inY -> eq1 x y
      | otherwise -> false
    MongoDB x
      | MongoDB y <- inY -> eq1 x y
      | otherwise -> false
    Couchbase x
      | Couchbase y <- inY -> eq1 x y
      | otherwise -> false
    MarkLogic x
      | MarkLogic y <- inY -> eq1 x y
      | otherwise -> false
    SparkHDFS x
      | SparkHDFS y <- inY -> eq1 x y
      | otherwise -> false
    SparkLocal x
      | SparkLocal y <- inY -> eq1 x y
      | otherwise -> false
    Mimir x
      | Mimir y <- inY -> eq1 x y
      | otherwise -> false
    Unknown xName x
      | Unknown yName y <- inY -> eq1 x y && eq xName yName
      | otherwise -> false

instance showMount ∷ (Show (f TS.TacitString), Functor f) => Show (MountF f) where
  show = 
    let 
      show' :: forall a. Show a => f a -> String
      show' = map (show >>> TS.hush) >>> show
    in case _ of
      View p -> "(View " <> show' p <> ")"
      Module p -> "(Module " <> show' p <> ")"
      MongoDB p -> "(MongoDB " <> show' p <> ")"
      Couchbase p -> "(Couchbase " <> show' p <> ")"
      MarkLogic p -> "(MarkLogic " <> show' p <> ")"
      SparkHDFS p -> "(SparkHDFS " <> show' p <> ")"
      SparkLocal p -> "(SparkLocal " <> show' p <> ")"
      Mimir p -> "(Mimir " <> show' p <> ")"
      Unknown n p -> "(Unknown " <> n <> " " <> show' p <> ")"

-- | Attempts to decode a mount listing value from Quasar's filesystem metadata,
-- | for a mount in the specified parent directory.
fromJSON ∷ DirPath → Json → Either String (Mount)
fromJSON parent = decodeJson >=> \obj → do
  mount ← obj .? "mount"
  typ ← obj .? "type"
  name ← obj .? "name"
  let 
    err :: forall a. Either String a
    err = Left $ "Unexpected type '" <> typ <> "' for mount '" <> mount <> "'"
    asFile = parent </> file name
    asDir = parent </> dir name
    onFile = if typ == "file" then Right $ Identity asFile else err
    onDir = if typ == "directory" then Right $ Identity asDir else err
    onAnyPath = case typ of 
      "file" → Right $ Identity $ Right asFile
      "directory" → Right $ Identity $ Left asDir
      _ → err
  case typeFromName mount of
    View _ → View <$> onFile
    Module _ → Module <$> onDir
    MongoDB _ → MongoDB <$> onDir
    Couchbase _ → Couchbase <$> onDir
    MarkLogic _ → MarkLogic <$> onDir
    SparkHDFS _ → SparkHDFS <$> onDir
    SparkLocal _ → SparkLocal <$> onDir
    Mimir _ → Mimir <$> onDir
    Unknown n _ → Unknown n <$> onAnyPath

getPath ∷ Mount → AnyPath
getPath = case _ of
  View file → Right $ unwrap file
  Module dir → Left $ unwrap dir
  MongoDB dir → Left $ unwrap dir
  Couchbase dir → Left $ unwrap dir
  MarkLogic dir → Left $ unwrap dir
  SparkHDFS dir → Left $ unwrap dir
  SparkLocal dir → Left $ unwrap dir
  Mimir dir → Left $ unwrap dir
  Unknown _ any → unwrap any

getName ∷ Mount → Either (Maybe DirName) FileName
getName = pathName <<< getPath


typeFromName ∷ String → MountType
typeFromName = case _ of
  "view" → View $ Const unit
  "module" → Module $ Const unit
  "mongodb" → MongoDB $ Const unit
  "couchbase" → Couchbase $ Const unit
  "marklogic" → MarkLogic $ Const unit
  "spark-hdfs" → SparkHDFS $ Const unit
  "spark-local" → SparkLocal $ Const unit
  "mimir" → Mimir $ Const unit
  other → Unknown other $ Const unit
