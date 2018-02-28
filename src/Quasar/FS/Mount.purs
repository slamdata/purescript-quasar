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

import Control.Alt ((<|>))
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Either (Either(..), note)
import Data.Eq (class Eq1, eq1)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Ord (class Ord1, compare1)
import Data.String.NonEmpty (fromString)
import Data.TacitString as TS
import Pathy (AbsDir, AbsFile, Dir, File, Name(..), AbsPath, dir', file', fileName, name, (</>))


data MountF f
  = View (f AbsFile)
  | Module (f AbsDir)
  | MongoDB (f AbsDir)
  | Couchbase (f AbsDir)
  | MarkLogic (f AbsDir)
  | SparkHDFS (f AbsDir)
  | SparkLocal (f AbsDir)
  | Mimir (f AbsDir)
  | Unknown String (f AbsPath)

type Mount = MountF Identity
type MountType = MountF (Const Unit)

instance eqMount ∷ Eq1 f ⇒ Eq (MountF f) where
  eq = case _, _ of
    View x, View y → eq1 x y
    View x, _ → false
    Module x, Module y → eq1 x y
    Module _, _ → false
    MongoDB x, MongoDB y → eq1 x y
    MongoDB _, _ → false
    Couchbase x, Couchbase y → eq1 x y
    Couchbase _, _ → false
    MarkLogic x, MarkLogic y → eq1 x y
    MarkLogic _, _ → false
    SparkHDFS x, SparkHDFS y → eq1 x y
    SparkHDFS _, _ → false
    SparkLocal x, SparkLocal y → eq1 x y
    SparkLocal _, _ → false
    Mimir x, Mimir y → eq1 x y
    Mimir _, _ → false
    Unknown xName x, Unknown yName y → eq xName yName && eq1 x y
    Unknown _ _, _ → false

instance ordMount ∷ Ord1 f ⇒ Ord (MountF f) where
  compare = case _, _ of
    View x, View y → compare1 x y
    View _, _ → LT
    _, View _ → GT
    Module x, Module y → compare1 x y
    Module _, _ → LT
    _, Module _ → GT
    MongoDB x, MongoDB y → compare1 x y
    MongoDB _, _ → LT
    _, MongoDB _ → GT
    Couchbase x, Couchbase y → compare1 x y
    Couchbase _, _ → LT
    _, Couchbase _ → GT
    MarkLogic x, MarkLogic y → compare1 x y
    MarkLogic _, _ → LT
    _, MarkLogic _ → GT
    SparkHDFS x, SparkHDFS y → compare1 x y
    SparkHDFS _, _ → LT
    _, SparkHDFS _ → GT
    SparkLocal x, SparkLocal y → compare1 x y
    SparkLocal _, _ → LT
    _, SparkLocal _ → GT
    Mimir x, Mimir y → compare1 x y
    Mimir _, _ → LT
    _, Mimir _ → GT
    Unknown xName x, Unknown yName y → compare xName yName <> compare1 x y

instance showMount ∷ (Show (f TS.TacitString), Functor f) ⇒ Show (MountF f) where
  show =
    let
      show' ∷ ∀ a. Show a ⇒ f a → String
      show' = map (show >>> TS.hush) >>> show
    in case _ of
      View p → "(View " <> show' p <> ")"
      Module p → "(Module " <> show' p <> ")"
      MongoDB p → "(MongoDB " <> show' p <> ")"
      Couchbase p → "(Couchbase " <> show' p <> ")"
      MarkLogic p → "(MarkLogic " <> show' p <> ")"
      SparkHDFS p → "(SparkHDFS " <> show' p <> ")"
      SparkLocal p → "(SparkLocal " <> show' p <> ")"
      Mimir p → "(Mimir " <> show' p <> ")"
      Unknown n p → "(Unknown " <> n <> " " <> show' p <> ")"

-- | Attempts to decode a mount listing value from Quasar's filesystem metadata,
-- | for a mount in the specified parent directory.
fromJSON ∷ AbsDir → Json → Either String Mount
fromJSON parent = decodeJson >=> \obj → do
  mount ← obj .? "mount"
  typ ← obj .? "type"
  name' ← note "empty name" <<< fromString =<< (obj .? "name")
  let
    err ∷ ∀ a. Either String a
    err = Left $ "Unexpected type '" <> typ <> "' for mount '" <> mount <> "'"
    onFile ∷ Either String (Identity AbsFile)
    onFile = if typ == "file" then Right $ Identity $ parent </> file' (Name name') else err
    onDir ∷ Either String (Identity AbsDir)
    onDir = if typ == "directory" then Right $ Identity $ parent </> dir' (Name name') else err
    onAnyPath ∷ Either String (Identity AbsPath)
    onAnyPath = map (map Left) onDir <|> map (map Right) onFile
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

foldPath ∷ ∀ r. (AbsDir → r) → (AbsFile → r) → Mount → r
foldPath onDir onPath = overPath (onDir >>> Const) (onPath >>> Const) >>> unwrap

getPath ∷ Mount → AbsPath
getPath = foldPath Left Right

getName ∷ Mount → Either (Maybe (Name Dir)) (Name File)
getName = getPath >>> bimap name fileName

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

overPath ∷ ∀ f. Functor f ⇒ (AbsDir → f AbsDir) → (AbsFile → f AbsFile) → Mount → f Mount
overPath overDir overFile = case _ of
  View (Identity file) → overFile file <#> Identity >>> View
  Module (Identity dir) → overDir dir <#> Identity >>> Module
  MongoDB (Identity dir) → overDir dir <#> Identity >>> MongoDB
  Couchbase (Identity dir) → overDir dir <#> Identity >>> Couchbase
  MarkLogic (Identity dir) → overDir dir <#> Identity >>> MarkLogic
  SparkHDFS (Identity dir) → overDir dir <#> Identity >>> SparkHDFS
  SparkLocal (Identity dir) → overDir dir <#> Identity >>> SparkLocal
  Mimir (Identity dir) → overDir dir <#> Identity >>> Mimir
  Unknown name (Identity path) → case path of
    Left dir → overDir dir <#> Left >>> Identity >>> Unknown name
    Right file → overFile file <#> Right >>> Identity >>> Unknown name
