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
import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Eq (class Eq1, eq1)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Newtype (un, unwrap)
import Data.Ord (class Ord1, compare1)
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
      show' :: forall a. Show a ⇒ f a → String
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
fromJSON ∷ DirPath → Json → Either String Mount
fromJSON parent = decodeJson >=> \obj → do
  mount ← obj .? "mount"
  typ ← obj .? "type"
  name ← obj .? "name"
  let
    err :: forall a. Either String a
    err = Left $ "Unexpected type '" <> typ <> "' for mount '" <> mount <> "'"
    onFile :: Either String (Identity FilePath)
    onFile = if typ == "file" then Right $ Identity $ parent </> file name else err
    onDir :: Either String (Identity DirPath)
    onDir = if typ == "directory" then Right $ Identity $ parent </> dir name else err
    onAnyPath :: Either String (Identity AnyPath)
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

getPath ∷ Mount → AnyPath
getPath = foldPath Left Right

getName ∷ Mount → Either (Maybe DirName) FileName
getName = getPath >>> pathName

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

foldPath ∷ ∀ r. (DirPath → r) → (FilePath → r) → Mount → r
foldPath onDir onPath = foldPath'
  (un Identity >>> onDir)
  (un Identity >>> onPath)
  (un Identity >>> either onDir onPath)

foldPath' ∷ ∀ r f. (f DirPath → r) → (f FilePath → r) → (f AnyPath → r) → MountF f → r
foldPath' onDir onPath onAny = overPath' (onDir >>> Const) (onPath >>> Const) (onAny >>> Const) >>> unwrap


overPath ∷ ∀ m. Functor m ⇒ (DirPath → m DirPath) → (FilePath → m FilePath) → Mount → m Mount
overPath overDir overFile = overPath'
  (un Identity >>> overDir >>> map Identity)
  (un Identity >>> overFile >>> map Identity)
  (\(Identity any) -> case any of
    Left dir → overDir dir <#> Left >>> Identity
    Right file → overFile file <#> Right >>> Identity)

overPath'
  ∷ ∀ f g m
  . Functor m
  ⇒ (f DirPath → m (g DirPath))
  → (f FilePath → m (g FilePath))
  → (f AnyPath → m (g AnyPath)) 
  → MountF f → m (MountF g)
overPath' overDir overFile overAny= case _ of
  View file → overFile file <#> View
  Module dir → overDir dir <#> Module
  MongoDB dir → overDir dir <#> MongoDB
  Couchbase dir → overDir dir <#> Couchbase
  MarkLogic dir → overDir dir <#> MarkLogic
  SparkHDFS dir → overDir dir <#> SparkHDFS
  SparkLocal dir → overDir dir <#> SparkLocal
  Mimir dir → overDir dir <#> Mimir
  Unknown name path → overAny path <#> Unknown name


newtype Move a = Move { from :: a, to :: a}

runMountMove :: MountF Move -> Move AnyPath
runMountMove = foldPath'
  (\(Move { to, from }) -> Move { to: Left to, from: Left from })
  (\(Move { to, from }) -> Move { to: Right to, from: Right from })
  id
