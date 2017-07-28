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
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Path.Pathy (DirName, FileName, dir, file, pathName, (</>))
import Quasar.Mount.Type as MT
import Quasar.Types (AnyPath, DirPath)

data Mount = Mount MT.MountType AnyPath

derive instance eqMount ∷ Eq Mount
derive instance ordMount ∷ Ord Mount
derive instance genericMount ∷ Generic Mount _
instance showMount ∷ Show Mount where show = genericShow

-- | Attempts to decode a mount listing value from Quasar's filesystem metadata,
-- | for a mount in the specified parent directory.
fromJSON ∷ DirPath → Json → Either String Mount
fromJSON parent = decodeJson >=> \obj → do
  mount ← obj .? "mount"
  typ ← obj .? "type"
  name ← obj .? "name"
  case typ of
    "file" → Right $ Mount (MT.fromName mount) (Right (parent </> file name))
    "directory" → Right $ Mount (MT.fromName mount) (Left (parent </> dir name))
    _ → Left $ "Unknown type '" <> typ <> "' for mount"

getPath ∷ Mount → AnyPath
getPath (Mount _ p) = p

getName ∷ Mount → Either (Maybe DirName) FileName
getName = pathName <<< getPath
