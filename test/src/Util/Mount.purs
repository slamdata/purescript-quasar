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

module Test.Util.Mount where

import Prelude

import Control.Monad.Aff (Aff, attempt, apathize)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)

import Data.Array as Arr
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.StrMap as SM
import Data.URI.Types as URI
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Tuple (Tuple(..))

import Test.Assert (assert)

import Node.FS (FS)
import Node.FS.Stats as FSS
import Node.FS.Aff as FSA

import Test.Assert (assert)
import Test.Util.Effect (Effects)

import Quasar.Mount.SparkFTP as FTP

import Debug.Trace as DT

test :: Aff Effects Unit
test = do
  liftEff $ assert $ expected == actual
  where
  actual = show $ FTP.toJSON { sparkHost: Tuple (URI.NameAddress "sparkhost") Nothing
    , ftpHost: Tuple (URI.NameAddress "ftphost") $ Just 75
    , path: Just $ Right $ rootDir </> dir "foo" </> file "bar"
    , user: Just "maximko"
    , password: Just "secret"
    , props: SM.empty
    }
  expected = "{\"spark-hdfs\":{\"connectionUri\":\"spark://sparkhost?hdfsUrl=ftp%3A%2F%2Fmaximko%3Asecret%40ftphost%3A75&rootPath=%2Ffoo%2Fbar\"}}"
