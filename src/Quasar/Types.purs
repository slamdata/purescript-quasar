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

module Quasar.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, decodeJson, (.?), jsonParser)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.Path.Pathy (AbsPath, AbsFile, AbsDir, Sandboxed, (</>))
import Data.Path.Pathy as Pt
import Data.StrMap (StrMap)
import Data.Traversable (traverse)

type AnyPath = AbsPath Sandboxed
type DirPath = AbsDir Sandboxed
type FilePath = AbsFile Sandboxed

type SQL = String
type Vars = StrMap String

type Pagination = { offset ∷ Int, limit ∷ Int }

type CompileResultR =
  { inputs ∷ Array AnyPath
  , physicalPlan ∷ String
  }

newtype CompileResult = CompileResult CompileResultR
runCompileResult ∷ CompileResult → CompileResultR
runCompileResult (CompileResult r) = r

instance decodeJsonCompileResult ∷ DecodeJson CompileResult where
  decodeJson = decodeJson >=> \obj →
      { inputs: _
      , physicalPlan: _
      }
      <$> ((obj .? "inputs") >>= traverse parseAnyPath)
      <*> ((obj .? "physicalPlan") <|> pure "")
      <#> CompileResult

parseFile ∷ String → Either String (Pt.AbsFile Pt.Sandboxed)
parseFile pt =
  Pt.parseAbsFile pt
  >>= Pt.sandbox Pt.rootDir
  <#> (Pt.rootDir </> _)
  # maybe (Left "Incorrect resource") pure

parseAnyPath ∷ String → Either String (Pt.AbsPath Pt.Sandboxed)
parseAnyPath pt = note "Incorrect resource" do
  anyPath ← Pt.parsePath (const Nothing) (Just <<< Left) (const Nothing) (Just <<< Right) pt
  sandboxed ← bitraverse (Pt.sandbox Pt.rootDir) (Pt.sandbox Pt.rootDir) anyPath
  pure $ bimap (Pt.rootDir </> _) (Pt.rootDir </> _) sandboxed

compileResultFromString ∷ String → Either String CompileResultR
compileResultFromString s =
  (jsonParser s >>= decodeJson <#> runCompileResult)
  <|>
  (pure { inputs: [ ], physicalPlan: s})
