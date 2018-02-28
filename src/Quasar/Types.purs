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
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Pathy (class IsDirOrFile, Abs, AbsDir, AbsFile, Path, parseAbsDir, parseAbsFile, posixParser, posixPrinter, printPath, sandboxAny)

printQPath ∷ ∀ b. IsDirOrFile b ⇒ Path Abs b → String
printQPath = sandboxAny >>> printPath posixPrinter

parseQFilePath ∷ String → Maybe AbsFile
parseQFilePath = parseAbsFile posixParser

parseQDirPath ∷ String → Maybe AbsDir
parseQDirPath = parseAbsDir posixParser

type Vars = StrMap String

type Pagination = { offset ∷ Int, limit ∷ Int }

type CompileResultR =
  { inputs ∷ Array AbsFile
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
      <$> ((obj .? "inputs") >>= traverse parseFile)
      <*> ((obj .? "physicalPlan") <|> pure "")
      <#> CompileResult

parseFile ∷ String → Either String AbsFile
parseFile = parseQFilePath >>> note "Could not parse resource"

compileResultFromString ∷ String → Either String CompileResultR
compileResultFromString s =
  (jsonParser s >>= decodeJson <#> runCompileResult)
  <|>
  (pure { inputs: [ ], physicalPlan: s})
