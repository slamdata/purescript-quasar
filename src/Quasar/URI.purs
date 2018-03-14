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

module Quasar.URI
  ( QAbsoluteURI
  , qAbsoluteURI
  , MongoURI
  , mongoURI
  , QRelativeRef
  , qRelativeRef
  , QURIRef
  , qURIRef
  , QURI
  , qURI
  , QHierarchicalPart
  , QRelativePart
  , QURIHost'
  , QURIHost
  , QURIHosts
  , QQuery
  , AbsPath
  , AnyPath
  , QAuthority
  , opts
  , module URI
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.Codec (BasicCodec, basicCodec)
import Data.Either (Either(..), either, note)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Record.Builder as Builder
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.These (These)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Pathy (Name(..), foldPath, posixParser)
import Pathy as Py
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Type.Row (class RowListNub, class RowToList)
import URI (PathAbsolute, PathRootless)
import URI (URI(..), RelativePart(..), Authority(..), AbsoluteURI(..), HierarchicalPart(..), HierPath, Host(..), Path(..), Port, RelativeRef(..), URIRef, Fragment, Query, UserInfo) as URI
import URI.AbsoluteURI (AbsoluteURIOptions) as URI
import URI.AbsoluteURI (print, parser) as AbsoluteURI
import URI.Common (URIPartParseError(..))
import URI.Extra.MultiHostPortPair (MultiHostPortPair) as URI
import URI.Extra.MultiHostPortPair (print, parser) as MultiHostPortPair
import URI.Extra.QueryPairs (print, parse, keyToString, valueToString, keyFromString, valueFromString) as QueryPairs
import URI.Extra.QueryPairs (QueryPairs(..), Key, Value) as URI
import URI.Extra.UserPassInfo (print, parse) as UserPassInfo
import URI.Extra.UserPassInfo (UserPassInfo(..)) as URI
import URI.HostPortPair (HostPortPair) as URI
import URI.HostPortPair (print, parser) as HostPortPair
import URI.Path (Path)
import URI.Path (print) as Path
import URI.Path.Absolute (print, PathAbsolute(..)) as PathAbsolute
import URI.Path.NoScheme (print, PathNoScheme(..)) as PathNoScheme
import URI.Path.Rootless (print) as PathRootless
import URI.Path.Segment (PathSegment, PathSegmentNZ, segmentFromString, unsafeSegmentNZFromString, unsafeSegmentNZNCFromString)
import URI.RelativeRef (print, parser, RelPath) as RelativeRef
import URI.RelativeRef (RelativeRefOptions) as URI
import URI.Scheme (Scheme) as URI
import URI.URIRef (print, parser) as URIRef
import URI.URIRef (URIRefOptions) as URI
import URI.URI (print, parser) as URI'
import URI.URI (URIOptions) as URI

type AbsPath = Py.AbsPath
type AnyPath = Either Py.AbsPath Py.RelPath

type QURIHost' = These URI.Host URI.Port
type QURIHost = URI.HostPortPair URI.Host URI.Port
type QURIHosts = URI.MultiHostPortPair URI.Host URI.Port

type QAuthority = URI.Authority URI.UserPassInfo QURIHost
type QQuery = URI.QueryPairs String String

type QHierarchicalPart = URI.HierarchicalPart URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath
type QRelativePart = URI.RelativePart URI.UserPassInfo QURIHost (Maybe AbsPath) AnyPath

type QAbsoluteURI = URI.AbsoluteURI URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath QQuery
type QAbsoluteURIOptions = URI.AbsoluteURIOptions URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath QQuery

type MongoURI = URI.AbsoluteURI URI.UserPassInfo QURIHosts (Maybe AbsPath) AbsPath QQuery
type MongoURIOptions = URI.AbsoluteURIOptions URI.UserPassInfo QURIHosts (Maybe AbsPath) AbsPath QQuery

type QRelativeRef = URI.RelativeRef URI.UserPassInfo QURIHost (Maybe AbsPath) AnyPath QQuery URI.Fragment
type QRelativeRefOptions = URI.RelativeRefOptions URI.UserPassInfo QURIHost (Maybe AbsPath) AnyPath QQuery URI.Fragment

type QURIRef = URI.URIRef URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath AnyPath QQuery URI.Fragment
type QURIRefOptions = URI.URIRefOptions URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath AnyPath QQuery URI.Fragment

type QURI = URI.URI URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath QQuery URI.Fragment
type QURIOptions = URI.URIOptions URI.UserPassInfo QURIHost (Maybe AbsPath) AbsPath QQuery URI.Fragment

qAbsoluteURI ∷ BasicCodec (Either ParseError) String QAbsoluteURI
qAbsoluteURI = basicCodec
  (flip runParser $ AbsoluteURI.parser opts.absoluteURI)
  (AbsoluteURI.print opts.absoluteURI)

mongoURI ∷ BasicCodec (Either ParseError) String MongoURI
mongoURI = basicCodec
  (flip runParser $ AbsoluteURI.parser opts.mongoURI)
  (AbsoluteURI.print opts.mongoURI)

qRelativeRef ∷ BasicCodec (Either ParseError) String QRelativeRef
qRelativeRef = basicCodec
  (flip runParser $ RelativeRef.parser opts.relativeRef)
  (RelativeRef.print opts.relativeRef)

qURIRef ∷ BasicCodec (Either ParseError) String QURIRef
qURIRef = basicCodec
  (flip runParser $ URIRef.parser opts.uriRef)
  (URIRef.print opts.uriRef)

qURI ∷ BasicCodec (Either ParseError) String QURI
qURI = basicCodec
  (flip runParser $ URI'.parser opts.uri)
  (URI'.print opts.uri)

opts ∷
  { absoluteURI ∷ Record QAbsoluteURIOptions
  , mongoURI ∷ Record MongoURIOptions
  , relativeRef ∷ Record QRelativeRefOptions
  , uriRef ∷ Record QURIRefOptions
  , uri ∷ Record QURIOptions
  }
opts =
  { absoluteURI: _common `union` _Host `union` _Path `union` _HierPath
  , mongoURI: _common `union` _Hosts `union` _Path `union` _HierPath
  , relativeRef: _common `union` _Host `union` _Path`union` _Fragment `union` _RelPath
  , uriRef: _common `union` _Host `union` _HierPath `union` _Path `union` _Fragment `union` _RelPath
  , uri: _common `union` _Host `union` _HierPath `union` _Path `union` _Fragment
  }
  where
  _common = _UserInfo `union` _Query

  _UserInfo = { parseUserInfo, printUserInfo }
  _Host = { parseHosts: parseHost, printHosts: printHost }
  _Hosts = { parseHosts, printHosts }
  _Query = { parseQuery, printQuery }
  _Path = { parsePath, printPath }
  _HierPath = { parseHierPath, printHierPath }
  _Fragment = { parseFragment, printFragment }
  _RelPath = { parseRelPath, printRelPath }

  parseQuery ∷ URI.Query → Either URIPartParseError QQuery
  parseQuery = QueryPairs.parse (QueryPairs.keyToString >>> pure) (QueryPairs.valueToString >>> pure)
  printQuery ∷ QQuery → URI.Query
  printQuery = QueryPairs.print QueryPairs.keyFromString QueryPairs.valueFromString

  parseUserInfo ∷ URI.UserInfo → Either URIPartParseError URI.UserPassInfo
  parseUserInfo = UserPassInfo.parse
  printUserInfo ∷ URI.UserPassInfo → URI.UserInfo
  printUserInfo = UserPassInfo.print

  parseHost ∷ Parser String QURIHost
  parseHost = HostPortPair.parser pure pure
  printHost ∷ QURIHost → String
  printHost = HostPortPair.print id id

  parseHosts ∷ Parser String QURIHosts
  parseHosts = MultiHostPortPair.parser pure pure
  printHosts ∷ QURIHosts → String
  printHosts = MultiHostPortPair.print id id

  parsePath ∷ Path → Either URIPartParseError (Maybe AbsPath)
  parsePath = case _ of
    URI.Path [] → pure Nothing
    p → Just <$> _parseAbsPath (Path.print p)
  printPath ∷ Maybe AbsPath → Path
  printPath = case _ of
    Nothing → URI.Path []
    Just absP →
      case bimap viewAbsDir viewAbsFile absP of
        Left d →
          URI.Path
            $ (fromFoldable d <#> runName >>> segmentFromString) <> [ forceTrailingSlash ]
        Right (Tuple d n) →
          URI.Path
            $ (fromFoldable d <#> asSegment) <> [asSegment n]


  parseHierPath ∷ Either PathAbsolute PathRootless → Either URIPartParseError AbsPath
  parseHierPath = _parseAbsPath <<< either PathAbsolute.print PathRootless.print
  printHierPath ∷ AbsPath → Either PathAbsolute PathRootless
  printHierPath = _printAbsPath >>> Left

  parseFragment ∷ URI.Fragment → Either URIPartParseError URI.Fragment
  parseFragment = Right
  printFragment ∷ URI.Fragment → URI.Fragment
  printFragment = id

  printRelPath ∷ AnyPath → RelativeRef.RelPath
  printRelPath = bimap _printAbsPath _printRelPath
  parseRelPath ∷ RelativeRef.RelPath → Either URIPartParseError AnyPath
  parseRelPath = bitraverse
    (PathAbsolute.print >>> _parseAbsPath)
    (PathNoScheme.print >>> _parseRelPath)

  _printAbsPath ∷ Py.AbsPath → PathAbsolute
  _printAbsPath = bimap viewAbsDir viewAbsFile >>> case _ of
    Left Nil → PathAbsolute.PathAbsolute Nothing
    Left (Cons head tail) → PathAbsolute.PathAbsolute $ Just
      $ Tuple (asSegmentNZ head)
      $ (asSegment <$> fromFoldable tail) <> [ forceTrailingSlash ]
    Right (Tuple d n) → case d of
      Nil → PathAbsolute.PathAbsolute $ Just $ Tuple (asSegmentNZ n) []
      Cons head tail → PathAbsolute.PathAbsolute
        $ Just
        $ Tuple (asSegmentNZ head)
        $ (asSegment <$> fromFoldable tail) <> [ asSegment n ]

  _printRelPath ∷ Py.RelPath → PathNoScheme.PathNoScheme
  _printRelPath = bimap viewRelDir viewRelFile >>> case _ of
    Left Nil → PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString currentDirSegment) []
    Left (Cons head tail) →
      PathNoScheme.PathNoScheme
        $ Tuple (unsafeSegmentNZNCFromString $ maybe parentDirSegment (un Name) head)
        $ (segmentFromString <<< maybe ".." runName <$> fromFoldable tail) <> [ forceTrailingSlash ]

    Right (Tuple d n) → case d of
      Nil → PathNoScheme.PathNoScheme $ Tuple (unsafeSegmentNZNCFromString $ un Name n) []
      Cons head tail → PathNoScheme.PathNoScheme
        $ Tuple (unsafeSegmentNZNCFromString $ maybe parentDirSegment (un Name) head)
        $ (segmentFromString <<< maybe ".." runName <$> fromFoldable tail) <> [ asSegment n ]

  
  -- Array of segments is joined using "/" so to have trailing slash in rendered
  -- string for dir pathes, we need to use this empty segment.
  forceTrailingSlash ∷ PathSegment
  forceTrailingSlash = segmentFromString ""
    
  currentDirSegment ∷ NonEmptyString
  currentDirSegment = case NES.fromString "." of
    Nothing → unsafeCrashWith "unreachable case in currentDirSegment"
    Just a → a
  parentDirSegment ∷ NonEmptyString
  parentDirSegment = case NES.fromString ".." of
    Nothing → unsafeCrashWith "unreachable case in parentDirSegment"
    Just a → a
  _parseAbsPath ∷ String → Either URIPartParseError Py.AbsPath
  _parseAbsPath =
    Py.parsePath posixParser
      (const Nothing)
      (Just <<< Left)
      (const Nothing)
      (Just <<< Right)
      Nothing
    >>> note (URIPartParseError "Could not parse valid absolute path")

  _parseRelPath ∷ String → Either URIPartParseError Py.RelPath
  _parseRelPath =
    Py.parsePath posixParser
      (Just <<< Left)
      (const Nothing)
      (Just <<< Right)
      (const Nothing)
      Nothing
    >>> note (URIPartParseError "Could not parse valid relative path")

-- Union which rejects duplicates
union
  ∷ ∀ r1 r2 r3 r3l
   . Union r1 r2 r3
  ⇒ RowToList r3 r3l
  ⇒ RowListNub r3l r3l
  ⇒ { | r1 }
  → { | r2 }
  → { | r3 }
union r1 r2 = Builder.build (Builder.merge r2) r1

asSegmentNZ ∷ ∀ a. Py.Name a → PathSegmentNZ
asSegmentNZ = un Py.Name >>> unsafeSegmentNZFromString

asSegment ∷ ∀ a. Py.Name a → PathSegment
asSegment = runName >>> segmentFromString

runName ∷ ∀ a. Py.Name a → String
runName = un Py.Name >>> NES.toString

viewAbsDir ∷ Py.Path Py.Abs Py.Dir → List (Py.Name Py.Dir)
viewAbsDir = reverse <<< go
  where
  go p = foldPath Nil
    (\_ → unsafeCrashWith "ParentOf node in viewDir")
    (flip Cons <<< go) p

viewAbsFile ∷ Py.Path Py.Abs Py.File → Tuple (List (Py.Name Py.Dir)) (Py.Name Py.File)
viewAbsFile = Py.peelFile >>> lmap viewAbsDir


viewRelDir ∷ Py.Path Py.Rel Py.Dir → List (Maybe (Py.Name Py.Dir))
viewRelDir = reverse <<< go
  where
  go p' = foldPath Nil
    (\p → Cons Nothing (go p))
    (\p n → Cons (Just n) (go p)) p'

viewRelFile ∷ Py.Path Py.Rel Py.File → Tuple (List (Maybe (Py.Name Py.Dir))) (Py.Name Py.File)
viewRelFile = Py.peelFile >>> lmap viewRelDir
