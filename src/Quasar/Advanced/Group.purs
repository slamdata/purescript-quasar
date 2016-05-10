module Quasar.Advanced.Group where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), jsonEmptyObject, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt

import Quasar.Advanced.User (UserId)

newtype Group = Group (Pt.AbsDir Pt.Sandboxed)
runGroup ∷ Group → Pt.AbsDir Pt.Sandboxed
runGroup (Group pt) = pt

instance encodeJsonGroup ∷ EncodeJson Group where
  encodeJson (Group pt) = encodeJson $ Pt.printPath pt

instance decodeJsonGroup ∷ DecodeJson Group where
  decodeJson js = do
    str ← decodeJson js
    pt ←
      maybe (Left "Incorrect group path") pure
        $ Pt.parseAbsDir str
        >>= Pt.sandbox Pt.rootDir
        <#> (Pt.rootDir </> _)
    pure $ Group pt


newtype GroupResponse =
  GroupResponse
    { members ∷ Array UserId
    , allMembers ∷ Array UserId
    , subGroups ∷ Array Group
    }

instance decodeJsonGroupResponse ∷ DecodeJson GroupResponse where
  decodeJson js = do
    obj ← decodeJson js
    r ←
      { members: _
      , allMembers: _
      , subGroups: _
      }
      <$> (obj .? "members")
      <*> (obj .? "allMembers")
      <*> (obj .? "subGroups")
    pure $ GroupResponse r

newtype ModifyGroupRequest =
  ModifyGroupRequest
    { addUsers ∷ Array UserId
    , removeUsers ∷ Array UserId
    }

instance encodeJsonModifyGroupRequest ∷ EncodeJson ModifyGroupRequest where
  encodeJson (ModifyGroupRequest obj) =
    "addUsers" := obj.addUsers
    ~> "removeUsers" := obj.removeUsers
    ~> jsonEmptyObject
