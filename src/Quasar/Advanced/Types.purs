module Quasar.Advanced.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pt
import Data.String as Str
import Data.Traversable (for, traverse)

import OIDCCryptUtils.JSONWebKey (JSONWebKey)
import OIDCCryptUtils.Types (Issuer(..), ClientID(..))

data Operation
  = Add
  | Read
  | Modify
  | Delete


instance eqOperation ∷ Eq Operation where
  eq Add Add = true
  eq Read Read = true
  eq Modify Modify = true
  eq Delete Delete = true
  eq _ _ = false

instance ordOperation ∷ Ord Operation where
  compare Add Add = EQ
  compare Modify Modify = EQ
  compare Read Read = EQ
  compare Delete Delete = EQ
  compare Add _ = LT
  compare _ Add = GT
  compare Read _ = LT
  compare _ Read = GT
  compare Modify _ = LT
  compare _ Modify = GT
  compare Delete _ = LT
  compare _ Delete = GT

instance encodeJsonOperation ∷ EncodeJson Operation where
  encodeJson Add = encodeJson "Add"
  encodeJson Read = encodeJson "Read"
  encodeJson Modify = encodeJson "Modify"
  encodeJson Delete = encodeJson "Delete"

instance decodeJsonOperation ∷ DecodeJson Operation where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "Add" → pure Add
      "Read" → pure Read
      "Modify" → pure Modify
      "Delete" → pure Delete
      _ → Left "Incorrect permission"


data AccessType
  = Structural
  | Content
  | Mount

instance eqAccessType ∷ Eq AccessType where
  eq Structural Structural = true
  eq Content Content = true
  eq Mount Mount = true

instance ordAccessType ∷ Ord AccessType where
  compare Structural Structural = EQ
  compare Content Content = EQ
  compare Mount Mount = EQ
  compare Structural _ = LT
  compare _ Structural = GT
  compare Content _ = LT
  compare _ Content = GT
  compare Mount _ = LT
  compare _ Mount = GT

instance encodeJsonAccessType ∷ EncodeJson AccessType where
  encodeJson Structural = encodeJson "Structural"
  encodeJson Content = encodeJson "Content"
  encodeJson Mount = encodeJson "Mount"

instance decodeJsonAccessType ∷ DecodeJson AccessType where
  decodeJson = decodeJson >=> case _ of
    "Structural" → pure Structural
    "Content" → pure Content
    "Mount" → pure Mount
    _ → Left "Incorrect resource type"

data Resource
  = File (Pt.AbsFile Pt.Sandboxed)
  | Dir (Pt.AbsDir Pt.Sandboxed)
  | Group (Pt.AbsFile Pt.Sandboxed)

instance eqResource ∷ Eq Resource where
  eq (File a) (File b) = Pt.printPath a == Pt.printPath b
  eq (Dir a) (Dir b) = Pt.printPath a == Pt.printPath b
  eq (Group a) (Group b) = Pt.printPath a == Pt.printPath b
  eq _ _ = false

instance ordResource ∷ Ord Resource where
  compare (File a) (File b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (Dir a) (Dir b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (Group a) (Group b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (File _) _ = LT
  compare _ (File _) = GT
  compare (Dir _) _ = LT
  compare _ (Dir _) = GT
  compare (Group _) _ = LT
  compare _ (Group _) = GT

instance encodeJsonResource ∷ EncodeJson Resource where
  encodeJson (File pt) = encodeJson $ "data:" <> Pt.printPath pt
  encodeJson (Dir pt) = encodeJson $ "data:" <> Pt.printPath pt
  encodeJson (Group pt) = encodeJson $ "group:" <> Pt.printPath pt


instance decodeJsonResource ∷ DecodeJson Resource where
  decodeJson js = do
    str ← decodeJson js
    let
      groupPath = Str.stripPrefix "group:" str
      filePath = Str.stripPrefix "data:" str
    case groupPath, filePath of
      Nothing, Nothing → Left "Incorrect resource"
      Just pt, _ →
        map Group $ lmap (const $ "Incorrect group resource") $ parseFile pt
      _, Just pt →
        (map File $ lmap (const $ "Incorrect file resource") $ parseFile pt)
        <|>
        (map Dir $ lmap (const $ "Incorrect directory resource") $ parseDir pt)

parseFile ∷ String → Either String (Pt.AbsFile Pt.Sandboxed)
parseFile pt =
  Pt.parseAbsFile pt
  >>= Pt.sandbox Pt.rootDir
  <#> (Pt.rootDir </> _)
  # maybe (Left "Incorrect resource") pure

parseDir ∷ String → Either String (Pt.AbsDir Pt.Sandboxed)
parseDir pt =
  Pt.parseAbsDir pt
  >>= Pt.sandbox Pt.rootDir
  <#> (Pt.rootDir </> _)
  # maybe (Left "Incorrect resource") pure

type ActionR =
  { operation ∷ Operation
  , resource ∷ Resource
  , accessType ∷ AccessType
  }

newtype Action = Action ActionR
runAction ∷ Action → ActionR
runAction (Action r) = r

instance eqAction ∷ Eq Action where
  eq (Action a) (Action b) =
    a.operation == b.operation
    && a.resource == b.resource
    && a.accessType == b.accessType

instance ordAction ∷ Ord Action where
  compare (Action a) (Action b) =
    compare a.operation b.operation
    <> compare a.resource b.resource
    <> compare a.accessType b.accessType

instance encodeJsonAction ∷ EncodeJson Action where
  encodeJson (Action obj) =
    "operation" := obj.operation
    ~> "resource" := obj.resource
    ~> "accessType" := obj.accessType
    ~> jsonEmptyObject

instance decodeJsonAction ∷ DecodeJson Action where
  decodeJson = decodeJson >=> \obj →
    { operation: _
    , resource: _
    , accessType: _
    }
    <$> (obj .? "operation")
    <*> (obj .? "resource")
    <*> (obj .? "accessType")
    <#> Action


newtype UserId = UserId String
runUserId ∷ UserId → String
runUserId (UserId s) = s

instance eqUserId ∷ Eq UserId where
  eq (UserId a) (UserId b) = a == b

instance ordUserId ∷ Ord UserId where
  compare (UserId a) (UserId b) = compare a b

instance encodeJsonUserId ∷ EncodeJson UserId where
  encodeJson = runUserId >>> encodeJson

instance decodeJsonUserId ∷ DecodeJson UserId where
  decodeJson = map UserId <<< decodeJson


newtype TokenId = TokenId String
runTokenId ∷ TokenId → String
runTokenId (TokenId s) = s

instance eqTokenId ∷ Eq TokenId where
  eq (TokenId a) (TokenId b) = eq a b

instance ordTokenId ∷ Ord TokenId where
  compare (TokenId a) (TokenId b) = compare a b

instance encodeJsonTokenId ∷ EncodeJson TokenId where
  encodeJson = runTokenId >>> encodeJson

instance decodeJsonTokenId ∷ DecodeJson TokenId where
  decodeJson j = Debug.Trace.spy $
    (map TokenId $ decodeJson j)

newtype PermissionId = PermissionId String
runPermissionId ∷ PermissionId → String
runPermissionId (PermissionId s) = s

instance eqPermissionId ∷ Eq PermissionId where
  eq (PermissionId a) (PermissionId b) = a == b

instance ordPermissionId ∷ Ord PermissionId where
  compare (PermissionId a) (PermissionId b) = compare a b

instance encodeJsonPermissionId ∷ EncodeJson PermissionId where
  encodeJson = runPermissionId >>> encodeJson

instance decodeJsonPermissionId ∷ DecodeJson PermissionId where
  decodeJson j =
    (map PermissionId $ decodeJson j)

data GrantedTo
  = UserGranted UserId
  | GroupGranted (Pt.AbsFile Pt.Sandboxed)
  | TokenGranted TokenId

instance eqGrangedTo ∷ Eq GrantedTo where
  eq (UserGranted a) (UserGranted b) = a == b
  eq (GroupGranted a) (GroupGranted b) = Pt.printPath a == Pt.printPath b
  eq (TokenGranted a) (TokenGranted b) = a == b

instance ordGrantedTo ∷ Ord GrantedTo where
  compare (UserGranted a) (UserGranted b) = compare a b
  compare (GroupGranted a) (GroupGranted b) = compare (Pt.printPath a) (Pt.printPath b)
  compare (TokenGranted a) (TokenGranted b) = compare a b
  compare (UserGranted _) _ = LT
  compare _ (UserGranted _) = GT
  compare (GroupGranted _) _ = LT
  compare _ (GroupGranted _) = GT
  compare (TokenGranted _) _ = LT
  compare _ (TokenGranted _) = GT

instance encodeJsonGrantedTo ∷ EncodeJson GrantedTo where
  encodeJson (UserGranted uid) = encodeJson uid
  encodeJson (GroupGranted pt) = encodeJson $ Pt.printPath pt
  encodeJson (TokenGranted tk) = encodeJson tk

instance decodeJsonGrantedTo ∷ DecodeJson GrantedTo where
  decodeJson j =
    (decodeJson j >>= parseGroup <#> GroupGranted)
    <|>
    (decodeJson j >>= checkUserId >>= parseUserId  <#> UserGranted)
    <|>
    (decodeJson j >>= parseTokenId <#> TokenGranted)
    where
    checkUserId ∷ String → Either String String
    checkUserId str =
      -- Right now we don't know what's exact format of TokenId
      -- I suggest not having `@` should be enough to say that
      -- string isn't email.
      if isJust (Str.indexOf "@" str)
        then pure str
        else Left "Incorrect email"

    parseUserId ∷ String → Either String UserId
    parseUserId str =
      Str.stripPrefix "user:" str # maybe (Left "Incorrect user") (pure <<< UserId)

    parseTokenId ∷ String → Either String TokenId
    parseTokenId str =
      Str.stripPrefix "token:" str # maybe (Left "Incorrect token") (pure <<< TokenId)

parseGroup ∷ String → Either String (Pt.AbsFile Pt.Sandboxed)
parseGroup string =
  Str.stripPrefix "group:" string
  # maybe (Left "Incorrect group") pure
  >>= parseFile


type PermissionR =
  { id ∷ PermissionId
  , action ∷ ActionR
  , grantedTo ∷ GrantedTo
  }

newtype Permission = Permission PermissionR
runPermission ∷ Permission → PermissionR
runPermission (Permission r) = r

instance decodeJsonPermission ∷ DecodeJson Permission where
  decodeJson =  decodeJson >=> \obj →
    { id: _
    , action: _
    , grantedTo: _
    }
    <$> (obj .? "id")
    <*> ((obj .? "action") <#> runAction)
    <*> (obj .? "grantedTo")
    <#> Permission


type GroupInfoR =
  { members ∷ Array UserId
  , allMembers ∷ Array UserId
  , subGroups ∷ Array (Pt.AbsFile Pt.Sandboxed)
  }

newtype GroupInfo = GroupInfo GroupInfoR
runGroupInfo ∷ GroupInfo → GroupInfoR
runGroupInfo (GroupInfo r) = r

instance decodeJsonGroupInfo ∷ DecodeJson GroupInfo where
  decodeJson = decodeJson >=> \obj →
    { members: _
    , allMembers: _
    , subGroups: _
    }
    <$> (obj .? "members")
    <*> (obj .? "allMembers")
    <*> ((obj .? "subGroups") >>= extractGroups)
    <#> GroupInfo
    where
    extractGroups ∷ Array String → Either String (Array (Pt.AbsFile Pt.Sandboxed))
    extractGroups =
      traverse (\x → maybe (Left "Incorrect subgroup") pure
                     $ Pt.parseAbsFile x
                     >>= Pt.sandbox Pt.rootDir
                     <#> (\x → Pt.rootDir </> x)
               )


type GroupPatchR =
  { addUsers ∷ Array UserId
  , removeUsers ∷ Array UserId
  }

newtype GroupPatch = GroupPatch GroupPatchR
runGroupPatch ∷ GroupPatch → GroupPatchR
runGroupPatch (GroupPatch r) = r

instance encodeJsonGroupPatch ∷ EncodeJson GroupPatch where
  encodeJson (GroupPatch obj) =
    "addUsers" := obj.addUsers
    ~> "removeUsers" := obj.removeUsers
    ~> jsonEmptyObject

data ShareableSubject
  = UserSubject UserId
  | GroupSubject (Pt.AbsFile Pt.Sandboxed)

instance encodJsonShareableSubject ∷ EncodeJson ShareableSubject where
  encodeJson (UserSubject (UserId uid)) =
    encodeJson $ "user:" <> uid
  encodeJson (GroupSubject fp) =
    encodeJson $ "group:" <> Pt.printPath fp


type ShareRequestR =
  { users ∷ Array UserId
  , groups ∷ Array (Pt.AbsFile Pt.Sandboxed)
  , actions ∷ Array ActionR
  }


newtype ShareRequest = ShareRequest ShareRequestR
runShareRequest ∷ ShareRequest → ShareRequestR
runShareRequest (ShareRequest r) = r

instance encodeJsonShareRequest ∷ EncodeJson ShareRequest where
  encodeJson (ShareRequest obj) =
    "subjects" := ((map (append "user:" <<< runUserId) obj.users)
                   <> map (append "group:" <<< Pt.printPath) obj.groups)
    ~> "actions" := (map Action $ obj.actions)
    ~> jsonEmptyObject


newtype TokenHash = TokenHash String
runTokenHash ∷ TokenHash → String
runTokenHash (TokenHash s) = s

instance encodeJsonTokenHash ∷ EncodeJson TokenHash where
  encodeJson = runTokenHash >>> encodeJson

instance decodeJsonTokenHash ∷ DecodeJson TokenHash where
  decodeJson = map TokenHash <<< decodeJson


newtype TokenName = TokenName String
runTokenName ∷ TokenName → String
runTokenName (TokenName s) = s

instance encodeJsonTokenName ∷ EncodeJson TokenName where
  encodeJson = runTokenName >>> encodeJson

instance decodeJsonTokenName ∷ DecodeJson TokenName where
  decodeJson = map TokenName <<< decodeJson

instance eqTokenName ∷ Eq TokenName where
  eq (TokenName a) (TokenName b) = a == b


type TokenR =
  { id ∷ TokenId
  , secret ∷ TokenHash
  , name ∷ Maybe TokenName
  , actions ∷ Array ActionR
  }

newtype Token = Token TokenR
runToken ∷ Token → TokenR
runToken (Token r) = r

instance decodeJsonToken ∷ DecodeJson Token where
  decodeJson = decodeJson >=> \obj →
    { id: _
    , secret: _
    , name: _
    , actions: _
    }
    <$> (obj .? "id")
    <*> (obj .? "secret")
    <*> (((obj .? "name")
            >>= \x → if runTokenName x == ""
                     then pure Nothing
                     else pure $ Just x)
         <|> pure Nothing)
    <*> (obj .? "actions" <#> (map runAction))
    <#> Token


type OpenIDConfigurationR =
  { issuer ∷ Issuer
  , authorizationEndpoint ∷ String
  , tokenEndpoint ∷ String
  , userinfoEndpoint ∷ String
  , jwks ∷ Array JSONWebKey
  }

newtype OpenIDConfiguration = OpenIDConfiguration OpenIDConfigurationR
runOpenIDConfiguration ∷ OpenIDConfiguration → OpenIDConfigurationR
runOpenIDConfiguration (OpenIDConfiguration r) = r

instance decodeJSONOIDC ∷ DecodeJson OpenIDConfiguration where
  decodeJson = decodeJson >=> \obj → do
    issuer ← Issuer <$> obj .? "issuer"
    authorizationEndpoint ← obj .? "authorization_endpoint"
    tokenEndpoint ← obj .? "token_endpoint"
    userinfoEndpoint ← obj .? "userinfo_endpoint"
    jwks ← obj .? "jwks"
    pure
      $ OpenIDConfiguration
          { issuer, authorizationEndpoint, tokenEndpoint, userinfoEndpoint, jwks }

type ProviderR =
  { displayName ∷ String
  , clientID ∷ ClientID
  , openIDConfiguration ∷ OpenIDConfigurationR
  }

newtype Provider = Provider ProviderR
runProvider ∷ Provider → ProviderR
runProvider (Provider r) = r

instance decodeJsonProvider ∷ DecodeJson Provider where
  decodeJson = decodeJson >=> \obj → do
    displayName ← obj .? "display_name"
    clientID ← ClientID <$> obj .? "client_id"
    openIDConfiguration ← obj .? "openid_configuration" <#> runOpenIDConfiguration
    pure $ Provider { displayName, clientID, openIDConfiguration }
