-- Copyright 2013 Metamocracy LLC
-- Written by Jim Snow (jsnow@metamocracy.com)
--
-- This file defines the data structures used by Polink,
-- plus all the acid-state boilerplate we need to make
-- the state persistent.
--
-- We use lenses to make state manipulation simpler, though a problem with
-- both acid-state and lenses is that they are both set up to work well
-- with the State monad, but here I use EitherT State.  There's consequently
-- a bit of extra complexity, and there are a lot of potential errors that
-- ought to throw exceptions, but instead fail silently.

-- needed for lens
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards,
             TemplateHaskell, TypeFamilies, OverloadedStrings #-}

-- needed for acid-state
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}

-- needed for ErrorT State instance of MonadState
{-# LANGUAGE FlexibleInstances #-}

-- needed for Yesod
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, 
             TemplateHaskell, OverloadedStrings, TypeSynonymInstances,
             FlexibleInstances, BangPatterns #-}

{-# LANGUAGE ScopedTypeVariables #-}

module PolinkState where

import Data.Data (Data, Typeable)
import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Sequence as SEQ
import Control.Lens -- hiding (left, right)
import Control.Lens.Setter
import Control.Monad.State
import Control.Monad.Trans.Either
--import Control.Error
--import Control.Monad.Error.Class(throwError)
import Control.Applicative  ( (<$>), pure )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, extension, deriveSafeCopy, Migrate, MigrateFrom, migrate )
import Data.Time.Calendar (Day)
import Data.Bits

-- Yesod needs to know its own domain, so it can generate urls appropriately
-- and so forth.  If True, we use http://polink.org as the domain.  If false,
-- it's http://localhost:3000.  (This is a kludge.)
production = True

-- We use a 64 bit int to store permissions in the persistent state, but we convert
-- to a more expressive type when convenient.  (The Int type allows us to extend
-- permissions by using more bits without having to migrate to a new type.)
data Perm = BasicAccess | Read | AddComment | DelComment | AddLink | EditLink | DelLink | AddEntity | EditEntity | DelEntity | AddTag | DelTag | SetPerm | Admin | AddIssue | EditIssue | DelIssue | AddIssueTag | DelIssueTag deriving (Eq, Ord, Show, Enum)

permTest :: Int64 -> Perm -> Bool
permTest i p = testBit i (fromEnum p)

permSet :: Int64 -> Perm -> Int64
permSet i p = setBit i (fromEnum p)

perms :: [Perm] -> Int64
perms ps = foldl permSet 0 ps

defaultPerm :: Int64
defaultPerm = perms [BasicAccess, Read, AddComment, AddLink, AddEntity, AddTag]

editorPerm = perms [BasicAccess, Read, AddComment, AddLink, AddEntity, AddTag, EditLink, DelLink, EditEntity, DelEntity, DelComment, DelTag]

adminPerm = complement 0
commentOnlyPerm = perms [BasicAccess, Read, AddComment]
limitedPerm = perms [BasicAccess, Read]

showPerms :: Int64 -> String
showPerms i =
  let perms = enumFrom BasicAccess
      ss = map (\p -> if permTest i p then show p else "") perms
  in
     L.intercalate " " ss

type Counter = Int64

newtype Url = Url {unUrl:: T.Text}  deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Url)

newtype Eid = Eid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Eid)

newtype Lid = Lid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Lid)

newtype Cid = Cid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Cid)

newtype Uid = Uid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Uid)

newtype PTid = PTid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''PTid)

newtype OTid = OTid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''OTid)

newtype UTid = UTid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''UTid)

newtype Iid = Iid Counter deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Iid)

data Id0 = E0 Eid | L0 Lid | C0 Cid | U0 Uid | PT0 PTid | OT0 OTid | UT0 UTid
  deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Id0)

data Id = E Eid | L Lid | C Cid | U Uid | PT PTid | OT OTid | UT UTid | I Iid
  deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 1 'extension ''Id)

instance Migrate Id where
  type MigrateFrom Id = Id0
  migrate id =
    case id of
      E0 x -> E x
      L0 x -> L x
      C0 x -> C x
      U0 x -> U x
      PT0 x -> PT x
      OT0 x -> OT x
      UT0 x -> UT x

idToInt :: Id -> Int64
idToInt id =
  case id of
    E (Eid eid) -> eid
    L (Lid lid) -> lid
    C (Cid cid) -> cid
    U (Uid uid) -> uid
    PT (PTid ptid) -> ptid
    OT (OTid otid) -> otid
    UT (UTid utid) -> utid
    I (Iid iid) -> iid

-- All the tag types are persisted as Ints.  This allows us to add new tag types
-- without migrating data.  We must be carefull, however, never to re-order or delete
-- constructors, otherwise the Enum typeclass methods will give us different values.
-- Allways add constructors to the end.

data PersonTag =
  Politician | Official | Lobbyist | Commentator | Journalist | Scientist | Author | Academic | BusinessPerson | Celebrity | Artist | Musician | Doctor | Lawyer | Engineer | Teacher | Entertainer | Athlete | ReligiousLeader | Military | PublicServant | CharityWorker | Patron | WhistleBlower | Activist | Criminal | Anonymous | Fictional | NotablePerson
    deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)
$(deriveSafeCopy 0 'base ''PersonTag)

newtype PTag = PTag {unPTag :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''PTag)

ptag :: PersonTag -> PTag
ptag pt = PTag $ fromEnum pt

data OrgTag =
  Nation | State | Province | Region | City | Government | NonProfit | PAC | NGO | Club | Committee | Union | ProfessionalOrg | SpecialInterestGroup | School | Party | Business | Media | Religious | Collaboration | Constituency | Office
    deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)

-- Cast to integer for serialization, so we can extend enum
-- later without having to migrate data.
newtype OTag = OTag {unOTag :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''OTag)


data UserTag =
  Friendly | Helpful | WellInformed 
    deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)

newtype UTag = UTag {unUTag :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''UTag)

data EntityType =
  Person (M.Map PTag PTid) | Organization (M.Map OTag OTid)
    deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''EntityType)

traversePerson :: Traversal EntityType EntityType (M.Map PTag PTid) (M.Map PTag PTid)
traversePerson f (Person m) = Person <$> f m
traversePerson _ o@(Organization _) = pure o

traverseOrg :: Traversal EntityType EntityType (M.Map OTag OTid) (M.Map OTag OTid)
traverseOrg f (Organization m) = Organization <$> f m
traverseOrg _ p@(Person _) = pure p

getPerson :: EntityType -> Maybe (M.Map PTag PTid)
getPerson (Person a) = Just a
getPerson _ = Nothing

getOrg :: EntityType -> Maybe (M.Map OTag OTid)
getOrg (Organization a) = Just a
getOrg _ = Nothing

-- Are two date ranges overlapping in time?
timeOverlap :: Maybe Day -> Maybe Day -> Maybe Day -> Maybe Day -> Bool
timeOverlap astart aend bstart bend =
  not $ aend `mlt` bstart || bend `mlt` astart
  where
    mlt Nothing _ = False
    mlt _ Nothing = False
    mlt a b       = a < b


data Entity = Entity {
  _eid      :: Eid,
  _ecname   :: T.Text,
  _elname   :: T.Text,
  _etype    :: EntityType,
  _ebirth   :: Maybe (Day, Bool),
  _edeath   :: Maybe (Day, Bool),
  _ewp      :: Maybe Url,
  _etwt     :: Maybe Url,
  _ehp      :: Maybe Url
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Entity)

newtype Money = USD {unUSD :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Money)

-- Don't re-order.
data PosLinkType =
  Agree | Endorse | Praise | Contribute | Assist | Marry | Romantic | ParentOf | Member | StudentOf | Award | BestowTitle | DoFavor | Nominate | Appoint | MakeDeal | SubOrg | Invest | Retire | Elect | WorksFor | ContractsFor | HoldsOffice | AsksForHelp | Forgive | Appologize | Defend | Protect | Trust
    deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)

-- Links that are positive in both directions.
isBiDirPL :: PosLinkType -> Bool
isBiDirPL pl =
  case pl of
    Marry        -> True
    Romantic     -> True
    ParentOf     -> True
    Member       -> True
    StudentOf    -> True
    MakeDeal     -> True
    SubOrg       -> True
    WorksFor     -> True
    ContractsFor -> True
    HoldsOffice  -> True
    _            -> False

-- We cast to an Int for the SafeCopy instance so that we can add new link types
-- without having to do migrations. 
newtype PLType = PLType {unPLType :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''PLType)

-- Don't re-order.
data NegLinkType =
  Disagree | Criticize | Discredit | Condemn | Accuse | Sue | Fine | Hinder | Fire | AcceptResignation | Resign | Divorce | Breakup | Assault | Threaten | Kill | Divest | Split | Recall | DeclinesToHelp | EndsRelationship | Insult | Oppose | Distrust | Ridicule
    deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)

-- Links that are negative in both directions.
isBiDirNL :: NegLinkType -> Bool
isBiDirNL nl =
  case nl of
    _ -> False

newtype NLType = NLType {unNLType :: Int} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''NLType)

data LinkType = PL PLType | NL NLType deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''LinkType)

isBiDir :: LinkType -> Bool
isBiDir (PL pl) = isBiDirPL (toEnum $ unPLType pl)
isBiDir (NL nl) = isBiDirNL (toEnum $ unNLType nl)

showLT :: LinkType -> T.Text
showLT (PL pl) = T.pack $ show $ ((toEnum $ unPLType pl) :: PosLinkType)
showLT (NL nl) = T.pack $ show $ ((toEnum $ unNLType nl) :: NegLinkType)

lToText :: LinkType -> T.Text
lToText (PL pl) =
  case toEnum $ unPLType pl of
    Agree ->        "agrees with"
    Endorse ->      "endorses"
    Praise ->       "praises"
    Contribute ->   "contributes money to"
    Assist ->       "assists"
    Marry ->        "marries"
    Romantic ->     "is in a romantic relationship with"
    ParentOf ->     "is the parent of"
    Member ->       "is a member of"
    StudentOf ->    "is a student of"
    Award ->        "bestows award"
    BestowTitle ->  "bestows title"
    DoFavor ->      "does a favor for"
    Nominate ->     "nominates"
    Appoint ->      "appoints"
    MakeDeal ->     "makes a deal with"
    SubOrg ->       "is a sub-organization of"
    Invest ->       "invests in"
    Retire ->       "retired from"
    Elect ->        "elected"
    WorksFor ->     "works for"
    ContractsFor -> "is a contractor for"
    HoldsOffice ->  "holds office"
    AsksForHelp ->  "asks for assistance from"
    Forgive ->      "forgives"
    Appologize ->   "appologizes"
    Defend ->       "defends"
    Protect ->      "protects"
    Trust ->        "trusts"

lToText (NL nl) =
  case toEnum $ unNLType nl of
    Disagree ->     "disagrees with"
    Criticize ->    "criticizes"
    Accuse    ->    "accuses"
    Discredit ->    "discredits"
    Condemn ->      "condemns"
    Sue ->          "sues"
    Fine ->         "fines"
    Hinder ->       "hinders"
    Fire ->         "fires"
    AcceptResignation -> "accepts resignation of"
    Resign  ->      "resigns from"
    Divorce ->      "divorces"
    Breakup ->      "broke off a romantic relationship with"
    Threaten ->     "threatens"
    Assault ->      "assaults"
    Kill ->         "kills"
    Divest ->       "ended investments in"
    Split ->        "split off from"
    Recall ->       "recalled"
    DeclinesToHelp -> "declines to assist"
    EndsRelationship -> "ends business/professional relationship"
    Insult ->       "insults"
    Oppose ->       "opposes"
    Distrust ->     "is suspicious of"
    Ridicule ->     "ridicules"

data Link = Link {
  _lid     :: Lid,
  _lsrc    :: Eid,
  _ldst    :: Eid,
  _ltype   :: LinkType,
  _ldate   :: (Day, Bool),
  _lend    :: Maybe (Day, Bool),
  _lmoney  :: Maybe Money,
  _lurl    :: [Url]
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Link)

data Comment = Comment {
  _cid      :: Cid,
  _author   :: Uid,
  _contents :: Maybe T.Text,
  _parent   :: Id
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Comment)

data User = User {
  _uid       :: Uid,
  _authority :: Int64,
  _name      :: T.Text,
  _email     :: [T.Text],
  _profile   :: Maybe T.Text,
  _contrib   :: SEQ.Seq Id,
  _changes   :: SEQ.Seq Id,
  _agrees    :: S.Set Id,
  _disagrees :: S.Set Id,
  _likes     :: S.Set Id,
  _dislikes  :: S.Set Id,
  _usertags  :: M.Map UTag UTid
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)

data Issue = Issue {
  _iid     :: Iid,
  _iname   :: T.Text,
  _idesc   :: Maybe T.Text,
  _iwp     :: Maybe Url,
  _itagged :: S.Set Id
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''Issue)

data GraphState0 = GraphState0 {
  _nextId0         :: Counter,
  _entities0       :: M.Map Eid Entity,
  _entitiesByName0 :: M.Map T.Text (S.Set Eid),
  _entitiesByWiki0 :: M.Map Url (S.Set Eid),
  _links0          :: M.Map Lid Link,
  _linksBySrc0     :: M.Map Eid (S.Set Lid),
  _linksByDst0     :: M.Map Eid (S.Set Lid),
  _comments0       :: M.Map Cid Comment,
  _cchildren0      :: M.Map Id [Cid],
  _users0          :: M.Map Uid User,
  _usersByName0    :: M.Map T.Text Uid,
  _usersByEmail0   :: M.Map T.Text Uid,
  _ptags0          :: M.Map PTid (PTag, Eid),
  _otags0          :: M.Map OTid (OTag, Eid),
  _utags0          :: M.Map UTid (UTag, Uid),
  _agree0          :: M.Map Id (S.Set Uid),
  _disagree0       :: M.Map Id (S.Set Uid),
  _like0           :: M.Map Id (S.Set Uid),
  _dislike0        :: M.Map Id (S.Set Uid),
  _recent0         :: SEQ.Seq Id
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 0 'base ''GraphState0)

data GraphState = GraphState {
  _nextId         :: Counter,
  _entities       :: M.Map Eid Entity,
  _entitiesByName :: M.Map T.Text (S.Set Eid),
  _entitiesByWiki :: M.Map Url (S.Set Eid),
  _links          :: M.Map Lid Link,
  _linksBySrc     :: M.Map Eid (S.Set Lid),
  _linksByDst     :: M.Map Eid (S.Set Lid),
  _comments       :: M.Map Cid Comment,
  _cchildren      :: M.Map Id [Cid],
  _users          :: M.Map Uid User,
  _usersByName    :: M.Map T.Text Uid,
  _usersByEmail   :: M.Map T.Text Uid,
  _ptags          :: M.Map PTid (PTag, Eid),
  _otags          :: M.Map OTid (OTag, Eid),
  _utags          :: M.Map UTid (UTag, Uid),
  _agree          :: M.Map Id (S.Set Uid),
  _disagree       :: M.Map Id (S.Set Uid),
  _like           :: M.Map Id (S.Set Uid),
  _dislike        :: M.Map Id (S.Set Uid),
  _recent         :: SEQ.Seq Id,
  _issues         :: M.Map Iid Issue,
  _issuetagged    :: M.Map Id (S.Set Iid)
} deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 1 'extension ''GraphState)

instance Migrate GraphState where
  type MigrateFrom GraphState = GraphState0
  migrate (GraphState0 a b c d e f g h i j k l m n o p q r s t) =
    GraphState a b c d e f g h i j k l m n o p q r s t M.empty M.empty


makeLenses ''Entity
makeLenses ''Link
makeLenses ''Comment
makeLenses ''User
makeLenses ''Issue
makeLenses ''GraphState

initialGraphState =
  GraphState {
    _nextId = 0,
    _entities = M.empty,
    _entitiesByName = M.empty,
    _entitiesByWiki = M.empty,
    _links = M.empty,
    _linksBySrc = M.empty,
    _linksByDst = M.empty,
    _comments = M.empty,
    _cchildren = M.empty,
    _users = M.empty,
    _usersByName = M.empty,
    _usersByEmail = M.empty,
    _ptags = M.empty,
    _otags = M.empty,
    _utags = M.empty,
    _agree = M.empty,
    _disagree = M.empty,
    _like = M.empty,
    _dislike = M.empty,
    _recent = SEQ.empty,
    _issues = M.empty,
    _issuetagged = M.empty
  }

type Err = String
type S a = EitherT Err (State GraphState) a

toEither (Just a) e = Right a
toEither Nothing e = Left e

-- With this instance, we don't need to lift every time we use a lens.
-- However, it's already included in the most recent libraries.  Uncomment
-- if compiling with older ghc.
{-
instance MonadState s (EitherT Err (State s)) where
  get = lift get
  put = lift . put
-}

-- I don't like using the Query and Update monads directly, because they're
-- tied to IO and they don't have a good error mechanism, so instead I
-- do everything in terms of ErrorT State, and then convert them with
-- these combinators.
toQ :: S a -> Query GraphState (Either Err a)
toQ action =
  do gs <- ask
     let (result,_) = runState (runEitherT action) gs
     return result


toU :: S a -> Update GraphState (Either Err a)
toU action =
  do gs <- get
     let (result,gs') = runState (runEitherT action) gs
     case result of
       Right val -> 
         do put gs'
            return result
       Left _ -> return result


-- State manipulation actions

-- Throw an error if the action doesn't return an error,
-- don't throw an error if it does.
invertErr :: S a -> Err -> S ()
invertErr action err =
  do succ <- lift $
       do result <- runEitherT action
          case result of
            Left _ -> return True
            Right _ -> return False
     if succ
     then return ()
     else left err

-- Get current state.  This is basically the only query you ever need.
getState :: S GraphState
getState = get

getStateQ = toQ getState

-- Generate a unique ID by incrementing a counter and returning the old value.
-- For simplicity, we use the same counter for everything that needs a unique
-- ID.
bumpId :: S Counter
bumpId = nextId <<%= (+1)

bumpIdU = toU bumpId

-- Get the "User" record corresponding to a UID.
getUser :: Uid -> S User
getUser id =
  do us <- use users
     case us ^. at id of
       Nothing -> left "no such user id"
       Just user -> return user

getUserQ = toQ . getUser

-- Get the "User" record corresponding to a user name.
getUserByName :: T.Text -> S User
getUserByName name =
  do usbn <- use usersByName
     case usbn ^. at name of
       Nothing -> left "no user by that name"
       Just id ->
         getUser id

getUserByNameQ = toQ . getUserByName

-- Get the "User" record corresponding to an email address.  (For browserID auth.)
getUserByEmail :: T.Text -> S User
getUserByEmail email =
  do usbe <- use usersByEmail
     case usbe ^. at email of
       Nothing -> left "no user with that email address"
       Just id ->
         getUser id

getUserByEmailQ = toQ . getUserByEmail

-- Create a user.
addUser :: T.Text -> T.Text -> S Uid
addUser name email =
  do invertErr (getUserByName name) "user by that name already exists"
     invertErr (getUserByEmail email) "user with that email already exists"
     id <- bumpId
     let uid = Uid id
     let perm = case id of
                  0 -> adminPerm
                  _ -> defaultPerm
     let user = User uid perm name [email] Nothing SEQ.empty SEQ.empty S.empty S.empty S.empty S.empty M.empty
     users . at uid ?= user
     usersByName . at name ?= uid
     usersByEmail . at email ?= uid
     addContrib uid (U uid)
     return uid

addUserU n p   = toU $ addUser n p

-- Update a user's access priviledges.
setUserPerm :: Uid -> Int64 -> S ()
setUserPerm uid p =
  do users . at uid . traverse %= (\u -> u {_authority = p})
     return ()

setUserPermU u p = toU $ setUserPerm u p

-- Add a comment to some parent object.  (Can be anything with an ID.)
addComment :: Uid -> T.Text -> Id -> S Cid
addComment uid contents parent =
  do id <- bumpId
     let cid = Cid id
     let comment = Comment cid uid (Just contents) parent
     comments . at cid ?= comment
     cchildren . at parent . traverse %= (cid :)
     addContrib uid (C cid)
     return cid

addCommentU u c p = toU $ addComment u c p


fromJust (Just a) = a
fromJust Nothing = error "coerce from maybe failed"

-- Basic delete, remove text but leave placeholder comment.
delComment :: Uid -> Cid -> S ()
delComment uid cid =
  comments . at cid . traverse . contents .= Nothing

-- Remove all trace of a comment.
removeComment :: Uid -> Cid -> S ()
removeComment uid cid =
  do mcomment <- use $ comments . at cid
     case mcomment of
       Nothing -> return ()
       Just c ->
         do comments %= (M.delete cid)
            cchildren . at (c ^. parent) . traverse %= (L.delete cid)

delCommentU u c = toU $ delComment u c
removeCommentU u c = toU $ removeComment u c

-- We do this data structure initialization because dealing with maps of
-- sets with lenses is easier when you know the map value is already present.
addADSet :: Id -> S ()
addADSet id =
  do agree    . at id ?= S.empty
     disagree . at id ?= S.empty
     like     . at id ?= S.empty
     dislike  . at id ?= S.empty

addCChildren :: Id -> S ()
addCChildren id =
  cchildren . at id ?= []

-- This user agrees with some ID-addressable object.
addAgree :: Uid -> Id -> S ()
addAgree uid id =
  do disagree . at id . traverse %= (S.delete uid)
     agree    . at id . traverse %= (S.insert uid)
     users   . at uid . traverse . agrees %= (S.insert id)
     users   . at uid . traverse . disagrees %= (S.delete id)

addAgreeU u i = toU $ addAgree u i

-- This user disagrees with some ID-addressable object.
addDisagree :: Uid -> Id -> S ()
addDisagree uid id =
  do disagree . at id . traverse %= (S.insert uid)
     agree    . at id . traverse %= (S.delete uid)
     users   . at uid . traverse . agrees %= (S.delete id)
     users   . at uid . traverse . disagrees %= (S.insert id)
     magreeSet <- use $ agree . at id
     -- if no one confirms it's valid, we delete it
     case magreeSet of
       Nothing -> return ()
       Just agreeSet ->
         if S.size agreeSet == 0
         then
           case id of 
             E eid   -> delEntity uid eid
             C cid   -> delComment uid cid
             L lid   -> delLink uid lid
             PT ptid -> delPTag uid ptid
             OT otid -> delOTag uid otid
             _ -> return ()
         else return ()

addDisagreeU u i = toU $ addDisagree u i

-- This user likes this ID-addressable object.
addLike :: Uid -> Id -> S ()
addLike uid id =
  do dislike . at id . traverse %= (S.delete uid)
     like    . at id . traverse %= (S.insert uid)
     users   . at uid . traverse . likes %= (S.insert id)
     users   . at uid . traverse . dislikes %= (S.delete id)

addLikeU u i = toU $ addLike u i

-- This user dislikes this ID-addressable object.
addDislike :: Uid -> Id -> S ()
addDislike uid id =
  do dislike . at id . traverse %= (S.insert uid)
     like    . at id . traverse %= (S.delete uid)
     users  . at uid . traverse . likes %= (S.delete id)
     users  . at uid . traverse . dislikes %= (S.insert id)

addDislikeU u i = toU $ addDislike u i

-- Add a tag to a person or org.  It probably would have made sense to
-- split this into two separate functions...
addTag :: Uid -> Eid -> Either PTag OTag -> S (Either PTid OTid)
addTag uid eid (Left ptag) =
  do ptid <- fmap PTid bumpId
     gs <- get
     let (mexists :: Maybe PTid) =
           do e <- gs ^. entities . (at eid)
              p <- e ^. etype ^. to getPerson
              p ^. (at ptag)
     case mexists of
       Just _ -> left "tag already exists"
       Nothing ->
         do entities . at eid . traverse . etype . traversePerson %= (M.insert ptag ptid) 
            ptags . at ptid ?= (ptag, eid)
            addContrib uid (PT ptid)
            return (Left ptid)

addTag uid eid (Right otag) =
  do otid <- fmap OTid bumpId
     gs <- get
     let (mexists :: Maybe OTid) =
           do e <- gs ^. entities . (at eid)
              p <- e ^. etype ^. to getOrg
              p ^. (at otag)
     case mexists of
       Just _ -> left "tag already exists"
       Nothing ->
         do entities . at eid . traverse . etype . traverseOrg %= (M.insert otag otid) 
            otags . at otid ?= (otag, eid)
            addContrib uid (OT otid)
            return (Right otid)

addTagU u e tag = toU $ addTag u e tag


-- Delete a tag associated with a person.
delPTag :: Uid -> PTid -> S ()
delPTag uid ptid =
  do mtag <- use $ ptags . at ptid
     case mtag of
       Nothing -> left "couldn't find tag"
       Just (ptag, eid) ->
         do entities . at eid . traverse . etype . traversePerson %= (M.delete ptag)
            ptags %= (M.delete ptid)

delPTagU u t = toU $ delPTag u t

-- Delete a tag associated with an organization.
delOTag :: Uid -> OTid -> S ()
delOTag uid otid =
  do mtag <- use $ otags . at otid
     case mtag of
       Nothing -> left "couldn't find tag"
       Just (otag, eid) ->
         do entities . at eid . traverse . etype . traverseOrg %= (M.delete otag)
            otags %= (M.delete otid)

delOTagU u t = toU $ delOTag u t

-- Add a new Entity (either a person or organization).
addEntity :: Uid -> Entity -> S Eid
addEntity uid ent' =
  do id <- bumpId
     let eid = Eid id
     let ent = ent' {_eid = eid}
     entities . at eid ?= ent
     mset <- use (entitiesByName . at (ent ^. ecname))
     case mset of
       Nothing -> entitiesByName . at (ent ^. ecname) ?= (S.singleton eid)
       Just _  -> (entitiesByName . at (ent ^. ecname) . traverse) %= (S.insert eid)

     mset <- use (entitiesByName . at (ent ^. elname))
     case mset of
       Nothing -> entitiesByName . at (ent ^. elname) ?= (S.singleton eid)
       Just _  -> (entitiesByName . at (ent ^. elname) . traverse) %= (S.insert eid)

     case (ent ^. ewp) of
       Nothing -> return ()
       Just wp ->
         do mset <- use (entitiesByWiki . at wp)
            case mset of
              Nothing -> entitiesByWiki . at wp ?= (S.singleton eid)
              Just _  -> (entitiesByWiki . at wp . traverse) %= (S.insert eid)

     linksBySrc %= (M.insert eid S.empty)
     linksByDst %= (M.insert eid S.empty)
     addContrib uid (E eid)
     return eid

addEntityU u e = toU $ addEntity u e

-- Update an existing Entity.
editEntity :: Uid -> Eid -> Entity -> S Eid
editEntity uid eid ent =
  do entities . at eid ?= ent {_eid = eid}
     addContrib uid (E eid)
     return eid

editEntityU u e ent = toU $ editEntity u e ent

-- Delete an existing Entity.
delEntity :: Uid -> Eid -> S ()
delEntity uid eid =
  do mentity <- use $ entities . at eid
     case mentity of
       Nothing -> return ()
       Just e ->
         do srcLinks <- use $ linksBySrc . at eid
            dstLinks <- use $ linksByDst . at eid
            case srcLinks of
              Nothing -> return ()
              Just ls -> forM_ (S.toList ls) (\lid -> delLink uid lid)
            case dstLinks of
              Nothing -> return ()
              Just ls -> forM_ (S.toList ls) (\lid -> delLink uid lid)
            entities   %= (M.delete eid)
            linksBySrc %= (M.delete eid)
            linksByDst %= (M.delete eid)

delEntityU u e = toU $ delEntity u e

-- When some ID-addressable object is added or modified, we call this method
-- to add it to that user's contributions list and also the "recent changes"
-- list on the front page.
addContrib :: Uid -> Id -> S ()
addContrib uid id =
  do users . at uid . traverse . contrib %= (id <|)
     addADSet id
     addAgree uid id
     recent %= (id <|)
     addCChildren id

-- Create a link between two Entities.
addLink :: Uid -> Eid -> LinkType -> Eid -> (Day, Bool) -> Maybe (Day, Bool) -> Maybe Money -> [Url] -> S Lid
addLink uid src lt dst date end money urls =
  do id <- bumpId
     let lid = Lid id
     let l   = Link lid src dst lt date end money urls
     links %= (M.insert lid l)
     linksBySrc . at src . traverse %= (S.insert lid)
     linksByDst . at dst . traverse %= (S.insert lid)
     addContrib uid $ L lid
     return lid

-- Update a link.
editLink :: Uid -> Lid -> Eid -> LinkType -> Eid -> (Day, Bool) -> Maybe (Day, Bool) -> Maybe Money -> [Url] -> S Lid
editLink uid lid@(Lid id) src lt dst date end money urls =
  do let l = Link lid src dst lt date end money urls
     links . at lid ?= l
     addContrib uid $ L lid
     return lid

-- Delete a link.
delLink :: Uid -> Lid -> S ()
delLink uid lid =
 do mlink <- use $ links . at lid
    case mlink of
      Nothing -> return ()
      Just l ->
        do links %= (M.delete lid)
           linksBySrc . at (l ^. lsrc) . traverse %= (S.delete lid)
           linksByDst . at (l ^. ldst) . traverse %= (S.delete lid)

addLinkU u src lt dst date end money us = toU $ addLink u src lt dst date end money us
editLinkU u lid src lt dst date end money us = toU $ editLink u lid src lt dst date end money us
delLinkU u l = toU $ delLink u l

weakInsert :: Ord k => k -> v -> M.Map k v -> M.Map k v
weakInsert k v m =
  if M.member k m
  then m
  else M.insert k v m

-- Add issue.
addIssue :: Uid -> Issue -> S Iid
addIssue uid issue' =
  do id <- bumpId
     let iid = Iid id
     let issue = issue' {_iid = iid}
     issues %= M.insert iid issue
     addContrib uid $ I iid
     return iid

delIssue :: Uid -> Iid -> S ()
delIssue uid iid =
  do missue <- use $ issues . at iid
     case missue of
       Nothing -> return ()
       Just issue ->
         do forM_ (S.toList $ issue ^. itagged)
                  (\id -> delIssueTag uid id iid)
     issues %= M.delete iid

addIssueTag :: Uid -> Id -> Iid -> S ()
addIssueTag uid id iid =
  do issues . at iid . traverse . itagged %= (S.insert id)
     issuetagged %= (weakInsert id S.empty) -- make sure the key is present
     issuetagged . at id . traverse %= (S.insert iid)
     addContrib uid (I iid)

delIssueTag :: Uid -> Id -> Iid -> S ()
delIssueTag uid id iid =
  do issues . at iid . traverse . itagged %= (S.delete id)
     issuetagged . at id . traverse %= (S.delete iid)

addIssueU u i = toU $ addIssue u i
delIssueU u iid = toU $ delIssue u iid
addIssueTagU u id iid = toU $ addIssueTag u id iid
delIssueTagU u id iid = toU $ delIssueTag u id iid

-- Invoke acid-state boilerplate to generate queries and updates.
$(makeAcidic ''GraphState ['getStateQ, 'bumpIdU, 'getUserQ, 'getUserByNameQ, 'getUserByEmailQ, 'addUserU, 'setUserPermU, 'addCommentU, 'delCommentU, 'removeCommentU, 'addAgreeU, 'addDisagreeU, 'addLikeU, 'addDislikeU, 'addEntityU, 'editEntityU, 'delEntityU, 'addLinkU, 'editLinkU, 'delLinkU, 'addTagU, 'delPTagU, 'delOTagU, 'addIssueU, 'delIssueU, 'addIssueTagU, 'delIssueTagU])


