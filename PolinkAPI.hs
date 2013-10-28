-- Copyright 2013 Metamocracy LLC
-- Written by Jim Snow (jsnow@metamocracy.com)
--
-- This is code related to the Polink rest API and data import/export
-- for various tools.
--
-- In particular, this file defines the Aeson instances that allow us to dump
-- the reputation system graph in a format that Pariah understands, the Aeson
-- instance for entities (needed for D3 interface Daniel is working on), and
-- code to generate graphviz source.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module PolinkAPI where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Time.Calendar (Day, toGregorian)

import PolinkState

mapMaybe f [] = []
mapMaybe f (x:xs) =
  case f x of
    Nothing -> mapMaybe f xs
    Just res -> res : mapMaybe f xs

mToL Nothing = []
mToL (Just x) = [x]

mLtoL Nothing = []
mLtoL (Just xs) = xs

mStoL Nothing = []
mStoL (Just xs) = S.toList xs

deMaybe :: [Maybe a] -> [a]
deMaybe [] = []
deMaybe (Just x:xs) = x : deMaybe xs
deMaybe (Nothing:xs) = deMaybe xs

resolveEids gs eids = 
  mapMaybe f eids
  where
    f eid = gs ^. entities . at eid

resolveEidsFst :: GraphState -> [(Eid, a)] -> [(Entity, a)]
resolveEidsFst gs eids =
  mapMaybe f eids
  where
    f (eid,x) = case gs ^. entities . at eid of
                  Nothing -> Nothing
                  Just e -> Just (e,x)


resolveUids gs uids =
  mapMaybe f uids
  where
    f uid = gs ^. users . at uid

resolveLids gs lids =
  mapMaybe f lids
  where
    f lid = gs ^. links . at lid

getAllLinks :: GraphState -> Eid -> [Link]
getAllLinks gs eid =
  let src = mStoL $ gs ^. linksBySrc . at eid
      dst = mStoL $ gs ^. linksByDst . at eid
  in
    mapMaybe (\lid -> gs ^. links . at lid) (src++dst)

overlap :: Maybe (Day, Day) -> Maybe (Day, Bool) -> Maybe (Day, Bool) -> Bool
overlap Nothing _ _ = True
overlap (Just (astart, aend)) mastart maend =
  timeOverlap (Just astart) (Just aend) (fmap fst mastart) (fmap fst maend)

-- Get all the links of a particular type.
getLinks :: GraphState -> Eid -> LinkType -> Maybe (Day, Day) -> Bool -> [(Eid, Link)]
getLinks gs eid lt mrange rev =
  case gs ^. (if rev then linksBySrc else linksByDst) . at eid of
    Nothing -> []
    Just lids ->
      mapMaybe f (S.toList lids)
  where
    f lid = case gs ^. links . at lid of
              Nothing -> Nothing
              Just link ->
                if (link ^. ltype) == lt && overlap mrange (Just $ link ^. ldate) (link ^. lend)
                then Just ((link ^. (if rev then ldst else lsrc)), link)
                else Nothing

-- Select just the Lids from a list of Ids.
{-
filterLids :: [Id] -> [Lid]
filterLids [] = []
filterLids ((L lid):xs) = lid : (filterLids xs)
filterLids (_:xs) = filterLids xs
-}

-- Get all the direct links associated with an issue.
getIssueLinks :: GraphState -> Iid -> Maybe (Day, Day) -> [Link]
getIssueLinks gs iid mrange =
  case gs ^. issues ^. at iid of
    Nothing -> []
    Just issue -> mapMaybe f $ S.toList $ issue ^. itagged
  where
    f (L lid) = do link <- gs ^. links . at lid
                   if overlap mrange (Just $ link ^. ldate) (link ^. lend)
                   then return link
                   else Nothing
    f _ = Nothing

-- Find all directly reachable suborganizations of an organization.
subOrgs :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
subOrgs gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum SubOrg) mrange False

superOrgs :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
superOrgs gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum SubOrg) mrange True

-- Find all members of an org.
members :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
members gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum Member) mrange False

isMember :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
isMember gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum Member) mrange True

-- Find all holders of an office.
offHolders :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
offHolders gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum HoldsOffice) mrange False

officesHeld :: GraphState -> Eid -> Maybe (Day, Day) -> [(Eid, Link)]
officesHeld gs eid mrange = getLinks gs eid (PL $ PLType $ fromEnum HoldsOffice) mrange True

-- All suborgs reachable from an org.
reachableSubOrgs :: GraphState -> Eid -> Maybe (Day, Day) -> S.Set Eid
reachableSubOrgs gs eid mrange =
  go (S.singleton eid) S.empty
  where
   go unvisited visited =
     if unvisited == S.empty
     then visited
     else
       let (e,unvs) = S.deleteFindMin unvisited
       in
         if S.member e visited
         then go unvs visited
         else
           let new = L.filter (\x -> not $ S.member x visited) (map fst $ subOrgs gs e mrange)
           in go (S.union unvs (S.fromList new)) (S.insert e visited)

-- All superorgs reachable from an org.
reachableSuperOrgs :: GraphState -> [Eid] -> Maybe (Day, Day) -> (S.Set Eid, S.Set Link)
reachableSuperOrgs gs eids mrange =
  go (S.fromList eids) S.empty S.empty
  where
    go unvisited visited links =
      if unvisited == S.empty
      then (visited, links)
      else
        let (e,unvs) = S.deleteFindMin unvisited
        in
          if (S.member e visited)
          then go unvs visited links
          else
            let new = superOrgs gs e mrange
            in go (S.union unvs (S.fromList (L.filter (\x -> not $ S.member x visited) (map fst new))))
                  (S.insert e visited)
                  (S.union links (S.fromList (map snd new)))


-- If an organization has more members than this, we just show membership count.
maxmembers = 20

-- Returns, for each entity, its sub-organizations, members, and office holders.
orgchart :: GraphState -> Eid -> Maybe (Day, Day) -> [(Entity, [(Entity, Link)], Either Int [(Entity,Link)], [(Entity, Link)])]
orgchart gs eid mrange =
  mapMaybe f (S.toList $ reachableSubOrgs gs eid mrange)
  where
    f eid2 = case gs ^. entities . at eid2 of
              Nothing -> Nothing
              Just e -> Just (e,
                              resolveEidsFst gs (subOrgs gs eid2 mrange),
                              mems eid2,
                              resolveEidsFst gs (offHolders gs eid2 mrange))

    -- Some groups have a lot of members; we can't show them all.
    mems :: Eid -> Either Int [(Entity, Link)]
    mems eid2 =
      let ms = members gs eid2 mrange
          mcount = length ms
      in if mcount > maxmembers
         then Left mcount
         else Right $ resolveEidsFst gs ms 


baseUrl =
  if production
  then "http://polink.org"
  else"http://localhost:3001"

eurl (Eid id) = baseUrl ++ "/e/" ++ (show id)
linkurl (Lid id) = baseUrl ++ "/l/" ++ (show id)

mapRight :: (b -> c) -> Either a [b] -> [c]
mapRight f (Right xs) = map f xs
mapRight f (Left _) = []

mapLeft :: (a -> c) -> Either a b -> c -> c
mapLeft f (Right _ ) def = def
mapLeft f (Left x) def = f x

entitygv :: Entity -> String
entitygv e =
  " " ++ (show (e^. ecname)) ++
  (case e ^. etype of
    Person _ -> " [shape=plaintext fontsize=8 URL=\""
    Organization _ -> " [shape=box fontsize=9 URL=\"") ++
  (eurl (_eid e)) ++ "\" target=\"_top\"]\n"


-- Some link types make better visual sense if they're reversed.
lswap l src dst =
  case (l ^. ltype) of
    PL plt ->
      case toEnum $ unPLType plt of 
                  HoldsOffice -> rev
                  SubOrg -> rev
                  Member -> rev
                  _ -> fwd
    NL _ -> fwd

  where
    fwd = (src, dst)
    rev = (dst, src)


linkgv :: GraphState -> Link -> String
linkgv gs l =
  case (do s <- gs ^. entities ^. at (l ^. lsrc) 
           d <- gs ^. entities ^. at (l ^. ldst)
           return (s,d)) of
    Nothing -> ""
    Just (src', dst') ->
      let (src, dst) = lswap l src' dst'
          color =
            case (l ^. ltype) of
              NL _ -> "red"
              PL plt ->
                case toEnum $ unPLType plt of 
                  HoldsOffice -> "navy"
                  SubOrg -> "blue4"
                  Member -> "lightseagreen"
                  _ -> "green"
      in
        " " ++ (show $ src ^. ecname) ++ " -> " ++ (show $ dst ^. ecname) ++
        " [color=" ++ color ++
        " URL=\"" ++ (linkurl (l ^. lid)) ++
        "\" target=\"_top\"]\n"

-- Produce graphviz source for an orgchart rooted at a particular eid.
-- Note: originally, I set "aspect=1.5", but that causes all kinds of
--       corruption inside graphviz.
--       See http://www.graphviz.org/mantisbt/view.php?id=2364

orgchartgv :: GraphState -> Eid -> Maybe (Day, Day) -> String
orgchartgv gs eid mrange =
  case gs ^. entities . at eid of
    Nothing -> error "no root"
    Just root ->
      let oc = orgchart gs eid mrange
          -- Use intermediate Map to eliminate duplicates.
          allOrgs :: [Entity]
          allOrgs   = M.elems $ M.fromList $ map eidtuple $ (concatMap (\(_,suborgs,_,_) -> (map fst suborgs)) oc)
          allPeople :: [Entity]
          allPeople = M.elems $ M.fromList $ map eidtuple $ (concatMap (\(_,_,members,_) -> (mapRight fst members)) oc)
          allOffs :: [Entity]
          allOffs   = M.elems $ M.fromList $ map eidtuple $ (concatMap (\(_,_,_,offs) -> (map fst offs)) oc)
          allTrunc :: [(Entity, Int)]
          allTrunc = concatMap
                       (\(parent, _, members, _) -> mapLeft (\count -> [(parent,count)]) members []) oc
      in
        "digraph {\n graph [bgcolor=\"transparent\"]\n" ++
          (entitygv root) ++
          (concatMap entitygv allOrgs) ++
          (concatMap entitygv (allOffs ++ allPeople)) ++
          (concatMap truncf allTrunc) ++
          (concatMap linkf oc) ++ "}\n"
  where
    orglinkf src link dst =
      " " ++ src ++ " -> " ++ (show (dst ^. ecname)) ++
      " [color=blue4  URL=\"" ++ (linkurl (_lid link)) ++ "\" target=\"_top\"]\n"
    memlinkf src link dst =
      " " ++ src ++ " -> " ++ (show (dst ^. ecname)) ++
      " [color=lightseagreen URL=\"" ++ (linkurl (_lid link)) ++ "\" target=\"_top\"]\n"
    offlinkf src link dst =
      " " ++ src ++ " -> " ++ (show (dst ^. ecname)) ++
      " [color=navy URL=\"" ++ (linkurl (_lid link)) ++ "\" target=\"_top\"]\n"

    linkf (ent, suborgs, members, offs) =
      let srcname = show (ent ^. ecname)
      in (concatMap (\(dst, link) -> orglinkf srcname link dst) suborgs) ++
         (concatMap (\(dst, link) -> memlinkf srcname link dst) (mapRight id members)) ++
         (concatMap (\(dst, link) -> offlinkf srcname link dst) offs) ++
         (mapLeft (\count -> trunclinkf ent count) members "")

    truncf (parent, count) =
      " \"" ++ (T.unpack (parent ^. ecname)) ++ "-members\" [label=\"" ++ (show count) ++ " members\" fontsize=8 shape=plaintext]\n"

    trunclinkf parent count =
      " " ++ (show (parent ^. ecname)) ++ " -> \"" ++ (T.unpack (parent ^. ecname)) ++ "-members\" [color=green]\n" 

    eidtuple ent = (_eid ent, ent)

-- Generate a graph from all the links associated with a given issues.

unique xs = S.toList (S.fromList xs)

issuegv :: GraphState -> Iid -> Maybe (Day, Day) -> String
issuegv gs iid mrange =
  "digraph {\n graph [bgcolor=\"transparent\"]\n" ++
  (concatMap entitygv ents) ++
  (concatMap (linkgv gs) links) ++
  "}\n"
  where 
    directlinks = getIssueLinks gs iid mrange
    directeids = concatMap (\l-> [l ^. lsrc, l ^. ldst]) directlinks
    indirectlinks = allParentLinks gs directeids mrange
    links = unique ((reverse indirectlinks) ++ directlinks)  
    ents = resolveEids gs (unique $ concatMap (\l-> [l ^. lsrc, l ^. ldst]) links)

allParentLinks :: GraphState -> [Eid] -> Maybe (Day, Day) -> [Link]
allParentLinks gs eids mrange =
  let members :: [(Eid, Link)]
      members = concatMap (\eid -> isMember gs eid Nothing) eids
      membereids = map fst members
      memberlids = map snd members

      offices :: [(Eid, Link)]
      offices = concatMap (\eid -> officesHeld gs eid Nothing) eids
      officeeids = map fst offices
      officelids = map snd offices
 
      supers :: (S.Set Eid, S.Set Link)
      supers = reachableSuperOrgs gs (eids ++ membereids ++ officeeids) mrange
      supereids = S.toList $ fst supers
      superlids = S.toList $ snd supers
  in
    memberlids ++ officelids ++ superlids


msquish :: Maybe (S.Set a) -> [a]
msquish Nothing = []
msquish (Just xs) = S.toList xs

-- For each entity, returns fwd pos, fwd neg,
-- things it's a member or suborg of, and users that like/dislike it.
-- We have to look at reverse links, because some are bi-directional.
elinks :: GraphState -> [(Entity, 
                          [(Entity, PosLinkType)],
                          [(Entity, NegLinkType)],
                          [Entity], [Entity], [User], [User])]
elinks gs =
  map f (M.elems (gs ^. entities))
  where
   f ent =
     let eid = _eid ent
         (fwdpos, fwdneg) = sortLinks $ msquish $ gs ^. linksBySrc . at eid
         (revpos, revneg) = bidirLinks $ msquish $ gs ^. linksByDst . at eid
         likes            = resolveUids gs $ msquish $ gs ^. like . at (E eid)
         dislikes         = resolveUids gs $ msquish $ gs ^. dislike . at (E eid)
     in (ent, fwdpos++revpos, fwdneg++revneg, [], [], likes, dislikes)

   -- Get the list of positive and negative fwd links.
   sortLinks lids = slgo [] [] lids
   slgo pos neg [] = (resolveEidsFst gs pos, resolveEidsFst gs neg)
   slgo pos neg (l:ls) =
     case gs ^. links . at l of
       Nothing -> slgo pos neg ls
       Just link ->
         case link ^. ltype of
           PL pl -> slgo (((link ^. ldst), toEnum $ unPLType pl) : pos) neg ls
           NL nl -> slgo pos (((link ^. ldst), toEnum $ unNLType nl) : neg) ls

   -- Get the list of positive and negative backlinks that are bidirectional.
   bidirLinks lids = bdgo [] [] lids
   bdgo pos neg [] = (resolveEidsFst gs pos, resolveEidsFst gs neg)
   bdgo pos neg (l:ls) =
     case gs ^. links . at l of
       Nothing -> bdgo pos neg ls
       Just link ->
         case link ^. ltype of
           PL pl -> let plt = toEnum $ unPLType pl
                    in if isBiDirPL plt
                       then bdgo (((link ^. lsrc), plt) : pos) neg ls
                       else bdgo pos neg ls 
           NL nl -> let nlt = toEnum $ unNLType nl
                    in if isBiDirNL nlt
                       then bdgo pos (((link ^. lsrc), nlt) : neg) ls
                       else bdgo pos neg ls

-- Given an Entity, generate a meaningful name for the reputation system.
genENameVerbose :: GraphState -> Entity -> T.Text
genENameVerbose gs e =
  et `T.append` "-" `T.append` (T.pack $ show $ e ^. eid) `T.append` "-" `T.append` (e ^. ecname) 
  where
    et =
      case e ^. etype of
        Person _ -> "person"
        Organization _ -> "org"

genEName :: GraphState -> Entity -> T.Text
genEName gs e =
  let (Eid id) = e ^. eid
  in "e" `T.append` (T.pack (show id))

-- Given a User, generate a group named after that user.
genUGNameVerbose :: GraphState -> T.Text -> User -> T.Text
genUGNameVerbose gs prefix u =
  prefix `T.append` (T.pack $ show $ u ^. uid) `T.append` "-" `T.append` (u ^. name)


genUGName :: GraphState -> T.Text -> User -> T.Text
genUGName gs prefix u =
  let (Uid id) = u ^. uid
  in prefix `T.append` ("u" `T.append` (T.pack $ show id))

instance ToJSON Uid where
  toJSON (Uid uid) = toJSON uid

-- Not a complete representation of the entire state;
-- this is just for the reputation system.
instance ToJSON GraphState where
  toJSON gs =
    let es = map f (elinks gs)
        f (ent, friends, foes, memberof, suborgof, likes, dislikes) = 
          object [ "user"    .= genEName gs ent
                 , "friends" .= map (genEName gs . fst) friends
                 , "foes"    .= map (genEName gs . fst) foes
                 , "groups"  .= ((map (genUGName gs "likedby-") likes) ++
                                 (map (genUGName gs "dislikedby-") dislikes))
                 ]
        groups = (map (genUGName gs "likedby-") $ M.elems (gs ^. users)) ++
                 (map (genUGName gs "dislikedby-") $ M.elems (gs ^. users))
    in object ["users" .= toJSON es, "groups" .= groups]

etypeString :: Entity -> T.Text
etypeString ent =
  case ent ^. etype of
    Person _ -> "person"
    Organization _ -> "organization"

instance ToJSON (Day, Bool) where
  toJSON (day, approx) =
    let (y,m,d) = toGregorian day
    in if approx
       then object ["year" .= y]
       else object ["month" .= m, "day" .= d, "year" .= y]

isPos (PL _) = True
isPos (NL _) = False

instance ToJSON (GraphState, Link) where
  toJSON (gs, l) =
    object [ "lid"      .= (idToInt $ L (l ^. lid))
           , "src"      .= (idToInt $ E (l ^. lsrc))
           , "dst"      .= (idToInt $ E (l ^. ldst))
           , "linktype" .= showLT lt
           , "linkdesc" .= lToText lt
           , "pos"      .= isPos lt
           , "bidir"    .= isBiDir lt
           ]
    where
      lt = l ^. ltype

instance ToJSON (GraphState, Entity) where
 toJSON (gs,e) =
   object ([ "eid"       .= (idToInt $ E (e ^. eid))
           , "etype"     .= etypeString e
           , "ename"     .= (e ^. ecname)
           , "links"     .= zip (L.repeat gs) ls
           ] ++
           (deMaybe [ fmap (\wp -> "wp" .= unUrl wp)       (e ^. ewp)
                    , fmap (\hp -> "homepage" .= unUrl hp) (e ^. ehp)
                    , fmap (\tw -> "twt" .= unUrl tw)      (e ^. etwt)
                    , fmap (\b -> "birth" .= b)            (e ^. ebirth)
                    , fmap (\d -> "death" .= d)            (e ^. edeath)
                    ]))
   where
     ls = getAllLinks gs (e ^. eid)


