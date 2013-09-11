-- Copyright 2013 Metamocracy LLC
-- Written by Jim Snow (jsnow@metamocracy.com)
--
-- This file includes all the Yesod handlers and html generation.

-- needed for Yesod
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, 
             TemplateHaskell, OverloadedStrings, TypeSynonymInstances,
             FlexibleInstances, BangPatterns #-}
-- makes life easier for local functions
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Yesod hiding (update, Entity)
import Yesod.Form.Jquery
import Yesod.Auth
import Yesod.Auth.BrowserId
import Web.ClientSession(getDefaultKey)
import Network.HTTP.Conduit (Manager, newManager, def)
import Control.Applicative
import Control.Monad (liftM)
import Data.Acid
import Data.Acid.Local
import Data.Acid.Advanced (query', update')
import Control.Exception (bracket)
import Control.Lens -- hiding (left, right)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as SEQ
import Data.List (sortBy)
import Data.Function (on)
import Text.Blaze
import Web.Cookie
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Text.Julius
import Text.Markdown (markdown, def)
import Data.Time.Calendar (Day, toGregorian, addDays, Day(..))
import qualified Data.Char as C (toLower)
import System.Random (getStdRandom, randomR)
import System.Process (runCommand, system)
import Data.Time.Clock (getCurrentTime, utctDay, secondsToDiffTime)
import Text.Blaze.Internal (text)
import Data.Foldable
import Control.Concurrent

import PolinkState
import PolinkAPI


-- YESOD-RELATED TYPES AND INSTANCES

type Txt = T.Text
data InfluenceGraph = InfluenceGraph {httpMgr :: Manager, acid :: AcidState GraphState, fslock :: MVar ()}

--type GW = GWidget InfluenceGraph InfluenceGraph ()
type GW = WidgetT InfluenceGraph IO ()

-- Things that a given handler can be expected to have access to.  See getConext.
data Ctx = Ctx {
  cgs     :: GraphState,   -- application state
  cmuser  :: Maybe User,   -- current logged in user
  cmcb    :: Maybe Entity, -- clipboard
  cgw     :: GW,           -- widget that displays the above
  cfslock :: MVar ()       -- filesystem mutex for svg-rendering path  
}

-- Necessary instances if we want to use these types in URLs and forms.
instance PathPiece Lid where
  fromPathPiece pp = fmap Lid (fromPathPiece pp)
  toPathPiece (Lid id) = toPathPiece id

instance PathPiece Cid where
  fromPathPiece pp = fmap Cid (fromPathPiece pp)
  toPathPiece (Cid id) = toPathPiece id

instance PathPiece Eid where
  fromPathPiece pp = fmap Eid (fromPathPiece pp)
  toPathPiece (Eid id) = toPathPiece id

instance PathPiece Uid where
  fromPathPiece pp = fmap Uid (fromPathPiece pp)
  toPathPiece (Uid id) = toPathPiece id

instance PathPiece PTid where
  fromPathPiece pp = fmap PTid (fromPathPiece pp)
  toPathPiece (PTid id) = toPathPiece id

instance PathPiece OTid where
  fromPathPiece pp = fmap OTid (fromPathPiece pp)
  toPathPiece (OTid id) = toPathPiece id

instance PathPiece UTid where
  fromPathPiece pp = fmap UTid (fromPathPiece pp)
  toPathPiece (UTid id) = toPathPiece id

instance PathPiece Iid where
  fromPathPiece pp = fmap Iid (fromPathPiece pp)
  toPathPiece (Iid id) = toPathPiece id

instance PathPiece Id where
  fromPathPiece pp =
    case reads (T.unpack pp) of
      (id, ""):_ -> Just id
      []         -> Nothing
      _          -> error "could not parse Id"

  toPathPiece id = T.pack $ show id

instance PathPiece (Maybe Int) where
  fromPathPiece "nothing" = Nothing
  fromPathPiece pp = Just $ fromPathPiece pp

  toPathPiece Nothing = "nothing"
  toPathPiece (Just i) = T.pack $ show i


instance ToMarkup Url where
  toMarkup (Url t) = toMarkup t

-- We support "DELETE" even though there isn't an easy way to invoke that
-- method from a plain html form.  (Browsers only seem to allow GET and POST.)
-- As a workaround, we have an alternate "POST" that just runs the delete handler.

mkYesod "InfluenceGraph" [parseRoutes|
  /                              HomeR            GET
  /auth                          AuthR Auth getAuth
  /newu                          NewUserR         GET POST
  /about                         AboutR           GET
  /rshelp                        RSHelpR          GET
  /recent                        RecentR          GET
  /u                             UsersR           GET
  /u/#Txt                        UserR            GET
  /uid/#Uid                      UserIDR          GET POST
  /e                             EntitiesR        GET
  /newperson                     NewPersonR       GET POST
  /neworg                        NewOrgR          GET POST
  /editperson/#Eid               EditPersonR      GET POST
  /editorg/#Eid                  EditOrgR         GET POST
  /newlink/#Eid/#Eid             NewLinkR         GET POST
  /newlink2/#Eid/#Eid            NewLink2R        POST
  /editlink/#Lid                 EditLinkR        GET POST
  /newtag/#Eid                   NewTagR          POST
  /newcomment                    NewCommentR      POST
  /ename/#Txt                    EntityByNameR    GET
  /e/#Eid                        EntityR          GET DELETE
  /e/#Eid/n/#Txt                 EntityCanonR     GET DELETE
  /e/#Eid/orgchart-dot           OrgChartDotR     GET
  /e/#Eid/orgchart-svg           OrgChartSvgR     GET
  /e/#Eid/orgchart               OrgChartR        GET
  /e/#Eid/orgchart-dot/date/#Int OrgChartDotDateR GET
  /e/#Eid/orgchart-svg/date/#Int OrgChartSvgDateR GET
  /e/#Eid/orgchart/date/#Int     OrgChartDateR    GET
  /random                        RandomR          GET
  /dele/#Eid                     DelEntityR       POST
  /l/#Lid                        LinkR            GET DELETE
  /dell/#Lid                     DelLinkR         POST
  /c/#Cid                        CommentR         GET DELETE
  /delc/#Cid                     DelCommentR      POST
  /remc/#Cid                     RemCommentR      POST
  /c-context/#Cid                CommentCtxR      GET
  /pt/#PTid                      PTagR            GET DELETE
  /delpt/#PTid                   DelPTagR         POST
  /ot/#OTid                      OTagR            GET DELETE
  /delot/#OTid                   DelOTagR         POST
  /ut/#UTid                      UTagR            GET
  /el/#Eid/#Eid                  LinksBetweenR    GET
  /i/#Iid                        IssueR           GET
  /i/#Iid/n/#Txt                 IssueCanonR      GET
  /i/#Iid/dot                    IssueDotR        GET
  /i/#Iid/svg                    IssueSvgR        GET
  /i                             IssuesR          GET
  /newissue                      NewIssueR        GET POST
  /editissue/#Iid                EditIssueR       GET POST
  /l/#Lid/newissuetag            NewIssueTagR     POST
  /deli/#Iid                     DelIssueR        POST
  /delit/#Lid/#Iid               DelIssueTagR     POST
  /wiki/#Txt                     WikiR            GET
  /rsstate                       RSStateR         GET
|]

instance Yesod InfluenceGraph
  where
    maximumContentLength _ _ = Just (128 * 1024) -- No need for lengthy request bodies.
    approot = if production
              then ApprootStatic "http://polink.org"
              else ApprootStatic "http://localhost:3001"
    defaultLayout = layout
--    makeSessionBackend _ =
--      do key <- getDefaultKey
--         return $ Just $ clientSessionBackend key (60 * 24 * 14) -- 2 week session timeout.

-- More recent versions of Yesod do this differently...
    makeSessionBackend _ =      
      do backend <- defaultClientSessionBackend (24*60*7) "mykey.aes"
         return $ Just backend

--      let setTimeout cookie = cookie { setCookieMaxAge = Just week }
--      in fmap (customizeSessionCookies setTimeout)

instance YesodAuth InfluenceGraph where
  type AuthId InfluenceGraph = T.Text
  getAuthId    = return . Just . credsIdent
  loginDest _  = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authBrowserId def] -- Persona/browserid support.
  authHttpManager = httpMgr
  maybeAuthId = do
    ms <- lookupSession "_ID"
    case ms of
        Nothing -> return Nothing
        Just s -> return $ fromPathPiece s


instance RenderMessage InfluenceGraph FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodJquery InfluenceGraph

-- MISC UTILITY FUNCTIONS

fromEither (Left e) = error e
fromEither (Right a) = a

mTtoMInt :: Maybe T.Text -> Maybe Int
mTtoMInt Nothing = Nothing
mTtoMInt (Just s) =
  case reads (T.unpack s) of
    [] -> Nothing
    ((i,[]):[]) -> Just i
    _ -> Nothing

tToInt s =
  case mTtoMInt s of
    Nothing -> 1
    Just a -> if a > 1 then a else 1

caseJust :: Maybe a -> b -> (a -> b) -> b
caseJust m n j =
  case m of
    Nothing -> n
    Just a -> j a

showDate :: Day -> String
showDate day = (showMonth m) ++ " " ++ (show d) ++ " " ++ (show y)
  where
    (y,m,d) = toGregorian day
    showMonth month =
      case month of
        1  -> "January"
        2  -> "February"
        3  -> "March"
        4  -> "April"
        5  -> "May"
        6  -> "June"
        7  -> "July"
        8  -> "August"
        9  -> "September"
        10 -> "October"
        11 -> "November"
        12 -> "December"
        _  -> error "invalid month"

showDateRange' :: Bool -> Bool -> Maybe (Day, Bool) -> Maybe (Day, Bool) -> String
showDateRange' _ _ Nothing Nothing = ""
showDateRange' brief forceRange start end =
  let mShowDate Nothing = ""
      mShowDate (Just (d,approx)) =
        if brief || approx
        then let (y,_,_) = toGregorian d in show y
        else showDate d
  in
    case end of
      Nothing -> "(" ++ (mShowDate start) ++ (if forceRange then "-" else "") ++ ")"
      Just _ -> "(" ++ (mShowDate start) ++ "-" ++ (mShowDate end) ++ ")"

showDateRange brief = showDateRange' brief False
showDateForceRange brief = showDateRange' brief True

reload =
  do cr <- curRoute
     redirect cr

redirectId id =
  case id of
    E eid   -> redirect $ EntityR eid
    L lid   -> redirect $ LinkR lid
    C cid   -> redirect $ CommentR cid
    U uid   -> redirect $ UserIDR uid
    PT ptid -> redirect $ PTagR ptid
    OT otid -> redirect $ OTagR otid
    UT utid -> redirect $ UTagR utid


clearAndReload cookie =
  do deleteCookie cookie "/"
     reload

setAndReload cookie val =
  do setCookie (def{setCookieName  = cookie,
                    setCookiePath  = Just "/",
                    setCookieValue = (B8.pack val)})
     reload

curRoute =
  do mr <- getCurrentRoute
     case mr of
       Nothing -> return HomeR
       Just r -> return r

ifSet param action1 action2 =
  do marg <- lookupGetParam param
     case marg of
       Just arg -> action1 arg
       _ -> action2

-- Check args to see if anything's set that we should act on.
-- This probably isn't the best way to register likes/agrees/etc...
handleArgs ig uid mid action =
  do case mid of
       Just id ->
         ifSet
           "like"
           (\_ -> liftIO (update (acid ig) (AddLikeU uid id)) >> reload)
           (ifSet
             "dislike"
             (\_ -> liftIO (update (acid ig) (AddDislikeU uid id)) >> reload)
             (ifSet
               "agree"
               (\_ -> liftIO (update (acid ig) (AddAgreeU uid id)) >> reload)
               (ifSet
                 "disagree"
                 (\_ -> liftIO (update (acid ig) (AddDisagreeU uid id)) >> reload)
                 action')))
       Nothing -> action'
  where
    action' =
      ifSet
        "setcb"
        (\setcb -> setAndReload "clipboard" (T.unpack setcb))
        (ifSet
          "clearcb"
          (\_ -> clearAndReload "clipboard")
          action)

-- Clipboard management.
-- (The clipboard allows us to create links.)
-- This could probably be handled entirely in javascript.
getSetEClipboardR :: Eid -> Handler TypedContent
getSetEClipboardR eid =
  do setCookie (def{setCookieName  = "clipboard",
                    setCookiePath  = Just "/",
                    setCookieValue = (B8.pack $ show eid)})
     getEntityR eid

getClearEClipboardR :: Handler Html
getClearEClipboardR =
  do deleteCookie "clipboard" "/"
     layout [whamlet|<p>clipboard cleared|]
          

-- If the user is authenticated via persona but hasn't created an account yet,
-- we redirect them to the account creation page.
-- newAccountRedirect :: Handler ()
newAccountRedirect =
  do cr <- getCurrentRoute
     --rtom <- getRouteToMaster
     --if ((fmap rtom cr) == (Just NewUserR)) -- break inevitable redirect loop
     if cr == Just (NewUserR)
     then return ()
     else
       do maid <- maybeAuthId
          case maid of
            Nothing -> return ()
            Just email ->
              do ig <- getYesod
                 gs' <- liftIO $ query (acid ig) GetStateQ
                 let gs = fromEither gs'
                 case gs ^. usersByEmail . at email of
                   Nothing -> redirect NewUserR
                   Just _ -> return ()


-- Figure out who we're logged in as and what the current graph state is,
-- return the graphstate, user, a widget with the appropriate interface
-- to display who the user is, and lock (used for filesystem access).
getContext :: Maybe Id -> Handler Ctx
getContext mid =
  do ig <- getYesod
     let lock = fslock ig
     gs' <- liftIO $ query (acid ig) GetStateQ
     let gs = fromEither gs'
     maid <- maybeAuthId
     let ment = case mid of
                  (Just (E eid)) -> gs ^. entities ^. at eid
                  _ -> Nothing
     case maid of
       Nothing -> return $ Ctx gs Nothing Nothing [whamlet|
                                                    <div class="header">
                                                      $maybe ent <- ment
                                                        <span id="thisentity" eid=#{show $ idToInt $ E $ _eid ent} ename=#{_ecname ent}>
                                                      <center>
                                                        <span class="home">
                                                          <a href="@{HomeR}">Polink</a></center>
                                                            <center><a href=@{AuthR LoginR}>login or register</a>
                                                    <hr>
                                                  |] lock
       Just email ->
         case gs ^. (usersByEmail . at email) of
           Nothing -> return $ Ctx gs Nothing Nothing [whamlet|<center><a href=@{NewUserR}>create user account</a>|] lock
           Just uid ->
             case gs ^. (users . at uid) of
               Nothing -> return $ Ctx gs Nothing Nothing [whamlet|<center>error retrieving user info|] lock
               Just user ->
                 handleArgs
                   ig uid mid
                   (do meids <- lookupCookie "clipboard"
                       (cbw,meid) <-
                         case meids of
                           Nothing -> return ([whamlet|empty|], Nothing)
                           Just eids ->
                             case reads (T.unpack eids) of
                               [] ->
                                 return ([whamlet|<p>clipboard failed to parse: #{eids}|], Nothing)
                               ((eid,_):_) ->
                                 do cr <- curRoute
                                    let w = [whamlet|
                                              ^{renderId gs (E eid)}
                                              \ <a href="@{cr}?clearcb=1">clear</a>
                                            |]
                                    return (w, gs ^. entities ^. at eid)
                       let (Uid ident) = _uid user
                       return (Ctx
                               gs
                               (Just user)
                               meid
                               [whamlet|
                                 <div class="header">
                                   $maybe ent <- ment
                                     <span id="thisentity" eid=#{show $ idToInt $ E $ _eid ent} ename=#{_ecname ent}>
                                   <center>
                                     <span class="home">
                                       <a href="@{HomeR}">Polink</a></center>
                                   <center>
                                     logged in as
                                     \ <a href=@{UserR (_name user)}>
                                         <span id="username" uid=#{show ident} uname=#{_name user}>
                                           #{_name user}
                                     \ <a href=@{AuthR LogoutR}>logout</a>
                                   <center><a href=@{NewPersonR}>add person</a> <a href=@{NewOrgR}>add organization</a>
                                   <center>clipboard: ^{cbw}
                                 <hr>
                               |] lock ))

-- Reject access if the current user doesn't have the given permission.
-- Otherwise, perform the action and return a widget showing the action results.
-- (However, we pretty much always do a redirect.)
reqAuth :: T.Text -> Perm -> (Ctx -> User -> Handler GW) -> Handler GW
reqAuth s p action =
  do ctx <- getContext Nothing
     case cmuser ctx of
       Nothing -> return $ errW $ T.append "you must be logged in in order to " s
       Just u ->
         do reqPerm u p
            action ctx u

-- Check if a user's account has a particular permission, bail out if it doesn't.
reqPerm :: User -> Perm -> Handler ()
reqPerm u p =
  if permTest (u ^. authority) p
  then return ()
  else permissionDenied "you don't have the proper permissions to do that"

-- Check permission, return bool.
hasPerm :: Ctx -> Perm -> Bool
hasPerm ctx p =
  case cmuser ctx of
    Nothing -> False
    Just u ->
      if permTest (u ^. authority) p
      then True
      else False

-- Layout instance we use for defaultLayout.
-- We call this directly, though, for the frivolous reason that it's less typing.
--layout :: GWidget s InfluenceGraph () -> GHandler s InfluenceGraph RepHtml
layout :: GW -> Handler Html
layout w =
  do newAccountRedirect
     content <- widgetToPageContent
                  (do w
                      toWidget [lucius| |]
                      toWidget [julius|
function showhideid(id1, id2) {
	showid(id1);
	hideid(id2);
}

function showid(id) {
	document.getElementById(id).style.display = 'block';
}

function hideid(id) {
	document.getElementById(id).style.display = 'none';
}

                      |])
     giveUrlRenderer
       [hamlet|
         $doctype 5
         <html>
           <head>
             <link rel="stylesheet" type="text/css" href="http://polink.org/static/polink.css"/>
             <title>#{pageTitle content}
             <meta charset=utf-8>
             <meta name="google-site-verification" content="5zrJy6KmCEdVaA9ypBMN3GotYm1XBzvkDuLDbpo7S5Q" />
             ^{pageHead content}
           <body>
             ^{pageBody content}
             <hr>
             <div class="footer">
               <center>
                 <a href="@{HomeR}">home</a>
                 <a href="http://www.google.com/cse/publicurl?cx=005382893767711408570:jzzodnuihb4">search</a>
             <script src="http://code.jquery.com/jquery-1.10.1.js"></script>
             <script src="http://polink.org/static/jquery.cookie.js"></script>
             <script src="http://polink.org/static/polink.js"></script>
       |]

-- Wrapper function for json generation, so we set the header appropriately.
jsonify j =
  do addHeader "Access-Control-Allow-Origin" "*"
     return $ toJSON j

-- Bail out with a given text string.
err :: T.Text -> Handler Html
err s = layout [whamlet|<p>#{s}|]

-- Same, but as a widget.
errW :: T.Text -> GW
errW s = [whamlet|<p>#{s}|]

-- Pagination.
-- Max number of entities we show at once.
perpage = 100

-- Given a Sequence of things, look at the get parameters to see which ones
-- we need to actually show, return those along with a navigation widget.
paginate :: SEQ.Seq a -> Handler (GW, SEQ.Seq a)
paginate xs =
  do pagenumt <- lookupGetParam "page"
     mall <- lookupGetParam "all"
     let all = case mall of 
                 Just _ -> True
                 _ -> False
     cr <- curRoute
     let pagenum = tToInt pagenumt
     let len = SEQ.length xs
     let doPrev = pagenum > 1
     let doNext = len > pagenum * perpage
     let doAll = len > perpage
     let prevpage = pagenum - 1
     let nextpage = pagenum + 1
     let first = if all then 1 else ((pagenum-1) * perpage) + 1
     let last = if all then len else min (len) ((first + perpage)-1)
     return
       ([whamlet|
          <center>
            <p>
              showing #{first}-#{last} of #{len}
              $if all
              $else
                $if doPrev
                  \ <a href="@{cr}?page=#{prevpage}">prev</a>
                $if doNext
                  \ <a href="@{cr}?page=#{nextpage}">next</a>
                $if doAll
                  \ <a href="@{cr}?all=1">all</a>
        |], if all
            then xs
            else SEQ.take perpage $ SEQ.drop ((pagenum-1)*perpage) xs)

-- Relatively lazy way to check length.  This avoids scanning the whole list,
-- which could be huge.
atLeastLength :: [a] -> Int -> Int
atLeastLength xs max = go xs max 0
  where
    go _ 0 acc = acc
    go [] max acc = acc
    go (x:xs) max acc = go xs (max-1) (acc+1) 

-- Same as paginate, but for lists.
paginateL :: [a] -> Handler (GW, [a])
paginateL xs =
  do pagenumt <- lookupGetParam "page"
     mall <- lookupGetParam "all"
     let all = case mall of
                 Just _ -> True
                 _ -> False
     cr <- curRoute
     let pagenum = tToInt pagenumt
     let xs' = drop ((pagenum-1)*perpage) xs
     let xs'' = take perpage xs'
     let doPrev = pagenum > 1
     let pseudolen = atLeastLength xs' (perpage+1)
     let doNext = pseudolen > perpage
     let doAll = doNext || doPrev
     let prevpage = pagenum - 1
     let nextpage = pagenum + 1
     let first = if all then 1 else ((pagenum-1) * perpage) + 1
     let last = if all then length xs else min ((first + pseudolen)-1) ((first + perpage)-1)
     return
       ([whamlet|
          <center>
            <p>
              $if pseudolen > 0
                showing #{first}-#{last}
                $if all
                  \ of #{last}
                $else
                  $if doPrev
                    \ <a href="@{cr}?page=#{prevpage}">prev</a>
                  $if doNext
                    \ <a href="@{cr}?page=#{nextpage}">next</a>
                  $if doAll
                    \ <a href="@{cr}?all=1">all</a>
              $else
                \ nothing here
        |], if all then xs else xs'')
       
-- Helper functions to render objects identified by various kinds of ID.
-- Return as a widget.

renderEid gs eid@(Eid id) =
  case (gs ^. entities ^. at eid) of
    Nothing -> [whamlet|deleted entity <a href="@{EntityR eid}">#{show eid}</a>|]
    Just e -> let en = e ^. ecname
              in [whamlet|
                    <span class="entity" id=#{show id}>
                      $case (_etype e)
                         $of Person p
                           <span class="person">
                             person
                         $of Organization o
                           <span class="org">
                             organization
                      \ <a href="@{EntityR eid}">#{en}</a>|]

renderLid gs lid =
  let (lw,ml) = renderLink gs lid
  in [whamlet|
       <a href=@{LinkR lid}>link</a>
       \ ^{lw}
     |]

getParent gs cid =
  case gs ^. comments . at cid of
    Nothing -> Nothing
    Just comment ->
      case comment ^. parent of
        C cid' -> getParent gs cid'
        root -> Just root 

renderCid gs cid@(Cid id) =
  let mcomment = gs ^. comments . at cid
  in
    [whamlet|
       <span class="comment" id=#{show id}>
         <a href=@{CommentR cid}>comment</a>
         \ by
         $maybe c <- mcomment
           \ ^{renderUid gs (_author c)}
         $nothing
           \ deleted user
         \ on
         $maybe p <- getParent gs cid
           \ ^{renderId gs p}
         $nothing
           \ deleted context
    |]

renderUid gs uid@(Uid id) =
  let mu = gs ^. users ^. at uid
  in
    [whamlet|
      $maybe u <- fmap _name mu
        <span class="user" id=#{show u}>
          <a href="@{UserR u}">#{u}</a>
      $nothing
        deleted user #{show uid}
    |]

renderPTid :: GraphState -> PTid -> GW
renderPTid gs ptid =
  let mtag = gs ^. ptags ^. at ptid
  in 
    [whamlet|
      $maybe (ptag, eid) <- mtag
        \ ^{renderId gs (E eid)} tagged #{ptToText ptag}
      $nothing
        \ deleted person tag
    |]

renderOTid :: GraphState -> OTid -> GW
renderOTid gs otid =
  let mtag = gs ^. otags ^. at otid
  in 
    [whamlet|
      $maybe (otag, eid) <- mtag
        \ ^{renderId gs (E eid)} tagged #{otToText otag}
      $nothing
        \ deleted organization tag
    |]

renderIid' :: Bool -> GraphState -> Iid -> GW
renderIid' terse gs iid =
  let missue = gs ^. issues ^. at iid
  in
    [whamlet|
      $maybe issue <- missue
        $if terse
        $else
          \ issue
        \ <a href="@{IssueR iid}">#{_iname issue}</a>
      $nothing
        \ deleted issue
    |]

renderIid = renderIid' False
renderIidTerse = renderIid' True

renderId :: GraphState -> Id -> GW
renderId gs id =
  case id of
    E eid -> renderEid gs eid
    L lid -> renderLid gs lid
    C cid -> renderCid gs cid
    U uid -> renderUid gs uid
    PT ptid -> renderPTid gs ptid
    OT otid -> renderOTid gs otid
    I iid -> renderIid gs iid

-- For a given Id, return the users that agree/disagree/like/dislike.
votesById :: GraphState -> Id -> Maybe (S.Set Uid, S.Set Uid, S.Set Uid, S.Set Uid)
votesById gs id =
  do a  <- gs ^. agree ^. at id
     da <- gs ^. disagree ^. at id
     l  <- gs ^. like ^. at id
     dl <- gs ^. dislike ^. at id
     return (a, da, l, dl)

-- Show the agree/disagree/like/dislike votes, along with vote buttons, for a given Id.
renderVotes :: GraphState -> Maybe User -> Id -> Route InfluenceGraph -> GW
renderVotes gs muser id cr  =
  case votesById gs id of
    Nothing -> errW $ "id lookup failed"
    Just (agrees, disagrees, likes, dislikes) ->
      let w (heading::T.Text) set (action::T.Text) (tag::T.Text) =
            [whamlet|
              <div class="votelists">
                <b>
                  #{heading}:
                $if S.null set
                  \ no one
                $else
                  $forall u <- set
                    \ ^{renderId gs (U u)}
                $maybe u <- muser
                  $if S.member (_uid u) set
                  $else
                    <form method="get">
                      <input type="hidden" name=#{action} value=1 />
                      <input type="submit" value=#{tag}>
            |]
      in
        [whamlet|
          ^{w "verified by" agrees    "agree"    "verify"}
          ^{w "disputed by" disagrees "disagree" "dispute"}
          $maybe u <- muser
            $if S.size agrees == 1
              $if S.member (_uid u) agrees
                 (you are the only verifier; disputing will delete this object)

          ^{w "liked by"    likes     "like"     "like"}
          ^{w "disliked by" dislikes  "dislike"  "dislike"}
        |]

-- Render friend/foe lists for a given user.
renderVotesUser :: GraphState -> Maybe User -> Id -> Route InfluenceGraph -> GW
renderVotesUser gs muser id cr  =
  case votesById gs id of
    Nothing -> errW $ "id lookup failed"
    Just (_, _, likes, dislikes) ->
      let w (heading::T.Text) set (action::T.Text) (tag::T.Text) =
            [whamlet|
              <p>
                <b>#{heading}:</b>
                $if S.null set
                  no one
                $else
                  $forall u <- set
                    \ ^{renderId gs (U u)}
                $maybe u <- muser
                  $if S.member (_uid u) set
                  $else
                    $if id == (U $ _uid u)
                    $else
                      \ <form method="get">
                        <input type="hidden" name=#{action} value=1 />
                        <input type="submit" value=#{tag} />
            |]
      in
        [whamlet|
          ^{w "friends"     likes     "like"     "add as friend"}
          ^{w "disliked by" dislikes  "dislike"  "add as foe"}
        |]


-- FRONT / INFORMATIONAL HANDLERS

-- Recent changes was getting cluttered by issue tags, so we use this to group
-- clusters of changes that are all about one thing.
mergedups :: Eq a => [a] -> [(a, Int)]
mergedups [] = []
mergedups (x:xs) = go (xs) x 1
  where
    go [] last count = [(last,count)]
    go (x:xs) last count =
      if x == last
      then go xs last (count+1)
      else (last, count) : go xs x 1 

-- Show what's happened recently.
recentChanges :: GraphState -> SEQ.Seq Id -> Handler GW
recentChanges gs allids =
  do (pw,ids) <- paginate allids
     let idws = map (\(id, count) -> (renderId gs id, count)) (mergedups $ toList ids)
     return $
       [whamlet|
         ^{pw}
         <ul>
           $forall (idw, count) <- idws
             <li> ^{idw}
               $if count > 1
                 \ (#{show count} changes)
         ^{pw}
       |]

getRecentR :: Handler Html
getRecentR = getHomeR

-- Front page.
getHomeR :: Handler Html
getHomeR =
  do ctx <- getContext Nothing
     let gs = cgs ctx
     changes <- recentChanges gs (_recent gs)
     layout $
       do setTitle "polink"
          [whamlet|
            ^{cgw ctx}
            <h1><center>Welcome to polink.org, the social network anyone can edit!</center>
            <p><center><a href="@{AboutR}">What is this?</a></center>
            <p><center><a href="@{RSHelpR}">What are those funny colored rectangle things?</a></center>
            <p><b>Points of interest:</b>
            <ul>
             <li><a href="@{EntityR (Eid 2)}">US Government</a> (<a href="@{OrgChartR (Eid 2)}">org chart</a>)
             <li><a href="@{EntityR (Eid 11)}">US Presidents</a>
             <li><a href="@{EntityR (Eid 48)}">US Supreme Court</a>
             <li><a href="@{EntityR (Eid 21)}">US Senate</a>
             <li><a href="@{EntityR (Eid 22)}">US House</a> (help us out by adding your representatives)

            <p><a href="@{EntitiesR}">all entities</a> <a href="@{IssuesR}">all issues</a> <a href="@{UsersR}">all users</a>

            $maybe u <- cmuser ctx
              <p><b>Things to do:</b>
              <p>If some important person is missing from our site, please <a href=@{NewPersonR}>add them</a>.  If you find inaccurate or duplicate information, please leave a comment and click the "dispute" button on that entry.  You can also help us out by visiting a <a href="@{RandomR}">random</a> entity and verifying that the data is correct.  (Use the "verify"/"dispute" buttons.)
              <p>You may also establish a link between two entities by using the clipboard.  For instance, to mark someone as a member of the House, click "copy to clipboard" on that person's page, then navigate to the <a href=@{EntityR (Eid 22)}>US House</a> and create the link with "member of" as the link type.

            <p><b>Recent Changes</b>
            ^{changes}
          |]

-- USER MANAGEMENT

minLen :: T.Text -> Int -> T.Text -> Either T.Text T.Text
minLen e min t
  | T.length t >= min = Right t
  | otherwise = Left e

maxLen :: T.Text -> Int -> T.Text -> Either T.Text T.Text
maxLen e max t
  | T.length t <= max = Right t
  | otherwise = Left e

bracketLen :: T.Text -> Int -> Int -> T.Text -> Either T.Text T.Text
bracketLen e min max t
  | T.length t < min || T.length t > max = Left e
  | otherwise = Right t

--newUserForm :: Html -> MForm InfluenceGraph InfluenceGraph (FormResult (T.Text, Bool), Widget)
newUserForm =
  renderDivs $
    (,)
      <$> areq userField "username" Nothing
      <*> areq trueField "I agree to the terms below." Nothing
  where
   userField = check (bracketLen "username must be between 4 and 64 characters" 4 64) textField
   trueField = checkBool (id) ("you must agree to the site policy"::T.Text) checkBoxField

getNewUserR :: Handler Html
getNewUserR =
  do maid <- maybeAuthId
     (widget, enctype) <- generateFormPost newUserForm
     layout
       [whamlet|
         $maybe id <- maid
           <p>
             Successfully authenticated via browserId.  You may now create a
             user acount.

           <p>Please enter a username.  This is the name that other users will see.
           <form method=post action=@{NewUserR} enctype=#{enctype}>
             ^{widget}
             <input type=submit value="create user"><p>

           <p><a href=@{AuthR LogoutR}>cancel</a>

           <h1>Acceptable Use
           <p>
             You may not post offensive content, spam, or information which you
             know to be factually incorrect or off-topic.

           <h1>Contributed Content
           <p>
             You grant the operators of this site a perpetual, royalty-free,
             worldwide, non-revocable license to copy and use the content you
             submit for any purposes, including making it available to third
             parties through web APIs.  You retain the copyright to
             your comments, and they will be attributed to the username of your
             account.

           <h1>Privacy
           <p>
             Your email address and ip address will be treated as private
             information and we will do our best to maintain that privacy.
             We may send you email if necessary to help you use our site, but
             we won't send you advertisements or give your email address to
             anyone who will.
             Everything else on this site including contributed content, comments,
             what you like and dislike, and your friends and foes will be visible
             to the site's other users and the public.

         $nothing
           <p>Something went wrong with browserId authentication.
       |]

postNewUserR :: Handler Html
postNewUserR =
  do ig   <- getYesod
     let as = acid ig
     maid <- maybeAuthId
     case maid of
       Nothing -> err "you must be authenticated via browserId before you can create an account"
       Just email ->
         do ((result, widget), enctype) <- runFormPost newUserForm
            case result of
              FormSuccess (uname, checked) ->
                if (checked)
                then do uid <- liftIO $ update as (AddUserU uname email)
                        redirect $ UserR uname
                else err "you must agree to the terms"
              _ -> err "invalid input"

getUsersR :: Handler Html
getUsersR =
  do ctx <- getContext Nothing
     let gs = cgs ctx
     let us = M.toList (gs ^. users)
     (pw, us') <- paginateL us
     layout
       [whamlet|
         ^{cgw ctx}
         ^{pw}
         $forall u <- us'
           $with uname <- _name $ snd u
             <p><a href="@{UserR uname}">#{uname}</a>
         ^{pw}
       |]

getUserR :: Txt -> Handler Html
getUserR name =
  do -- We need to resolve the username to an id we can hand into getContext.
     -- This isn't ideal, since we should ideally never query the state twice.
     -- If we race, it shouldn't be a big deal.
     ig <- getYesod
     egs <- liftIO $ query (acid ig) GetStateQ
     let muid =
           case egs of
             Left _ -> Nothing
             Right gs -> gs ^. usersByName ^. at name
     case muid of
       Nothing -> err "no such user"
       Just uid -> getUserIDR uid

data UserRole = AdminRole | EditorRole | NormalRole | CommentOnlyRole | RestrictedRole
 deriving (Eq, Ord, Show)

roleToPerm AdminRole       = adminPerm
roleToPerm EditorRole      = editorPerm
roleToPerm NormalRole      = defaultPerm
roleToPerm CommentOnlyRole = commentOnlyPerm
roleToPerm RestrictedRole  = limitedPerm

userPermForm =
  renderDivs $
    id <$> areq (selectFieldList roles) "role" (Just NormalRole)
  where
    roles :: [(T.Text, UserRole)]
    roles =
      [("admin",        AdminRole),
       ("editor",       EditorRole),
       ("normal",       NormalRole),
       ("comment only", CommentOnlyRole),
       ("restricted",   RestrictedRole)]


getUserIDR :: Uid -> Handler Html
getUserIDR uid =
  do ctx <- getContext $ Just $ U uid
     let gs = cgs ctx
     let mu = gs ^. users ^. at uid
     case mu of
       Nothing -> err "no such user"
       Just u ->
         do contrib <- recentChanges gs (_contrib u)
            (fw, enctype) <- generateFormPost userPermForm
            layout $
              do setTitle $ text $ u ^. name
                 [whamlet|
                   ^{cgw ctx}
                   <h1>user #{_name u}
                   <hr>
                   <h2>account permissions
                   $if hasPerm ctx Admin
                      <form method=post action=@{UserIDR uid} enctype=#{enctype}>
                        ^{fw}
                        <input type=submit value="change user role">
                   <p>#{showPerms (_authority u)}
                   <hr>
                   ^{renderVotesUser gs (cmuser ctx) (U $ _uid u) (UserR (_name u))}
                   <hr>
                   <h2>Contributions:
                   ^{contrib}
                 |]


postUserIDR :: Uid -> Handler Html
postUserIDR uid =
  do w <-
       reqAuth
         "modify user permissions"
         Admin
         (\ctx user ->
           do let gs = cgs ctx
                  mu = gs ^. users ^. at uid
              case mu of
                Nothing -> return $ errW "could not find that user"
                Just u ->
                  do ((result, widget), enctype) <- runFormPost userPermForm
                     case result of
                       FormSuccess role ->
                         do let p = roleToPerm role
                            ig <- getYesod
                            liftIO $ update (acid ig) (SetUserPermU uid p)
                            redirect $ UserR (_name u)
                       _ -> return $ errW "invalid input")
     layout [whamlet|^{w}|]



-- SITE HELP/DOCUMENTATION

getAboutR :: Handler Html
getAboutR = 
  do ctx <- getContext Nothing
     layout
       [whamlet|
          ^{cgw ctx}
          <h1>Polink.org
          <h2>the social network anyone can edit
          <h3>What is this?
          <p>
            Polink is a tool to aggregate relationship information about people
            and organizations, and to use that data to make it easier to 
            understand how they fit into the larger political context.
          <p>
            These relationships are often too complex and numerous for any one
            person to easily keep track of.  The purpose of this site is to
            make it easier for those of us who are interested in these
            connections to pool our data.
          <p>
            Any registered user may add a person or organization to our
            database, and may create links between these entities.  Other users
            may either verify or contest the validity of those entities or links.

          <h3>What sort of people or organizations should we add?
          <p>
            For the moment, I'd like to restrict it to people or organizations
            who are relevant to U.S. national politics or current events.
            If someone is a household name or in the news, they're an acceptable
            candidate.  If they are in a position to influence national politics,
            they're also relavant.
          <p>
            State governors are okay, but I'd like to avoid
            state legislators or town mayors and the like for now, unless they've
            somehow made themselves relevant to national politics.
          <p>
            International figures are fair game if they have some kind of
            relationship with the U.S. (whether friendly or adversarial).

          <h3>How do I add a person or organization?
          <p>
            When you login, you will see links at the top of the page to
            add a person or organization.  Fill out the form with the relevant
            information (name they are best known by, legal name, date of birth
            or date organization was created, links to the relevant wikipedia page,
            twitter account and website if any).

          <h3>How do I create links between people and/or organizations?
          <p>
            Links are a little complicated.  Let's say you come across a news
            article that says some politician <b>A</b> has endorsed some other
            politician <b>B</b>, and you want to add that link to our database.
          <p>
            First, you must browse to the page for one of those two politicians
            (or create them, if they aren't already there).  Then, click the
            "copy to clipboard" link.  From now on, that person will be shown in
            the clipboard at the top of the page.
          <p>
            Now, whenever you visit the page of any other person or organization,
            you will be given the option of creating a link between the person or
            organization in your clipboard and the one shown on the current page.
            Select the appropriate link type ("endorses" in this example) and
            click the "create link" button.
          <p>
            You will be taken to a form where you can input the date the endorsement
            occurred and a link to the news article.  (For some kinds of relationships
            that cover a span of time, it makes sense to add an end date as well.)

          <h3>What sort of links should I add?
          <p>
            Links should be independently verifiable.  A citation url is required
            for creating links, and it should point to an appropriate article.
            Wikipedia links are ok for things that are common knowledge, but
            for more contetious links you should provide a more authoritative
            source.
          <p>
            Links have a type, which should be set appropriately.
          <p>
            Links usually also have a direction.  "<b>A</b> disagrees with <b>B</b>"
            is not the same as "<b>B</b> disagrees with <b>A</b>".
            Links should have the subject first and the object last, as in
            the "subject-verb-object" order of English.
          <p>
            For some kinds of links, a link in one direction implies a link in
            the other.  For instance, "Barack Obama is married to Michelle Obama"
            implies that "Michelle Obama is married to Barack Obama."  We don't
            have any explicit policy that those kind of links should be one way
            or the other, but try not to create links in both directions.
            (We may eventually modify the software to create or infer those
            reverse links automatically.)

          <h3>How does the tagging system work?
          <p>
            Entities (people or organizations) can be tagged.  For instance,
            president Obama might be tagged as "politician", "author",
            "teacher" and "lawyer".  Tags can be voted on just like entities
            and links, and each has its own comment thread.
          <p>
            Tags should be used to represent objective facts, not your own
            opinions.
            For instance, it's acceptable to tag Bernie Madoff as "criminal"
            because he was successfully convicted of fraud, but it isn't
            ok to tag a politician you believe broke the law as "criminal"
            if they haven't actually been formally charged or convicted.

          <h3>How do I express my approval or disapproval?
          <p>
            Every entity, link, tag, and comment has "like" and "dislike"
            links you may use to register your opinion. User profile pages also
            have "friend" and "foe" links, which work the same way.
          <p>
            Note that your likes, dislikes, friends, and foes
            are publicly viewable by everyone.  Be careful with the "foe" button!
          <p>
            Also, be aware that you may switch your vote, but you cannot unvote.

          <h3>This information is wrong!  How do I fix it?
          <p>
            Currently, regular users may not edit an existing entity.  However,
            you may create a new entity or link in its place.  (It is quite
            possible to have a given person or organization or link in the
            database more than once.)
          <p>
            If information is wrong, please post a comment in the appropriate
            place, and click the "contest" link.  If you were the one that
            created the item, and yours is the only "verify" vote, then
            contesting the entry will cause it to be deleted.  (Items only exist so
            long as they are verified by at least one person.  If no one
            believes that something is true, then why keep it around?)  This is
            an easy way to delete newly-created content if you accidentally
            typed something in wrong.
            Contested content may be deleted by an editor or admin.

          <h3>How do I find stuff?
          <p>
            The easiest way is to use the search function (follow the link at
            the bottom of the page).
          <p>
            Another trick if you know the wikipedia address of the thing you're
            looking for is to replace "en.wikipedia.org" with "polink.org"
            in the url, and if the item is in our database with that wiki
            link, it'll take you to the right place.
            For example: <a href="@{WikiR "Barack_Obama"}">http://polink.org/wiki/Barack_Obama</a>.
            You can also browse the <a href="@{EntitiesR}">list of all entities</a>.

          <h3>Who is running this site?
          <p>
            This project is run by
            <a href="http://jsnow.bootlegether.net">Jim Snow</a> and Daniel Connor
            at <a href="http://metamocracy.com">Metamocracy LLC</a>.
          <p>
            The <a href="@{HomeR}">Polink.org</a> software is written by
            <a href="http://jsnow.bootlegether.net">Jim Snow</a>.
          <p>
            We created this site originally as a platform for experimenting with
            <a href="http://metamocracy.com/design/anonymity-and-trust">reputation systems</a>,
            but we also want it to be useful in its own right to provide more
            visibility into the complex connections between powerful people and
            organizations.
          <p>
            If you would like to contact us for any reason, you can email
            <a href="mailto:jsnow@metamocracy.com">Jim Snow</a> or
            <a href="mailto:dconnor@metamocracy.com">Daniel Connor</a>.

          <h3>What is the technology stack?
          <p>
            The Polink software is written in Haskell and uses the
            <a href="http://www.yesodweb.com/">Yesod</a> web framework.
            Persistance is handled by
            <a href="http://hackage.haskell.org/package/acid-state">acid-state</a>,
            which can be thought of as an in-memory database with an on-disk log.
          <p>
            This is an open-source project.  Our source code is available on
            Github <a href="https://github.com/jimsnow/polink">here</a>.
       |]

getRSHelpR :: Handler Html
getRSHelpR = 
  do ctx <- getContext Nothing
     layout
       [whamlet|
          ^{cgw ctx}
          <h1>The Reputation System
          <p>
            You may have noticed little rectangles with colored bars in them
            littered about the site, and wondered what they mean.
          <p>
            For example: ^{renderEid (cgs ctx) (Eid 12)}
          <p>
            (If you aren't seeing them, it may mean your "point of view" is set
            to "none" or your browser does not render SVG, or
            you have javascript disabled, or something has gone wrong on our end.  Sorry.
            Also, if you see plain rectangles but no colored bars inside,
            see the question below about point of view.)

          <h3>So, what do they mean?
          <p>
            The colored bars represent the reputation of the person or organization
            they are displayed next to.
          <p>
            On the left side, we have positive reputation.  The more positive
            reputation someone has, the bigger the green bar.
          <p>
            On the right side, we have negative reputation.  There are two kinds;
            the red, lower bar shows the degree to which people don't like that person.
            It's what people generally mean when they say that someone has a bad reputation.
          <p>
            The yellow-orange, upper bar is a little more subtle.  It shows the degree
            to which that person associates with people who have a bad reputation.

          <h3>How do you calculate reputation?
          <p>
            Reputations are calculated based on the connections between entities.
            If person <b>A</b> disagrees with person <b>B</b>, <b>B</b>'s red bar will get
            little bigger.  If organization <b>C</b> praises person <b>B</b>,
            <b>B</b>'s  green bar will get bigger, but <b>C</b>'s yellow bar will
            also get bigger because <b>A</b> disagrees with <b>B</b>.
          <p>
            The actual calculation is done using a software package called <b>Pariah</b>.
            Curious souls can read more about it
            <a href="http://metamocracy.com/design/anonymity-and-trust">here</a>.
            You can also grab the <a href="@{RSStateR}">raw data</a> if you have your
            own ideas about how reputations ought to be computed.
          <p>
            Note that, currently, the software that computes reputation values works off of
            a copy of the data that it fetches once every three minutes, so you
            won't see changes quite in real time.

          <h3>What's with the "point of view" selector at the bottom of the page?
          <p>
            Reputation is subjective.  Someone can have a good reputation from
            one person's point of view, and a bad reputation from someone else's.
            To accomodate the subtleties of reputation, we allow you to select
            from several points of view.  Here are the options:

          <ul>
            <li>
              <p>
                <b>None</b>:
              <p>Turn off the reputation system entirely.
            <li>
              <p><b>Neutral</b>:
              <p>
                Every person or organization is treated the same way and
                has the same initial influence.  (People may gain influence
                by having a lot of followers, but that influence is
                ultimately derived from individual users or organizations.)
            <li>
              <p>
                <b>Liked by me</b>:
              <p>
                Only people or oragizations that are connected to people or organizations
                you like have any influence.  This assumes you are logged in.
                If you don't like anyone (i.e. you haven't clicked the "like" button on
                any person or organization's page), you'll just see blank results.
            <li>
              <p>
                <b>Disliked by me</b>:
              <p>
                Similar to "liked by me" but instead, we compute reputations from
                the point of view of people or organizations you don't like.
            <li>
              <p>
                <b>Some person or organization</b>:
              <p>
                If you navigate to the page of some person or organization, they will
                show up as an additional option in the dropdown menu.  This will show
                you reputations from their point of view.

          <h3>This can't be right.  These reputation values don't make sense.
          <p>
            The reputation algorithm is only as good as the input data.  Right now,
            connections between people are pretty sparse, so a single new link can
            cause reputations to change drastically.  Once we have a fairly dense
            set of connections, things should stabilize.
          <p>
            You can help us out by adding in missing links, especially the boring
            ones that seem so obvious as to be hardly worth mentioning.

          <h3>If I like or dislike a person or organization, will that affect the reputations that other people see?
          <p>
            No, it only affects what you see.

          <h3>If I add another user as a friend or foe, will it affect the reputations that I see?
          <p>
            No, links between users are completely separate.  They currently don't
            affect anything in the system.
       |]

-- ENTITY MANAGEMENT

getEntitiesR :: Handler Html
getEntitiesR =
  do ctx <- getContext Nothing
     let gs = cgs ctx
     let es = M.toList (gs ^. entities)
     (pw, es') <- paginateL es
     layout
       [whamlet|
         ^{cgw ctx}
         ^{pw}
         $forall (eid,e) <- es'
           <p>^{renderEid gs eid}
         ^{pw}
       |]

getEntityByNameR :: Txt -> Handler Html
getEntityByNameR = undefined

renderLink :: GraphState -> Lid -> (GW, Maybe Link)
renderLink gs lid@(Lid id) =
  case gs ^. (links . at lid) of
    Nothing -> ([whamlet|deleted link|], Nothing)
    Just link ->
      case gs ^. (entities . at (link ^. lsrc)) of
        Nothing -> (errW "no such link source", Nothing)
        Just e1 ->
          case gs ^. (entities . at (link ^. ldst)) of
            Nothing -> (errW "no such link destination", Nothing)
            Just e2 ->
              let date1 = link ^. ldate
                  date2 = link ^. lend
                  e1name = e1 ^. ecname
                  e2name = e2 ^. ecname
                  lt     = link ^. ltype
                  urls   = link ^. lurl
              in
                  ([whamlet|
                     <span class="link" id=#{show id}>
                       <a href="@{EntityR (_eid e1)}">#{e1name}</a>
                       \ <a href="@{LinkR lid}">#{lToText lt}</a>
                       \ <a href="@{EntityR (_eid e2)}">#{e2name}</a>
                       $case lt
                         $of PL _
                           \ (<span class="poslink">+</span>)
                         $of NL _
                           \ (<span class="neglink">-</span>)
                       $maybe m <- (_lmoney link)
                         \ #{show $ unUSD m} USD
                       \ #{showDateRange False (Just date1) date2}
                       $forall url <- urls
                         \ <a href="#{url}" rel=nofollow>citation</a>
                   |], Just link)

showTwt :: Url -> T.Text
showTwt (Url twt)
  | T.head twt == '@' = T.append (T.pack "https://twitter.com/") (T.tail twt)
  | otherwise = twt

renderEntity :: GraphState -> Eid -> (GW, Maybe Entity)
renderEntity gs eid =
  case gs ^. (entities . at eid) of
    Nothing -> (errW "deleted entity", Nothing)
    Just e ->
      let cname   = e ^. ecname
          lname   = e ^. elname
          eb      = e ^. ebirth
          ed      = e ^. edeath
          wp      = e ^. ewp
          blog    = e ^. ehp
          twt     = e ^. etwt
          Eid id  = eid
      in
        ([whamlet|
          <center>
            <h1>
              <span class="entity" id=#{show id}>
                #{cname}
            $case (_etype e)
              $of Person _
                 <p>person
              $of Organization _
                 <p>organization (<a href=@{OrgChartR eid}>org chart</a>)
            <h2>#{lname} #{showDateForceRange False eb ed}
            <p>
              $maybe w <- wp
                <a href="http://en.wikipedia.org/wiki/#{w}">wikipedia</a>
              $nothing
                _
              $maybe b <- blog
                \ <a href="#{b}">website</a>
              $nothing
                \ _
              $maybe t <- twt
                \ <a href="#{showTwt t}">twitter</a>
              $nothing
                \ _

        |], Just e)

msetToList (Just s) = S.toList s
msetToList Nothing = []

isPerson :: Eid -> Ctx -> Maybe Bool
isPerson eid ctx =
  case (cgs ctx) ^. entities . at eid of
    Nothing -> Nothing
    Just entity ->
      case entity ^. etype of
        Person _ -> Just True
        Organization _ -> Just False

getEntityR' :: Eid -> Handler TypedContent
getEntityR' eid = 
  do ctx <- getContext $ Just $ E eid
     let gs = cgs ctx
     let (entitywidget, mentity) = renderEntity gs eid
     let srcLinks = map (renderLink gs) (msetToList $ gs ^. linksBySrc . at eid)
     let dstLinks = map (renderLink gs) (msetToList $ gs ^. linksByDst . at eid)
     tagw <- tagForm (cmuser ctx) mentity
     cr <- curRoute
     cw <- renderComments gs (cmuser ctx) (E eid)
     lfs <- 
       case cmcb ctx of
         Nothing -> return []
         Just cbe ->
           do lf1 <- renderLinkPart1 ctx eid (_eid cbe)
              lf2 <- renderLinkPart1 ctx (_eid cbe) eid
              if eid == (_eid cbe)
              then return [lf1]
              else return [lf1, lf2]
     selectRep $ do
       provideRep $
         layout 
           (do case mentity of
                 Nothing -> return ()
                 Just ent -> setTitle $ text $ ent ^. ecname
               [whamlet|
                 ^{cgw ctx}
                 ^{entitywidget}
 
                 $if (hasPerm ctx EditEntity)
                   $maybe isP <- isPerson eid ctx
                     $if isP
                       <form method="get" action="@{EditPersonR eid}">
                         <input type="submit" value="edit" />
                     $else
                       <form method="get" action="@{EditOrgR eid}">
                         <input type="submit" value="edit" />

                 $if (hasPerm ctx DelEntity)
                   <form method="post" action="@{DelEntityR eid}">
                     <input type="submit" value="delete" />

                 <hr>

                 $maybe e <- mentity
                   $maybe cb <- cmcb ctx
                     $forall lf <- lfs
                       ^{lf}
                     <hr>
                   $nothing
                     <p><a href="@{cr}?setcb=#{show eid}">copy</a> #{_ecname e} to clipboard
                     <hr>

                 $with ename <- caseJust mentity "deleted" _ecname
           
                   $maybe u <- (cmuser ctx)
                     ^{renderTags gs u eid}
                     <br>
                     ^{tagw}
                     <hr>
                   ^{renderVotes gs (cmuser ctx) (E eid) cr}
                   <hr>

                   $if not $ null srcLinks
                     <h2>#{ename}'s links
                     <ul>
                     $forall lw <- srcLinks
                       <li> ^{fst lw}

                   $if not $ null dstLinks
                     <h2>#{ename}'s backlinks
                     <ul>
                     $forall lw <- dstLinks
                       <li> ^{fst lw}

                   $if not $ null srcLinks
                     <hr>
                   $else
                     $if not $ null dstLinks
                       <hr>
                   comments:
                   ^{cw}
               |])
       provideRep $
         (case mentity of
            Just ent -> jsonify $ toJSON (cgs ctx, ent)
            Nothing -> error "not found")         

-- Clean up text so it looks good in a URL
urlifier :: T.Text -> T.Text
urlifier s =
  T.map f s
  where
    f ' ' = '-'
    f c = C.toLower c

-- We like to have the entity name appear in the url rather than just a number,
-- so we do an automatic redirect if we use the simplified url that doesn't
-- have the name.  (The name passed to the final handler gets ignored -- it's
-- just there for looks.)
getEntityR :: Eid -> Handler TypedContent
getEntityR eid = 
  do ig <- getYesod
     gs' <- liftIO $ query (acid ig) GetStateQ
     let gs = fromEither gs'
     case gs ^. entities . at eid of
       Nothing -> getEntityR' eid
       Just e -> redirect (EntityCanonR eid (urlifier $ e ^. ecname))

-- If you supply a name, we ignore it and go straight to the regulary entity
-- handler.
getEntityCanonR :: Eid -> T.Text -> Handler TypedContent
getEntityCanonR eid name = getEntityR' eid

deleteEntityCanonR :: Eid -> T.Text -> Handler Html
deleteEntityCanonR eid name = deleteEntityR eid

-- Helper function for writing delete handlers.
-- Given a permission, a string describing that action, an action, and a
-- type-safe url, we check that the user has the permission to do the action.
-- If it works, we redirect to the type-safe url.
deleteThing auth string action redir =
  do w <-
       reqAuth
         string
         auth
         (\ctx user ->
           do ig <- getYesod
              res <- liftIO $ update (acid ig) (action user)
              case res of
                Left err -> return $ errW $ T.pack err
                Right _ -> redirect redir)
     layout [whamlet|^{w}|]


deleteEntityR :: Eid -> Handler Html
deleteEntityR eid =
  deleteThing DelEntity "delete entity" (\user -> DelEntityU (user ^. uid) eid) (EntityR eid)

postDelEntityR = deleteEntityR



addBool :: Maybe a -> Maybe (a, Bool)
addBool m = fmap (\x -> (x, False)) m

eToPerson cn ln b d wp hp twt =
  Entity (Eid (-1)) cn ln (Person M.empty)       (addBool b) (addBool d) (fmap Url wp) (fmap Url twt) (fmap Url hp) 

eToOrg    cn ln b d wp hp twt =
  Entity (Eid (-1)) cn ln (Organization M.empty) (addBool b) (addBool d) (fmap Url wp) (fmap Url twt) (fmap Url hp)

mConcat :: Maybe (Maybe a) -> Maybe a
mConcat (Just x) = x
mConcat Nothing = Nothing

bTextField = check (bracketLen "text field must be between 3 and 256 characters" 3 256) textField

newPersonForm ment =
  renderDivs $
    eToPerson
      <$> areq bTextField "name in popular usage"         (fmap _ecname ment)
      <*> areq bTextField "legal name"                    (fmap _elname ment)
      <*> aopt dayField   "birth"                         (fmap (fmap fst . _ebirth) ment)
      <*> aopt dayField   "death (if applicable)"         (fmap (fmap fst . _edeath) ment)
      <*> aopt bTextField "http://en.wikipedia.org/wiki/" (fmap (fmap unUrl . _ewp) ment)
      <*> aopt bTextField "personal website/blog"         (fmap (fmap unUrl . _ehp) ment)
      <*> aopt bTextField "twitter"                       (fmap (fmap unUrl . _etwt) ment)

newOrganizationForm ment =
  renderDivs $ 
    eToOrg
      <$> areq bTextField "name in popular usage"         (fmap _ecname ment)
      <*> areq bTextField "official name of entity"       (fmap _elname ment)
      <*> aopt dayField   "organization formed"           (fmap (fmap fst . _ebirth) ment)
      <*> aopt dayField   "organization disbanded"        (fmap (fmap fst . _edeath) ment)
      <*> aopt bTextField "http://en.wikipedia.org/wiki/" (fmap (fmap unUrl . _ewp) ment)
      <*> aopt bTextField "website/blog"                  (fmap (fmap unUrl . _ehp) ment)
      <*> aopt bTextField "twitter"                       (fmap (fmap unUrl . _etwt) ment)

getNewPersonR' :: Maybe Eid -> Handler Html
getNewPersonR' meid = 
  do ctx <- getContext Nothing
     let mentity = mConcat $ fmap (\eid -> (cgs ctx) ^. entities . at eid) meid
     (widget, enctype) <- generateFormPost $ newPersonForm mentity
     layout
       [whamlet|
         ^{cgw ctx}
         $maybe u <- cmuser ctx
           $maybe eid <- meid
             <h1>Edit a person.
             <form method=post action=@{EditPersonR eid} enctype=#{enctype}>
               ^{widget}
               <input type=submit value="submit changes">
           $nothing
             <h1>Add a person.
             <form method=post action=@{NewPersonR} enctype=#{enctype}>
               ^{widget}
               <input type=submit value="add person">
         $nothing
           <p>You must be logged in to add or modify a person.
       |]

getNewOrgR' :: Maybe Eid -> Handler Html
getNewOrgR' meid =
  do ctx <- getContext Nothing
     let mentity = mConcat $ fmap (\eid -> (cgs ctx) ^. entities . at eid) meid
     (widget, enctype) <- generateFormPost $ newOrganizationForm mentity
     layout
       [whamlet|
         $maybe u <- cmuser ctx
           $maybe eid <- meid
             <h1>Edit an organization.
             <form method=post action=@{EditOrgR eid} enctype=#{enctype}>
               ^{widget}
               <input type=submit value="submit changes">
           $nothing
             <h1>Add an organization.
             <form method=post action=@{NewOrgR} enctype=#{enctype}>
               ^{widget}
               <input type=submit value="add organization">
         $nothing
           <p>You must be logged in to add or modify a person.
       |]

getNewPersonR = getNewPersonR' Nothing
getNewOrgR = getNewOrgR' Nothing

getEditPersonR eid = getNewPersonR' (Just eid)
getEditOrgR eid = getNewOrgR' (Just eid)


postNewPersonR' :: Maybe Eid -> Handler Html
postNewPersonR' meid =
  do w <- 
       reqAuth
         (case meid of
           Nothing -> "add person"
           Just _ -> "edit person")         
         (case meid of
           Nothing -> AddEntity
           Just _ -> EditEntity)
         (\ctx user ->
            do ((result, widget), enctype) <- runFormPost $ newPersonForm Nothing
               case result of
                 FormSuccess ent ->
                   do ig <- getYesod
                      res <-
                        case meid of
                          Nothing -> liftIO $ update (acid ig) (AddEntityU (user ^. uid) ent)
                          Just eid -> liftIO $ update (acid ig) (EditEntityU (user ^. uid) eid ent)
                      case res of
                        Left err ->
                          return $ errW (T.pack err)
                        Right eid ->
                          redirect $ EntityR eid
                 _ -> return $ errW "invalid input")
     layout [whamlet|
              ^{w}
            |]


postNewOrgR' :: Maybe Eid -> Handler Html
postNewOrgR' meid =
  do w <- 
       reqAuth
         (case meid of
            Nothing -> "add organization"
            Just _ -> "edit organization")
         (case meid of
            Nothing -> AddEntity
            Just _ -> EditEntity)
         (\ctx user ->
            do ((result, widget), enctype) <- runFormPost $ newOrganizationForm Nothing
               case result of
                 FormSuccess ent ->
                   do ig <- getYesod
                      res <-
                        case meid of
                          Nothing -> liftIO $ update (acid ig) (AddEntityU (user ^. uid) ent)
                          Just eid -> liftIO $ update (acid ig) (EditEntityU (user ^. uid) eid ent)
                      case res of
                        Left err ->
                          return $ errW (T.pack err)
                        Right eid ->
                          redirect $ EntityR eid
                 _ -> return $ errW "invalid input")
     layout [whamlet|^{w}|]

postNewPersonR = postNewPersonR' Nothing
postNewOrgR = postNewOrgR' Nothing

postEditPersonR eid = postNewPersonR' (Just eid)
postEditOrgR eid = postNewOrgR' (Just eid)


-- LINK MANAGEMENT

pgeneric_lt :: [PosLinkType]
pgeneric_lt = [Agree, Endorse, Praise, Contribute, Trust, Assist, DoFavor, MakeDeal, AsksForHelp, Appologize, Forgive, Defend, Protect, Manage]

ngeneric_lt :: [NegLinkType]
ngeneric_lt = [Disagree, Criticize, Discredit, Distrust, Accuse, Condemn, Insult, Sue, Oppose, Hinder, Threaten, EndsRelationship, DeclinesToHelp, Ridicule, Attack]

pp_lt = ([Marry, ParentOf, Nominate, Appoint, WorksFor, ContractsFor], [Divorce, Breakup, Fire, Assault, Kill])
po_lt = ([Member, Invest, Retire, WorksFor, ContractsFor, HoldsOffice], [Resign, Divest])
op_lt = ([Nominate, Appoint, Elect, BestowTitle, Award], [Recall])
oo_lt = ([Invest, SubOrg, Member], [Divest, Split])

mksfl :: ([PosLinkType],[NegLinkType]) -> [(T.Text, LinkType)]
mksfl (pspec, nspec) = (sortBy (compare `on` fst) pl) ++ (sortBy (compare `on` fst) nl)
  where
    ps = pgeneric_lt ++ pspec
    ns = ngeneric_lt ++ nspec
    pl = map (\p -> let plt = PL $ PLType (fromEnum p) in (lToText plt, plt)) ps
    nl = map (\p -> let nlt = NL $ NLType (fromEnum p) in (lToText nlt, nlt)) ns


{-
    (map (\p -> (lToText (PL p), PL p)) ps) ++
    (map (\n -> (lToText (NL n), NL n)) ns)


 map (\t -> let pt = PTag (fromEnum t) in (ptToText pt, pt))

-}

flattenMaybe :: [Maybe a] -> [a]
flattenMaybe [] = []
flattenMaybe (x:xs) =
  case x of
    Nothing -> flattenMaybe xs
    Just a -> a : flattenMaybe xs

-- newLinkFormPart1 :: Entity -> Entity -> [(T.Text, LinkType)] -> Html -> MForm InfluenceGraph InfluenceGraph (FormResult LinkType, Widget)
newLinkFormPart1 :: Entity -> Entity -> [(T.Text, LinkType)] -> Html -> MForm Handler (FormResult LinkType, Widget)
newLinkFormPart1 e1 e2 sfl extra =
  do (ltRes, ltView) <- mreq (selectFieldList sfl) "link type" Nothing
     let formRes = id <$> ltRes
     let w =
          [whamlet|
            #{extra}
            <p>
              #{_ecname e1}
              \ ^{fvInput ltView}
              \ #{_ecname e2} 
              \ <input type=submit value="create link">
          |]
     return (formRes, w)

renderLinkPart1 :: Ctx -> Eid -> Eid -> Handler GW
renderLinkPart1 ctx eid1 eid2 =
  if hasPerm ctx AddLink
  then
  case getsfl (cgs ctx) eid1 eid2 of
    Nothing -> return $ errW "could not find entities"
    Just (sfl, e1, e2) ->
       do (w, enctype) <- generateFormPost (newLinkFormPart1 e1 e2 sfl)
          return
            [whamlet|
               <form method=post action=@{NewLink2R eid1 eid2} enctype=#{enctype}>
                 ^{w}
            |]
  else
    return [whamlet||]

mhead [] = Nothing
mhead (x:_) = Just x

mconcat (Just (Just a)) = Just a
mconcat _ = Nothing

-- Not quite the same as >> or the mappend instance...
meither :: Maybe a -> Maybe a -> Maybe a
meither j@(Just a) _ = j
meither _ b = b

newLinkForm sfl mlt mlink hideMoney =
  renderDivs $
    (,,,,,,) <$> areq (selectFieldList sfl) "link type"         (meither mlt (fmap _ltype mlink))
             <*> areq dayField   "date event happened or began" (fmap fst (fmap _ldate mlink))
             <*> aopt dayField   "end date (if any)"            (fmap (fmap fst) (fmap _lend mlink))
             <*> (if hideMoney
                  then areq hiddenField ""                      Nothing
                  else aopt intField   "money in USD"           (fmap ((fmap unUSD) . _lmoney) mlink))
             <*> areq bTextField "reference url"                (mconcat (fmap (fmap unUrl . mhead . _lurl) mlink))
             <*> aopt bTextField "reference url (optional)"     (fmap (fmap unUrl . mhead . drop 1 . _lurl) mlink)
             <*> aopt bTextField "reference url (optional)"     (fmap (fmap unUrl . mhead . drop 2 . _lurl) mlink)
  where
   bTextField = check (bracketLen "reference must be a url no longer than 512 characters" 3 512) textField

--  where
--   sft = e1 `T.append` " [x] " `T.append` e2

-- retreive select field list for two eids
getsfl gs eid1 eid2 = 
  let mets = (do e1t <- gs ^. entities ^. at eid1
                 e2t <- gs ^. entities ^. at eid2
                 return (e1t, e2t))
  in case mets of
       Nothing -> Nothing
       Just (e1, e2) ->
         Just $
           (case (e1 ^. etype, e2 ^. etype) of
              (Person _, Person _) -> mksfl pp_lt
              (Person _, Organization _) -> mksfl po_lt
              (Organization _, Person _) -> mksfl op_lt
              (Organization _, Organization _) -> mksfl oo_lt,
            e1,
            e2)


getNewLinkR' :: Eid -> Eid -> Maybe LinkType -> Handler Html
getNewLinkR' eid1 eid2 mlt =
  do w <-
       reqAuth
         "add a link"
         AddLink
         (\ctx user ->
           do let gs = cgs ctx 
              case getsfl gs eid1 eid2 of
                Nothing -> return $ errW "could not find entities"
                Just (sfl, e1, e2) ->
                  do (widget, enctype) <- generateFormPost (newLinkForm sfl mlt Nothing hideMoney)
                     return
                       [whamlet|
                         <h1>Add a link: #{_ecname e1} &rarr; #{_ecname e2}
                         <form method=post action=@{NewLinkR eid1 eid2} enctype=#{enctype}>
                           ^{widget}
                           <input type=submit value="add link">
                       |])
     layout [whamlet|^{w}|]
  where
    hideMoney =
      case mlt of
        Nothing -> True
        Just (PL plt) ->
          case toEnum (unPLType plt) of
            Contribute -> False
            _ -> True
        Just (NL nlt) ->
          case toEnum (unNLType nlt) of
            Sue -> False
            Fine -> False
            _ -> True


getNewLinkR eid1 eid2 = getNewLinkR' eid1 eid2 Nothing

postNewLink2R :: Eid -> Eid -> Handler Html
postNewLink2R eid1 eid2 =
  do ctx <- getContext $ Nothing
     case getsfl (cgs ctx) eid1 eid2 of
       Nothing -> layout $ errW "could not find entities"
       Just (sfl, e1, e2) ->
         do ((result, widget), enctype) <- runFormPost (newLinkFormPart1 e1 e2 sfl)
            case result of
              FormSuccess linktype -> getNewLinkR' eid1 eid2 (Just linktype)
              _ -> layout $ errW "invalid input"
     

postNewLinkR :: Eid -> Eid -> Handler Html
postNewLinkR eid1 eid2 =
  do w <- 
       reqAuth
         "add a link"
         AddLink
         (\ctx user ->
           do let gs = cgs ctx 
              case getsfl gs eid1 eid2 of
                Nothing -> return $ errW "could not find entities"
                Just (sfl, e1, e2) ->
                  do ((result, widget), enctype) <- runFormPost (newLinkForm sfl Nothing Nothing False)
                     case result of
                       FormSuccess (lt, start, end, money, url1, url2, url3) ->
                         do let urls = flattenMaybe [Just url1, url2, url3]
                            ig <- getYesod
                            mlid <- liftIO $ update (acid ig) (AddLinkU (user ^. uid) eid1 lt eid2 (start, False) (addBool end) (fmap USD money) (map Url urls))
                            case mlid of
                              Left err -> return $ errW (T.pack err)
                              Right lid -> redirect $ LinkR lid
                       _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]

getLinkR :: Lid -> Handler Html
getLinkR lid =
  do ctx <- getContext $ Just $ L lid
     let gs = cgs ctx
     cr <- curRoute
     let (linkwidget, mlink) = renderLink (cgs ctx) lid
     issuetagform <- renderIssueTagForm gs lid
     comments <- renderComments gs (cmuser ctx) (L lid)
     layout
       [whamlet|
         ^{cgw ctx}
         ^{linkwidget}
         $if (hasPerm ctx EditLink)
           <form method="get" action="@{EditLinkR lid}">
             <input type="submit" value="edit" />

         $if (hasPerm ctx DelLink)
           <form method="post" action="@{DelLinkR lid}">
             <input type="submit" value="delete" />

         $if (hasPerm ctx AddIssueTag)
           <hr>
           ^{issuetagform}
         <hr>
         ^{renderVotes gs (cmuser ctx) (L lid) cr}
         <hr>
         comments:
         ^{comments}
       |]

getEditLinkR :: Lid -> Handler Html
getEditLinkR lid =
  do ctx <- getContext $ Just $ L lid
     case (cgs ctx) ^. links . at lid of
       Nothing -> layout $ errW "no such link"
       Just link ->
         let eid1 = link ^. lsrc
             eid2 = link ^. ldst
         in case getsfl (cgs ctx) eid1 eid2 of
              Nothing -> layout $ errW "could not find entities"
              Just (sfl, e1, e2) ->
                do (widget, enctype) <- generateFormPost (newLinkForm sfl Nothing (Just link) False)
                   layout $
                     [whamlet|
                       ^{cgw ctx}

                       <form method="post" action=@{EditLinkR lid} enctype=#{enctype}>
                          ^{widget}
                          <input type=submit value="update link">
                     |]

postEditLinkR :: Lid -> Handler Html
postEditLinkR lid =
  do w <- 
       reqAuth
         "edit a link"
         EditLink
         (\ctx user ->
           do let gs = cgs ctx 
              case do oldlink <- gs ^. links . at lid
                      let eid1 = oldlink ^. lsrc
                      let eid2 = oldlink ^. ldst
                      (sfl, e1, e2) <- getsfl gs eid1 eid2
                      return (sfl, oldlink, eid1, eid2) of
                Nothing -> return $ errW "could not find entities"
                Just (sfl, oldlink, eid1, eid2) ->
                  do ((result, widget), enctype) <- runFormPost (newLinkForm sfl Nothing (Just oldlink) False)
                     case result of
                       FormSuccess (lt, start, end, money, url1, url2, url3) ->
                         do let urls = flattenMaybe [Just url1, url2, url3]
                            ig <- getYesod
                            mlid <- liftIO $ update (acid ig) (EditLinkU (user ^. uid) lid eid1 lt eid2 (start, False) (addBool end) (fmap USD money) (map Url urls))
                            case mlid of
                              Left err -> return $ errW (T.pack err)
                              Right lid -> redirect $ LinkR lid
                       _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]



deleteLinkR :: Lid -> Handler Html
deleteLinkR lid =
  deleteThing DelLink "delete link" (\user -> DelLinkU (user ^. uid) lid) (LinkR lid)

postDelLinkR = deleteLinkR

getLinksBetweenR :: Eid -> Eid -> Handler Html
getLinksBetweenR = undefined


-- TAG MANAGEMENT

renderTags :: GraphState -> User -> Eid -> GW
renderTags gs u eid =
  case gs ^. entities . at eid of
    Nothing -> [whamlet||]
    Just ent ->
      case (ent ^. etype) of
        Person ptags ->
          [whamlet|
            $forall (tag, tid) <- M.toList ptags
              <div class="ptag">
                <a href="@{PTagR tid}">#{ptToText tag}</a>
          |]
        Organization otags ->
          [whamlet|
            $forall (tag, tid) <- M.toList otags
              <div class="otag">
                <a href="@{OTagR tid}">#{otToText tag}</a>
          |]

getPTagR :: PTid -> Handler Html
getPTagR ptid =
  do ctx <- getContext $ Just $ PT ptid
     cr <- curRoute
     let gs = cgs ctx
     let mtag = gs ^. ptags . at ptid
     case mtag of
       Nothing -> err "tag not found"
       Just (tag, eid) ->
         case renderEntity gs eid of
           (w, Nothing) -> err "tag refers to non-existant entity"
           (w, Just (ent)) ->
             do comments <- renderComments gs (cmuser ctx) (PT ptid)
                layout
                  [whamlet|
                    ^{cgw ctx}
                    ^{renderId gs (E eid)} is tagged as: #{ptToText tag}
                    $if (hasPerm ctx DelTag)
                      <form method="post" action="@{DelPTagR ptid}">
                        <input type="submit" value="delete" />
                    <hr>
                    ^{renderVotes gs (cmuser ctx) (PT ptid) cr}
                    <hr>
                    comments:
                    ^{comments}
                  |]

deletePTagR :: PTid -> Handler Html
deletePTagR ptid =
  deleteThing DelTag "delete tag" (\user -> DelPTagU (user ^. uid) ptid) (PTagR ptid)

postDelPTagR = deletePTagR

getOTagR :: OTid -> Handler Html
getOTagR otid = 
  do ctx <- getContext $ Just $ OT otid
     cr <- curRoute
     let gs = cgs ctx
     let mtag = gs ^. otags . at otid
     case mtag of
       Nothing -> err "tag not found"
       Just (tag, eid) ->
         case renderEntity gs eid of
           (w, Nothing) -> err "tag refers to non-existant entity"
           (w, Just (ent)) ->
             do comments <- renderComments gs (cmuser ctx) (OT otid)
                layout
                  [whamlet|
                    ^{cgw ctx}
                    ^{renderId gs (E eid)} is tagged as: #{otToText tag}
                    $if (hasPerm ctx DelTag)
                      <form method="post" action="@{DelOTagR otid}">
                        <input type="submit" value="delete" />
                    <hr>
                    ^{renderVotes gs (cmuser ctx) (OT otid) cr}
                    <hr>
                    comments:
                    ^{comments}
                  |]

deleteOTagR :: OTid -> Handler Html
deleteOTagR otid =
  deleteThing DelTag "delete tag" (\user -> DelOTagU (user ^. uid) otid) (OTagR otid)

postDelOTagR = deleteOTagR

getUTagR :: UTid -> Handler Html
getUTagR utid = err "user tags not implemented"

ptToText :: PTag -> T.Text
ptToText pt =
  case toEnum (unPTag pt) of
    Politician ->           "politician"
    Official ->             "government official"
    Lobbyist ->             "lobbyist"
    Commentator ->          "commentator"
    Journalist ->           "journalist"
    Scientist ->            "scientist"
    Author ->               "author"
    Academic ->             "academic" 
    BusinessPerson ->       "businessperson"
    Celebrity ->            "celebrity"
    Artist ->               "artist"
    Musician ->             "musician"
    Doctor ->               "doctor"
    Lawyer ->               "lawyer"
    Engineer ->             "engineer"
    Teacher ->              "teacher"
    Entertainer ->          "entertainer"
    Athlete ->              "athlete"
    ReligiousLeader ->      "religious leader"
    Military ->             "member or veteran of the military"
    PublicServant ->        "public servant"
    CharityWorker ->        "charity worker"
    Patron ->               "patron"
    WhistleBlower ->        "whistle blower"
    Activist ->             "activist"
    Criminal ->             "criminal"
    Fictional ->            "fictional charater"
    Anonymous ->            "anonymous person"
    NotablePerson ->        "notable person"


otToText :: OTag -> T.Text
otToText ot =
  case toEnum (unOTag ot) of
    Nation ->               "nation"
    State ->                "state"
    Province ->             "province"
    Region ->               "region"
    City ->                 "city"
    Government ->           "government"
    NonProfit ->            "non-profit"
    PAC ->                  "political action committee (PAC)"
    NGO ->                  "non-governmental organization (NGO)"
    Club ->                 "club"
    Committee ->            "committee"
    Union ->                "union"
    ProfessionalOrg ->      "professional organization"
    SpecialInterestGroup -> "special interest group"
    School ->               "educational institution"
    Party ->                "political party"
    Business ->             "business"
    Media ->                "media organization"
    Religious ->            "religious institution"
    Collaboration ->        "collaboration"
    Constituency ->         "constituency"
    Office ->               "office"


newPersonTagForm =
  let sfl = map (\t -> let pt = PTag (fromEnum t) in (ptToText pt, pt))
                [Politician, Official, Lobbyist, Commentator, Journalist,
                 Scientist, Author, Academic, BusinessPerson, Celebrity,
                 Artist, Musician,  Doctor, Lawyer, Engineer, Teacher,
                 Entertainer, Athlete, ReligiousLeader, Military,
                 PublicServant, CharityWorker, Patron, WhistleBlower, Activist,
                 Criminal, Anonymous, Fictional, NotablePerson]
  in
    renderTable $
      Left <$> areq (selectFieldList sfl) "tag" Nothing

newOrgTagForm =
  let sfl = map (\t -> let ot = OTag (fromEnum t) in (otToText ot, ot))
                [Nation, State, Province, Region, City, Government, NonProfit,
                 PAC, NGO, Club, Committee, Union,  ProfessionalOrg,
                 SpecialInterestGroup, School, Party, Business, Media,
                 Religious, Collaboration, Constituency]
  in
    renderTable $
      Right <$> areq (selectFieldList sfl) "tag" Nothing

tagForm Nothing _ =
  return [whamlet|login to tag|]
tagForm _ Nothing =
  return [whamlet|no entity|]

tagForm (Just u) (Just entity) =
  do (widget, enctype) <- generateFormPost form
     return
       [whamlet|
         <form method=post action=@{NewTagR (_eid entity)} enctype=#{enctype}>
           ^{widget}
           <input type=submit value="tag">
       |]
  where
    form = case (entity ^. etype) of
             Person _ -> newPersonTagForm
             Organization _ -> newOrgTagForm
    

postNewTagR :: Eid -> Handler Html
postNewTagR eid =
  do w <- 
       reqAuth
         "tag"
         AddTag
         (\ctx user ->
           let gs = cgs ctx 
               entity = gs ^. entities ^. at eid
           in case entity of
               Nothing -> return $ errW "could not find entity"
               Just e ->
                 do ((result, widget), enctype) <-
                      case e ^. etype of
                        Person _ -> runFormPost newPersonTagForm
                        Organization _ -> runFormPost newOrgTagForm

                    case result of
                      FormSuccess (tag) ->
                        do ig <- getYesod
                           mtid <- liftIO $ update (acid ig) (AddTagU (user ^. uid) eid tag)
                           case mtid of
                             Left err -> return $ errW (T.pack err)
                             Right tid ->
                               redirect (EntityR eid) 
                      _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]

-- COMMENT MANAGEMENT

{-
-- We should be able to make this a top-level function, but I wasn't able to
-- satisfy the typechecker...
commentField :: T.Text -> Field (Handler Int) Textarea Textarea
commentField s = check lencheck textareaField
 where
   lencheck :: Textarea -> Either T.Text Textarea
   lencheck ta =
     let len = T.length (unTextarea ta)
     in if len < 1
        then Left $ s T.append " must be be at least one character long"
        else if len > 8192
             then Left $ s T.append " must be no more than 8192 characters"
             else Right ta
-}

newCommentForm parent =
  renderDivs $
   f <$> areq commentField "" Nothing
     <*> areq hiddenField "" parent
  where
   f :: Textarea -> Id -> (Textarea, Id)
   f = (,)

   commentField = check lencheck textareaField
     where
       lencheck :: Textarea -> Either T.Text Textarea
       lencheck ta =
         let len = T.length (unTextarea ta)
         in if len < 1
            then Left "comment must be be at least one character long"
            else if len > 8192
                 then Left "comment must be no more than 8192 characters"
                 else Right ta


renderComment :: GraphState -> Maybe User -> Cid -> GW
renderComment gs muser cid@(Cid id) =
  let mcomment = gs ^. comments . at cid
  in case mcomment of
       Nothing -> errW "comment not found"
       Just comment -> 
         [whamlet|
           <p><b>^{renderUid gs (_author comment)}</b>
              \ <a href="@{CommentR cid}">permalink</a>
           $maybe content <- (_contents comment)
             <div class="comment-content" id=#{show id}>
               #{markdown def $ LT.fromStrict content}
           $nothing
             <p>comment deleted
         |]

renderComments :: GraphState -> Maybe User -> Id -> Handler (GW)
renderComments gs muser parent =
  let mchildren = fmap reverse $ gs ^. cchildren . at parent
  in case mchildren of
       Nothing -> return $ errW $ "comments not properly initialized for this entity"
       Just cids ->
         do children <- mapM (\cid -> do let child = renderComment gs muser cid
                                         gchildren <- renderComments gs muser (C cid)
                                         return (child, gchildren)) cids
            (fwidget, enctype) <- generateFormPost $ newCommentForm $ Just parent
            formid <- newIdent
            buttonid <- newIdent
            return $
              do toWidget [julius| |]
                 [whamlet|
                   $maybe u <- muser
                     <div id="#{formid}" style="display: none">
                       <form method=post action="@{NewCommentR}" enctype=#{enctype}>
                         ^{fwidget}
                         <input type=submit value="add comment">
                     <div id="#{buttonid}" style="display: block">
                       <input type=submit value="reply" onclick="showhideid('#{formid}', '#{buttonid}')">


                   <table class=commentthread>
                     <tr>
                       <td colspan=1>
                         <div style="visibility:hidden">
                           tab
                       <td>
                         $forall (child, gchildren) <- children
                           ^{child}
                           ^{gchildren}
                 |]

getCommentR :: Cid -> Handler Html
getCommentR cid =
  do ctx <- getContext $ Just $ C cid
     cr <- curRoute
     let gs = cgs ctx
     let muser = cmuser ctx
     let mcomment = gs ^. comments . at cid
     case mcomment of
       Nothing -> err "comment not found"
       Just (comment) ->
         do children <- renderComments gs muser (C cid)
            layout
              [whamlet|
                ^{cgw ctx}
                <p><b>parent:</b> ^{renderId gs (_parent comment)}
                <hr>
                ^{renderComment gs muser cid}
                $if (hasPerm ctx DelComment)
                  <form method="post" action="@{DelCommentR cid}">
                    <input type="submit" value="delete" />
                  <form method="post" action="@{RemCommentR cid}">
                    <input type="submit" value="remove" />
                <hr>
                ^{renderVotes gs muser (C cid) cr}
                <hr>
                comments:
                ^{children}
              |]

deleteCommentR :: Cid -> Handler Html
deleteCommentR cid =
  deleteThing DelComment "delete comment" (\user -> DelCommentU (user ^. uid) cid) (CommentR cid)

postDelCommentR = deleteCommentR

postRemCommentR cid =
  deleteThing DelComment "remove comment" (\user -> RemoveCommentU (user ^. uid) cid) (CommentR cid)

getCommentCtxR :: Cid -> Handler Html
getCommentCtxR cid =
  do ctx <- getContext $ Just $ C cid
     let gs = cgs ctx
     let mparent = getParent gs cid
     case mparent of
       Nothing -> err "comment context not found"
       Just parent -> redirectId parent

isJust Nothing = False
isJust (Just _) = True

-- Is this a valid ID?
validateId :: GraphState -> Id -> Bool
validateId gs id =
  case id of
    U uid   -> isJust $ gs ^. users     . at uid
    E eid   -> isJust $ gs ^. entities  . at eid
    L lid   -> isJust $ gs ^. links     . at lid
    C cid   -> isJust $ gs ^. comments  . at cid
    PT ptid -> isJust $ gs ^. ptags     . at ptid
    OT otid -> isJust $ gs ^. otags     . at otid
    

postNewCommentR :: Handler Html
postNewCommentR =
  do w <- 
       reqAuth
         "comment"
         AddComment
         (\ctx user ->
             do let gs = cgs ctx 
                ((result, widget), enctype) <- runFormPost $ newCommentForm Nothing
                case result of
                  FormSuccess (contents, parent) ->
                    do ig <- getYesod
                       mcid <- liftIO $ update (acid ig) (AddCommentU (user ^. uid) (unTextarea contents) parent)
                       case mcid of
                         Left err -> return $ errW (T.pack err)
                         Right cid ->
                           redirect (CommentCtxR cid) 
                  _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]


-- ISSUE MANAGEMENT

getIssuesR :: Handler Html
getIssuesR =
  do ctx <- getContext Nothing
     (pw, is) <- paginateL (M.keys $ (cgs ctx) ^. issues)
     layout $
       [whamlet|
         ^{cgw ctx}
         <center>
           <h1>
             All Issues

         <hr>
           <ul>
             $forall i <- is
               <li>
                 ^{renderId (cgs ctx) (I i)}
                 $if (hasPerm ctx DelIssue)
                   <form method="post" action="@{DelIssueR i}">
                     <input type="submit" value="delete issue" />
         $if (hasPerm ctx AddIssue)
           <p><a href="@{NewIssueR}">Add an issue</a>
       |]


getIssueCanonR :: Iid -> T.Text -> Handler Html
getIssueCanonR iid _ =
  do ctx <- getContext (Just $ I iid)
     let gs = cgs ctx
     case gs ^. issues ^. at iid of
       Nothing -> err "deleted issue"
       Just issue ->
         do (pw, ids) <- paginateL (S.toList $ issue ^. itagged)
            layout $
              do setTitle $ text $ issue ^. iname
                 [whamlet|
                   ^{cgw ctx}
                   <center>
                     <h1>#{_iname issue}
                     $maybe desc <- _idesc issue
                       <h2>#{desc}
                     <p>
                       $maybe wp <- _iwp issue
                         <a href="http://en.wikipedia.org/wiki/#{wp}">wikipedia</a>

                   $if (hasPerm ctx EditIssue)
                     <form method="get" action="@{EditIssueR iid}">
                       <input type="submit" value="edit" />

                   <hr>
                     <center>
                       <object data=@{IssueSvgR iid} class="issuegraph" type="image/svg+xml"></object>

                   <hr>
                   <ul>
                     ^{pw}
                     $forall id <- ids
                       <li>^{renderId gs id}
                       $if (hasPerm ctx DelIssueTag)
                         $case id
                           $of L lid
                             <form method="post" action="@{DelIssueTagR lid iid}">
                               <input type="submit" value="remove from issue" />

                     ^{pw}
                 |]

-- We like to have the entity name appear in the url rather than just a number,
-- so we do an automatic redirect if we use the simplified url that doesn't
-- have the name.  (The name passed to the final handler gets ignored -- it's
-- just there for looks.)
getIssueR :: Iid -> Handler Html
getIssueR iid = 
  do ig <- getYesod
     gs' <- liftIO $ query (acid ig) GetStateQ
     let gs = fromEither gs'
     case gs ^. issues . at iid of
       Nothing -> err "no such issue"
       Just i -> redirect (IssueCanonR iid (urlifier $ i ^. iname))

newIssueForm missue =
  renderDivs $
    (,,)
      <$> areq bTextField "issue name"                    (fmap _iname missue)
      <*> aopt bTextField "issue description"             (fmap _idesc missue)
      <*> aopt bTextField "http://en.wikipedia.org/wiki/" (fmap (fmap unUrl . _iwp) missue)

getNewIssueR :: Handler Html
getNewIssueR =
  do w <-
       reqAuth
         "add an issue"
         AddIssue
         (\ctx user ->
           do (widget, enctype) <- generateFormPost (newIssueForm Nothing)
              return
                [whamlet|
                   <h1>Add new issue:
                   <form method=post action=@{NewIssueR} enctype=#{enctype}>
                     ^{widget}
                     <input type=submit value="add issue">
                |])
     layout [whamlet|^{w}|]

postNewIssueR :: Handler Html
postNewIssueR =
  do w <- 
       reqAuth
         "add issue"
         AddIssue
         (\ctx user ->
             do let gs = cgs ctx 
                ((result, widget), enctype) <- runFormPost $ newIssueForm Nothing
                case result of
                  FormSuccess (name, desc, wp) ->
                    do ig <- getYesod
                       miid <- liftIO $ update (acid ig) (AddIssueU (user ^. uid) (Issue (Iid (-1))name desc (fmap Url wp) S.empty))
                       case miid of
                         Left err -> return $ errW (T.pack err)
                         Right iid ->
                           redirect (IssueR iid) 
                  _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]

getEditIssueR :: Iid -> Handler Html
getEditIssueR iid =
  do w <-
       reqAuth
         "edit an issue"
         EditIssue
         (\ctx user ->
           case (cgs ctx) ^. issues ^. at iid of
             Nothing -> return $ errW "issue not found"
             Just i ->
               do (widget, enctype) <- generateFormPost (newIssueForm $ Just i)
                  return
                    [whamlet|
                       <h1>Edit issue:
                       <form method=post action=@{EditIssueR iid} enctype=#{enctype}>
                         ^{widget}
                         <input type=submit value="update issue">
                    |])
     layout [whamlet|^{w}|]

postEditIssueR :: Iid -> Handler Html
postEditIssueR iid =
  do w <- 
       reqAuth
         "edit issue"
         EditIssue
         (\ctx user ->
             do let gs = cgs ctx 
                ((result, widget), enctype) <- runFormPost $ newIssueForm Nothing
                case result of
                  FormSuccess (name, desc, wp) ->
                    do ig <- getYesod
                       miid <- liftIO $ update (acid ig) (EditIssueU (user ^. uid) (Issue iid name desc (fmap Url wp) S.empty))
                       case miid of
                         Left err -> return $ errW (T.pack err)
                         Right iid ->
                           redirect (IssueR iid) 
                  _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]



issueTagForm gs =
  let is = M.elems (gs ^. issues)
      sfl = map (\i -> (i ^. iname, i ^. iid)) is
  in
    renderTable $ areq (selectFieldList sfl) "" Nothing

msflatten :: Maybe (S.Set a) -> [a]
msflatten Nothing = []
msflatten (Just s) = S.toList s

renderIssueTagForm :: GraphState -> Lid -> Handler GW
renderIssueTagForm gs lid =
  do (widget, enctype) <- generateFormPost $ issueTagForm gs
     let issues = msflatten $ gs ^. issuetagged ^. at (L lid)
     return
       [whamlet|
         issues:
         $forall i <- issues
           \ ^{renderIidTerse gs i}

         <form method=post action=@{NewIssueTagR lid} enctype=#{enctype}>
           ^{widget}
           <input type=submit value="tag issue">
       |]

postNewIssueTagR :: Lid -> Handler Html
postNewIssueTagR lid =
  do w <- 
       reqAuth
         "tag issue"
         AddIssueTag
         (\ctx user ->
             do let gs = cgs ctx 
                ((result, widget), enctype) <- runFormPost $ issueTagForm gs
                case result of
                  FormSuccess iid ->
                    do ig <- getYesod
                       miid <- liftIO $ update (acid ig) (AddIssueTagU (user ^. uid) (L lid) iid)
                       case miid of
                         Left err -> return $ errW (T.pack err)
                         Right iid ->
                           redirect (LinkR lid) 
                  _ -> return $ errW "invalid input")

     layout [whamlet|^{w}|]



postDelIssueR :: Iid -> Handler Html
postDelIssueR iid =
  deleteThing DelIssue "delete issue" (\user -> DelIssueU (user ^. uid) iid) (IssuesR)

postDelIssueTagR :: Lid -> Iid -> Handler Html
postDelIssueTagR lid iid =
  deleteThing DelIssueTag "delete issue tag" (\user -> DelIssueTagU (user ^. uid) (L lid) iid) (IssueR iid)


-- MISC HANDLERS

getWikiR :: Txt -> Handler Html
getWikiR wiki =
  do ctx <- getContext Nothing
     let gs = cgs ctx
     let mes = (gs ^. entitiesByWiki ^. at (Url wiki))
     let miss = [whamlet|
                  ^{cgw ctx}
                  <p>We don't have a matching entry for that wikipedia page.
                     Why not add one?
                     Add <a href="@{NewPersonR}">person</a>,
                     add <a href="@{NewOrgR}">organization</a>.
                |]
     let multi es = [whamlet|
                      ^{cgw ctx}
                      <p>We have multiple entities that reference that wikipedia page.
                      $forall e <- es
                        <p>^{renderId gs (E e)}
                    |] 
     case mes of
       Nothing -> layout miss
       Just es ->
         let count = S.size es
         in case count of
              0 -> layout miss
              1 -> redirect (EntityR (S.findMin es))
              _ -> layout (multi es)

getRandomR' :: Int -> Handler Html
getRandomR' 0 = error "couldn't find a suitable entitiy"
getRandomR' n =
  do ctx <- getContext Nothing
     let gs = cgs ctx
     let max = gs ^. nextId
     rnum <- liftIO $ getStdRandom (randomR (1, max))
     case gs ^. entities . at (Eid rnum) of
       Nothing -> getRandomR' (n-1)
       Just e -> redirect (EntityR (e ^. eid))

getRandomR = getRandomR' 100

-- We'll include anything that overlaps the current date by a couple of days.
dayrange :: Day -> Maybe (Day, Day)
dayrange day =
  Just (addDays (-2) day, addDays 2 day)

-- Annoying detail: the instance of fromPathPiece for Int uses "decimal"
-- rather than "signed" from Data.Text.Read, thus we cannot parse negative
-- numbers.  ModifiedJulianDay starts from 1858, which isn't as far back
-- as I'd like.
intToDay :: Int -> Day
intToDay i = ModifiedJulianDay $ fromIntegral i

getOrgChartDotR :: Eid -> Handler RepPlain
getOrgChartDotR eid =
  do ctx <- getContext (Just $ E eid)
     utc <- liftIO $ getCurrentTime
     let now = utctDay utc
     return $ RepPlain $ toContent $ orgchartgv (cgs ctx) eid (dayrange now)

getOrgChartDotDateR :: Eid -> Int -> Handler RepPlain
getOrgChartDotDateR eid date =
  do ctx <- getContext (Just $ E eid)
     return $ RepPlain $ toContent $ orgchartgv (cgs ctx) eid (dayrange $ intToDay date)


getIssueDotR :: Iid -> Handler RepPlain
getIssueDotR iid =
  do ctx <- getContext (Just $ I iid)
     return $ RepPlain $ toContent $ issuegv (cgs ctx) iid


-- Write dot output to a file, run graphviz on that file to generate svg,
-- then send the result back to the client.  The lock keeps concurrent threads
-- from clobbering the file.
-- Todo: check if the svg file is recent; if so, don't render it again.
-- Todo: We should hold the lock while doing the sendFile, but unfortunately
--       that seems to short-circuit the rest of the handler, so we can't drop
--       the lock later...  Not sure what the right solution is here.
renderSvg :: MVar () -> FilePath -> String -> Handler a
renderSvg lock path dot =
  let dotfile = path ++ ".dot"
      svgfile = path ++ ".svg"
  in
    do l <- liftIO $ takeMVar lock
       liftIO $ writeFile dotfile dot
       _ <- liftIO $ system $ "dot -Tsvg " ++ dotfile ++ " >" ++ svgfile
       liftIO $ putMVar lock l
       sendFile "image/svg+xml" svgfile
       

-- Get svg rendering of the graph of links associated with an issue.
getIssueSvgR :: Iid -> Handler RepPlain
getIssueSvgR iid@(Iid id) =
  do ctx <- getContext (Just $ I iid)
     let dot = issuegv (cgs ctx) iid
     renderSvg (cfslock ctx) ("./nginx/static/issues/" ++ (show id)) dot

-- Get svg rendering of an organization's org chart.
getOrgChartSvgR :: Eid -> Handler Html
getOrgChartSvgR eid@(Eid id) =
  do ctx <- getContext (Just $ E eid)
     utc <- liftIO $ getCurrentTime
     let now = utctDay utc
     let dot = orgchartgv (cgs ctx) eid (dayrange now)
     renderSvg (cfslock ctx) ("./nginx/static/orgcharts/org-" ++ (show id)) dot

-- Same as above, but specific to a date.
getOrgChartSvgDateR :: Eid -> Int -> Handler Html
getOrgChartSvgDateR eid@(Eid id) date =
  do ctx <- getContext (Just $ E eid)
     let dot = orgchartgv (cgs ctx) eid (dayrange $ intToDay date)
     renderSvg (cfslock ctx) ("./nginx/static/orgcharts/org-" ++ (show id)) dot

getOrgChartR :: Eid -> Handler Html
getOrgChartR eid =
  do ctx <- getContext $ Just $ E eid
     layout $
       [whamlet|
         ^{cgw ctx}
         <center>
           <object data=@{OrgChartSvgR eid} class="orgchart" type="image/svg+xml"></object>
       |]

getOrgChartDateR :: Eid -> Int -> Handler Html
getOrgChartDateR eid date =
  do ctx <- getContext $ Just $ E eid
     layout $
       [whamlet|
         ^{cgw ctx}
         <center>
           <object data=@{OrgChartSvgDateR eid date} type="image/svg+xml"></object>
       |] 

-- Dump the entire graph of links in a simplified format understood by Pariah.
-- getRSStateR :: Handler TypedContent
getRSStateR =
  do ctx <- getContext Nothing
     jsonify $ toJSON (cgs ctx)

-- MAIN

main :: IO ()
main =
  bracket
    (openLocalState initialGraphState)
    (createCheckpointAndClose)
    (\acid ->
       do mgr <- newManager def
          lock <- newMVar ()
          warp 3001 $ InfluenceGraph mgr acid lock)
