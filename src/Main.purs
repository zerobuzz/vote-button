module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException)
import Data.Either
import Data.Foreign hiding (parseJSON)
import Data.Foreign.Class
import Data.JSON
import Data.Maybe
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Prelude

import qualified Data.Map as M
import qualified Thermite.Action as T
import qualified Thermite as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Elements as T
import qualified Thermite.Types as T


-- next: implement delegation.  it's all very simple.
-- next: visualize delegation graph (d3).


-- state types

data DB = DB { votes :: M.Map String Vote, user :: String }

instance showDB :: Show DB where
  show (DB o) = "[" ++ show o.votes ++ "][" ++ show o.user ++ "]"

instance fromJSONDB :: FromJSON DB where
  parseJSON (JObject o) = do
    i <- o .: "_votes"
    n <- o .: "_user"
    return $ DB { votes: i, user: n }
  parseJSON _ = fail "DB parse failed."

instance toJSONDB :: ToJSON DB where
  toJSON (DB { votes: m, user: n }) = object ["_votes" .= toJSON m, "_user" .= n]


data Vote = Yay | Nay | Abstain

instance showVote :: Show Vote where
  show Yay = "Yay"
  show Nay = "Nay"
  show Abstain = "Abstain"

instance fromJSONVote :: FromJSON Vote where
  parseJSON (JString "Yay") = return Yay
  parseJSON (JString "Nay") = return Nay
  parseJSON (JString "Abstain") = return Abstain

instance toJSONVote :: ToJSON Vote where
  toJSON vote = toJSON $ show vote


initialState :: DB
initialState = DB { votes: M.empty, user: "" }


-- action types

data DBAction = DropVote | ChangeVote Vote | NewUserName String

data TextAreaEvent = TextAreaEvent String

-- FUTURE WORK:
-- http://pursuit.purescript.org/packages/purescript-dom/0.2.6/docs/DOM.Event.Event#v:target
-- Thu Aug 27 16:30:11 PDT 2015 -- 16:27 < garyb> `value` needs more things we don't have unfortunately :( as it only belongs to certain elements, so we'd needed to have implemented a `DOM.HTML.HTMLInputElement` interface, for  instance

instance textAreaEventIsForeign :: IsForeign TextAreaEvent where
  read value = TextAreaEvent <$> (readProp "target" value >>= readProp "value")

newUserName :: T.FormEvent -> DBAction
newUserName e = case read $ toForeign e of
  Right (TextAreaEvent n) -> NewUserName n


-- render function

render :: T.Render _ DB _ DBAction
render ctx s _ _ = T.div' [outcome, username, buttons]
  where
  outcome :: T.Html _
  outcome =
    T.p'
      [ T.text "Value: "
      , T.text $ show s
      ]

  username :: T.Html _
  username =
    T.textarea (T.onChange ctx newUserName) []

  buttons :: T.Html _
  buttons =
    let f vote = T.button (T.onClick ctx (\_ -> ChangeVote vote)) [ T.text (show vote) ]
        g      = T.button (T.onClick ctx (\_ -> DropVote))        [ T.text "withdraw" ] in
    T.p' $ (f <$> [Yay, Nay, Abstain]) ++ [g]


-- event handler function

performAction :: T.PerformAction _ DB _ DBAction
performAction _ dbAction = do
  let upd (DB o) = case dbAction of
        ChangeVote v  -> DB o { votes = M.insert o.user v o.votes }
        DropVote      -> DB o { votes = M.delete o.user o.votes }
        NewUserName n -> DB o { user = n }

  newState <- upd <$> T.getState
  T.setState newState
  T.async $ \cb -> runAff throwException return (putState newState) >>= cb


-- backend communication

getState :: forall ajax eff . Aff (ajax :: AJAX | eff) DB
getState = do
  res <- get "/_get"
  case eitherDecode res.response of
    Left e  -> return initialState  -- FIXME
    Right v -> return v

putState :: forall ajax eff . DB -> Aff (ajax :: AJAX | eff) Unit
putState state = do
  res <- affjax defaultRequest
    { url = "/_put"
    , method = PUT
    , headers = [ContentType applicationJSON]
    , content = Just (encode state)
    }

  liftEff $ log $ "PUT /_put response: " ++ res.response
  return unit


-- main

spec :: DB -> T.Spec _ DB _ DBAction
spec istate = T.simpleSpec istate performAction render

main = do
  log "Hello sailor!"
  runAff throwException
    (\ istate -> do
        let component = T.createClass $ spec istate
        T.render component {})
    getState
