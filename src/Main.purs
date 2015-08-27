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

import qualified Thermite.Action as T
import qualified Thermite as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Elements as T
import qualified Thermite.Types as T


-- state types

data DB = DB { counter :: Number, user :: String }

instance dbShow :: Show DB where
  show (DB o) = "[" ++ show o.counter ++ "][" ++ show o.user ++ "]"

instance dbFromJSON :: FromJSON DB where
  parseJSON (JObject o) = do
    i <- o .: "_counter"
    n <- o .: "_user"
    return $ DB {counter: i, user: n}
  parseJSON _ = fail "DB parse failed."

instance dbToJSON :: ToJSON DB where
  toJSON (DB { counter: i, user: n }) = object ["_counter" .= i, "_user" .= n]

initialState :: DB
initialState = DB { counter: 0.0, user: "" }


-- action types

data DBAction = Increment | Decrement | NewUserName String

data TextAreaEvent = TextAreaEvent String

instance textAreaEventIsForeign :: IsForeign TextAreaEvent where
  read value = TextAreaEvent <$> (readProp "target" value >>= readProp "value")

newUserName :: T.FormEvent -> DBAction
newUserName e = case read $ toForeign e of
  Right (TextAreaEvent n) -> NewUserName n


-- render function

render :: T.Render _ DB _ DBAction
render ctx s _ _ = T.div' [counter, username, buttons]
  where
  counter :: T.Html _
  counter =
    T.p'
      [ T.text "Value: "
      , T.text $ show s
      ]

  username :: T.Html _
  username =
    T.textarea (T.onChange ctx newUserName) []

  buttons :: T.Html _
  buttons =
    T.p'
      [ T.button (T.onClick ctx (\_ -> Increment))
                 [ T.text "Increment" ]
      , T.button (T.onClick ctx (\_ -> Decrement))
                 [ T.text "Decrement" ]
      ]


-- event handler function

performAction :: T.PerformAction _ DB _ DBAction
performAction _ dbAction = do
  let upd (DB o) = case dbAction of
        Increment     -> DB o { counter = o.counter + 1.0 }
        Decrement     -> DB o { counter = o.counter - 1.0 }
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
