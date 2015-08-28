module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException)
import Data.Either
import Data.Foreign.Class
import Data.Foreign hiding (parseJSON)
import Data.JSON
import Data.Maybe
import Data.Tuple
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader
import Prelude

import qualified Data.List as L
import qualified Data.Map as M
import qualified Thermite.Action as T
import qualified Thermite as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Html.Elements as T
import qualified Thermite.Types as T


-- state types

data DB = DB { colors :: Array String, user :: String, userChanges :: String }

instance showDB :: Show DB where
  show (DB o) = "[" ++ show o.colors ++ "][" ++ show o.user ++ "][" ++ show o.userChanges ++ "]"

instance fromJSONDB :: FromJSON DB where
  parseJSON (JObject o) = do
    i  <- o .: "_colors"
    n  <- o .: "_user"
    n' <- o .: "_userChanges"
    return $ DB { colors: i, user: n, userChanges: n' }
  parseJSON _ = fail "DB parse failed."

instance toJSONDB :: ToJSON DB where
  toJSON (DB { colors: m, user: n, userChanges: n' }) = object ["_colors" .= toJSON m, "_user" .= n, "_userChanges" .= n']


initialState :: DB
initialState = DB { colors: [], user: "", userChanges: "" }


-- action types

data DBAction = UserNameKeyPress Int | UserChanges String

instance showDBAction :: Show DBAction where
  show (UserNameKeyPress k) = "UserNameKeyPress " ++ show k
  show (UserChanges n')     = "UserChanges " ++ show n'

foreign import getValue :: forall event. event -> String
foreign import getKeyCode :: T.KeyboardEvent -> Int


-- render function

render :: T.Render _ DB _ DBAction
render ctx db@(DB s) _ _ = T.div' [outcome]
  where
  outcome :: T.Html _
  outcome = T.div'
    [ T.pre' [ T.text ("raw state: " ++ encode db)  ]
    , T.table'
      [ T.tr' [ T.td' [ T.text "current user:" ]
              , T.td' [ T.text (s.user)
                      , T.br' []
                      , T.text "change: "
                      , T.input (A.className "form-control"
                              <> A.placeholder s.user
                              <> A.value s.userChanges
                              <> T.onKeyUp ctx (UserNameKeyPress <<< getKeyCode)
                              <> T.onChange ctx (UserChanges <<< getValue)
                                )
                                []
                      ]
              ]
      , T.tr' [ T.td' [ T.text "favorite color candidates:" ]
              , T.td' [ T.table'
                  let h = T.tr'
                        [ T.th' [ T.text "color" ]
                        , T.th' [ T.text "vote" ]
                        , T.th' [ T.text "score" ]
                        ]
                      f c = T.tr'
                        [ T.td' [ T.text c ]
                        , T.td' [ ]
                        , T.td' [ ]
                        ]
                  in [ h ] ++ (f <$> s.colors)
                ]

                -- (See FIXME, same place, in vote-button core package)

              ]
      ]
    ]


-- event handler

performAction :: T.PerformAction _ DB _ DBAction
performAction _ ev = do
  T.sync $ log (show ev)
  case ev of
    UserNameKeyPress k -> do
      case k of
        13 -> do
          T.modifyState $ \(DB o) -> DB (o { user = o.userChanges })
          T.getState >>= \(DB o) -> T.sync $ runAff throwException return (changeUser o.user)
        27 -> do
          T.modifyState $ \(DB o) -> DB (o { userChanges = o.user })
        _  -> return unit
    UserChanges n' -> do
      T.modifyState $ \(DB o) -> DB (o { userChanges = n' })


-- backend communication

defaultRequest' :: AffjaxRequest Unit
defaultRequest' = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

getColors :: forall ajax eff . Aff (ajax :: AJAX | eff) (Array String)
getColors = do
  res <- affjax defaultRequest' { url = "/colors" }
  return case eitherDecode res.response of
    Left e  -> []  -- (See FIXME, same place, in vote-button core package)
    Right v -> v

getInitialState :: forall ajax eff . Aff (ajax :: AJAX | eff) DB
getInitialState = do
  colors <- getColors
  return (case initialState of \(DB o) -> DB (o { colors = colors }))

changeUser :: forall ajax eff . String -> Aff (ajax :: AJAX | eff) Unit
changeUser uname = do
  res <- affjax defaultRequest' { url = "/user/" ++ uname, method = PUT }
  liftEff <<< log $ "changeUser: " ++ res.response  -- without this, type error!
  return unit


-- main

spec :: DB -> T.Spec _ DB _ DBAction
spec istate = T.simpleSpec istate performAction render

main = do
  runAff throwException
    (\ istate -> do
        let component = T.createClass $ spec istate
        T.render component {})
    getInitialState
