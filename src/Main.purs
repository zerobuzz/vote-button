module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class
import Prelude

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Action = Increment | Decrement

type DB = { counter :: Number }

initialState :: DB
initialState = { counter: 0.0 }

render :: T.Render _ DB _ Action
render ctx s _ _ = T.div' [counter, buttons]
  where
  counter :: T.Html _
  counter =
    T.p'
      [ T.text "Value: "
      , T.text $ show s.counter
      ]

  buttons :: T.Html _
  buttons =
    T.p'
      [ T.button (T.onClick ctx (\_ -> Increment))
                 [ T.text "Increment" ]
      , T.button (T.onClick ctx (\_ -> Decrement))
                 [ T.text "Decrement" ]
      ]

performAction :: T.PerformAction _ DB _ Action
performAction _ Increment = T.modifyState \o -> { counter: o.counter + 1.0 }
performAction _ Decrement = T.modifyState \o -> { counter: o.counter - 1.0 }

spec :: DB -> T.Spec _ DB _ Action
spec istate = T.simpleSpec istate performAction render

main = do
  log "Hello sailor!"
  istate <- getState
  let component = T.createClass $ spec istate
  T.render component {}


getState :: forall eff . Eff eff DB
getState = return initialState
