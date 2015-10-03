module Main where

import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception  -- (EXCEPTION, throwException)
import Control.Monad.Free (Free())
import Data.Array (zipWith, range, length)
import Data.Either
import Data.Foreign.Class
import Data.Int (round)
import Data.Foreign hiding (parseJSON)
import Data.JSON (encode)
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
import qualified Halogen as H
import qualified Halogen.HTML.Events.Indexed as HE
import qualified Halogen.HTML.Events.Types as HE
import qualified Halogen.HTML.Indexed as HH
import qualified Halogen.HTML.Properties.Indexed as HP
import qualified Halogen.Query as H
import qualified Halogen.Util as HU

import Types

import qualified Rest as Rest


-- render function

foreign import getValue :: forall event. event -> String

render :: forall p. H.Render DB DBAction p
render db@(DB state) = HH.div_
    [ HH.pre_ [ HH.text ("raw state: " ++ encode db)  ]
    , HH.table_
      [ HH.tr_ [ HH.td_ [ HH.text "current user:" ]
               , HH.td_ [ HH.text ("[" ++ state.user ++ "]")
                        , HH.br_
                        , HH.text "change: "
                        , HH.input
                            [ HP.class_ (HH.className "form-control")
                            , HP.placeholder state.user
                            , HP.value state.userChanges
                            , HE.onKeyUp (HE.input UserNameKeyPress <<< \e -> round e.keyCode)
                            , HE.onChange (HE.input UserChanges <<< \e -> getValue e.target)
                            ]
                        ]
               ]
      , HH.tr_ [ HH.td_ [ HH.text "add new color:" ]
               , HH.td_ [ HH.input
                            [ HP.class_ (HH.className "form-control")
                            , HP.placeholder state.newColor
                            , HP.value state.newColor
                            , HE.onKeyUp (HE.input NewColorKeyPress <<< \e -> round e.keyCode)
                            , HE.onChange (HE.input NewColorChanges <<< \e -> getValue e.target)
                            ]
                       ]
               ]
      , HH.tr_ [ HH.td_ [ HH.text "favorite color candidates:" ]
               , HH.td_ [ HH.table_
                   let h :: HH.HTML p (DBAction Unit)
                       h = HH.tr_
                         [ HH.th_ [ HH.text "color" ]
                         , HH.th_ [ HH.text "vote" ]
                         , HH.th_ [ HH.text "score" ]
                         , HH.th_ [ HH.text "" ]
                         ]
                       f :: Int -> String -> HH.HTML p (DBAction Unit)
                       f i c = HH.tr []  -- FIXME: should be `[ HP.key (show i) ]`
                         [ HH.td_ [ HH.text c ]
                         , HH.td_ [ ]
                         , HH.td_ [ ]
                         , HH.td_ [ HH.a [ HP.title "remove"
                                         , HE.onClick (HE.input DropColor <<< \_ -> c)
                                         ]
                                       [ HH.text "[X]" ] ]
                         ]
                   in [h] ++ zipWith f (range 0 (length state.colors - 1)) state.colors
                 ]
               ]
       ]
    ]


-- event handler

eval :: forall g ajax err console eff.
    ( MonadEff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | eff) g
    , MonadAff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | eff) g
    )
    => H.Eval DBAction DB DBAction g
eval ev = do
  H.liftEff' $ log (show ev)
  case ev of
    UserNameKeyPress k cont -> do
      case k of
        13 -> do
          resetUser
          H.get >>= \(DB o) -> H.liftEff' $ runAff throwException return (Rest.changeUser o.user)
        27 -> do
          resetUserChanges
        _  -> return unit
      pure cont

    UserChanges n' cont -> do
      updateUserChanges n'
      pure cont

    NewColorKeyPress k cont -> do
      case k of
        13 -> do
          newColor <- (\(DB o) -> o.newColor) <$> H.get
          newColors <- H.liftAff' $ Rest.updateColor POST newColor
          updateColors newColors
          H.modify (\(DB o) -> DB (o { newColor = "" }))
        27 -> do
          H.modify $ \(DB o) -> DB (o { newColor = "" })
        _  -> return unit
      pure cont

    NewColorChanges c cont -> do
      updateNewColor c
      pure cont

    DropColor c cont -> do
      newColors <- H.liftAff' $ Rest.updateColor DELETE c
      updateColors newColors
      pure cont

resetUser           :: forall f g.                 Free (H.HalogenF DB f g) Unit
resetUser            = H.modify $ \(DB o) -> DB (o { user = o.userChanges })

resetUserChanges    :: forall f g.                 Free (H.HalogenF DB f g) Unit
resetUserChanges     = H.modify $ \(DB o) -> DB (o { userChanges = o.user })

updateUserChanges   :: forall f g. String       -> Free (H.HalogenF DB f g) Unit
updateUserChanges n' = H.modify $ \(DB o) -> DB (o { userChanges = n' })

updateNewColor      :: forall f g. String       -> Free (H.HalogenF DB f g) Unit
updateNewColor c     = H.modify $ \(DB o) -> DB (o { newColor = c })

updateColors        :: forall f g. Array String -> Free (H.HalogenF DB f g) Unit
updateColors cs      = H.modify $ \(DB o) -> DB (o { colors = cs })


-- main

ui :: forall g p ajax err console eff.
    ( MonadEff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | eff) g
    , MonadAff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | eff) g
    , Functor g
    )
    => H.Component DB DBAction g p
ui = H.component render eval

type AppEffects eff = H.HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    | eff )

main :: Eff (AppEffects ()) Unit
main = do
  log "booting favcolors..."
  runAff throwException (const (pure unit)) $ do
    app <- H.runUI ui initialState
    HU.appendToBody app.node
    liftEff $ log "done!"
