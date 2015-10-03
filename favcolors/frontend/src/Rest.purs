module Rest where

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

import Types


defaultRequest' :: AffjaxRequest Unit
defaultRequest' = defaultRequest { headers = [ContentType applicationJSON, Accept applicationJSON] }

getColors :: forall ajax eff . Aff (ajax :: AJAX | eff) (Array String)
getColors = do
  res <- affjax defaultRequest' { url = "/colors" }
  return case eitherDecode res.response of
    Left e  -> []  -- (See FIXME, same place, in vote-button core package)
    Right v -> v

updateColor :: forall ajax eff . Method -> String -> Aff (ajax :: AJAX | eff) (Array String)
updateColor meth colr = do
  res <- affjax defaultRequest' { url = "/colors/" ++ colr, method = meth }
  liftEff <<< log $ "updateColor: " ++ res.response  -- without this, type error!
  return case eitherDecode res.response of
    Left e -> []  -- (see above)
    Right v -> v

getInitialState :: forall ajax eff . Aff (ajax :: AJAX | eff) DB
getInitialState = (\ clrs -> case initialState of (DB o) -> DB (o { colors = clrs })) <$> getColors

changeUser :: forall ajax eff . String -> Aff (ajax :: AJAX | eff) Unit
changeUser uname = do
  res <- affjax defaultRequest' { url = "/user/" ++ uname, method = PUT }
  liftEff <<< log $ "changeUser: " ++ res.response  -- without this, type error!
  return unit
