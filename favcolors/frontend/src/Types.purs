module Types where

{-
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
import Data.Maybe
import Data.Tuple
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import qualified Data.List as L
import qualified Data.Map as M
-}

import Data.JSON
import Prelude


-- state types

data DB = DB
  { colors :: Array String
  , user :: String
  , userChanges :: String
  , newColor :: String
  }

instance showDB :: Show DB where
  show (DB o) = "[" ++ show o.colors ++ "][" ++ show o.user ++ "][" ++ show o.userChanges ++ "][" ++ show o.newColor ++ "]"

instance fromJSONDB :: FromJSON DB where
  parseJSON (JObject o) = do
    i  <- o .: "colors"
    n  <- o .: "user"
    n' <- o .: "userChanges"
    c  <- o .: "newColor"
    return $ DB { colors: i, user: n, userChanges: n', newColor: c }
  parseJSON _ = fail "DB parse failed."

instance toJSONDB :: ToJSON DB where
  toJSON (DB { colors: m, user: n, userChanges: n', newColor: c }) =
    object ["colors" .= toJSON m, "user" .= n, "userChanges" .= n', "newColor" .= c]


initialState :: DB
initialState = DB { colors: [], user: "", userChanges: "", newColor: "" }


-- action types

data DBAction a =
    UserNameKeyPress Int a
  | UserChanges String a
  | NewColorKeyPress Int a
  | NewColorChanges String a
  | DropColor String a

instance showDBAction :: Show (DBAction a) where
  show (UserNameKeyPress k _) = "UserNameKeyPress " ++ show k
  show (UserChanges n' _)     = "UserChanges " ++ show n'
  show (NewColorKeyPress k _) = "NewColorKeyPress " ++ show k
  show (NewColorChanges c _)  = "NewColorChanges " ++ show c
  show (DropColor c _)        = "DropColor " ++ show c
