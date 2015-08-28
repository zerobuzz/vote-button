{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Applicative ((<$>))
import Control.Lens (makeLenses)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Control.Monad.Trans.Either
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles (serveDirectory)

import qualified Data.Map as M


-- * config

dbPath :: FilePath
dbPath = "./.state"

htmlPath :: FilePath
htmlPath = "./public_html"


-- * data types

data DB = DB { _votes :: M.Map String Vote, _user :: String }
  deriving (Eq, Show, Typeable, Generic)

data Vote = Yay | Nay | Abstain
  deriving (Eq, Show, Typeable, Generic)

type St = AcidState DB

instance FromJSON DB
instance ToJSON DB

instance FromJSON Vote
instance ToJSON Vote

makeLenses ''DB
$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''Vote)


-- * persistence

openDB :: IO St
openDB = openLocalStateFrom dbPath (DB M.empty "")

getDB :: Query DB DB
getDB = ask

putDB :: DB -> Update DB ()
putDB = put

$(makeAcidic ''DB ['getDB, 'putDB])


-- * rest

type Rest =
       "_get" :> Get '[JSON] DB
  :<|> "_put" :> ReqBody '[JSON] DB :> Put '[JSON] ()
  :<|> Raw

rest :: St -> Server Rest
rest state = gt :<|> pt :<|> serveDirectory htmlPath
  where
    gt :: EitherT ServantErr IO DB
    gt = query' state GetDB

    pt :: DB -> EitherT ServantErr IO ()
    pt = update' state . PutDB


-- * main

main :: IO ()
main = openDB >>= runRest

runRest :: St -> IO ()
runRest = runSettings settings . serveRest
  where
    settings = setPort 8000 . setHost "127.0.0.1" $ defaultSettings

serveRest :: St -> Application
serveRest = serve (Proxy :: Proxy Rest) . rest
