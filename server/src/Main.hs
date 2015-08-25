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
import Data.Proxy
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles (serveDirectory)


-- * config

dbPath :: FilePath
dbPath = "./.state"

htmlPath :: FilePath
htmlPath = "./public_html"


-- * data types

data DB = DB { _counter :: Double }
  deriving (Eq, Show, Typeable, Generic)

type St = AcidState DB

instance FromJSON DB
instance ToJSON DB

makeLenses ''DB
$(deriveSafeCopy 0 'base ''DB)

-- * persistence

openDB :: IO St
openDB = openLocalStateFrom dbPath (DB 0)

getDB :: Query DB DB
getDB = ask

putDB :: DB -> Update DB ()
putDB = put

$(makeAcidic ''DB ['getDB, 'putDB])


-- * rest

type Rest =
       "rest" :> Get '[JSON] DB
  :<|> "rest" :> ReqBody '[JSON] DB :> Put '[JSON] ()
  :<|> Raw

rest :: St -> Server Rest
rest state = gt :<|> up :<|> serveDirectory htmlPath
  where
    gt :: EitherT ServantErr IO DB
    gt = query' state GetDB

    up :: DB -> EitherT ServantErr IO ()
    up = update' state . PutDB


-- * main

main :: IO ()
main = openDB >>= runRest

runRest :: St -> IO ()
runRest = runSettings settings . serveRest
  where
    settings = setPort 8000 . setHost "127.0.0.1" $ defaultSettings

serveRest :: St -> Application
serveRest = serve (Proxy :: Proxy Rest) . rest
