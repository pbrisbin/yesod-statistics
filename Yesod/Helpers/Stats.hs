{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Helpers.Stats
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-- Collect and store information about a given request. Your site must 
-- be an instance of YesodPersist and PersistentBackend/YesodDB. Add the 
-- 'logRequest' function to any route function (ex: getRootR) and 
-- information about any requests made for that route will be stored in 
-- the DB.
--
-- Use 'loggedRequests' to get all requests that have been logged in a 
-- list sorted with most recent first. See "Yesod.Helpers.Stats.Widgets" 
-- for some importable widgets that show the data in convenient ways.
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Stats
    ( YesodStats(..)
    , StatsEntry(..)
    , migrateStats
    , logRequest
    , loggedRequests
    ) where

import Yesod hiding (Request)
import Network.Wai
import Control.Monad               (forM)
import Data.ByteString.Internal    (w2c)
import Data.Time.Clock             (UTCTime, getCurrentTime)
import Data.Time.Format            (formatTime)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)
import System.Locale               (defaultTimeLocale)
import Language.Haskell.TH.Syntax hiding (lift)

import qualified Data.ByteString as B

share2 mkPersist (mkMigrate "migrateStats") [$persist|
StatsEntry
    date          UTCTime Asc Desc
    requestMethod String
    pathInfo      String
    queryString   String
    serverName    String
    serverPort    Int
    isSecure      Bool
    remoteHost    String
|]

data Stats = Stats

class (Yesod m, YesodPersist m) => YesodStats m where 
    -- | A list of Remote hosts to not log (localhost, etc)
    blacklist :: GHandler s m [String]

-- | Add this anywhere in a route function to have that route logged
logRequest :: (YesodStats m,
               YesodPersist m, 
               PersistBackend (YesodDB m (GHandler s m)))
           => GHandler s m ()
logRequest = do
    mentry <- parseRequest
    case mentry of
        Just entry -> runDB (insert entry) >> return ()
        Nothing    -> return ()

-- | todo: fix all this staircasing...
parseRequest :: YesodStats m => GHandler s m (Maybe StatsEntry)
parseRequest = do
    toMaster <- getRouteToMaster
    mroute   <- getCurrentRoute
    case mroute of
        Nothing    -> return Nothing
        Just route -> do
            time  <- liftIO getCurrentTime
            req   <- waiRequest
            blist <- blacklist
            if asString (remoteHost req) `elem` blist
                    then return Nothing
                    else return $ Just StatsEntry
                        { statsEntryDate          = time
                        , statsEntryRequestMethod = asString $ requestMethod req
                        , statsEntryPathInfo      = asString $ pathInfo      req
                        , statsEntryQueryString   = asString $ queryString   req
                        , statsEntryServerName    = asString $ serverName    req
                        , statsEntryServerPort    = serverPort req
                        , statsEntryIsSecure      = isSecure   req
                        , statsEntryRemoteHost    = asString $ remoteHost req
                        }

asString :: B.ByteString -> String
asString = map w2c . B.unpack

-- | Return all the logged requests in a list
loggedRequests :: (YesodStats m,
                   YesodPersist m, 
                   PersistBackend (YesodDB m (GHandler s m)))
               => GHandler s m [StatsEntry]
loggedRequests = return . map snd =<< runDB (selectList [] [StatsEntryDateDesc] 0 0)
