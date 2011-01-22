{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
-------------------------------------------------------------------------------
module Yesod.Helpers.Stats
    ( Stats
    , getStats
    , YesodStats(..)
    , StatsEntry(..)
    , logRequest
    , loggedRequests
    , migrateStats
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

getStats :: a -> Stats
getStats = const Stats

class (Yesod m, 
       YesodPersist m, 
       PersistBackend (YesodDB m (GHandler Stats m))) => YesodStats m where 
    -- | A list of IPs to not log (localhost, etc)
    blacklist :: GHandler s m [String]

    -- | This is the main \"view stats\" page. Easiest thing to do is
    --   import some pre-built widgets from
    --   "Yesod.Helpers.Stats.Widgets" and throw them in defaultLayout.
    viewLayout   :: GWidget s m ()

mkYesodSub "Stats" 
    [ ClassP ''YesodStats [ VarT $ mkName "master" ]
    ] 
    [$parseRoutes|
    /    StatsR GET
    /log LogR GET
    |]

-- | Return all the logged requests in a list
loggedRequests :: (YesodStats m,
                   YesodPersist m, 
                   PersistBackend (YesodDB m (GHandler s m)))
               => GHandler s m [StatsEntry]
loggedRequests = do
    results <- runDB $ selectList [] [StatsEntryDateDesc] 0 0
    return $ map snd results

getStatsR :: (YesodStats m,
              YesodPersist m, 
              PersistBackend (YesodDB m (GHandler Stats m)))
          => GHandler Stats m RepHtml
getStatsR = defaultLayout $ do
    tm <- liftHandler getRouteToMaster
    cr <- liftHandler getCurrentRoute
    setTitle $ string "Stats"
    addHamlet [$hamlet| %h1 Stats |]
    viewLayout
    addHamlet [$hamlet|
        .stats_log_link
            %p
                %a!href=@tm.LogR@ all requests
        |]

-- | A simple table of everything logged to date
getLogR :: (YesodStats m,
            YesodPersist m, 
            PersistBackend (YesodDB m (GHandler s m)))
        => GHandler s m RepHtml
getLogR = do
    stats <- loggedRequests
    defaultLayout $ do
        setTitle $ string "Logged Requests"
        addHamlet [$hamlet|
            .stats_full_log
                %h1 Logged Requests
                %table
                    %tr
                        %th Date
                        %th Method
                        %th Path info
                        %th Query string
                        %th Server name
                        %th Server port
                        %th SSL
                        %th Remote host

                    $forall stats stat
                        %tr
                            %td $string.format.statsEntryDate.stat$
                            %td $string.statsEntryRequestMethod.stat$
                            %td 
                                %a!href=$string.statsEntryPathInfo.stat$ $string.statsEntryPathInfo.stat$
                            %td $string.statsEntryQueryString.stat$
                            %td $string.statsEntryServerName.stat$
                            %td $string.show.statsEntryServerPort.stat$
                            %td $string.yesno.statsEntryIsSecure.stat$
                            %td $string.statsEntryRemoteHost.stat$
            |]
    where
        -- some formatting
        format = formatTime defaultTimeLocale "%Y%m%d%H%M%S"
        yesno b = if b then "y" else "n"

-- | Add this anywhere in a route function to have that route logged
logRequest :: (YesodStats m,
               YesodPersist m, 
               PersistBackend (YesodDB m (GHandler s m)))
           => GHandler s m ()
logRequest = do
    mentry <- parseRequest
    case mentry of
        Just entry -> (runDB $ insert entry) >> return ()
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
            if (asString $ remoteHost req) `elem` blist
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
