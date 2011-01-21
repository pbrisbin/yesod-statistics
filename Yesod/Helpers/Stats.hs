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
    ident         String  Eq
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
    -- | A unique \"friendly\" identifier to use for each route logged,
    --   setting this to 'Nothing' will prevent the route from being
    --   logged at all.
    requestIdent :: Route m -> GHandler s m (Maybe String)

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
        addCassius [$cassius|
            .yesod_stats th
                text-align: left
                border-bottom: solid 1px

            .yesod_stats td
                padding-left:  10px
                padding-right: 10px
            |]

        addHamlet [$hamlet|
            .yesod_stats
                %h1 Logged Requests
                %table
                    %tr
                        %th Request id
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
                            %td $string.statsEntryIdent.stat$
                            %td $string.format.statsEntryDate.stat$
                            %td $string.statsEntryRequestMethod.stat$
                            %td $string.statsEntryPathInfo.stat$
                            %td $string.statsEntryQueryString.stat$
                            %td $string.statsEntryServerName.stat$
                            %td $string.show.statsEntryServerPort.stat$
                            %td $string.yesno.statsEntryIsSecure.stat$
                            %td $string.statsEntryRemoteHost.stat$
            |]
    where
        -- some formatting
        format = formatTime defaultTimeLocale "%a, %d %b %Y %X (%Z)"
        yesno b = if b then "Yes" else "No"

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

parseRequest :: YesodStats m => GHandler s m (Maybe StatsEntry)
parseRequest = do
    toMaster <- getRouteToMaster
    mroute   <- getCurrentRoute
    case mroute of
        Nothing    -> return Nothing
        Just route -> do
            mident <- requestIdent $ toMaster route
            case mident of
                Nothing    -> return Nothing
                Just ident -> do
                    time  <- liftIO getCurrentTime
                    req   <- waiRequest
                    return $ Just StatsEntry
                        { statsEntryIdent         = ident
                        , statsEntryDate          = time
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
