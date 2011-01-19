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
    , logRequest
    , migrateStats
    ) where

import Yesod hiding (Request)
import Network.Wai
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

class (Yesod m, YesodPersist m, PersistBackend (YesodDB m (GHandler Stats m))) 
    => YesodStats m where requestIdent :: Route m -> GHandler s m (Maybe String)

mkYesodSub "Stats" 
    [ ClassP ''YesodStats [ VarT $ mkName "master" ]
    ] 
    [$parseRoutes|
    / StatsR GET
    |]


-- for now just a simple table
getStatsR :: (YesodStats m,
              YesodPersist m, 
              PersistBackend (YesodDB m (GHandler s m)))
          => GHandler s m RepHtml
getStatsR = do
    stats <- runDB $ selectList [] [StatsEntryDateDesc] 0 0
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
                        %td $string.statsEntryIdent.snd.stat$
                        %td $string.format.statsEntryDate.snd.stat$
                        %td $string.statsEntryRequestMethod.snd.stat$
                        %td $string.statsEntryPathInfo.snd.stat$
                        %td $string.statsEntryQueryString.snd.stat$
                        %td $string.statsEntryServerName.snd.stat$
                        %td $string.show.statsEntryServerPort.snd.stat$
                        %td $string.yesno.statsEntryIsSecure.snd.stat$
                        %td $string.statsEntryRemoteHost.snd.stat$
        |]
    where
        -- some formatting
        format = formatTime defaultTimeLocale "%d %b %Y %X %z"
        yesno b = if b then "Yes" else "No"

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
