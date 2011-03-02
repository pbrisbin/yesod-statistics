{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Helpers.Stats.Widgets
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-- Some importable widgets showing logged aggregated statistics stored 
-- via 'logRequest'
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Stats.Widgets
    ( overallStats
    , topRequests
    , allRequests
    ) where
        
import Yesod
import Yesod.Helpers.Stats

import Control.Arrow    ((&&&))
import Data.List        (nub, sortBy, group, sort)
import Data.Ord         (comparing)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)
import Text.Blaze       (toHtml)
import Text.Regex.Posix ((=~))

-- | Some overal stats
--
-- Ex:
--
-- > overallStats
-- >
-- > -- would addHamlet for:
-- > --
-- > -- Log period from:       21 Jan 23:21:37 (UTC)
-- > -- Unique visitors:       42
-- > -- Most frequent visitor: 83.184.104.11
-- > --
--
overallStats :: (YesodStats m,
                 PersistBackend (YesodDB m (GGHandler s m IO)))
             => GWidget s m ()
overallStats = do
    statsEntries <- lift $ loggedRequests 0

    case statsEntries of
        [] -> addHamlet [$hamlet| <em>No entries found |]
        _  -> do
            let periodFrom   = statsEntryDate $ last statsEntries
            let allIps       = map statsEntryRemoteHost statsEntries
            let uniqueVisits = show . length . nub       $ allIps
            let frequentIp   = fst  . head   . frequency $ allIps

            addHamlet [$hamlet|
                <div .stats_overall_stats>
                    <table>
                        <tr>
                            <th>Log period from:
                            <td>#{format periodFrom}
                        <tr>
                            <th>Unique visitors:
                            <td>#{uniqueVisits}
                        <tr>
                            <th>Most frequent visitor:
                            <td>#{frequentIp}
                |]
    where 
        format = formatTime defaultTimeLocale "%d %b %X (%Z)"

-- | Display a listing of requests matching a certain regex sorted by 
--   frequency
--
-- Ex:
--
-- > topRequests 3 ("foos", "^/foo/.*")
-- >
-- > -- would addHamlet for:
-- > --
-- > -- popular foos:
-- > --
-- > --    250    /foo/bar
-- > --    150    /foo/baz
-- > --    30     /foo/bat
-- > --
--
topRequests :: (YesodStats m,
                PersistBackend (YesodDB m (GGHandler s m IO)))
                => Int              -- ^ limit number reported (0 means unlimited)
                -> (String,String)  -- ^ (name,regex), ex: ("media files","^/media/.*")
                -> GWidget s m ()
topRequests n (s,r) = do
    statsEntries <- lift $ loggedRequests 0
    
    case statsEntries of
        [] -> addHamlet [$hamlet| <em>No entries found |]
        _  -> do
            let counts = limit n 
                       . frequency 
                       .  map statsEntryPathInfo
                       $ filter (isDownloadOf r) statsEntries

            addHamlet [$hamlet|
                <div .stats_top_requests>
                    <p>popular #{s}:

                    <table>
                        $forall count <- counts
                            <tr>
                                <td>#{show (snd count)}
                                <td>
                                    <a href="#{fst count}">#{fst count}

                |]
    where 
        isDownloadOf s e = statsEntryPathInfo e =~ s :: Bool
        limit 0          = id
        limit n          = take n

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = reverse . sortBy (comparing snd) . map (head &&& length) . group . sort

-- | Present all logged requests in a table
allRequests :: (YesodStats m,
                PersistBackend (YesodDB m (GGHandler s m IO))) 
            => Int -- ^ limit resultset
            -> GWidget s m ()
allRequests n = do
    statsEntries <- lift $ loggedRequests n

    case statsEntries of
        [] -> addHamlet [$hamlet| <em>No entries found |]
        _  -> addHamlet [$hamlet|
            <div .stats_all_requests>
                <h1>Logged Requests
                <table>
                    <tr>
                        <th>Date
                        <th>Method
                        <th>Path info
                        <th>Query string
                        <th>Server name
                        <th>Server port
                        <th>SSL
                        <th>Remote host

                    $forall stat <- statsEntries
                        <tr>
                            <td>#{toHtml (format (statsEntryDate stat))}
                            <td>#{toHtml (statsEntryRequestMethod stat)}
                            <td>
                                <a href="#{toHtml (statsEntryPathInfo stat)}">#{toHtml (statsEntryPathInfo stat)}
                            <td>#{toHtml (statsEntryQueryString stat)}
                            <td>#{toHtml (statsEntryServerName stat)}
                            <td>#{toHtml (show (statsEntryServerPort stat))}
                            <td>#{toHtml (yesno (statsEntryIsSecure stat))}
                            <td>#{toHtml (statsEntryRemoteHost stat)}
                |]
    where
        format  = formatTime defaultTimeLocale "%Y%m%d%H%M%S"
        yesno q = if q then "y" else "n"
