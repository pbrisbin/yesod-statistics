{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
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
-- Import some widgets and add them to your viewLayout.
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Stats.Widgets
    ( overallStats
    , topRequests
    ) where
        
import Yesod

import Yesod.Helpers.Stats

import Control.Arrow    ((&&&))
import Control.Monad    (when)
import Data.Function    (on)
import Data.List        (nub, sortBy, group, sort)
import Data.Ord         (comparing)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)
import Text.Regex.Posix ((=~))

-- | Overall statistics in a table.
overallStats :: (YesodStats m,
                 PersistBackend (YesodDB m (GHandler s m)))
             => GWidget s m ()
overallStats = do
    statsEntries <- liftHandler loggedRequests

    case statsEntries of
        [] -> addHamlet [$hamlet| %em No entries found |]
        _  -> do
            let periodFrom   = statsEntryDate $ last statsEntries
            let allIps       = map statsEntryRemoteHost statsEntries
            let uniqueVisits = show . length . nub       $ allIps
            let frequentIp   = fst  . head   . frequency $ allIps

            addHamlet [$hamlet|
                .stats_general
                    %table
                        %tr
                            %th Log period from:
                            %td $format.periodFrom$
                        %tr
                            %th Unique visitors:
                            %td $uniqueVisits$
                        %tr
                            %th Most frequent visitor:
                            %td $frequentIp$
                |]
    where 
        format = formatTime defaultTimeLocale "%d %b %X (%Z)"

-- | Display a listing of requests matching a certain regex sorted by 
--   frequency
topRequests :: (YesodStats m,
                PersistBackend (YesodDB m (GHandler s m)))
                => Int              -- ^ limit number reported (0 means unlimited)
                -> (String,String)  -- ^ (name,regex), ex: ("media files","^/media/.*")
                -> GWidget s m ()
topRequests n (s,r) = do
    statsEntries <- liftHandler loggedRequests
    
    case statsEntries of
        [] -> addHamlet [$hamlet| %em No entries found |]
        _  -> do
            let counts = limit n 
                       . frequency 
                       .  map statsEntryPathInfo
                       $ filter (isDownloadOf r) statsEntries

            addHamlet [$hamlet|
                .stats_top_entry
                    %p popular $s$:

                    %table
                        $forall counts count
                            %tr
                                %td $show.snd.count$
                                %td 
                                    %a!href=$fst.count$ $fst.count$
                |]
    where 
        isDownloadOf s e = statsEntryPathInfo e =~ s :: Bool
        limit 0          = id
        limit n          = take n

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = reverse . sortBy (comparing snd) . map (head &&& length) . group . sort
