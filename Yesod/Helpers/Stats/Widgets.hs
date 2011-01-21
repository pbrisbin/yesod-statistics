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
module Yesod.Helpers.Stats.Widgets where
        
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

overallStats :: (YesodStats m,
                 PersistBackend (YesodDB m (GHandler s m)))
             => GWidget s m ()
overallStats = do
    timeNow      <- liftIO getCurrentTime
    statsEntries <- liftHandler loggedRequests

--    when (statsEntries == []) todo:
--        addHamlet [$hamlet| %em No entries found |]

    let periodFrom   = statsEntryDate $ last statsEntries

    let allIps       = map statsEntryRemoteHost statsEntries
    let uniqueVisits = show . length . nub       $ allIps
    let frequentIp   = fst  . head   . frequency $ allIps

    addHamlet [$hamlet|
        .stats_general
            %table
                %tr
                    %th stats generated:
                    %td $format.timeNow$
                %tr
                    %th log period from:
                    %td $format.periodFrom$
                %tr
                    %th unique visits:
                    %td $uniqueVisits$
                %tr
                    %th most frequent visitor:
                    %td $frequentIp$
        |]
    where format = formatTime defaultTimeLocale "%a, %d %b %X (%Z)"

topRequests :: (YesodStats m,
                PersistBackend (YesodDB m (GHandler s m)))
                => (String,String) -> GWidget s m ()
topRequests (s,r) = do
    statsEntries <- liftHandler loggedRequests
    
--    when (statsEntries == []) todo:
--        addHamlet [$hamlet| %em No entries found |]

    let downloads = filter (isDownloadOf r) statsEntries

    let total  = length downloads
    let counts = frequency $ map statsEntryPathInfo downloads

    addHamlet [$hamlet|
        .stats_top_entry
            .stats_top_entry_total
                %table
                    %tr
                        %th total $s$ downloaded: 
                        %td $show.total$

            .stats_top_entry_counts
                %table
                    $forall counts count
                        %tr
                            %td $show.snd.count$
                            %td 
                                %a!href=$fst.count$ $fst.count$
        |]
    where isDownloadOf s e = statsEntryPathInfo e =~ s :: Bool

-- | Ex: frequency [a, b, c, b, b, a] -> [(b, 3), (a, 2), (c, 1)]
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency = reverse . sortBy (comparing snd) . map (head &&& length) . group . sort
