{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
-- pbrisbin 2010
--
-- How to use Yesod.Helpers.Stats
--
module Test where

import Yesod.Helpers.Stats
import Yesod.Helpers.Stats.Widgets

import Yesod
import Text.Blaze (toHtml)
import Network.Wai.Handler.Warp (run)
import Database.Persist.Sqlite
import Database.Persist.GenericSql

data TestApp = TestApp { connPool :: ConnectionPool }
type Handler = GHandler TestApp TestApp

mkYesod "TestApp" [$parseRoutes| 
/             RootR  GET 
/test/#String TestR  GET
/stats        StatsR GET
|]

instance Yesod TestApp where approot _ = ""

instance YesodPersist TestApp where
    type YesodDB TestApp = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= runSqlPool db

instance YesodStats TestApp where blacklist  = return []

withConnectionPool :: MonadPeelIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "stats.s3db" 10

getRootR :: Handler RepHtml
getRootR = do
    let links = [ "foo", "foh", "bar", "baz", "batt" ]
    defaultLayout $ do
        setTitle  $ toHtml ("test homepage" :: String)
        addHamlet [$hamlet|
            <h1>Test Page
            <hr>

            <p>
                Welcome to my stats test page. Please make some requests 
                by clicking on the links below. After doing so, head to 
                the 
                <a href="@{StatsR}">stats page
                \ and see the collected data.

            <h3>Links

            $forall link <- links
                <p>
                    <a href="@{TestR link}">#{link}
        |]

getTestR :: String -> Handler RepHtml
getTestR name = do
    logRequest

    setMessage [$hamlet| <em>your request for #{name} was logged |]
    redirect RedirectTemporary RootR

getStatsR :: Handler RepHtml
getStatsR = defaultLayout $ do
    addHamlet [$hamlet| <h3>General statistics |]
    overallStats 

    addHamlet [$hamlet| <h3>Popular requests |]
    topRequests 5 ("pages matching \"^/test/fo.+\"" , "^/test/fo.+" )
    topRequests 5 ("pages matching \"^/test/ba./$\"", "^/test/ba./$")

main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)

withCommentTest :: (Application -> IO a) -> IO a
withCommentTest f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migrateStats) p
    let h = TestApp p
    toWaiApp h >>= f
