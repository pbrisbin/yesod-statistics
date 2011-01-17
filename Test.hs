{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- pbrisbin 2010
--
-- How to use Yesod.Stats
--
module Test where

import Yesod.Helpers.Stats

import Yesod
import Network.Wai.Handler.SimpleServer (run)
import Database.Persist.Sqlite
import Database.Persist.GenericSql

data TestApp = TestApp { connPool :: ConnectionPool }
type Handler = GHandler TestApp TestApp
type Widget  = GWidget TestApp TestApp

mkYesod "TestApp" [$parseRoutes| 
/             RootR GET 
/test/#String TestR GET

/stats StatsR Stats getStats
|]

instance Yesod TestApp where approot _ = ""

instance YesodPersist TestApp where
    type YesodDB TestApp = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

instance YesodStats TestApp where
   requestIdent RootR     = return $ Just "homepage"
   requestIdent (TestR s) = return $ Just ("test_" ++ s)
   requestIdent _         = return Nothing

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "stats.s3db" 10

getRootR :: Handler RepHtml
getRootR = do
    let links = ["foo", "bar", "baz", "bat"]
    defaultLayout $ do
    setTitle  $ string "test homepage"
    addHamlet [$hamlet|
        #header
            %h1 Test Page
            %hr
        #body
            %p 
                Welcome to my stats test page. Please make some requests by 
                clicking on the links below. After doing so head to the 
                stats page and see the collected data

            %h3 Links

            $forall links link
                %p
                    %a!href=@TestR.link@ $string.link$

            %p
                And the 
                %a!href="/stats" stats
                \ page.
        |] -- why can't i put @StatsR@ here?

getTestR :: String -> Handler RepHtml
getTestR name = do
    logRequest
    setMessage [$hamlet| %em request for $string.name$ was logged |]
    redirect RedirectTemporary RootR

main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)

withCommentTest :: (Application -> IO a) -> IO a
withCommentTest f = withConnectionPool $ \p -> do
    -- | make sure you run the migration to create the necessary tables
    runSqlPool (runMigration migrateStats) p
    let h = TestApp p
    toWaiApp h >>= f
