# Yesod Statistics

### Description

A tool for logging requests and aggregating/viewing the logged data

### Stats

The stats module just creates the subsite and provides to important 
functions:

* `logRequest` should be placed in any route function you want to log
* `loggedRequests` will return the list of all that has been logged for 
  use in a widget.

### Widgets

This module imports the `StatsEntry` data type and the `loggedRequests` 
function. It then just uses the list of logged values to present 
interesting data in the form of a widget. You can then call these 
widgets to show your stats.

Please, I would love for other people to contribute to this module. I've 
written two widgets that provide the stats I used to come up with 
manually from the servers access log and I'm not sure how much further 
I'll go.

### Usage

See ./Test.hs or read the 
[haddocks](http://pbrisbin.com/haskell/doc/html/yesod-statistics)

### Try it

Assuming you've got the required dependencies you can run the Test app 
directly. If you don't, but are willing to install them, just `cabal 
install` from within the yesod-statistics directory.

    git clone git://github.com/pbrisbin/yesod-statistics.git
    cd yesod-statistics
    runhaskell Test.hs
    $BROWSER http://localhost:3000

Here's the code I've added to my site.

    import Yesod.Helpers.Stats
    import Yesod.Helpers.Stats.Widget

    -- 
    -- add a stats route
    -- 
    -- add logRequest to the routes of interest
    -- 

    instance YesodStats DevSite where
        blacklist  = return ["192.168.0.1","66.30.118.211"]
        viewLayout = do
            addHamlet [$hamlet| %h3 General statistics |]
            overallStats 
            addHamlet [$hamlet| %h3 Files requested    |]
            topRequests ("posts", "^/posts/.*")

And here's the result:

![Yesod Stats Shot](http://pbrisbin.com/static/fileshare/yesod_stats.png)
