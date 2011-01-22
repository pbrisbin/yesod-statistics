# Yesod Statistics

### Description

A tool for logging requests and aggregating/viewing the logged data

### Stats

The stats module provides two important functions:

* `logRequest` should be placed in any route function you want to log
* `loggedRequests` will return the list of all that has been logged for 
  use in a widget.

### Widgets

This module imports the `StatsEntry` data type and the `loggedRequests` 
function. It then just uses the list of logged values to present 
interesting data in the form of widgets. You can then call these widgets 
to show your stats.

### Usage

See ./Test.hs or read the 
[haddocks](http://pbrisbin.com/haskell/docs/html/yesod-statistics)

### Try it

Assuming you've got the required dependencies you can run the Test app 
directly. If you don't, but are willing to install them, just `cabal 
install` from within the yesod-statistics directory.

    git clone git://github.com/pbrisbin/yesod-statistics.git
    cd yesod-statistics
    runhaskell Test.hs
    $BROWSER http://localhost:3000

