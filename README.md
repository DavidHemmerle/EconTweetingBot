# EconTweetingBot
Twitter Bot for the German #EconTwitter bubble curated by [Makronom Magazin](https://makronom.de/twitter-ranking-econ-einzelwertung).

## Implementation
* Scrape profiles from most current ranking, e.g. https://makronom.de/twitter-ranking-econ-einzelwertung-2-2
* Create Twitter list with profiles: https://twitter.com/i/lists/1182746180033372165?s=20
* Use cronjobs to load Twitter data about profiles on a regular basis
* Modify credentials in `post_script.R` and run

## Scrape profiles
For setting up the Twitter list we need profile names. On Makronom Magazin can be found a list (updated half-yearly), which can be used to scrape the profiles from.
```
# Scrape current Makronom Magazin Einzelwertung page
library(rvest)
page <- read_html("https://makronom.de/twitter-ranking-econ-einzelwertung-2-2")
```

## Cronjobs

Due to the Twitter API rate limits, the mentions and favorite data takes some time to query. Calculate 30 minutes for 50 profiles or so. 

```
0 8 * * * /pathToFile/keepTweetsUpToDate.R >> /pathToFile/logs/keepTweetsUpToDate.log 2>&1
0 6 * * * /pathToFile/keepMentionsUpToDate.R >> /pathToFile/logs/keepMentionsUpToDate.log 2>&1
0 4 * * * /pathToFile/keepFavsUpToDate.R >> /pathToFile/logs/keepFavsUpToDate.log 2>&1
```

## Credentials

### Twitter 
Register an app on https://developer.twitter.com/en to receive API keys.
```
library(rtweet)
consumer_key=''             #Consumer key provided by Twitter API
consumer_secret=''          #Consumer secret provided by Twitter API
access_token=''             #Access token provided by Twitter API
access_secret=''            #Access secret provided by Twitter API

token <- create_token(      #Create the Twitter API access token
  app = "App name",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret)
```

### SQL Server
Current setup is based on SQL server for storing and retrieving the data. 
```
library(RMySQL)
# MySQL server configuration
host = ""
database = ""
port = 3306
dbuser = ""
dbpasswort = ""

tryCatch({
  con <- dbConnect(RMySQL::MySQL(), 
                   dbname = database, 
                   host = host, 
                   port = port,
                   user = dbuser,
                   password = dbpasswort)
  }, error = function(e) {
  message(e)
  post_message("ERROR: Unsuccessfull SQL connection", sendMessageToUser)
}, warning = function(w) {
  message(w)
  post_message("WARNING: Unsuccessfull SQL connection", sendMessageToUser)
})
dbGetQuery(con,'SET NAMES utf8mb4')
``` 
