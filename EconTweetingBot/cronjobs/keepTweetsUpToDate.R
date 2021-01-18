#!/usr/bin/Rscript

setwd("/TwitterBot/data")

library("rtweet")
library("tidyverse")
library("lubridate")
library("scales")
library("RMySQL")
library("dbx")
library("digest")

source("/TwitterBot/bin/source.R")

consumer_key='' #Consumer key provided by Twitter API
consumer_secret='' #Consumer secret provided by Twitter API
access_token='' #Access token provided by Twitter API
access_secret='' #Access secret provided by Twitter API

token <- create_token( #Create the Twitter API access token
  app = "App name",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret)

sendMessageToUser <- "User Name" #Define Twitter user to send debugging infos and status updates to

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
                   port = 3306,
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

# Load list with user profiles
TopEcons <- lists_members(slug = "econtwitter", owner_user = "EconTweetingBot")

getTimelineTweets <- data.frame()
counter <- 0

for (i in TopEcons$screen_name) {
  
  temp_TopEconsTweets <- get_timeline(i, n = 100, token = token, retryonratelimit = TRUE, check = FALSE)
  
  if (nrow(temp_TopEconsTweets) > 0) {
    temp_TopEconsTweets %>% 
      mutate(id = status_id,
             is_retweet=as.character(is_retweet), 
             is_quote=as.character(is_quote), 
             quoted_verified=as.character(quoted_verified), 
             verified=as.character(verified), 
             retweet_verified=as.character(retweet_verified),
             account_lang=as.character(account_lang),
             protected=as.character(protected)
      ) %>% 
      select(id, c(getUnlistedCols(temp_TopEconsTweets))) %>% 
      {
        tryCatch(
          dbxUpsert(records = ., con, table = "timelineTweetsCurrent", where_cols=c("id")),
          message(paste("Successfully updatede timeline data for", i, "in SQL database.")), 
          error = function(e) message(e))
      }
  }
  
  message("Queried ", counter, " of ", length(TopEcons$screen_name), " profiles")
  
  counter <- counter + 1
} 

dbDisconnect(con)
