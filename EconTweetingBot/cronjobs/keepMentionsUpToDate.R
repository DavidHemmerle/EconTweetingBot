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

tryCatch({
  
  existingTimelineTweets <- dbGetQuery(con, "SELECT screen_name, created_at, status_id FROM mentionsCurrent WHERE created_at > NOW() - INTERVAL 1 DAY")
  
  getMentions <- data.frame()
  count <- 0
  counter <- 0
  for (i in TopEcons$screen_name) {
    count <- count + 1
    sinceID <- tryCatch(max(existingTimelineTweets$status_id[existingTimelineTweets$screen_name == i]), warning = function(e) min(existingTimelineTweets$status_id))
    tryCatch(
      {
        temp_TopEconsTweets <- tryCatch({
          message(paste0("Getting data for ", i))
          search_tweets(paste("@", i, sep = "", collapse = ""), n = 10000, since_id = sinceID) #, since_id = sinceID, token = token,
        },
        error = function(e) {
          reset <- tryCatch({
            rtlimit <- rate_limit(token, "search/tweets")
            reset <- rtlimit[["reset"]]
            reset <- as.numeric(reset, "secs")
          }, error = function(e) {
            message(e)
            reset <- 900
            return(900)
          })
          
          message(paste0(
            "queried ", count, " of 286\n",
            "the time is ",
            now(),
            "\nretry on rate limit...\n",
            "waiting about ",
            round(reset / 60, 0),
            " minutes..."))
          
          message(paste("queried ", percent(count/286), " of 286 mentions, Waiting for", round(reset / 60, 0), "minutes.", sep = "")) 
          
          Sys.sleep(reset + 2)
          
          search_tweets(paste("@", i, sep = "", collapse = ""), n = 10000, since_id = sinceID) #, since_id = sinceID, token = token,
        },
        warning = function(e) {
          reset <- tryCatch({
            rtlimit <- rate_limit(token, "search/tweets")
            reset <- rtlimit[["reset"]]
            reset <- as.numeric(reset, "secs")
          }, error = function(e) {
            message(e)
            reset <- 900
            return(900)
          })
          
          message(paste0(
            "queried ", count, " of 286\n",
            "the time is ",
            now(),
            "\nretry on rate limit...\n",
            "waiting about ",
            round(reset / 60, 0),
            " minutes..."))
          
          message(paste("queried ", percent(count/286), " of 286 mentions, Waiting for", round(reset / 60, 0), "minutes.", sep = "")) 
          
          Sys.sleep(reset + 2)
          
          search_tweets(paste("@", i, sep = "", collapse = ""), n = 10000, since_id = sinceID) #, since_id = sinceID, token = token,
        })
        
        #     }
        if (nrow(temp_TopEconsTweets) > 0) {
          getMentions <- rbind(temp_TopEconsTweets, getMentions)
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
                dbxUpsert(records = ., con, table = "mentionsCurrent", where_cols=c("id")),
                message(paste("Successfully updated mention data for", i, "in SQL database with", nrow(temp_TopEconsTweets), "new rows.")), 
                error = function(e) message(e))
            }
        }

      }, error = function(err) {
        err 
        print(err)
        post_message(paste(err), sendMessageToUser)
      }, warning = function(cond) {
        cond
        print(cond)
        post_message(paste(cond), sendMessageToUser)
      }, finally = {
        counter <- counter + 1
      })
  } 
  
  diff <- nrow(getMentions) - nrow(existingTimelineTweets)
  
}, error = function(e) {
  
  post_message("Error in update of mentions: ", sendMessageToUser)
  post_message(paste(e), sendMessageToUser)
  
})

dbDisconnect(con)
