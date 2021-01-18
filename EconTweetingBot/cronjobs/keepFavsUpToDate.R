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

`%notin%` <- Negate(`%in%`)

tryCatch({
  
  existingTimelineTweets <- dbGetQuery(con, "SELECT id, created_at, status_id FROM favoritesCurrent WHERE created_at > NOW() - INTERVAL 1 DAY")
  
  sinceID <- min(existingTimelineTweets$status_id)
  
  count <- 0
  getEconFavs <- data.frame()
  doneList2 <- data.frame()
  for (i in TopEcons$screen_name) {
    count <- count + 1
    temp_TopEconsFavs <- tryCatch({
      message(paste0("Getting data for ", i))
      get_favorites(i, n = 50, token = token, since_id = sinceID)
    },
    error = function(e) {
      rtlimit <- rate_limit(token, "favorites/list")
      
      reset <- rtlimit[["reset"]]
      reset <- as.numeric(reset, "secs")
      
      message(paste("queried ", percent(count/286), " of 286", sep = "")) 
      
      message(paste0(
        "queried ", count, " of 286\n",
        "the time is ",
        now(),
        "\nretry on rate limit...\n",
        "waiting about ",
        round(reset / 60, 0),
        " minutes..."))
      
      Sys.sleep(reset + 2)
      
      get_favorites(i, n = 300, token = token, since_id = sinceID)
    },
    warning = function(e) {
      rtlimit <- rate_limit(token, "favorites/list")
      
      reset <- rtlimit[["reset"]]
      reset <- as.numeric(reset, "secs")
      
      message(paste("queried ", percent(count/286), " of 286", sep = "")) 
      
      message(paste0(
        "queried ", count, " of 286\n",
        "the time is ",
        now(),
        "\nretry on rate limit...\n",
        "waiting about ",
        round(reset / 60, 0),
        " minutes..."))
      
      Sys.sleep(reset + 2)
      
      get_favorites(i, n = 300, token = token, since_id = sinceID)
    })
    
    if (nrow(temp_TopEconsFavs) > 0) {
      for (row in 1:nrow(temp_TopEconsFavs)) {
        
        temp_TopEconsFavs$id[row] <- digest(paste(temp_TopEconsFavs$status_id[row], temp_TopEconsFavs$favorited_by[row], collapse = " "), algo = "md5", serialize = FALSE)
        
      }
      
      tryCatch(getEconFavs <- rbind(temp_TopEconsFavs, getEconFavs), error = function(e) message(e))
      
      temp_TopEconsFavs %>% 
        filter(!duplicated(id)) %>%
        mutate(is_retweet=as.character(is_retweet), 
               is_quote=as.character(is_quote), 
               quoted_verified=as.character(quoted_verified), 
               verified=as.character(verified), 
               retweet_verified=as.character(retweet_verified),
               account_lang=as.character(account_lang),
               protected=as.character(protected)
        ) %>% 
        select(id, c(getUnlistedCols(temp_TopEconsFavs))) %>% 
        {
          tryCatch(
            dbxUpsert(records = ., con, table = "favoritesCurrent", where_cols=c("id")),
            message(paste("Successfully stored favorite data for", i, "in SQL database.")), 
            error = function(e) message(e))
        }
    }
      } 
  

}, error = function(e) {
  
  post_message("Error in update of favorites: ", sendMessageToUser)
  post_message(paste(e), sendMessageToUser)
  
})

dbDisconnect(con)

