############### Build relevant functions for later use ################

getUnlistedCols <- function(dataframe) { # this function identifies and excludes columns in a dataframe which are lists (cannot be saved as csv)
  myCols <- data.frame(colnames(dataframe))
  colnames(myCols) <- "columns"
  for (i in colnames(dataframe)) {
    myCols[myCols$columns == i,"isList"] <- lapply(dataframe[,i], typeof)
  }
  myNewCols <- as.character(myCols$columns[myCols$isList != "list"])
  return(myNewCols)
}

get_statusTweets <- function(x, list, round) { # this function sorts all replies according to their logical order
  df <- data.frame()
  for (i in list) {
    repliesToTweets_temp <- x[x$reply_to_status_id == i,]
    repliesToTweets_temp$level <- round
    df <- rbind(df, repliesToTweets_temp)
    df <- df[!is.na(df$user_id),]
  }
  return(df)  
}

repliesIterate <- function(x,y) { # this function iterates through all replies and orders them
  j <- 1
  repliesToTweets <- data.frame()
  repliesToTweets <- get_statusTweets(x, y$status_id, 1)
  lengtha <- 1
  repeat {
    repliesToTweets <- rbind(repliesToTweets, get_statusTweets(x, repliesToTweets$status_id[repliesToTweets$level == j], j+1))
    if (lengtha == length(rownames(repliesToTweets))) {
      break
    } else {
      j <- j+1
      lengtha <- length(rownames(repliesToTweets))
    }
  }
  return(repliesToTweets)
}

makeReplyTree <- function(mergeData, levelData, x) { # this function makes a new table where each column refers to a new level of replies for each original status
  x2 <- 1
  thisIsAnotherTryXY <- merge(mergeData, levelData[levelData$level == x, c("status_id","screen_name","reply_to_status_id")], by.x = paste("status_id", x2, sep = ""), by.y = "reply_to_status_id", all.x = TRUE, suffixes = c(paste(""),paste(x)))
  x <- x+1
  repeat {
    x2 <- x-1
    thisIsAnotherTryXY <- merge(thisIsAnotherTryXY, levelData[levelData$level == x, c("status_id","screen_name","reply_to_status_id")], by.x = paste("status_id", x2, sep = ""), by.y = "reply_to_status_id", all.x = TRUE, suffixes = c(paste(""),paste(x)))
    if (sum(is.na(thisIsAnotherTryXY[,length(thisIsAnotherTryXY)])) == length(rownames(thisIsAnotherTryXY))) {
      break
    } else {
      x <- x+1
    }
  }
  return(thisIsAnotherTryXY)
}

getReplyCount <- function(data) { # this function sums up the replies by user and tweet
  
  getReplyData <- 
    data[,grepl("status", names(data))] %>% 
    gather(key = status,
           value = Value,
           -status_id) %>% 
    group_by(status_id) %>% 
    summarise(count = n_distinct(Value)) %>% 
    arrange(desc(count))
  
  getReplyUserData <-
    data %>% 
    select(status_id, starts_with("screen")) %>% 
    gather(key = screenName,
           value = Value,
           -status_id) %>%  
    group_by(status_id) %>% 
    summarise(count = n_distinct(Value)) %>% 
    arrange(desc(count))
  
  mergeReplyDFs <- merge(getReplyData, getReplyUserData, by = "status_id", suffixes = c("Replies", "User"))
  mergeReplyDFs$countReplies <- mergeReplyDFs$countReplies-1
  mergeReplyDFs$countUser <- mergeReplyDFs$countUser-1
  mergeReplyDFs$score <- (mergeReplyDFs$countReplies*2) + mergeReplyDFs$countUser
  return(mergeReplyDFs)
}


iterateReplies <- function(econSearchTweets, getRootTweets) {
  
  ## assign replies to respective tweets ## 
  getReplyTweets_iterated <- repliesIterate(econSearchTweets, getRootTweets)
  
  getReplyTweets_merged <- 
    merge(getRootTweets[,c("status_id","screen_name")], getReplyTweets_iterated[getReplyTweets_iterated$level == 1, c("status_id","screen_name","reply_to_status_id")], by.x = "status_id", by.y = "reply_to_status_id", all.x = TRUE, suffixes = c("", "1"))
  
  getReplyTweets_merged <- makeReplyTree(getReplyTweets_merged, getReplyTweets_iterated, 2)
  
  return(getReplyTweets_merged)
}

countReplies <- function(reply_tweets) {
  getReplyTweets_counted <- getReplyCount(reply_tweets)
  
  return(getReplyTweets_counted)
}

myReplyPost <- function(tweet, media = NULL) { # custom made reply function for threading
  TwitterBotTimeline <- get_timeline("EconTweetingBot", n=1)
  post_tweet(tweet, in_reply_to_status_id = TwitterBotTimeline$status_id[1], media = media, token = token)
}


