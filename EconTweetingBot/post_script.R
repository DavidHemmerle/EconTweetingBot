#!/usr/bin/Rscript

setwd("/TwitterBot/data")

library("widyr")
library("tidyverse")
library("tidytext")
library("igraph")
library("rtweet")
library("lubridate")
library("scales")
library("tm")
library("stopwords")
library("qdapRegex")     # Removing URLs 
library("ggraph")
library("ggrepel")
library("RMySQL")
library("wordcloud2")
library("webshot")
library("htmlwidgets")

source("/TwitterBot/bin/source.R")

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

sendMessageToUser <- "User Name"         #Define Twitter user to send debugging infos and status updates to

# MySQL server configuration
host = ""
database = ""
port = 3306
dbuser = ""
dbpasswort = ""

# Change color code for all plots
plot_bg_color = "#103C87"   # blue background color
plot_fg_color = "#FFFFFF"     # white foreground color

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

# Load list with user profiles
TopEcons <- lists_members(slug = "econtwitter", owner_user = "EconTweetingBot")


# Load data and set settings ----------------------------------------------

zeitreferenz <- if_else(weekdays(today()) == "Sonntag", "Last week,", "Yesterday,")
zeitreferenz1 <- if_else(weekdays(today()) == "Sonntag", "last week", "yesterday")

interval <- if_else(weekdays(today()) == "Sonntag", 10, 2)

message("Loading timeline data...")
econTimelineTweets <- dbGetQuery(con, paste0("SELECT screen_name, 
                                            name, user_id, 
                                            text, status_id, 
                                            status_url, 
                                            created_at, 
                                            followers_count, is_retweet, is_quote, reply_to_status_id, reply_to_screen_name, quoted_status_id, quoted_screen_name, retweet_screen_name, retweet_status_id,
                                            retweet_count, retweet_favorite_count,
                                            quote_count, 
                                            favorite_count, profile_image_url, profile_url, lang FROM timelineTweetsCurrent WHERE created_at > NOW() - INTERVAL ", interval, " DAY"))

message("Loading mentions data...")
econSearchTweets <- dbGetQuery(con, paste0("SELECT screen_name, 
                                            name, user_id, 
                                            text, status_id, 
                                            created_at, 
                                            is_retweet, is_quote, reply_to_status_id, reply_to_screen_name, quoted_status_id, quoted_screen_name,
                                            favorite_count, lang, status_url FROM mentionsCurrent WHERE created_at > NOW() - INTERVAL ", interval, " DAY"))

message("Loading favorite data...")
TopEconsFavs <- dbGetQuery(con, paste0("SELECT created_at, status_id, screen_name, favorited_by, followers_count, status_url FROM favoritesCurrent WHERE created_at > NOW() - INTERVAL ", interval, " DAY"))

message("Loading gender list...")
genderList <- dbGetQuery(con, paste0("SELECT * FROM genderList2020"))

genderTweets <- dbGetQuery(con, "SELECT status_id, created_at, screen_name, is_retweet, retweet_count, favorite_count FROM timelineTweetsCurrent WHERE DATE(created_at) > CURDATE() - INTERVAL 30 DAY")

###### WEEKDAY: Apply prebuilt functions to tweets #########

## Make new tables for selected time frame ##
if (weekdays(today()) == "Sonntag") {
  econTimelineTweets <- 
    econTimelineTweets %>% 
    filter(created_at > paste(today()-7, "00:00:00") & created_at < paste(today(), "00:00:00")) 
  
  econSearchTweets <- 
    econSearchTweets %>% 
    filter(created_at > paste(today()-7, "00:00:00") & created_at < paste(today(), "00:00:00")) 
} else {
  econTimelineTweets <- 
    econTimelineTweets %>% 
    filter(created_at > paste(today()-1, "00:00:00") & created_at < paste(today(), "00:00:00")) 
  
  econSearchTweets <- 
    econSearchTweets %>% 
    filter(created_at > paste(today()-1, "00:00:00") & created_at < paste(today(), "00:00:00")) 
}

## filter non-reply tweets only ##
getRootTweets <- 
  econTimelineTweets %>% 
  filter(is.na(reply_to_status_id), is_retweet == "FALSE")

## filter non-retweets only ##
econSearchTweets <-
  econSearchTweets %>% 
  filter(is_retweet == "FALSE", duplicated(status_id) == "FALSE")

## apply custom function to count replies
econReplies <- iterateReplies(econSearchTweets, getRootTweets)

## apply custom function to count replies
econReplyCounts <- countReplies(econReplies)

# Prepare data on tweet level --------------------------------------------------------------

startPoint <- if_else(weekdays(today()) == "Sonntag", paste(today()-7, "00:00:00"), paste(today()-1, "00:00:00"))

mostLikedTweets <-
  TopEconsFavs %>%
  group_by(screen_name, status_url) %>%
  filter(created_at > startPoint, created_at < paste(today(), "00:00:00"), screen_name %in% TopEcons$screen_name) %>% #
  summarise(n = n(), score = n + mean(followers_count * 0.000001)) %>%
  arrange(desc(score)) %>%
  mutate(screen_name_low = tolower(screen_name)) %>% 
  left_join(genderList %>% mutate(screen_name_low = tolower(screen_name)) %>% select(-screen_name, -vorname, -nachname), by = c("screen_name_low" = "screen_name_low")) %>%
  mutate(pronome = if_else(gender == "male", "his", "her"))

mostRetweetedTweet <-
econTimelineTweets %>% 
  filter(created_at > startPoint, created_at < paste(today(), "00:00:00"), is_retweet == "TRUE", retweet_screen_name != screen_name) %>% #
  group_by(retweet_status_id, retweet_screen_name) %>% 
  summarise(econRetweetCount = n(), score = econRetweetCount+(mean(as.numeric(retweet_favorite_count))*0.000001)) %>% 
  arrange(desc(score)) %>% 
  mutate(status_url = paste("https://twitter.com", retweet_screen_name, "status", retweet_status_id, sep = "/")) %>% 
  mutate(screen_name_low = tolower(retweet_screen_name)) %>% 
  left_join(genderList %>% mutate(screen_name_low = tolower(screen_name)) %>% select(-screen_name, -vorname, -nachname), by = c("screen_name_low" = "screen_name_low")) %>%
  mutate(pronome = if_else(gender == "male", "his", "her")) %>% 
  rename(screen_name = retweet_screen_name)

mostQuotedTweet <-
  econTimelineTweets %>% 
  filter(created_at > startPoint, created_at < paste(today(), "00:00:00"), is_quote == "TRUE" & quoted_screen_name != screen_name) %>% #
  group_by(quoted_status_id, quoted_screen_name) %>% 
  summarise(econQuoteCount = n(), score = sum(favorite_count*0.0005+retweet_count*0.002), score= econQuoteCount+score) %>% 
  arrange(desc(score)) %>% 
  mutate(status_url = paste("https://twitter.com", quoted_screen_name, "status", quoted_status_id, sep = "/")) %>% 
  mutate(screen_name_low = tolower(quoted_screen_name)) %>% 
  left_join(genderList %>% mutate(screen_name_low = tolower(screen_name)) %>% select(-screen_name, -vorname, -nachname), by = c("screen_name_low" = "screen_name_low")) %>%
  mutate(pronome = if_else(gender == "male", "his", "her"))%>% 
  rename(screen_name = quoted_screen_name)

mostDebatedTweets <- 
  econReplies %>% 
  select(status_id, screen_name) %>% 
  gather(key = screenName,
         value = Tweet,
         -status_id) %>%  
  group_by(status_id, Tweet) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  left_join(econReplies %>% 
              select(status_id, matches("screen_name.")) %>% 
              gather(key = screenName,
                     value = Reply,
                     -status_id) %>%  
              filter(screenName != "NA"), 
            by = "status_id") %>% 
  filter(Reply %in% TopEcons$screen_name, Reply != Tweet) %>% 
  ungroup() %>% 
  group_by(status_id) %>% 
  summarise(dist = n_distinct(Reply), n = n(), score = 0.9*dist+0.1*n) %>% 
  arrange(desc(score)) %>% 
  left_join(econTimelineTweets %>% distinct(status_id, status_url, screen_name), by = "status_id") %>% 
  mutate(screen_name_low = tolower(screen_name)) %>% 
  left_join(genderList %>% mutate(screen_name_low = tolower(screen_name)) %>% select(-screen_name, -vorname, -nachname), by = c("screen_name_low" = "screen_name_low")) %>%
  mutate(pronome = if_else(gender == "male", "his", "her"))


# Prepare data on user level --------------------------------------------------------------

likesPerEcon <-
  TopEconsFavs %>%
  group_by(screen_name) %>%
  filter(created_at > startPoint, created_at < paste(today(), "00:00:00"), screen_name %in% TopEcons$screen_name) %>% #
  summarise(likes = n(), likesDistinct = n_distinct(favorited_by)) %>%
  arrange(desc(likes)) 

quotesPerEcon <- 
  econTimelineTweets %>% 
  group_by(quoted_screen_name) %>% 
  filter(quoted_screen_name != "NA", quoted_screen_name %in% TopEcons$screen_name, quoted_screen_name != screen_name) %>% 
  summarise(quotes = n(), quotesDistinct = n_distinct(screen_name)) %>% 
  arrange(desc(quotesDistinct)) 

retweetsPerEcon <- 
  econTimelineTweets %>% 
  group_by(retweet_screen_name) %>% 
  filter(retweet_screen_name != "NA", retweet_screen_name %in% TopEcons$screen_name, retweet_screen_name != screen_name) %>% 
  summarise(retweets = n(), retweetsDistinct = n_distinct(screen_name)) %>% 
  arrange(desc(retweetsDistinct)) 

repliesPerEcon <- 
econTimelineTweets %>% 
  group_by(reply_to_screen_name) %>% 
  filter(reply_to_screen_name != "NA", reply_to_screen_name %in% TopEcons$screen_name, reply_to_screen_name != screen_name) %>% 
  summarise(replies = n(), repliesDistinct = n_distinct(screen_name)) %>% 
  arrange(desc(repliesDistinct)) 

TopEconsBase <- TopEcons[,2:4]

IntraBubbleStats <-
TopEconsBase %>% 
  left_join(likesPerEcon) %>% 
  left_join(quotesPerEcon, by = c("screen_name" = "quoted_screen_name")) %>% 
  left_join(retweetsPerEcon, by = c("screen_name" = "retweet_screen_name")) %>% 
  left_join(repliesPerEcon, by = c("screen_name" = "reply_to_screen_name")) %>% 
  left_join(econTimelineTweets %>% filter(is_retweet == F) %>%  group_by(screen_name) %>% summarise(tweets = n())) %>% 
  mutate(screen_name_low = tolower(screen_name)) %>% 
  left_join(genderList %>% mutate(screen_name_low = tolower(screen_name)) %>% select(-screen_name, -vorname, -nachname), by = c("screen_name_low" = "screen_name_low")) %>%
  mutate(pronome = if_else(gender == "male", "his", "her"))  %>% 
  group_by(screen_name) %>% 
  mutate(rt_quotes = sum(quotes, na.rm=T) + sum(retweets, na.rm = T)) %>% 
  arrange(desc(likes))


# Make scatter plot -------------------------------------------------------

IntraBubbleStats %>% 
  group_by(screen_name) %>% 
  summarise(likes = sum(likes, na.rm = T), retweets = sum(c(retweets, quotes), na.rm = T), replies = sum(replies, na.rm = T), score = sum(likes, retweets*3, na.rm = T)) %>% 
  arrange(desc(score)) %>%
  top_n(20, score) %>% 
  ggplot(aes(x=likes, y=retweets, size=replies)) +
  geom_jitter(show.legend = F, color = plot_bg_color) + #, color = "#103C87"
  scale_x_continuous(limits = c(1, max(IntraBubbleStats$likes, na.rm=T) + max(IntraBubbleStats$likes, na.rm=T)*0.3)) + 
  geom_label_repel(aes(label=screen_name), label.size = NA,  show.legend = F, fill = plot_bg_color, color = plot_fg_color,force = 0.15, segment.alpha = 0.1) +
  theme_classic() +
  labs(x="Likes", y="Retweets", 
       title="#EconTwitter: Number of retweets and likes per user name", 
       caption = paste0("Based on ", zeitreferenz1, "'s tweet data\nProudly presented by @EconTweetingBot"),
       subtitle = "Label size shows number of replies\n") +
  theme(legend.position = "none", 
        axis.text = element_text(colour = plot_fg_color, margin=margin(10,10,10,10, unit = "pt")), 
        axis.line = element_line(plot_fg_color), #element_blank(),
        plot.background = element_rect(plot_bg_color), 
        panel.background = element_rect(plot_bg_color), 
        axis.ticks = element_line(plot_fg_color), 
        plot.title = element_text(colour = plot_fg_color), #, hjust = 0.5, face = "bold"
        plot.subtitle = element_text(colour = plot_fg_color, size = 12), #, hjust = 0.5, face = "bold", size = 14
        axis.title = element_text(colour = plot_fg_color, margin=margin(10,10,10,10,"pt")), 
        axis.title.x = element_text(margin=margin(t = 15,"pt"), size = 12), 
        axis.title.y = element_text(margin=margin(r = 15,"pt"), size = 12), 
        text = element_text(size = 16, color = plot_fg_color), 
        plot.margin = unit(c(20,20,0,20), units = "pt"),
        plot.caption = element_text(vjust = 10, size = 8)
  ) +
  ggsave("econBubble.png", dpi = "retina", width = 10, height = 8)

# Prepare data for PageRank calculation ------------------------------------------------------------

FavGraph <-
  TopEconsFavs %>%
  rename(econ_receiver = screen_name, econ_sender = favorited_by) %>% 
  group_by(econ_sender, econ_receiver) %>%
  filter(created_at > startPoint, created_at < paste(today(), "00:00:00"), econ_receiver %in% TopEcons$screen_name, econ_receiver != econ_sender) %>% #
  select(econ_sender, econ_receiver) %>% 
  mutate(type = "likes")

RepGraph <- 
  econTimelineTweets %>% 
  rename(econ_receiver = reply_to_screen_name, econ_sender = screen_name) %>% 
  group_by(econ_sender, econ_receiver) %>% 
  filter(econ_receiver != "NA", econ_receiver %in% TopEcons$screen_name, econ_receiver != econ_sender) %>% 
  select(econ_sender, econ_receiver)  %>% 
  mutate(type = "replies")

RepFavGraph <- bind_rows(FavGraph, RepGraph)

profile_collection <- data.frame(unique(RepFavGraph$econ_receiver))

colnames(profile_collection) <- "screen_name"

for (type in unique(RepFavGraph$type)) {
  
  filter_type <- type
  
  RepFavGraphMatrix <-
    RepFavGraph %>% 
    filter(type == filter_type) %>% 
    as.matrix()
  
  econNetwork <- graph_from_data_frame(RepFavGraphMatrix, directed = TRUE) 
  
  econPageRank <- as.data.frame(page.rank(econNetwork, directed = T, damping = 0.85)$vector)
  
  colnames(econPageRank) <- type
  
  econPageRank <- tibble::rownames_to_column(econPageRank, "screen_name")
  
  profile_collection <- merge(profile_collection, econPageRank, all.x = T)
  
}

RepFavGraphMatrix <-
  RepFavGraph %>% 
  as.matrix()

econNetwork <- graph_from_data_frame(RepFavGraphMatrix, directed = TRUE) 
econPageRank <- as.data.frame(page.rank(econNetwork, directed = T, damping = 0.85)$vector)

colnames(econPageRank) <- "all"

econPageRank <- tibble::rownames_to_column(econPageRank, "screen_name")

profile_collection <- merge(profile_collection, econPageRank, all.x = T)


# Make PageRank plot ------------------------------------------------------

profile_collection %>% 
  mutate(screen_name = fct_reorder(screen_name, all)) %>% 
  top_n(20, all) %>% 
  gather(type, rank, -screen_name) %>%
  ggplot(aes(x = rank, y = screen_name, label=round(rank, 3))) +
  geom_col(fill=plot_fg_color) +
  geom_text(nudge_x = max(profile_collection$all, na.rm=T)*-0.1, size = 3, color = plot_fg_color, fontface = "bold") + #(-0.1*likesPerEcon$n*(40/likesPerEcon$n))
  scale_x_continuous(expand = c(0.001, 0.001), limits = c(0,NA)) +
  facet_grid(.~type) +
  theme_classic() +
  labs(title = "PageRank of #EconTwitter profiles",
       subtitle = paste("Ranking #EconTwitter profiles according their network affinity through likes and replies\n", sep = ""),
       caption = paste("Based on tweet data from:", today()-1, "\nProudly presented by @EconTweetingBot"),
       x = "", y = "") +
  theme(strip.text.x = element_text(size = 11, face = "bold"),
        axis.text = element_text(colour = plot_fg_color), 
        axis.text.x = element_text(size = 7), 
        axis.line = element_blank(),
        plot.margin = unit(c(20,20,10,20), units = "pt"),
        plot.background = element_rect(plot_bg_color), 
        plot.caption = element_text(vjust = 5, size = 10),
        panel.background = element_rect(plot_bg_color),
        axis.ticks = element_line(plot_fg_color), 
        axis.ticks.y = element_blank(),
        plot.title = element_text(colour = plot_fg_color), #, hjust = 0.5, face = "bold")
        plot.subtitle = element_text(colour = plot_fg_color, size = 13), #, hjust = 0.5, face = "bold"
        axis.title = element_text(colour = plot_fg_color, margin=c(10,10,10,10)), 
        text = element_text(size = 20, color = plot_fg_color),
        strip.text = element_text(colour = plot_fg_color),
        strip.background = element_rect(fill = plot_bg_color,colour = plot_fg_color)
  ) +
  ggsave("econPageRank.png", dpi = "retina", width = 12, height = 9)


# Make gender plot -------------------------------------------------------------

genderTweets <-
  genderTweets %>% 
  filter(duplicated(status_id) == FALSE, is_retweet == "FALSE")

genderTweets <- merge(genderTweets, genderList[,c("screen_name","gender")])
genderTweets <- data.frame(genderTweets)

genderTweets %>% 
  mutate(date = strftime(ymd_hms(created_at),'%Y-%m-%d')) %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  group_by(date, gender) %>% 
  summarize(tweetShare = n()/mean(total)) %>% 
  ggplot(aes(x=as.Date(date), y=tweetShare, fill=gender)) +
  geom_area() +
  # geom_line() +
  ggplot2::annotate("text", y=0.2, x=as.Date(max(genderTweets$created_at))-days(15), label= "Men`s tweets", size=5, color=plot_fg_color, face="normal") + 
  ggplot2::annotate("text", y=0.97, x=as.Date(max(genderTweets$created_at))-days(15), label= "Women`s tweets", size=5, color=plot_bg_color, face="normal") + 
  coord_cartesian(expand = 0, clip = "off") +
  scale_fill_manual(values = c(plot_fg_color, plot_bg_color)) + #"skyblue","darkslategray4" #103C87 #D4752F
  geom_hline(aes(yintercept = (1-0.1934), linetype="quota"), color = "#D4752F") +
  ggplot2::annotate("text", y=(1-0.22), x=as.Date(max(genderTweets$created_at))-days(1), label= "List quota", size=4, color="#D4752F", face="normal") + 
  scale_linetype_manual(name = "auota", values = c(1,1), guide= guide_legend(override.aes = list(color = c("#478BFF")))) +
  labs(x="", y="", fill="Gender", title = "Share of Voice by Gender on #EconTwitter", subtitle = "Based on the number of tweets of members of the @MakronomMagazin ranking\n") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 11, face = "bold"),
        axis.text = element_text(colour = plot_fg_color), 
        axis.line = element_line(plot_fg_color), 
        plot.margin = unit(c(20,20,10,20), units = "pt"),
        plot.background = element_rect(plot_bg_color), 
        plot.caption = element_text(vjust = 10, size = 10),
        panel.background = element_rect(plot_bg_color), 
        axis.ticks = element_line(plot_fg_color), 
        axis.ticks.y = element_blank(),
        plot.title = element_text(colour = plot_fg_color), #, hjust = 0.5, face = "bold")
        plot.subtitle = element_text(colour = plot_fg_color, size = 14), #, hjust = 0.5, face = "bold"
        axis.title = element_text(colour = plot_fg_color, margin=c(10,10,10,10)), 
        text = element_text(size = 20, color = plot_fg_color),
        legend.position = "none"
        ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggsave("econGenderQuota.png", dpi = "retina", width = 10, height = 8)
  
genderMean30 <-
genderTweets %>% 
  mutate(date = strftime(ymd_hms(created_at),'%Y-%m-%d')) %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(total = n()) %>% 
  group_by(gender) %>% 
  summarize(tweetShare = n()/mean(total)) 


# Make wordcloud ---------------------------------------------------------------

sw <- tibble(word = stopwords("de", source = "stopwords-iso"))
sw2 <- tibble(word = c(sw$word, "via", "danke", "zb", "frage", "klar", "einfach", "bleib", "leider", "sagen", "genau", "scheint", "eigentlich"))
remove_reg <- "&amp;|&lt;|&gt;"

TopEconsWordcloud <-
  econTimelineTweets %>% 
  filter(is_retweet == FALSE) %>% 
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>% 
  filter(!word %in% sw2$word,
         !word %in% str_remove_all(sw2$word, "'"),
         str_detect(word, "[a-z]"), 
         !word %in% stop_words$word,
        !word %in% str_remove_all(stop_words$word, "'"))

TopEconsWordcloud$word <- 
  TopEconsWordcloud$word %>% 
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S{0}") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%              # Remove any @ mentions
  str_remove("^\\w{1}$") %>%             # Remove any hashtags
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp"))  %>%            # Remove any @ mentions %>% 
  removePunctuation()
  
TopEconsWordcloud %>% 
  group_by(word) %>% 
  count(word, sort = T) %>% 
  filter(!str_detect(word, "^@"), !str_detect(word, "^$"), n > 2, nchar(word) >3) %>% 
  wordcloud2(backgroundColor = plot_bg_color, color = plot_fg_color, gridSize = 30, size = 2) %>% 
  saveWidget(.,"wordcloud.html",selfcontained = F) 

webshot("wordcloud.html", "wordcloud2.png", delay = 5, vwidth = 3500, vheight=3500)


# Make tweets on tweet level ----------------------------------------------

# Collect most important tweets in one df
tweetCollection <- data.frame(mostLikedTweets[which.max(mostLikedTweets$score), c("screen_name", "status_url", "score", "pronome")], type="liked")
tweetCollection <- bind_rows(tweetCollection, 
                             data.frame(mostRetweetedTweet[which.max(mostRetweetedTweet$score), c("screen_name", "status_url", "score", "pronome")], type="retweeted"))
tweetCollection <- bind_rows(tweetCollection, 
                             data.frame(mostQuotedTweet[which.max(mostQuotedTweet$score), c("screen_name", "status_url", "score", "pronome")], type="quoted"))
tweetCollection <- bind_rows(tweetCollection, 
                             data.frame(mostDebatedTweets[which.max(mostDebatedTweets$score), c("screen_name", "status_url", "score", "pronome")], type="replied to"))

# Make name vector for greetings tweets
screenNames <- ""
counter <- 1
for (name in unique(tweetCollection$screen_name)) {
  vec_length <- length(unique(tweetCollection$screen_name))
  if (counter < (vec_length-1)) {
    screenNames <- append(screenNames, paste0("@", name, ", "))
  } else if (counter == (vec_length-1)) {
    screenNames <- append(screenNames, paste0("@", name, " and "))
  } else {
    screenNames <- append(screenNames, paste0("@", name, "."))
  }
  counter <- counter + 1
}

screenNames <- paste(screenNames, collapse = "")

# Make greeting tweet
if (weekdays(today()) == "Sonntag") {
  greetingEcons <- paste("Welcome to the weekly review of the #EconTwitter list by @MakronomMagazin, today with tweets by", screenNames, "#rstats", sep = " ")
} else {
  greetingEcons <- paste("Welcome to the daily review of the #EconTwitter list by @MakronomMagazin, today with tweets by", screenNames, "#rstats", sep = " ")
}

tryCatch({
  post_tweet(greetingEcons, token = token, media = "wordcloud2.png")
  message(greetingEcons)
}, error = function(err) {
  post_message(paste(err), sendMessageToUser)
  message(err)
})

# Bubble post --------------------------------------------------------------------------
  
econBubblePost <- paste0(zeitreferenz, 
                         " @", IntraBubbleStats$screen_name[which.max(IntraBubbleStats$likes)], 
                         " received the most likes, while ",
                         if_else(IntraBubbleStats$screen_name[which.max(IntraBubbleStats$likes)] != IntraBubbleStats$screen_name[which.max(IntraBubbleStats$rt_quotes)], paste0("@", IntraBubbleStats$screen_name[which.max(IntraBubbleStats$rt_quotes)], " was retweeted and quoted most often."),"also being retweeted and quoted most often."),
                         " Thanks also to @", IntraBubbleStats$screen_name[which.max(IntraBubbleStats$replies)],
                         " for keeping the debate going!")

tryCatch(myReplyPost(econBubblePost, media = c("econBubble.png")))

# PageRank Post --------------------------------------------------------------------------
  
# Make PageRank post
rankPost <- paste0("Using Google`s PageRank algorithm to determine the connectedness betweens #EconTwitter users by considering links through likes and replies, @",
                   econPageRank$screen_name[which.max(econPageRank$all)],
                   " turns out to be of a high rank in ", 
                   zeitreferenz1, 
                   "s discussions."
)

tryCatch(myReplyPost(rankPost, media = c("econPageRank.png")))

# Gender Post --------------------------------------------------------------------------

# Make gender Post
genderPost <- paste0("The female quota of the #EconTwitter list by @MakronomMagazin is 19.34%. ",
                     "The 30-day average share of female tweets is ", percent(genderMean30$tweetShare[genderMean30$gender == "female"]), "."
)
tryCatch(myReplyPost(genderPost, media = c("econGenderQuota.png")))

# Loop through best tweets ------------------------------------------------

counterUrl <- 1
for (tweetUrl in unique(tweetCollection$status_url)) {
  
  typeVec <- tweetCollection$type[tweetCollection$status_url == tweetUrl]
  tweetName <- unique(tweetCollection$screen_name[tweetCollection$status_url == tweetUrl])
  tweetPronome <- unique(tweetCollection$pronome[tweetCollection$status_url == tweetUrl])
  
  tweetTypes <- ""
  if (length(typeVec) > 2) {
    
    counter <- 1
    for (tweetType in typeVec) {
      vec_length <- length(typeVec)
      if (counter < (vec_length-1)) {
        tweetTypes <- append(tweetTypes, paste0(tweetType, ", "))
      } else if (counter == (vec_length-1)) {
        tweetTypes <- append(tweetTypes, paste0(tweetType, " and "))
      } else {
        tweetTypes <- append(tweetTypes, paste0(tweetType))
      }
      counter <- counter + 1
    }
    
    tweetTypes <- paste(tweetTypes, collapse = "")
    
    makePost <- paste(zeitreferenz, " @", 
                      tweetName,
                      "'s tweet was ", 
                      tweetTypes,
                      " most often by ", 
                      if_else(length(tweetPronome)>1, paste(tweetPronome, "fellow"), "the other"), 
                      " economists on the #EconTwitter list. ",
                      tweetUrl, sep = "")
    
    myReplyPost(makePost)
    message(makePost)
    
  } else if (length(typeVec) > 1) {
    
    tweetTypes <- paste(typeVec[1], "and", typeVec[2], collapse = "")
    
    makePost <- paste(zeitreferenz, " @", 
                      tweetName,
                      "'s tweet was ", 
                      tweetTypes,
                      " most often by ", 
                      if_else(length(tweetPronome)>1, paste(tweetPronome, "fellow"), "the other"), 
                      " economists on the #EconTwitter list. ",
                      tweetUrl, sep = "")
    
    myReplyPost(makePost)
    message(makePost)
    
  } else {
    
    tweetTypes <- typeVec
    
    makePost <- paste(zeitreferenz, " @", 
                      tweetName,
                      "'s tweet was ", 
                      tweetTypes,
                      " most often by ", 
                      if_else(length(tweetPronome)>1, paste(tweetPronome, "fellow"), "the other"), 
                      " economists on the #EconTwitter list. ",
                      tweetUrl, sep = "")
    
    myReplyPost(makePost)
    message(makePost)
    
  }
  
  counterUrl <- counterUrl + 1
  
}

dbDisconnect(con)

