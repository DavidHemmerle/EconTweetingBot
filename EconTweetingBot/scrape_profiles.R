library(rvest)   
library(genderizeR)  
library(rtweet)
library("RMySQL")

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

TopEcons <- lists_members(slug = "econtwitter", owner_user = "EconTweetingBot")

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

# Scrape current Makronom Magazin Einzelwertung page
page <- read_html("https://makronom.de/twitter-ranking-econ-einzelwertung-2-2")

# Identify node path to table with Profiles
ranking_nodes<- 
  page %>% 
  html_nodes("table > tbody > tr > td > span.td-span-name > a") 

# Clean links and isolate profile names
ranking_table <- 
  ranking_nodes %>% 
  html_attr("href") %>% 
  str_remove("https://twitter.com/") %>% 
  str_remove("MakronomMagazin") %>% 
  str_remove("\t") %>% 
  str_remove("\t")

ranking_table <- as.data.frame(ranking_table)

# Add real name to table
ranking_table$name <-
  ranking_nodes %>% 
  html_text(trim = TRUE)

# Split first and surname
ranking_table2 <-
  ranking_table %>% 
  separate(name, c("vorname", "nachname"), sep = " ")

# Iterate over every first name and determin gender with genderize API
for (name in ranking_table2$vorname[is.na(ranking_table2$gender)]) {
  givenNames <- genderizeR::findGivenNames(name, textPrepare = F)
  
  if (nrow(givenNames) < 1) {
    gender <- NA
  } else {
    gender <- givenNames$gender
  }
  
  ranking_table2$gender[ranking_table2$vorname == name] <- unique(gender)
}

# Look at users where gender is NA and set it manually
ranking_table2[is.na(ranking_table2$gender),]
ranking_table2$gender[ranking_table2$ranking_table %in% c("", "")] <- "male"

ranking_table2 <-
ranking_table2 %>% 
  rename(screen_name = ranking_table)

ranking_table_final <- rbind(ranking_table2, ranking_table3)

## Update Original List
post_list(as.character(ranking_table_final$screen_name), list_id = "")

# Get gender quota
ranking_table_final2 %>% 
  group_by(gender) %>% 
  summarise(n = n(), share = n/n())

# Upload to SQL table
ranking_table_final2 %>% 
  dbWriteTable(., conn = con, name = "genderList", overwrite = TRUE, row.names=FALSE)
  

# Funktion für Genderization ----------------------------------------------

makeGender <- function (x) {
  
  myTable <-
    x %>% 
    select(screen_name, name) %>% 
    separate(name, c("vorname", "nachname"), sep = " ")
  
  for (name in myTable$vorname) {
    givenNames <- genderizeR::findGivenNames(name, textPrepare = F)
    
    if (nrow(givenNames) < 1) {
      gender <- NA
    } else {
      gender <- givenNames$gender
    }
    
    myTable$gender[myTable$vorname == name] <- unique(gender)
  }
  
  return(myTable)
  
}
