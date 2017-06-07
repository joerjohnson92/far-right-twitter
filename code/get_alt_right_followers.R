#Get Twitter followers of Alt-Right accounts
library(devtools)
require(streamR)
require(twitteR)
require(RJSONIO)
require(smappR)
require(data.table)
library(dplyr)
library(reshape2)
library(stringr)
library(RSQLite)
library(sqldf)
library(ROAuth)
library(readr)

oauthFolder = "auth"

alt_right_accounts <- read_csv("data/Alt Right Twitter Handles.csv")
alt_right_accounts <- alt_right_accounts %>% mutate(Handle = gsub("@", "", Handle))
sleep_len = 10

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3", create = T)

for(i in 1:22){
  x <- getFollowers(screen_name = as.character(alt_right_accounts[i,1]), 
               oauth_folder=oauthFolder, sleep=sleep_len)
  alt_right_followers <- cbind(x, alt_right_accounts[i,1]) 
  names(alt_right_followers) <- c("TWitter_ID", "AltR_Acc_Handle")
  
  #write to sql 
  if(i == 1){
    copy_to(alt_right_db, alt_right_followers, temporary = FALSE)
  } else {
    db_insert_into( con = alt_right_db$con, table = "alt_right_followers", values = alt_right_followers)
  }
}




