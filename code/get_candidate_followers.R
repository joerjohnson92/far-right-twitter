# Get Twitter followers of GOP candidates
 
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

oauthFolder = "auth"

load("data/candidate_handles_gop.rdata")

#Trump needs seperate consideration since too many followers - prone to errors
skip <- "@realDonaldTrump" 
candidate_handles <- candidate_handles %>% filter(!(cand_twitter_handle %in% skip) )

#set time between API calls
sleep_len = 20 

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3", create = F)

for(i in 1:nrow(candidate_handles)){
  x <- try(
     getFollowers(screen_name = candidate_handles[i, 2], 
     oauth_folder=oauthFolder, sleep=sleep_len)
      )
  
  
  if(!inherits(x, "try-error") & length(candidate_followers ) > 1){
    
    candidate_followers <- as.data.frame(cbind(x, candidate_handles[i, 1]))
    names(candidate_followers) <- c("TWitter_ID", "bonica_rid")
    
    #write to sql 
    if(i == 1){
      copy_to(alt_right_db, candidate_followers, temporary = FALSE)
    } else {
      db_insert_into( con = alt_right_db$con, table = "candidate_followers", values = candidate_followers)
    }
  }
  if(i %% 25 == 0){
    print(paste("candidate", i, "completed"))
  }
}

#Seperate code for Trump since he has far more followers than other candidates
# In case of crashing - rerun with latest cursor 
# Writes Trump's followers to the DB as the function runs 
source("code/getFollowers_Modified.R")
x <-  getFollowers_Trump(screen_name = "realDonaldTrump",
                      oauth_folder=oauthFolder, sleep=20, database = alt_right_db, cursor =  -1)

