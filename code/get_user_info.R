# Initial code to test extracting the tweets of politician's twitter followers
library(streamR)
library(plyr)
library(twitteR)
library(RJSONIO)
library(ROAuth)
library(smappR)
library(data.table)
library(dplyr)
library(reshape2)
library(stringr)

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3")
oauthFolder = "auth"

#Getting distinct users takes long time - I have saved it to a file to save time
politician <- tbl(alt_right_db, sql("SELECT DISTINCT TWitter_ID FROM candidate_followers"))
politicianD <- as.data.frame(politician)

bi_lower <- 0:floor((nrow(politicianD)) / 1000) * 1000
bi_upper <- 1:floor((nrow(politicianD) ) / 1000) * 1000
bi_upper <- bi_upper - 1
bi_lower <- bi_lower[-length(bi_lower)]
indexes <- 1:floor(nrow(politicianD))
test <- seq(728000, 16461000, by = 1000)
test_1 <- test - 1
test_1 <- test_1[-1]
test_1 <- c(test_1, test_1[length(test_1)] + 1000)

for(i in 1:length(test_1) ){
  user_data <- getUsersBatch( ids = politicianD$TWitter_ID[test[i]:test_1[i]], 
                              oauth_folder = oauthFolder, include_entities = "false", 
                         verbose = FALSE)
  if(i == 1){
   copy_to(alt_right_db, user_data, temporary = FALSE)
  } else {
    db_insert_into( con = alt_right_db$con, table = "user_data", values = user_data)
  }
}

#Alt Right Followers user info
query <- "SELECT TWitter_ID FROM alt_right_followers LEFT JOIN user_data ON user_data.id_str = alt_right_followers.TWitter_ID WHERE user_data.id_str IS NULL"
AR_all <- tbl(alt_right_db, sql(query))
AR_all <- as.data.frame(AR_all)
AR_all <- AR_all[,1]

for(i in 1:10){
  user_data <- getUsersBatch( ids = AR_all[1:100000 + (i - 1)*(100000)],
                              oauth_folder = oauthFolder, include_entities = "false",
                              verbose = FALSE)
  db_insert_into( con = alt_right_db$con, table = "user_data", values = user_data)
}

user_data <- getUsersBatch( ids = AR_all[(1000000 + 1):length(AR_all)],
                            oauth_folder = oauthFolder, include_entities = "false",
                            verbose = FALSE)
db_insert_into( con = alt_right_db$con, table = "user_data", values = user_data)


# Non candidate user info
query <- "SELECT TWitter_ID FROM non_candidate_followers LEFT JOIN user_data ON user_data.id_str = non_candidate_followers.TWitter_ID WHERE user_data.id_str IS NULL"
nonC_follow <- tbl(alt_right_db, sql(query))
nonC_follow <- as.data.frame(nonC_follow)
nonC_follow <- nonC_follow[,1]

for(i in 11:44){
  user_data <- getUsersBatch( ids = nonC_follow[1:100000 + (i - 1)*(100000)], 
                              oauth_folder = oauthFolder, include_entities = "false", 
                              verbose = FALSE)
  db_insert_into( con = alt_right_db$con, table = "user_data", values = user_data)
}

user_data <- getUsersBatch( ids = nonC_follow[(1+(i)*(100000)):4463957], 
                            oauth_folder = oauthFolder, include_entities = "false", 
                            verbose = FALSE)
db_insert_into( con = alt_right_db$con, table = "user_data", values = user_data)



