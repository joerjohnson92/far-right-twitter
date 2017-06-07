# get Twitter followers from conservative news outlets and some Trump campaign surrogates
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
sleep_len = 15

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3", create = F)

accounts <- c("reason", "NRO", "WSJopinion", "theblaze", "DRUDGE_REPORT", "foxnewspolitics", 
             "CNNpolitics", "MSNBC", "seanhannity", "rushlimbaugh", "RedState", "glennbeck", 
             "AnnCoulter", "IngrahamAngle", "townhallcom", "USChamber", 
             "EricTrump", "DonaldJTrumpJr", "IvankaTrump", "KellyannePolls", "TomiLahren", "heritage", "weeklystandard", "NRA", "FocusFamily",
              "FedSoc", "nytdavidbrooks", "FRCdc")

for(i in 1:length(accounts) ){
  x <- getFollowers(screen_name = accounts[i],
               oauth_folder=oauthFolder, sleep=sleep_len)
  non_candidate_followers <- as.data.frame(cbind(x, accounts[i]))
  names(non_candidate_followers) <- c("TWitter_ID", "Non_Candidate")
  
  if(i == 1){
    copy_to(alt_right_db, non_candidate_followers, temporary = FALSE)
  } else {
    db_insert_into( con = alt_right_db$con, table = "non_candidate_followers", values = non_candidate_followers)
  }
}

