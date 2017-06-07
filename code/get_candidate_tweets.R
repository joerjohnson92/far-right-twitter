# gather tweets from candidates 

library(streamR)
library(dplyr)
library(twitteR)
library(RJSONIO)
library(ROAuth)
library(smappR)
library(data.table)
library(tm)
library(tokenizers)
library(stringr)
library(glmnet)
library(wordcloud)
library(text2vec)
library(glmnet)

oauthFolder = "auth"

load("data/candidate_handles_gop.rdata")

datalist = list()

n = nrow(candidate_handles)

for(i in 1:n){
  tweets <- try(getTimeline(screen_name = candidate_handles$cand_twitter_handle[i], 
                            filename = "temp.json", 
                            n=3200, sleep = .05, 
                            oauth_folder = oauthFolder))
  
  if(!inherits(tweets, "try-error") ){
    user_tweets_parsed <- parseTweets("temp.json" )

    datalist[[i]] <- cbind(candidate_handles$bonica_rid[i], user_tweets_parsed)
    try(file.remove("temp.json"))
  }
}
tweet_data <- bind_rows(datalist)
save(tweet_data, file = "congress_tweet.RData")

load("congress_tweet.RData")

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3", create = F)

copy_to(alt_right_db, tweet_data, temporary = FALSE)



