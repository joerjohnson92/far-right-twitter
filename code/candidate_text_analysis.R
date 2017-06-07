# word analysis of candidates' tweets 
library(dplyr)
library(data.table)
library(tm)
library(stringr)
library(wordcloud)
library(text2vec)
library(tokenizers)
library(ggplot2)
library(caTools)
library(randomForest)
library(pdp)
library(caret)
library(caTools)
library(cognizer)

load("data/AR_Following_Ranking_remove_trump.RData")

# Join with tweet data 
load("data/congress_tweet.RData")

#convert tweet data to word matrix
tweet_data$text <- gsub('http\\S+\\s*', '', tweet_data$text)
tweet_data$text <- gsub("[\r\n]", "", tweet_data$text)
tweet_data$text <- gsub("RT ", "", tweet_data$text)
tweet_data$text <- gsub("@\\w+ *", "", tweet_data$text)
tweet_data$text <- gsub('[[:punct:]]', '', tweet_data$text)
tweet_data$text <- str_to_lower(tweet_data$text)

#Combine all tweets into one observation per candidate
names(tweet_data)[1] <- "bonica_rid"
agg_tweets <- tweet_data %>% group_by(bonica_rid) %>% summarize(Text =  paste(text, collapse = "")) %>% arrange(bonica_rid)

agg_tweets <- agg_tweets %>% inner_join( cands_AR_Ranking_no_trump %>% select(bonica_rid, AR_Score))

#Convert tweet data to word matrix
words <- tokenize_ngrams(agg_tweets$Text, n = 1, n_min = 1)

tokens <- itoken(words, 
                preprocessor = words, 
                tokenizer = word_tokenizer, 
                progressbar = FALSE)

vocab = create_vocabulary(tokens, stopwords = c(tm::stopwords(), "rt", "https"))
vocab <- prune_vocabulary(vocab, term_count_min = 5L,
                          doc_proportion_max = .5,
                          doc_proportion_min = 0.05)
vectorizer <- vocab_vectorizer(vocab,
                               # don't vectorize input
                               grow_dtm = T,
                               # use window of 5 for context words
                               skip_grams_window = 5L)

wordMatrix = create_dtm(tokens, vectorizer)

dim(wordMatrix)

# Random forest to classify between groups
set.seed(123)
split <- sample.split(agg_tweets$AR_Score, SplitRatio = .8)

train <- as.matrix(wordMatrix[split,])
test <- as.matrix(wordMatrix[-split,])

y <- agg_tweets$AR_Score
y <- as.factor(y < 0)

ytrain <- y[split]
ytest <- y[-split]

# Acuracy
rf_AR <- randomForest(x = train, y = ytrain, ntrees = 200)
pred <- predict(rf_AR, test, type="response", s=cv$lambda.min)
sum(pred == ytest) / length(ytest)

#Variable Importance and partial dependency
importance <- varImp(rf_AR)
important_vars <- head(rownames(importance)[order(-importance$Overall)], 100)

pdp_list <- lapply(important_vars, FUN = function(x) partial(rf_AR, pred.var = x, train = train, prob = T))

pdp_plots <- list()
for(i in 1:9){
  pdp_plots[[i]] <- autoplot(pdp_list[[i]] )
}

pdf(file = "results/PDP_Plots.pdf", width = 12,   height = 3)
grid.arrange(pdp_plots[[1]] + ylab("Conditional P(Alt-Right Score > 0)"), 
             pdp_plots[[2]] + ylab(""), 
             pdp_plots[[3]] + ylab(""), 
             pdp_plots[[4]] + ylab(""), ncol = 4)
dev.off()

#Function to convert pdp object to direction table
direction <- function(pdp_object){
  n <- nrow(pdp_object)
  yhat_low <- pdp_object$yhat[1]
  yhat_high <- pdp_object$yhat[n]
  increasing<- (yhat_high - yhat_low) > 0
  increasing_str <- ifelse(increasing, "+", "-")
  return(c(names(pdp_object)[1], increasing_str))
}

pdp_dir <- as.data.frame(t(sapply(pdp_list, FUN = direction)))
names(pdp_dir) <- c("word", "direction")

write.csv(pdp_dir %>% filter(direction == "-"), file = "results/word_dependencies_neg.csv")
write.csv(pdp_dir %>% filter(direction == "+"), file = "results/word_dependencies_pos.csv")

#Pt 2 - Tone analyzer from Watson (excluded from paper)

#Look up API keys from IBM Watson service
SERVICE_API_KEY = ""
SERVICE_USERNAME_PASSWORD = ""

load("data/AR_Following_Ranking.RData")
load("data/congress_tweet.RData")
ids <- cands_AR_Ranking %>% dplyr::select(bonica_rid)
tweet_data <- tweet_data %>% dplyr::inner_join(ids)

# Weird Twitter character for ellipses needs to be removed
load("weird_letter.RData")
names(tweet_data)[1] <- "bonica_rid"

#Clean up tweet data
tweet_data$text <- gsub('http\\S+\\s*', '', tweet_data$text)
tweet_data$text <- gsub("[\r\n]", "", tweet_data$text)
tweet_data$text <- gsub("RT ", "", tweet_data$text)
tweet_data$text <- gsub("@\\w+ *", "", tweet_data$text)
tweet_data$text <- gsub('[[:punct:]]', '', tweet_data$text)
tweet_data$text <- gsub("[^[:alnum:][:space:]!?]", '', tweet_data$text)
tweet_data$text <- str_trim(qdap::clean(tweet_data$text))
tweet_data$text <- str_replace_all(tweet_data$text, weird_letter, "")
tweet_data <- tweet_data[tweet_data$text != "", ]

agg_tweets <- tweet_data %>% dplyr::group_by(bonica_rid) %>% dplyr::summarize(Text =  paste(text, collapse = " ")) %>% dplyr::arrange(bonica_rid)

tone_mat <- matrix(nrow = nrow(agg_tweets), ncol = 13 )
colnames(tone_mat) <- c("Anger" , "Disgust", "Fear", "Joy" , "Sadness", "Analytical", "Confident", "Tentative", "Openness" , 
                        "Conscientiousness", "Extraversion", "Agreeableness", "Emotional Range")

for(i in 1:nrow(agg_tweets)){ 
  #Shorten text if it is too long or else it will return error
  text <- agg_tweets$Text[i]
  text_short <- ifelse(str_length(text) > 100000, substr(text, 1, 100000), text)
  toneAnalysis <- text_tone(text = text_short, userpwd = SERVICE_USERNAME_PASSWORD, sentences = "FALSE" )
  
  #Extract traits
  emotion <- toneAnalysis[[1]]$document_tone$tone_categories$tones[[1]][,1]
  tone <- toneAnalysis[[1]]$document_tone$tone_categories$tones[[2]][,1]
  personality <- toneAnalysis[[1]]$document_tone$tone_categories$tones[[3]][,1]
  try(tone_mat[i, ] <- c(emotion, tone, personality))
}

save(tone_mat, file = "data/Tone_Matrix.RData")

