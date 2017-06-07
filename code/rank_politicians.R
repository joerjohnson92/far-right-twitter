# Code to estimate alt-right scores from politician's twitter followers
# Due to large DB size many of these commands take a long time to run
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
library(splines)
library(DBI)
library(quantreg)

oauthFolder = "auth2"

alt_right_db <- src_sqlite("data/alt_right_db_2.sqlite3", create = F)

#count total followers for each candidate
query <- "SELECT i.bonica_rid, COUNT(i.TWitter_ID) FROM candidate_followers as i 
INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99 GROUP BY i.bonica_rid"
total_cand_followers <- tbl(alt_right_db, sql(query))
total_cand_followers_df <- as.data.frame(total_cand_followers)

#followers of both GOP candidates and Alt-Right accounts
cand_followers <- tbl(alt_right_db, sql("SELECT * FROM candidate_followers  as i 
INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99"))

AR_followers <- tbl(alt_right_db, sql("SELECT DISTINCT TWitter_ID FROM alt_right_followers as i 
INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99"))
intersect_followers <- inner_join(cand_followers, AR_followers, by = "TWitter_ID" )

intersect_followers_total <- intersect_followers %>% dplyr::group_by(bonica_rid) %>% dplyr::summarise(total = n())
intersect_followers_total <- as.data.frame(intersect_followers_total)

AR_Ranking <- left_join(total_cand_followers_df, intersect_followers_total, by = "bonica_rid")
names(AR_Ranking)[2:3] = c("Total_Followers", "Total_AR_Followers")

#Fix trump label
AR_Ranking[1,1] <- "cand100270"


# Trump surrogate scoring for family members and campaign associates
total_surr_followers <- tbl(alt_right_db, sql("SELECT Non_Candidate, COUNT(TWitter_ID) FROM 
                                              non_candidate_followers as i 
                                              INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99 AND (i.Non_Candidate =
                                              'EricTrump' or i.Non_Candidate = 'DonaldJTrumpJr'
                                              or i.Non_Candidate = 'IvankaTrump' or i.Non_Candidate = 'KellyannePolls') GROUP BY i.Non_Candidate"))
total_surr_followers <- as.data.frame(total_surr_followers)

surr_followers <- tbl(alt_right_db, sql("SELECT * FROM non_candidate_followers as i 
                                        INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99 AND (i.Non_Candidate =
                                        'EricTrump' or i.Non_Candidate = 'DonaldJTrumpJr'
                                        or i.Non_Candidate = 'IvankaTrump'  or i.Non_Candidate = 'KellyannePolls')"))

intersect_nc_followers <- inner_join(surr_followers, AR_followers, by = "TWitter_ID" )
intersect_nc_followers_total <- intersect_nc_followers %>% dplyr::group_by(Non_Candidate) %>% dplyr::summarise(total = n())
intersect_nc_followers_total <- as.data.frame(intersect_nc_followers_total)

AR_Ranking_nc <- left_join(total_surr_followers, intersect_nc_followers_total, by = "Non_Candidate")
# rename to combine with candidates
names(AR_Ranking_nc) <- c("name", "Total_Followers", "Total_AR_Followers")


# Combine with ideological and demographic data on candidates
load("data/dime_recipients_all_1979_2014.Rdata")
cands.all$cycle <- as.numeric(cands.all$cycle)
cands.all <- cands.all %>% 
    select(bonica.rid, name, lname, fname, state, recipient.cfscore, dwnom1, dwnom2, seat, cycle, cand.gender, seat) %>%
    arrange(bonica.rid, desc(cycle)) %>% 
    group_by(bonica.rid) %>% 
    filter(row_number() == 1)

#Fix up presidential candidates that are missing from the candidate database
carson <- list(bonica.rid = "carson", name = "ben carson", lname = "carson", fname = "ben", state = "MD", seat = "federal:president", cycle = 2016)
mcmullin <- list(bonica.rid = "mcmullin", name = "evan mcmullin", lname = "mcmullin", fname = "evan", state = "UT", seat = "federal:president", cycle = 2016)
kasich <- list(bonica.rid = "kasich", name = "john kasich", lname = "kasich", fname = "john", state = "OH", seat = "federal:president", cycle = 2016)

cands.all <- bind_rows(cands.all, carson, mcmullin, kasich)

AR_Ranking[AR_Ranking$bonica_rid == "fiorina", 1]  <- as.character((cands.all %>% filter(lname == "fiorina"))[1,1])
AR_Ranking[AR_Ranking$bonica_rid == "christie", 1] <- as.character((cands.all %>% filter(lname == "christie" & state == "NJ"))[1,1] )
AR_Ranking[AR_Ranking$bonica_rid == "gilmore", 1]  <- as.character((cands.all %>% filter(lname == "gilmore"))[1,1])
AR_Ranking[AR_Ranking$bonica_rid == "walker", 1]  <-  as.character((cands.all %>% filter(lname == "walker" & fname == "scott" & state == "WI"))[1,1])

names(cands.all)[1] <- "bonica_rid"

#merge data
cands_AR_Ranking <- inner_join(cands.all, AR_Ranking, by = "bonica_rid") %>% filter(seat == "federal:senate" & state == "TX" & lname == "christie")
cands_AR_Ranking <- as.data.frame(cands_AR_Ranking)

# Fraction of account followers that are alt-right
cands_AR_Ranking <- cands_AR_Ranking %>% mutate(Cand_Frac_AR = Total_AR_Followers / Total_Followers)

#Get get percent of alt-right followers that follow candidates
TOTAL_AR <- tbl(alt_right_db, sql("SELECT COUNT(DISTINCT TWitter_ID) FROM alt_right_followers  as i 
INNER JOIN user_data as ii ON ii.id_str = i.TWitter_ID WHERE ii.followers_count > 24 AND ii.statuses_count > 99"))
TOTAL_AR <- as.data.frame(TOTAL_AR)[1,1]
cands_AR_Ranking <- cands_AR_Ranking %>% mutate(AR_Frac_Cand = Total_AR_Followers / TOTAL_AR)

# Restrict to candidates with over 1000 followers
cands_AR_Ranking <- cands_AR_Ranking %>% filter(Total_Followers > 1000)
#Remove sarah palin and house GOP (could be interesting later on)
cands_AR_Ranking <- cands_AR_Ranking %>% filter(bonica_rid != "cand143184")
cands_AR_Ranking <- cands_AR_Ranking %>% filter(bonica_rid != "cand23468")

# Generate a score based on their popularity - use splines to capture nonlinearity and quantile regression for outliers
cands_AR_Ranking_no_trump <- cands_AR_Ranking %>% filter(lname != "trump"& fname != "donald")

scoreReg <- rq(log(AR_Frac_Cand) ~ ns(log(Total_Followers), 3) , data = cands_AR_Ranking_no_trump)
scoreRegTrump <- rq(log(AR_Frac_Cand) ~ ns(log(Total_Followers), 3) , data = cands_AR_Ranking)

scoreReg <- rq(log(Total_AR_Followers) ~ ns(log(Total_Followers), 3) , data = cands_AR_Ranking_no_trump)
scoreRegTrump <- rq(log(Total_AR_Followers) ~ ns(log(Total_Followers), 3) , data = cands_AR_Ranking)

estimated <- predict(scoreReg)
cands_AR_Ranking_no_trump$AR_Score = log(cands_AR_Ranking_no_trump$Total_AR_Followers) - estimated
cands_AR_Ranking_no_trump$Residual = (estimated - log(cands_AR_Ranking_no_trump$Total_AR_Followers))^2

estimated <- predict(scoreRegTrump)
cands_AR_Ranking$AR_Score = log(cands_AR_Ranking$Total_AR_Followers) - estimated
cands_AR_Ranking$Residual = (estimated - log(cands_AR_Ranking$Total_AR_Followers))^2

# Trump surrogates
estimated <- predict(scoreReg, newdata = AR_Ranking_nc)
AR_Ranking_nc$AR_Score = log(AR_Ranking_nc$Total_AR_Followers) - estimated
AR_Ranking_nc$Residual = (estimated - log(AR_Ranking_nc$Total_AR_Followers))^2

#Save file
save(cands_AR_Ranking_no_trump, file = "data/AR_Following_Ranking_remove_trump.RData")
save(cands_AR_Ranking, file = "data/AR_Following_Ranking_w_trump.RData")
save(AR_Ranking_nc, file = "data/AR_Following_Ranking_trump_surrogates.RData")

#Save regression
save(scoreReg, file = "data/regression_estimate_scores_noTrump.RData")
save(scoreRegTrump, file = "data/regression_estimate_scores_w_trump.RData")

