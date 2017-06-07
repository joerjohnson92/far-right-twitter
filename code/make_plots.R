#Plots
library(dplyr)
library(ggplot2)
library(reshape2)
library(quantreg)
library(gridExtra)

# AR Scoing plots
#load data
load("data/AR_Following_Ranking_remove_trump.RData")
load("data/AR_Following_Ranking_w_trump.RData")

#load regression
load("data/regression_estimate_scores_noTrump.RData")
load("data/regression_estimate_scores_w_trump.RData")

#Plot alt-right fractions and popularity

#Raw data
pdf(file = "results/Trump_Raw.pdf", width=6,   height=6)
ggplot(data = cands_AR_Ranking, aes(x = Total_Followers, y = Total_AR_Followers)) + geom_point(alpha = .9) + 
  xlab("Total Followers") + ylab("Total Alt Right Followers")
dev.off()

#Without Trump
estimated <- predict(scoreReg)
prd <- data.frame(estimated, Followers = log(cands_AR_Ranking_no_trump$Total_Followers))

pdf(file = "results/No_Trump_Reg.pdf", width=6,   height=6)
ggplot(data = cands_AR_Ranking_no_trump, aes(x = log(Total_Followers), y = log(Total_AR_Followers))) + 
  geom_point(alpha = .9) + 
  geom_line(data = prd, aes(x = Followers, y = estimated), col = "firebrick2", size = 1) + 
  xlab("log Total Followers") + ylab("log Total Alt Right Followers")
dev.off()


#With Trump
estimated <- predict(scoreRegTrump)
prd <- data.frame(estimated, Followers = log(cands_AR_Ranking$Total_Followers))

svg(file = "results/Trump_Reg.pdf", width=6,   height=6)
ggplot(data = cands_AR_Ranking, aes(x = log(Total_Followers), y = log(Total_AR_Followers))) + 
  geom_point(alpha = .9) + 
  geom_line(data = prd, aes(x = Followers, y = estimated), col = "firebrick2", size = 1) + 
  xlab("log Total Followers") + ylab("log Total Alt Right Followers")
dev.off()

# Plot scores for different groups 
# Main one - 2016 presidential election 
# Get candidates 
cands_AR_Ranking_no_trump <- cands_AR_Ranking_no_trump %>% mutate(clean_name = stringr::str_to_title(paste(fname, lname)))

last_name <- c("mcmullin", "trump", "cruz", "rubio", "carson", "bush","kasich", "gilmore", "christie", "fiorina", "santorum", "paul", "huckabee", "perry", "jindal", "santorum", "pataki")
presidential_cands <- cands_AR_Ranking_no_trump %>% filter(lname %in% last_name) %>% filter(fname != "joe")
presidential_cands$fname <- ifelse(presidential_cands$fname == "rafael", "ted", presidential_cands$fname) 
presidential_cands$fname <- ifelse(presidential_cands$fname == "richard", "rick", presidential_cands$fname) 
presidential_cands$fname <- ifelse(presidential_cands$fname == "james", "rick", presidential_cands$fname)
presidential_cands$fname <- ifelse(presidential_cands$fname == "chris & guadagno", "chris", presidential_cands$fname)
presidential_cands$fname <- ifelse(presidential_cands$fname == "william", "jim", presidential_cands$fname)
presidential_cands$fname <- ifelse(presidential_cands$fname == "john & taylor", "john", presidential_cands$fname)
#Add gary johnson
presidential_cands <- presidential_cands %>% bind_rows(cands_AR_Ranking_no_trump %>% filter(fname == "gary" & lname == "johnson"))

presidential_cands <- presidential_cands %>% mutate(clean_name = stringr::str_to_title(paste(fname, lname)))

pdf(file="results/2016_GOP_Pres.pdf", width=12,   height=6)

ggplot(data = presidential_cands, aes(x = reorder(clean_name, AR_Score), y = AR_Score)) + 
  geom_bar(stat='identity', color = "firebrick2", fill = "firebrick3")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_hline() + 
  xlab("") + ylab("Alt-Right Score")
dev.off()

load("data/AR_Following_Ranking_trump_surrogates.RData")
AR_Ranking_nc$clean_name <- c("Donald Trump Jr.", "Eric Trump", "Ivanka Trump", "Kellyanne Conway")
pdf(file="results/2016_GOP_Pres_Trump_Surrogates.pdf", width=12,   height=6)
presidential_cands %>% bind_rows(AR_Ranking_nc) %>% 
  ggplot(aes(x = reorder(clean_name, AR_Score), y = AR_Score)) + 
    geom_bar(stat='identity', color = "firebrick2", fill = "firebrick3")  + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_hline() + 
    xlab("") + ylab("Alt-Right Score")
dev.off()

# Senators - top/bottom 20
senators <- cands_AR_Ranking_no_trump %>% filter(seat == "federal:senate") %>% filter(Total_Followers > 10000) %>% 
  filter(lname != "fiorina") # Remove carly fiorina since she was a candidate from 2010
senators$fname <- ifelse(senators$fname == "rafael", "ted", senators$fname) 
senators$fname <- ifelse(senators$fname == "charles", "chuck", senators$fname) 
senators$fname <- ifelse(senators$fname == "timothy", "tim", senators$fname) 

senators <- senators %>% mutate(clean_name = stringr::str_to_title(paste(fname, lname)))

n_senate <- nrow(senators)

pdf(file = "results/Senators.pdf", width=12,   height=6)
ggplot(data = senators #%>% slice(c(1:10, (n_senate-10):n_senate)) 
       , aes(x = reorder(clean_name, AR_Score), y = AR_Score)) + 
  geom_bar(stat='identity', color = "firebrick2", fill = "firebrick3")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_hline() + 
  xlab("") + ylab("Alt-Right Score")
dev.off() 


# Overall - top/bottom 20 
n_total <- nrow(cands_AR_Ranking_no_trump)
pdf(file="results/Bottom20.pdf", width=12,   height=6)
ggplot(data = cands_AR_Ranking_no_trump %>% arrange(AR_Score) %>% slice(c(1:20))
       , aes(x = reorder(clean_name, AR_Score), y = AR_Score)) + 
  geom_bar(stat='identity', color = "firebrick2", fill = "firebrick3")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_hline() + 
  xlab("") + ylab("Alt-Right Score")
dev.off() 

pdf(file="results/Top20.pdf", width=12,   height=6)
ggplot(data = cands_AR_Ranking_no_trump %>% arrange(AR_Score) %>% slice(c((n_total-20):n_total))
       , aes(x = reorder(clean_name, AR_Score), y = AR_Score)) + 
  geom_bar(stat='identity', color = "firebrick2", fill = "firebrick3")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_hline() + 
  xlab("") + ylab("Alt-Right Score")
dev.off() 

# Media and Interest Groups
load("data/media_ranking.RData")

#Plot a smaller subset: only media organizations
AR_Ranking_media_2 <- AR_Ranking_media %>% filter(!(Account %in% c("FocusFamily", "NRA", "FedSoc", "USChamber", "heritage", "FRCdc")))
AR_Ranking_media_2
pdf(file="results/Media.pdf", width=12,   height=6)
ggplot(data = AR_Ranking_media_2, aes(x = reorder(media_names, AR_Score), y = AR_Score)) + 
  geom_bar(stat='identity', fill = "firebrick2", color = "firebrick2")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Alt-Right Score")
dev.off() 

# Ideology plots 

g1 <-  cands_AR_Ranking_no_trump %>% filter(AR_Score > -1) %>% 
  ggplot( aes(x = dwnom1 , y = AR_Score)) + 
  geom_point() +  theme_bw() +
  geom_smooth(method='lm')  + ylab("Alt-Right Score") + xlab("DW-Nominate Score")

g2 <- cands_AR_Ranking_no_trump %>% filter(AR_Score > -1 & recipient.cfscore < 2) %>% 
  ggplot(aes(x = recipient.cfscore , y = AR_Score)) + 
  geom_point() + theme_bw() + 
  geom_smooth(method='lm') + ylab("") + xlab("Campaign Finance Score") 

pdf(file = "results/Ideology.pdf", width=12,   height=6)
grid.arrange(g1, g2, ncol = 2)
dev.off() 

# Bonica issue scores - data received on request from Adam Bonica
load("data/svr_results.rda")

data <- svm.results$rc_issue_score_immigration$omat2
data <- cbind(rownames(data), data)
names(data)[1] <- "bonica_rid"
data <- data %>% select(bonica_rid, rc_issue_score_abortion_and_social_conservatism, rc_issue_score_banking_and_finance, 
                        rc_issue_score_economy, rc_issue_score_civil_rights, rc_issue_score_immigration, rc_issue_score_guns, 
                        rc_issue_score_defense_and_foreign_policy, rc_issue_score_intelligence_and_surveillance, rc_issue_score_healthcare, rc_issue_score_womens_issues)
names(data) <- c("bonica_rid", "Abortion + Social Conservatism", "Banking + Finance", "Economy", "Civil Rights", "Immigration", 
                 "Guns", "Defense + National Security", "Intel + Surveillance", "Health Care", "Women's Issues")

#Plot relationship between issue scores and alt-right following
issue_scoring <- cands_AR_Ranking_no_trump %>% select(AR_Score, bonica_rid) %>% inner_join(data)
issue_scoring_m <- melt(issue_scoring, id.vars = c("bonica_rid", "AR_Score"))

pdf(file = "results/Ideal_Points.pdf", width=12,   height=6)
issue_scoring_m %>% filter(AR_Score > -1) %>% ggplot(aes(x = value, y =   AR_Score)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  facet_wrap(ncol = 5, ~ variable, scales = "free") + 
  theme_bw() + ylab("Alt-Right Score") + xlab("Bonica (2016) Legislator Ideal Points")
dev.off()

# Plots with Tone Matrix data
load("Tone_Matrix.RData")
personality <- inner_join(cands_AR_Ranking_no_trump, tone_mat)
personality_no_outliers <- personality %>% filter(AR_Score > -1)

pdf("results/tone.pdf", height = 5, width = 7)
ggplot(personality_no_outliers, aes(x = Anger, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Disgust, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Joy, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Sadness, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Fear, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")

ggplot(personality_no_outliers, aes(x = Analytical, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Tentative, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Confident, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")

ggplot(personality_no_outliers, aes(x = Openness, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Conscientiousness, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Extraversion, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = Agreeableness, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")
ggplot(personality_no_outliers, aes(x = `Emotional Range`, y = AR_Score)) + geom_point() + stat_smooth(method = lm) + theme_bw() + ylab("Alt-Right Score")

dev.off()

