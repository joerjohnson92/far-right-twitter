#Regressions
library(dplyr)
library(ggplot2)
library(reshape2)
library(quantreg)
library(gridExtra)
library(stargazer)

# AR Scoing plots
#load file
load("data/AR_Following_Ranking_remove_trump.RData")
load("data/AR_Following_Ranking_w_trump.RData")

#load regression
load("data/regression_estimate_scores_noTrump.RData")
load("data/regression_estimate_scores_w_trump.RData")

load("data/TeaParty.RData")
load("data/NeverTrump.RData")

# Regressions
cands_AR_Ranking_no_trump <- cands_AR_Ranking_no_trump %>% mutate(TeaParty = bonica_rid %in% TeaParty)
cands_AR_Ranking_no_trump <- cands_AR_Ranking_no_trump %>% mutate(NeverTrump = bonica_rid %in% NeverTrump)

cands_AR_Ranking_reg <-cands_AR_Ranking_no_trump %>% filter(AR_Score > -1)

lm_DW <- lm(formula = AR_Score ~ dwnom1, data = cands_AR_Ranking_reg )
lm_CF <- lm(formula = AR_Score ~ recipient.cfscore, data = cands_AR_Ranking_reg )
lm_TP <- lm(formula = AR_Score ~ TeaParty, data = cands_AR_Ranking_reg )
lm_NT <- lm(formula = AR_Score ~ NeverTrump, data = cands_AR_Ranking_reg )
lm_all <- lm(formula = AR_Score ~ recipient.cfscore + NeverTrump + TeaParty, data = cands_AR_Ranking_reg )

stargazer(lm_DW, lm_CF, lm_TP, lm_NT, lm_all, type = "latex", covariate.labels = c("DW-NOMINATE",  "CF-Score", "Tea Party",  "Never Trump"), 
            omit.stat=c("LL","ser","f"), dep.var.labels = "Alt-Right Score")


