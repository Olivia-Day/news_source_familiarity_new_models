###
#R Script to News Reputations and Selections
###

library(lfe)
library(plyr)
library(xtable)
library(stargazer)
library(apsrtable)
library(lme4)

###
#News Choice by Familiarity
###

load('news.choice.frame.RData')

# ---------------4.1 Model 4: New analysis--------------
# This new model is related to table 2 in the article
familiarity.choice.model.outlet1 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Newspaper Local")) 
familiarity.choice.model.outlet2 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Huffington Post"))
familiarity.choice.model.outlet3 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="New York Times"))
familiarity.choice.model.outlet4 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Nonprofit Local"))
familiarity.choice.model.outlet5 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Fox News"))
familiarity.choice.model.outlet6 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="USA Today"))
familiarity.choice.model.outlet7 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Partisan Local"))
familiarity.choice.model.outlet8 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="Fake Local"))
familiarity.choice.model.outlet9 <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame, subset=which(outlet=="RT"))

# ---------------4.2 Model 4: New analysis table--------------
# table 4
stargazer(familiarity.choice.model.outlet1, familiarity.choice.model.outlet2, familiarity.choice.model.outlet3,
          familiarity.choice.model.outlet4,
          keep=c("aware"),covariate.labels=c("Familiar with Source"),
          column.labels=c("Newspaper Local","Huffington Post", "New York Times", "Nonprofit Local"),
          column.separate=c(1,1),
          notes=c("Robust Standard Errors, Clustered by Respondent"),
          star.cutoffs=c(0.05),notes.align='l',digits=2,title="News Source Choice by Familiarity")
# table 5
stargazer(familiarity.choice.model.outlet5, familiarity.choice.model.outlet6,
          familiarity.choice.model.outlet7, familiarity.choice.model.outlet8, familiarity.choice.model.outlet9,
          keep=c("aware"),covariate.labels=c("Familiar with Source"),
          column.labels=c("Fox News", "USA Today", "Partisan Local", "Fake Local", "RT"),
          column.separate=c(1,1),
          notes=c("Robust Standard Errors, Clustered by Respondent"),
          star.cutoffs=c(0.05),notes.align='l',digits=2,title="News Source Choice by Familiarity")

# ---------------4.3 Model 4: The original analysis--------------
familiarity.choice.model <- felm(selected ~ aware | 0 | 0 | rid,data=news.choice.frame)
summary.familiarity.choice.model <- summary(familiarity.choice.model)

# point estimate
summary.familiarity.choice.model$coefficients['aware', 'Estimate']
# standard error
summary.familiarity.choice.model$coefficients['aware', 'Cluster s.e.']

