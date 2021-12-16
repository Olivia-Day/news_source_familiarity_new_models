###
#R Script to Estimate Effects of News Coverage on Public Opinion
###

library(plyr)
library(metafor)
library(xtable)
library(apsrtable)


load('survey1.RData')
load('survey2.RData')


##
#Overall Effects of Familiar and Unfamiliar Sources on Public Opinion
##
# ---------------Model 1--------------------
# -------1.1 convert age column to a categorical variable----------
range(survey1$age)
survey1$age_category <- cut(survey1$age, breaks = c(17, 30, 41, 74, 96))

range(survey2$age)
survey2$age_category <- cut(survey2$age, breaks = c(17, 30, 41, 74, 92))

# -------1.2 convert ethnicity columns to a categorical variable----------
# data sets have 4 columns recording ethnicity
# convert them into a categorical variable called ethnicity

survey1$ethnicity <- ifelse(survey1$black == 1, 'black',
                            ifelse(survey1$hispanic.binary == 1, 'hispanic.binary',
                                   ifelse(survey1$white == 1, 'white',
                                          ifelse(survey1$other.race == 1, 'other.race', 0))))

survey2$ethnicity <- ifelse(survey2$black == 1, 'black',
                            ifelse(survey2$hispanic.binary == 1, 'hispanic.binary',
                                   ifelse(survey2$white == 1, 'white',
                                          ifelse(survey2$other.race == 1, 'other.race', 0))))

#-------1.4 Model 1: A crude model----------
#Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion
study1.reg.crude <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar"),data=survey1)
study2.reg.crude <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + affective.polarization,data=survey1)
study3.reg.crude <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + trump.biden,data=survey1)
study4.reg.crude <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment,data=survey2)
study5.reg.crude <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment,data=survey2)

names(study1.reg.crude$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study2.reg.crude$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study3.reg.crude$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study4.reg.crude$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study5.reg.crude$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')

# table 1
apsrtable(study1.reg.crude, study2.reg.crude, study3.reg.crude, study4.reg.crude, study5.reg.crude,
          coef.names=c("Intercept","Unfamiliar Source","Familiar Source","Affective Polarization","Trump-Biden Therm","Tax Views","Cyber Concern"),
          model.names=c("1","2","3","4","5"))

#-----1.5 The Original Analysis for Model 1 and Model 2--------
#Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion
study1.reg <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text),data=survey1)
study2.reg <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + affective.polarization,data=survey1)
study3.reg <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + trump.biden,data=survey1)
study4.reg <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment + factor(party_text),data=survey2)
study5.reg <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment + factor(party_text),data=survey2)

names(study1.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study2.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study3.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study4.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study5.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')

# table 2
apsrtable(study1.reg,study2.reg,study3.reg,study4.reg,study5.reg,coef.names=c("Intercept","Unfamiliar Source","Familiar Source","Independent","Republican","Affective Polarization","Trump-Biden Therm","Tax Views","Cyber Concern"),
          model.names=c("1","2","3","4","5"))
#-----2.1 Model 2: Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion--------
#Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion
study1.reg.add.new.perdictors <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text) + 
                                            factor(age_category) + factor(ethnicity) + factor(college),
                                    data=survey1)
study2.reg.add.new.perdictors <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + 
                                            factor(age_category) + factor(ethnicity) + factor(college) + affective.polarization,
                                    data=survey1)
study3.reg.add.new.perdictors <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text)+ 
                                            factor(age_category) + factor(ethnicity) + factor(college) + trump.biden,
                                    data=survey1)
study4.reg.add.new.perdictors <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar")+ factor(party_text)+ 
                                            factor(age_category) + factor(ethnicity) + factor(college) + tax.pretreatment,
                                    data=survey2)
study5.reg.add.new.perdictors <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text)+ 
                                            factor(age_category) + factor(ethnicity) + factor(college) + cyber.pretreatment,
                                    data=survey2)

names(study1.reg.add.new.perdictors$coefficients)[2:12] <- c('Unfamiliar Source','Familiar Source', 'Independent', 'Republican',
                                                            "Age Range (30,41]", "Age Range (41,74]", "Age Range (74,96]", "Hispanic", "Other race", "White", "College")
names(study2.reg.add.new.perdictors$coefficients)[2:12] <- c('Unfamiliar Source','Familiar Source', 'Independent', 'Republican',
                                                             "Age Range (30,41]", "Age Range (41,74]", "Age Range (74,96]", "Hispanic", "Other race", "White", "College")
names(study3.reg.add.new.perdictors$coefficients)[2:12] <- c('Unfamiliar Source','Familiar Source', 'Independent', 'Republican',
                                                             "Age Range (30,41]", "Age Range (41,74]", "Age Range (74,96]", "Hispanic", "Other race", "White", "College")
names(study4.reg.add.new.perdictors$coefficients)[2:12] <- c('Unfamiliar Source','Familiar Source', 'Independent', 'Republican',
                                                             "Age Range (30,41]", "Age Range (41,74]", "Age Range (74,96]", "Hispanic", "Other race", "White", "College")
names(study5.reg.add.new.perdictors$coefficients)[2:12] <- c('Unfamiliar Source','Familiar Source', 'Independent', 'Republican',
                                                             "Age Range (30,41]", "Age Range (41,74]", "Age Range (74,96]", "Hispanic", "Other race", "White", "College")
# table 3
apsrtable(study1.reg.add.new.perdictors, study2.reg.add.new.perdictors, study3.reg.add.new.perdictors, study4.reg.add.new.perdictors, study5.reg.add.new.perdictors,
          model.names=c("1","2","3","4","5"))
#--------------Model 3------------
#-------------3.1 Model 3: Preparation---------------------
# figure 1
# QQ plots of the original regressions
pdf("figure1-model3.pdf",height=6,width=12)
par(mar=c(4.25,7.35,7.35,0.4),mfrow=c(2,3))
plot(study1.reg, which = 2, main = "Study 1", cex.lab=1.8,cex.main=1.8)
plot(study2.reg, which = 2, main = "Study 2", cex.lab=1.8,cex.main=1.8)
plot(study3.reg, which = 2, main = "Study 3", cex.lab=1.8,cex.main=1.8)
plot(study4.reg, which = 2, main = "Study 4", cex.lab=1.8,cex.main=1.8)
plot(study5.reg, which = 2, main = "Study 5", cex.lab=1.8,cex.main=1.8)
dev.off()
# QQ plots shows that the errors may not follow normal distribution

#-------------3.2 Model 3: Original regression---------------------
#Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion
study1.reg <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text),data=survey1)
study2.reg <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + affective.polarization,data=survey1)
study3.reg <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + trump.biden,data=survey1)
study4.reg <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment + factor(party_text),data=survey2)
study5.reg <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment + factor(party_text),data=survey2)

names(study1.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study2.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study3.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study4.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
names(study5.reg$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')

unfamiliar.source.1.point <- study1.reg$coefficients[2]
unfamiliar.source.1.var <- vcov(study1.reg)[2,2]
unfamiliar.source.1.se <- sqrt(vcov(study1.reg)[2,2])

unfamiliar.source.2.point <- study2.reg$coefficients[2]
unfamiliar.source.2.var <- vcov(study2.reg)[2,2]
unfamiliar.source.2.se <- sqrt(vcov(study2.reg)[2,2])

unfamiliar.source.3.point <- study3.reg$coefficients[2]
unfamiliar.source.3.var <- vcov(study3.reg)[2,2]
unfamiliar.source.3.se <- sqrt(vcov(study3.reg)[2,2])

unfamiliar.source.4.point <- study4.reg$coefficients[2]
unfamiliar.source.4.var <- vcov(study4.reg)[2,2]
unfamiliar.source.4.se <- sqrt(vcov(study4.reg)[2,2])

unfamiliar.source.5.point <- study5.reg$coefficients[2]
unfamiliar.source.5.var <- vcov(study5.reg)[2,2]
unfamiliar.source.5.se <- sqrt(vcov(study5.reg)[2,2])

unfamiliar.est <- c(unfamiliar.source.1.point,unfamiliar.source.2.point,unfamiliar.source.3.point,unfamiliar.source.4.point,unfamiliar.source.5.point)
unfamiliar.var <- c(unfamiliar.source.1.var,unfamiliar.source.2.var,unfamiliar.source.3.var,unfamiliar.source.4.var,unfamiliar.source.5.var)
unfamiliar.se <- c(unfamiliar.source.1.se,unfamiliar.source.2.se,unfamiliar.source.3.se,unfamiliar.source.4.se,unfamiliar.source.5.se)

unfamiliar.effect.frame <- cbind.data.frame(unfamiliar.est,unfamiliar.var,unfamiliar.se)

unfamiliar.meta <- rma(yi=unfamiliar.effect.frame[,1],vi=unfamiliar.effect.frame[,2],method="FE",data=unfamiliar.effect.frame)

unfamiliar.meta.row <- c(unfamiliar.meta$beta,sqrt(unfamiliar.meta$se),unfamiliar.meta$se)
unfamiliar.effect.frame <- rbind(unfamiliar.effect.frame,unfamiliar.meta.row)
unfamiliar.effect.frame$lower <- unfamiliar.effect.frame[,1] - 1.96*unfamiliar.effect.frame[,3]
unfamiliar.effect.frame$upper <- unfamiliar.effect.frame[,1] + 1.96*unfamiliar.effect.frame[,3]

familiar.source.1.point <- study1.reg$coefficients[3]
familiar.source.1.var <- vcov(study1.reg)[3,3]
familiar.source.1.se <- sqrt(vcov(study1.reg)[3,3])

familiar.source.2.point <- study2.reg$coefficients[3]
familiar.source.2.var <- vcov(study2.reg)[3,3]
familiar.source.2.se <- sqrt(vcov(study2.reg)[3,3])

familiar.source.3.point <- study3.reg$coefficients[3]
familiar.source.3.var <- vcov(study3.reg)[3,3]
familiar.source.3.se <- sqrt(vcov(study3.reg)[3,3])

familiar.source.4.point <- study4.reg$coefficients[3]
familiar.source.4.var <- vcov(study4.reg)[3,3]
familiar.source.4.se <- sqrt(vcov(study4.reg)[3,3])

familiar.source.5.point <- study5.reg$coefficients[3]
familiar.source.5.var <- vcov(study5.reg)[3,3]
familiar.source.5.se <- sqrt(vcov(study5.reg)[3,3])

familiar.est <- c(familiar.source.1.point,familiar.source.2.point,familiar.source.3.point,familiar.source.4.point,familiar.source.5.point)
familiar.var <- c(familiar.source.1.var,familiar.source.2.var,familiar.source.3.var,familiar.source.4.var,familiar.source.5.var)
familiar.se <- c(familiar.source.1.se,familiar.source.2.se,familiar.source.3.se,familiar.source.4.se,familiar.source.5.se)

familiar.effect.frame <- cbind.data.frame(familiar.est,familiar.var,familiar.se)

familiar.meta <- rma(yi=familiar.effect.frame[,1],vi=familiar.effect.frame[,2],method="FE",data=familiar.effect.frame) # fixed-effect model

familiar.meta.row <- c(familiar.meta$beta,sqrt(familiar.meta$se),familiar.meta$se)
familiar.effect.frame <- rbind(familiar.effect.frame,familiar.meta.row)
familiar.effect.frame$lower <- familiar.effect.frame[,1] - 1.96*familiar.effect.frame[,3]
familiar.effect.frame$upper <- familiar.effect.frame[,1] + 1.96*familiar.effect.frame[,3]

#Difference in Treatment Effects of Familiar and Unfamiliar Sources on Public Opinion
diff1.point <- study1.reg$coefficients[3] - study1.reg$coefficients[2]
diff1.var <- vcov(study1.reg)[3,3] + vcov(study1.reg)[2,2] - 2*vcov(study1.reg)[2,3]
diff1.se <- sqrt(diff1.var)

diff2.point <- study2.reg$coefficients[3] - study2.reg$coefficients[2]
diff2.var <- vcov(study2.reg)[3,3] + vcov(study2.reg)[2,2] - 2*vcov(study2.reg)[2,3]
diff2.se <- sqrt(diff2.var)

diff3.point <- study3.reg$coefficients[3] - study3.reg$coefficients[2]
diff3.var <- vcov(study3.reg)[3,3] + vcov(study3.reg)[2,2] - 2*vcov(study3.reg)[2,3]
diff3.se <- sqrt(diff3.var)

diff4.point <- study4.reg$coefficients[3] - study4.reg$coefficients[2]
diff4.var <- vcov(study4.reg)[3,3] + vcov(study4.reg)[2,2] - 2*vcov(study4.reg)[2,3]
diff4.se <- sqrt(diff4.var)

diff5.point <- study5.reg$coefficients[3] - study5.reg$coefficients[2]
diff5.var <- vcov(study5.reg)[3,3] + vcov(study5.reg)[2,2] - 2*vcov(study5.reg)[2,3]
diff5.se <- sqrt(diff5.var)

combined.frame1 <- familiar.effect.frame[1:5,c('familiar.est','familiar.var')]
names(combined.frame1) <- c('est','var')
combined.frame1$familiar <- 1

combined.frame2 <- unfamiliar.effect.frame[1:5,c('unfamiliar.est','unfamiliar.var')]
names(combined.frame2) <- c('est','var')
combined.frame2$familiar <- 0

message.heterogeneity.frame <- rbind.data.frame(combined.frame1,combined.frame2)
messagediff.meta <- rma(yi=message.heterogeneity.frame[,1],vi=message.heterogeneity.frame[,2],mods= ~ message.heterogeneity.frame[,3], method="FE", data=message.heterogeneity.frame)
interaction.meta.row <- c(messagediff.meta$beta[2],sqrt(messagediff.meta$se[2]),messagediff.meta$se[2])

differences.est <- c(diff1.point,diff2.point,diff3.point,diff4.point,diff5.point,messagediff.meta$beta[2])
differences.var <- c(diff1.var,diff2.var,diff3.var,diff4.var,diff5.var,sqrt(messagediff.meta$se[2]))
differences.se <- c(diff1.se,diff2.se,diff3.se,diff4.se,diff5.se,messagediff.meta$se[2])

interaction.frame <- cbind.data.frame(differences.est,differences.var,differences.se)
interaction.frame$lower <- interaction.frame[,1] - 1.96*interaction.frame[,3]
interaction.frame$upper <- interaction.frame[,1] + 1.96*interaction.frame[,3]
#-------------3.3 Model 3: Bootstrap---------------------
# Use bootstrap to get the confidence intervals
study1.reg.coef <- function(data,indices) {
        coef(lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text),data=survey1[indices, ]))
}
study2.reg.coef <- function(data,indices) {
        coef(lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + affective.polarization,data=survey1[indices, ]))
}
study3.reg.coef <- function(data,indices) {
        coef(lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + trump.biden,data=survey1[indices, ]))
}
study4.reg.coef <- function(data,indices) {
        coef(lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment + factor(party_text),data=survey2[indices, ]))
}
study5.reg.coef <- function(data,indices) {
        coef(lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment + factor(party_text),data=survey2[indices, ]))
}

study1.reg.coef.Boot <- boot::boot(data = survey1, statistic = study1.reg.coef, R = 1000)
study2.reg.coef.Boot <- boot::boot(data = survey1, statistic = study2.reg.coef, R = 1000)
study3.reg.coef.Boot <- boot::boot(data = survey1, statistic = study3.reg.coef, R = 1000)
study4.reg.coef.Boot <- boot::boot(data = survey2, statistic = study4.reg.coef, R = 1000)
study5.reg.coef.Boot <- boot::boot(data = survey2, statistic = study5.reg.coef, R = 1000)

study1.var <- apply(study1.reg.coef.Boot$t[, 2:3], 2, var)
study2.var <- apply(study2.reg.coef.Boot$t[, 2:3], 2, var)
study3.var <- apply(study3.reg.coef.Boot$t[, 2:3], 2, var)
study4.var <- apply(study4.reg.coef.Boot$t[, 2:3], 2, var)
study5.var <- apply(study5.reg.coef.Boot$t[, 2:3], 2, var)

study1.se <- sqrt(study1.var)
study2.se <- sqrt(study2.var)
study3.se <- sqrt(study3.var)
study4.se <- sqrt(study4.var)
study5.se <- sqrt(study5.var)

# unfamiliar
unfamiliar.source.1.Boot.point <- study1.reg.coef.Boot$t0[2]
unfamiliar.source.1.Boot.var <- study1.var[1]
unfamiliar.source.1.Boot.se <- study1.se[1]

unfamiliar.source.2.Boot.point <- study2.reg.coef.Boot$t0[2]
unfamiliar.source.2.Boot.var <- study2.var[1]
unfamiliar.source.2.Boot.se <- study2.se[1]

unfamiliar.source.3.Boot.point <- study3.reg.coef.Boot$t0[2]
unfamiliar.source.3.Boot.var <- study3.var[1]
unfamiliar.source.3.Boot.se <- study3.se[1]

unfamiliar.source.4.Boot.point <- study4.reg.coef.Boot$t0[2]
unfamiliar.source.4.Boot.var <- study4.var[1]
unfamiliar.source.4.Boot.se <- study4.se[1]

unfamiliar.source.5.Boot.point <- study5.reg.coef.Boot$t0[2]
unfamiliar.source.5.Boot.var <- study5.var[1]
unfamiliar.source.5.Boot.se <- study5.se[1]

unfamiliar.est.boot <- c(unfamiliar.source.1.Boot.point,unfamiliar.source.2.Boot.point,unfamiliar.source.3.Boot.point,unfamiliar.source.4.Boot.point,unfamiliar.source.5.Boot.point)
unfamiliar.var.boot <- c(unfamiliar.source.1.Boot.var,unfamiliar.source.2.Boot.var,unfamiliar.source.3.Boot.var,unfamiliar.source.4.Boot.var,unfamiliar.source.5.Boot.var)
unfamiliar.se.boot <- c(unfamiliar.source.1.Boot.se,unfamiliar.source.2.Boot.se,unfamiliar.source.3.Boot.se,unfamiliar.source.4.Boot.se,unfamiliar.source.5.Boot.se)

unfamiliar.effect.frame.boot <- cbind.data.frame(unfamiliar.est.boot,unfamiliar.var.boot,unfamiliar.se.boot)

unfamiliar.meta.boot <- rma(yi=unfamiliar.effect.frame.boot[,1],vi=unfamiliar.effect.frame.boot[,2],method="FE",data=unfamiliar.effect.frame.boot)

unfamiliar.meta.row.boot <- c(unfamiliar.meta.boot$beta,(unfamiliar.meta.boot$se)^2,unfamiliar.meta.boot$se)
unfamiliar.effect.frame.boot <- rbind(unfamiliar.effect.frame.boot,unfamiliar.meta.row.boot)
unfamiliar.effect.frame.boot$lower <- unfamiliar.effect.frame.boot[,1] - 1.96*unfamiliar.effect.frame.boot[,3]
unfamiliar.effect.frame.boot$upper <- unfamiliar.effect.frame.boot[,1] + 1.96*unfamiliar.effect.frame.boot[,3]

# familiar
familiar.source.1.Boot.point <- study1.reg.coef.Boot$t0[3]
familiar.source.1.Boot.var <- study1.var[2]
familiar.source.1.Boot.se <- study1.se[2]

familiar.source.2.Boot.point <- study2.reg.coef.Boot$t0[3]
familiar.source.2.Boot.var <- study2.var[2]
familiar.source.2.Boot.se <- study2.se[2]

familiar.source.3.Boot.point <- study3.reg.coef.Boot$t0[3]
familiar.source.3.Boot.var <- study3.var[2]
familiar.source.3.Boot.se <- study3.se[2]

familiar.source.4.Boot.point <- study4.reg.coef.Boot$t0[3]
familiar.source.4.Boot.var <- study4.var[2]
familiar.source.4.Boot.se <- study4.se[2]

familiar.source.5.Boot.point <- study5.reg.coef.Boot$t0[3]
familiar.source.5.Boot.var <- study5.var[2]
familiar.source.5.Boot.se <- study5.se[2]

familiar.est.boot <- c(familiar.source.1.Boot.point,familiar.source.2.Boot.point,familiar.source.3.Boot.point,familiar.source.4.Boot.point,familiar.source.5.Boot.point)
familiar.var.boot <- c(familiar.source.1.Boot.var,familiar.source.2.Boot.var,familiar.source.3.Boot.var,familiar.source.4.Boot.var,familiar.source.5.Boot.var)
familiar.se.boot <- c(familiar.source.1.Boot.se,familiar.source.2.Boot.se,familiar.source.3.Boot.se,familiar.source.4.Boot.se,familiar.source.5.Boot.se)

familiar.effect.frame.boot <- cbind.data.frame(familiar.est.boot,familiar.var.boot,familiar.se.boot)

familiar.meta.boot <- rma(yi=familiar.effect.frame.boot[,1],vi=familiar.effect.frame.boot[,2],method="FE",data=familiar.effect.frame.boot)

familiar.meta.row.boot <- c(familiar.meta.boot$beta,(familiar.meta.boot$se)^2,familiar.meta.boot$se)
familiar.effect.frame.boot <- rbind(familiar.effect.frame.boot,familiar.meta.row.boot)
familiar.effect.frame.boot$lower <- familiar.effect.frame.boot[,1] - 1.96*familiar.effect.frame.boot[,3]
familiar.effect.frame.boot$upper <- familiar.effect.frame.boot[,1] + 1.96*familiar.effect.frame.boot[,3]

# difference (familiar-unfamiliar)
diff1.point.boot <- study1.reg.coef.Boot$t0[3]-study1.reg.coef.Boot$t0[2]
diff1.var.boot <- sum(study1.var)-2*cov(study1.reg.coef.Boot$t[, 2:3])[1,2]
diff1.se.boot <- sqrt(diff1.var.boot)

diff2.point.boot <- study2.reg.coef.Boot$t0[3]-study2.reg.coef.Boot$t0[2]
diff2.var.boot <- sum(study2.var)-2*cov(study2.reg.coef.Boot$t[, 2:3])[1,2]
diff2.se.boot <- sqrt(diff2.var.boot)

diff3.point.boot <- study3.reg.coef.Boot$t0[3]-study3.reg.coef.Boot$t0[2]
diff3.var.boot <- sum(study3.var)-2*cov(study3.reg.coef.Boot$t[, 2:3])[1,2]
diff3.se.boot <- sqrt(diff3.var.boot)

diff4.point.boot <- study4.reg.coef.Boot$t0[3]-study4.reg.coef.Boot$t0[2]
diff4.var.boot <- sum(study4.var)-2*cov(study4.reg.coef.Boot$t[, 2:3])[1,2]
diff4.se.boot <- sqrt(diff4.var.boot)

diff5.point.boot <- study5.reg.coef.Boot$t0[3]-study5.reg.coef.Boot$t0[2]
diff5.var.boot <- sum(study1.var)-2*cov(study5.reg.coef.Boot$t[, 2:3])[1,2]
diff5.se.boot <- sqrt(diff5.var.boot)

combined.frame1.boot <- familiar.effect.frame.boot[1:5,c('familiar.est.boot','familiar.var.boot')]
names(combined.frame1.boot) <- c('est','var')
combined.frame1.boot$familiar <- 1

combined.frame2.boot <- unfamiliar.effect.frame.boot[1:5,c('unfamiliar.est.boot','unfamiliar.var.boot')]
names(combined.frame2.boot) <- c('est','var')
combined.frame2.boot$familiar <- 0

message.heterogeneity.frame.boot <- rbind.data.frame(combined.frame1.boot,combined.frame2.boot)
messagediff.meta.boot <- rma(yi=message.heterogeneity.frame.boot[,1],vi=message.heterogeneity.frame.boot[,2],mods= ~ message.heterogeneity.frame.boot[,3], method="FE", data=message.heterogeneity.frame.boot)
interaction.meta.row.boot <- c(messagediff.meta.boot$beta[2],sqrt(messagediff.meta.boot$se[2]),messagediff.meta.boot$se[2])

differences.est.boot <- c(diff1.point.boot, diff2.point.boot, diff3.point.boot, diff4.point.boot, diff5.point.boot,messagediff.meta.boot$beta[2])
differences.var.boot <- c(diff1.var.boot,diff2.var.boot,diff3.var.boot,diff4.var.boot,diff5.var.boot,messagediff.meta.boot$se[2]^2)
differences.se.boot <- c(diff1.se.boot,diff2.se.boot,diff3.se.boot,diff4.se.boot,diff5.se.boot,messagediff.meta.boot$se[2])

interaction.frame.boot <- cbind.data.frame(differences.est.boot,differences.var.boot,differences.se.boot)
interaction.frame.boot$lower <- interaction.frame.boot[,1] - 1.96*interaction.frame.boot[,3]
interaction.frame.boot$upper <- interaction.frame.boot[,1] + 1.96*interaction.frame.boot[,3]

#------3.4 Model 3: Replot Figure 1-------------------
# figure 2
pdf("figure1-3.pdf",height=6,width=12)
par(mar=c(4.25,7.35,0.25,0.25),mfrow=c(1,2))
# Left
# Original
plot(y=6:1+.3,x=familiar.effect.frame$familiar.est,pch=16,cex=2.5,xlim=c(-.04,.325),
     yaxt='n',xaxt='n',ylab='',xlab="Effect of News Article on Opinion (SD Units)",
     ylim=c(.5,6.5),main="",cex.lab=1.4,cex.axis=1.4,cex.main=1.4)
abline(v=0)
segments(y0=6:1+.3,x0=familiar.effect.frame$lower,x1=familiar.effect.frame$upper,lwd=5)
points(y=6:1-.15,x=unfamiliar.effect.frame$unfamiliar.est,pch=16,cex=2.5,col='gray50')
segments(y0=6:1-.15,x0=unfamiliar.effect.frame$lower,x1=unfamiliar.effect.frame$upper,lwd=5,col='gray50')

# Bootstrap
points(y=6:1+.15,x=familiar.effect.frame.boot$familiar.est,pch=16,cex=2.5, col = '660066')
abline(v=0)
segments(y0=6:1+.15,x0=familiar.effect.frame.boot$lower,x1=familiar.effect.frame.boot$upper,lwd=5, col = '660066')
points(y=6:1-.3,x=unfamiliar.effect.frame.boot$unfamiliar.est,pch=16,cex=2.5,col='663366')
segments(y0=6:1-.3,x0=unfamiliar.effect.frame.boot$lower,x1=unfamiliar.effect.frame.boot$upper,lwd=5,col='663366')
legend("bottomright",legend=c('Familiar Source','Bootstrap Familiar','Unfamiliar Source', 'Bootstrap Unfamiliar'),
       col=c('black','660066','gray50','663366'),pch=c(16,16,16,16),pt.cex=c(2,2),cex=0.8)
axis(side=2,at=c(6,5,4,3,2,1),
     labels=c("State Tax \n Policy (1)",
              "Perceived \n Polarization",
              "Election \n Integrity",
              "State Tax \n Policy (2)",
              "Cyber \n Security","Pooled"),
     las=1,cex.axis=1.4)
axis(side=1,at=c(0,.1,.2,.3),cex.axis=1.4)

abline(h=1.5,lty=2)

# Right
# Original
plot(y=6:1+.15,x=interaction.frame$differences.est,pch=16,cex=2.5,xlim=c(-.14,.14),yaxt='n',xaxt='n',ylab='',xlab="Difference in Effect (Familiar - Unfamiliar)",ylim=c(.5,6.5),main="",cex.lab=1.4,cex.axis=1.4,cex.main=1.4)
abline(v=0)
segments(y0=6:1+.15,x0=interaction.frame$lower,x1=interaction.frame$upper,lwd=5)

# Bootstrap
points(y=6:1-.15,x=interaction.frame.boot$differences.est,pch=16,cex=2.5, col = '660066')
abline(v=0)
segments(y0=6:1-.15,x0=interaction.frame.boot$lower,x1=interaction.frame.boot$upper,lwd=5, col = '660066')
legend("bottomright",legend=c('Difference','Bootstrap Difference'),
       col=c('black','660066'),pch=c(16,16),pt.cex=c(2,2),cex=0.8)
axis(side=2,at=c(6,5,4,3,2,1),labels=c("State Tax 
Policy (1)","Perceived
Polarization","Election
Integrity","State Tax
Policy (2)","Cyber
Security","Pooled"),las=1,cex.axis=1.4)
axis(side=1,at=c(-.1,0,.1),cex.axis=1.4)
abline(h=1.5,lty=2)
dev.off()



#--------------Model 5------------
# Conditional Effects of Familiar and Unfamiliar Sources on Public Opinion by News Preference
# ----------5.1: Selected----------------
# Effect of Unfamiliar Sources Among Those Who Selected Unfamiliar Sources
study1.reg.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text)+ 
                                           factor(age_category) + factor(ethnicity) + factor(college),
                                   data=survey1,subset=which(survey1$chose.partisan.local.1==1))
names(study1.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study1.select.unfamiliar.share <- mean(survey1$chose.partisan.local.1,na.rm=TRUE)

study2.reg.select.unfamiliar <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + 
                                           factor(age_category) + factor(ethnicity) + factor(college) + affective.polarization,
                                   data=survey1,subset=which(survey1$chose.rt.national==1))
names(study2.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study2.select.unfamiliar.share <- mean(survey1$chose.rt.national,na.rm=TRUE)

study3.reg.select.unfamiliar <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + 
                                           factor(age_category) + factor(ethnicity) + factor(college) + trump.biden,data=survey1,subset=which(survey1$chose.fake.local.2==1))
names(study3.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study3.select.unfamiliar.share <- mean(survey1$chose.fake.local.2,na.rm=TRUE)

study4.reg.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text)+ 
                                           factor(age_category) + factor(ethnicity) + factor(college) + tax.pretreatment,
                                   data=survey2,subset=which(survey2$chose.partisan.local.1==1))
names(study4.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study4.select.unfamiliar.share <- mean(survey2$chose.partisan.local.1==1,na.rm=TRUE)

study5.reg.select.unfamiliar <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text)+ 
                                           factor(age_category) + factor(ethnicity) + factor(college) + cyber.pretreatment,
                                   data=survey2,subset=which(survey2$chose.rt.national==1))
names(study5.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study5.select.unfamiliar.share <- mean(survey2$chose.rt.national==1,na.rm=TRUE)

unfamiliar.selected.source.1.point <- study1.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.1.var <- vcov(study1.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.1.se <- sqrt(vcov(study1.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.2.point <- study2.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.2.var <- vcov(study2.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.2.se <- sqrt(vcov(study2.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.3.point <- study3.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.3.var <- vcov(study3.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.3.se <- sqrt(vcov(study3.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.4.point <- study4.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.4.var <- vcov(study4.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.4.se <- sqrt(vcov(study4.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.5.point <- study5.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.5.var <- vcov(study5.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.5.se <- sqrt(vcov(study5.reg.select.unfamiliar)[2,2])

unfamiliar.selected.est <- c(unfamiliar.selected.source.1.point,unfamiliar.selected.source.2.point,unfamiliar.selected.source.3.point,unfamiliar.selected.source.4.point,unfamiliar.selected.source.5.point)
unfamiliar.selected.var <- c(unfamiliar.selected.source.1.var,unfamiliar.selected.source.2.var,unfamiliar.selected.source.3.var,unfamiliar.selected.source.4.var,unfamiliar.selected.source.4.se)
unfamiliar.selected.se <- c(unfamiliar.selected.source.1.se,unfamiliar.selected.source.2.se,unfamiliar.selected.source.3.se,unfamiliar.selected.source.4.se,unfamiliar.selected.source.5.se)

unfamiliar.selected.effect.frame <- cbind.data.frame(unfamiliar.selected.est,unfamiliar.selected.var,unfamiliar.selected.se)

unfamiliar.selected.meta <- rma(yi=unfamiliar.selected.effect.frame[,1],vi=unfamiliar.selected.effect.frame[,2],method="FE",data=unfamiliar.selected.effect.frame)
unfamiliar.selected.share.average <- ((study1.select.unfamiliar.share + study2.select.unfamiliar.share + study3.select.unfamiliar.share + study4.select.unfamiliar.share + study5.select.unfamiliar.share)/5)
unfamiliar.selected.share <- round(c(study1.select.unfamiliar.share,study2.select.unfamiliar.share,study3.select.unfamiliar.share,study4.select.unfamiliar.share,study5.select.unfamiliar.share,unfamiliar.selected.share.average),digits=2)

unfamiliar.selected.meta.row <- c(unfamiliar.selected.meta$beta,(unfamiliar.selected.meta$se)^2,unfamiliar.selected.meta$se)
unfamiliar.selected.effect.frame <- rbind(unfamiliar.selected.effect.frame,unfamiliar.selected.meta.row)
unfamiliar.selected.effect.frame$lower <- unfamiliar.selected.effect.frame[,1] - 1.96*unfamiliar.selected.effect.frame[,3]
unfamiliar.selected.effect.frame$upper <- unfamiliar.selected.effect.frame[,1] + 1.96*unfamiliar.selected.effect.frame[,3]
unfamiliar.selected.effect.frame <- round(unfamiliar.selected.effect.frame,digits=2)
unfamiliar.selected.effect.frame$out.text <- paste(unfamiliar.selected.effect.frame$unfamiliar.selected.est,' [',unfamiliar.selected.effect.frame$lower,',',unfamiliar.selected.effect.frame$upper,']',sep='')
unfamiliar.selected.effect.frame$share <- unfamiliar.selected.share

# ----------5.2: Avoided----------------
#Effect of Unfamiliar Sources Among Those Who Avoided Unfamiliar Sources
study1.reg.didnot.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text)+ 
                                                  factor(age_category) + factor(ethnicity) + factor(college),
                                          data=survey1,subset=which(survey1$chose.partisan.local.1==0))
names(study1.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study1.didnot.select.unfamiliar.share <- mean(I(survey1$chose.partisan.local.1==0),na.rm=TRUE)

study2.reg.didnot.select.unfamiliar <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text)+ 
                                                  factor(age_category) + factor(ethnicity) + factor(college) + affective.polarization,
                                          data=survey1,subset=which(survey1$chose.rt.national==0))
names(study2.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study2.didnot.select.unfamiliar.share <- mean(I(survey1$chose.rt.national==0),na.rm=TRUE)

study3.reg.didnot.select.unfamiliar <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text)+ 
                                                  factor(age_category) + factor(ethnicity) + factor(college) + trump.biden,
                                          data=survey1,subset=which(survey1$chose.fake.local.2==0))
names(study3.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study3.didnot.select.unfamiliar.share <- mean(I(survey1$chose.fake.local.2==0),na.rm=TRUE)

study4.reg.didnot.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text)+ 
                                                  factor(age_category) + factor(ethnicity) + factor(college) + tax.pretreatment ,
                                          data=survey2,subset=which(survey2$chose.partisan.local.1==0))
names(study4.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study4.didnot.select.unfamiliar.share <- mean(I(survey2$chose.partisan.local.1==0),na.rm=TRUE)

study5.reg.didnot.select.unfamiliar <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text)+ 
                                                  factor(age_category) + factor(ethnicity) + factor(college) + cyber.pretreatment,
                                          data=survey2,subset=which(survey2$chose.rt.national==0))
names(study5.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study5.didnot.select.unfamiliar.share <- mean(I(survey2$chose.rt.national==0),na.rm=TRUE)

unfamiliar.not.selected.source.1.point <- study1.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.1.var <- vcov(study1.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.1.se <- sqrt(vcov(study1.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.2.point <- study2.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.2.var <- vcov(study2.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.2.se <- sqrt(vcov(study2.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.3.point <- study3.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.3.var <- vcov(study3.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.3.se <- sqrt(vcov(study3.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.4.point <- study4.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.4.var <- vcov(study4.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.4.se <- sqrt(vcov(study4.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.5.point <- study5.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.5.var <- vcov(study5.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.5.se <- sqrt(vcov(study5.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.est <- c(unfamiliar.not.selected.source.1.point,unfamiliar.not.selected.source.2.point,unfamiliar.not.selected.source.3.point,unfamiliar.not.selected.source.4.point,unfamiliar.not.selected.source.5.point)
unfamiliar.not.selected.var <- c(unfamiliar.not.selected.source.1.var,unfamiliar.not.selected.source.2.var,unfamiliar.not.selected.source.3.var,unfamiliar.not.selected.source.4.var,unfamiliar.not.selected.source.5.var)
unfamiliar.not.selected.se <- c(unfamiliar.not.selected.source.1.se,unfamiliar.not.selected.source.2.se,unfamiliar.not.selected.source.3.se,unfamiliar.not.selected.source.4.se,unfamiliar.not.selected.source.5.se)

unfamiliar.not.selected.effect.frame <- cbind.data.frame(unfamiliar.not.selected.est,unfamiliar.not.selected.var,unfamiliar.not.selected.se)

unfamiliar.not.selected.meta <- rma(yi=unfamiliar.not.selected.effect.frame[,1],vi=unfamiliar.not.selected.effect.frame[,2],method="FE",data=unfamiliar.not.selected.effect.frame)
unfamiliar.not.selected.share.average <- ((study1.didnot.select.unfamiliar.share + study2.didnot.select.unfamiliar.share + study3.didnot.select.unfamiliar.share + study4.didnot.select.unfamiliar.share + study5.didnot.select.unfamiliar.share)/5)
unfamiliar.not.selected.share <- round(c(study1.didnot.select.unfamiliar.share,study2.didnot.select.unfamiliar.share,study3.didnot.select.unfamiliar.share,study4.didnot.select.unfamiliar.share,study5.didnot.select.unfamiliar.share,unfamiliar.not.selected.share.average),digits=2)

unfamiliar.not.selected.meta.row <- c(unfamiliar.not.selected.meta$beta,(unfamiliar.not.selected.meta$se)^2,unfamiliar.not.selected.meta$se)
unfamiliar.not.selected.effect.frame <- rbind(unfamiliar.not.selected.effect.frame,unfamiliar.not.selected.meta.row)
unfamiliar.not.selected.effect.frame$lower <- unfamiliar.not.selected.effect.frame[,1] - 1.96*unfamiliar.not.selected.effect.frame[,3]
unfamiliar.not.selected.effect.frame$upper <- unfamiliar.not.selected.effect.frame[,1] + 1.96*unfamiliar.not.selected.effect.frame[,3]
unfamiliar.not.selected.effect.frame <- round(unfamiliar.not.selected.effect.frame,digits=2)
unfamiliar.not.selected.effect.frame$out.text <- paste(unfamiliar.not.selected.effect.frame$unfamiliar.not.selected.est,' [',unfamiliar.not.selected.effect.frame$lower,',',unfamiliar.not.selected.effect.frame$upper,']',sep='')
unfamiliar.not.selected.effect.frame$share <- unfamiliar.not.selected.share

#--------5.3 Model 5: Table -----------
# table 6
conditional.effect.table <- cbind.data.frame(unfamiliar.selected.effect.frame$out.text,unfamiliar.not.selected.effect.frame$out.text)
names(conditional.effect.table) <- c("Chose unfamiliar source","Avoided unfamiliar source")
rownames(conditional.effect.table) <- c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5","Pooled")

xtable(conditional.effect.table)

#-------------5.4 Original analysis-------------------
##
#Conditional Effects of Familiar and Unfamiliar Sources on Public Opinion by News Preference
##

#Effect of Unfamiliar Sources Among Those Who Selected Unfamiliar Sources
study1.reg.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text),data=survey1,subset=which(survey1$chose.partisan.local.1==1))
names(study1.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study1.select.unfamiliar.share <- mean(survey1$chose.partisan.local.1,na.rm=TRUE)

study2.reg.select.unfamiliar <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + affective.polarization,data=survey1,subset=which(survey1$chose.rt.national==1))
names(study2.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study2.select.unfamiliar.share <- mean(survey1$chose.rt.national,na.rm=TRUE)

study3.reg.select.unfamiliar <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + trump.biden,data=survey1,subset=which(survey1$chose.fake.local.2==1))
names(study3.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study3.select.unfamiliar.share <- mean(survey1$chose.fake.local.2,na.rm=TRUE)

study4.reg.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment + factor(party_text),data=survey2,subset=which(survey2$chose.partisan.local.1==1))
names(study4.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study4.select.unfamiliar.share <- mean(survey2$chose.partisan.local.1==1,na.rm=TRUE)

study5.reg.select.unfamiliar <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment + factor(party_text),data=survey2,subset=which(survey2$chose.rt.national==1))
names(study5.reg.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study5.select.unfamiliar.share <- mean(survey2$chose.rt.national==1,na.rm=TRUE)

unfamiliar.selected.source.1.point <- study1.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.1.var <- vcov(study1.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.1.se <- sqrt(vcov(study1.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.2.point <- study2.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.2.var <- vcov(study2.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.2.se <- sqrt(vcov(study2.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.3.point <- study3.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.3.var <- vcov(study3.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.3.se <- sqrt(vcov(study3.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.4.point <- study4.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.4.var <- vcov(study4.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.4.se <- sqrt(vcov(study4.reg.select.unfamiliar)[2,2])

unfamiliar.selected.source.5.point <- study5.reg.select.unfamiliar$coefficients[2]
unfamiliar.selected.source.5.var <- vcov(study5.reg.select.unfamiliar)[2,2]
unfamiliar.selected.source.5.se <- sqrt(vcov(study5.reg.select.unfamiliar)[2,2])

unfamiliar.selected.est <- c(unfamiliar.selected.source.1.point,unfamiliar.selected.source.2.point,unfamiliar.selected.source.3.point,unfamiliar.selected.source.4.point,unfamiliar.selected.source.5.point)
unfamiliar.selected.var <- c(unfamiliar.selected.source.1.var,unfamiliar.selected.source.2.var,unfamiliar.selected.source.3.var,unfamiliar.selected.source.4.var,unfamiliar.selected.source.4.se)
unfamiliar.selected.se <- c(unfamiliar.selected.source.1.se,unfamiliar.selected.source.2.se,unfamiliar.selected.source.3.se,unfamiliar.selected.source.4.se,unfamiliar.selected.source.5.se)

ori.unfamiliar.selected.effect.frame <- cbind.data.frame(unfamiliar.selected.est,unfamiliar.selected.var,unfamiliar.selected.se)

ori.unfamiliar.selected.meta <- rma(yi=ori.unfamiliar.selected.effect.frame[,1],vi=ori.unfamiliar.selected.effect.frame[,2],method="FE",data=ori.unfamiliar.selected.effect.frame)
ori.unfamiliar.selected.share.average <- ((study1.select.unfamiliar.share + study2.select.unfamiliar.share + study3.select.unfamiliar.share + study4.select.unfamiliar.share + study5.select.unfamiliar.share)/5)
ori.unfamiliar.selected.share <- round(c(study1.select.unfamiliar.share,study2.select.unfamiliar.share,study3.select.unfamiliar.share,study4.select.unfamiliar.share,study5.select.unfamiliar.share,ori.unfamiliar.selected.share.average),digits=2)

ori.unfamiliar.selected.meta.row <- c(ori.unfamiliar.selected.meta$beta,(ori.unfamiliar.selected.meta$se)^2,ori.unfamiliar.selected.meta$se)
ori.unfamiliar.selected.effect.frame <- rbind(ori.unfamiliar.selected.effect.frame,ori.unfamiliar.selected.meta.row)
ori.unfamiliar.selected.effect.frame$lower <- ori.unfamiliar.selected.effect.frame[,1] - 1.96*ori.unfamiliar.selected.effect.frame[,3]
ori.unfamiliar.selected.effect.frame$upper <- ori.unfamiliar.selected.effect.frame[,1] + 1.96*ori.unfamiliar.selected.effect.frame[,3]
ori.unfamiliar.selected.effect.frame <- round(ori.unfamiliar.selected.effect.frame,digits=2)
ori.unfamiliar.selected.effect.frame$out.text <- paste(ori.unfamiliar.selected.effect.frame$unfamiliar.selected.est,' [',ori.unfamiliar.selected.effect.frame$lower,',',ori.unfamiliar.selected.effect.frame$upper,']',sep='')
ori.unfamiliar.selected.effect.frame$share <- ori.unfamiliar.selected.share

#Effect of Unfamiliar Sources Among Those Who Avoided Unfamiliar Sources
study1.reg.didnot.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + factor(party_text),data=survey1,subset=which(survey1$chose.partisan.local.1==0))
names(study1.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study1.didnot.select.unfamiliar.share <- mean(I(survey1$chose.partisan.local.1==0),na.rm=TRUE)

study2.reg.didnot.select.unfamiliar <- lm(polarization_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + factor(party_text) + affective.polarization,data=survey1,subset=which(survey1$chose.rt.national==0))
names(study2.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study2.didnot.select.unfamiliar.share <- mean(I(survey1$chose.rt.national==0),na.rm=TRUE)

study3.reg.didnot.select.unfamiliar <- lm(fraud_scale ~ I(localvotetreat=="unfamiliar") + I(localvotetreat=="familiar") + factor(party_text) + trump.biden,data=survey1,subset=which(survey1$chose.fake.local.2==0))
names(study3.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study3.didnot.select.unfamiliar.share <- mean(I(survey1$chose.fake.local.2==0),na.rm=TRUE)

study4.reg.didnot.select.unfamiliar <- lm(localtax_scale ~ I(localtaxtreat=="unfamiliar") + I(localtaxtreat=="familiar") + tax.pretreatment + factor(party_text),data=survey2,subset=which(survey2$chose.partisan.local.1==0))
names(study4.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study4.didnot.select.unfamiliar.share <- mean(I(survey2$chose.partisan.local.1==0),na.rm=TRUE)

study5.reg.didnot.select.unfamiliar <- lm(cyber_scale ~ I(nationaltreat=="unfamiliar") + I(nationaltreat=="familiar") + cyber.pretreatment + factor(party_text),data=survey2,subset=which(survey2$chose.rt.national==0))
names(study5.reg.didnot.select.unfamiliar$coefficients)[2:3] <- c('Unfamiliar Source','Familiar Source')
study5.didnot.select.unfamiliar.share <- mean(I(survey2$chose.rt.national==0),na.rm=TRUE)

unfamiliar.not.selected.source.1.point <- study1.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.1.var <- vcov(study1.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.1.se <- sqrt(vcov(study1.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.2.point <- study2.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.2.var <- vcov(study2.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.2.se <- sqrt(vcov(study2.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.3.point <- study3.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.3.var <- vcov(study3.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.3.se <- sqrt(vcov(study3.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.4.point <- study4.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.4.var <- vcov(study4.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.4.se <- sqrt(vcov(study4.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.source.5.point <- study5.reg.didnot.select.unfamiliar$coefficients[2]
unfamiliar.not.selected.source.5.var <- vcov(study5.reg.didnot.select.unfamiliar)[2,2]
unfamiliar.not.selected.source.5.se <- sqrt(vcov(study5.reg.didnot.select.unfamiliar)[2,2])

unfamiliar.not.selected.est <- c(unfamiliar.not.selected.source.1.point,unfamiliar.not.selected.source.2.point,unfamiliar.not.selected.source.3.point,unfamiliar.not.selected.source.4.point,unfamiliar.not.selected.source.5.point)
unfamiliar.not.selected.var <- c(unfamiliar.not.selected.source.1.var,unfamiliar.not.selected.source.2.var,unfamiliar.not.selected.source.3.var,unfamiliar.not.selected.source.4.var,unfamiliar.not.selected.source.5.var)
unfamiliar.not.selected.se <- c(unfamiliar.not.selected.source.1.se,unfamiliar.not.selected.source.2.se,unfamiliar.not.selected.source.3.se,unfamiliar.not.selected.source.4.se,unfamiliar.not.selected.source.5.se)

ori.unfamiliar.not.selected.effect.frame <- cbind.data.frame(unfamiliar.not.selected.est,unfamiliar.not.selected.var,unfamiliar.not.selected.se)

ori.unfamiliar.not.selected.meta <- rma(yi=ori.unfamiliar.not.selected.effect.frame[,1],vi=ori.unfamiliar.not.selected.effect.frame[,2],method="FE",data=ori.unfamiliar.not.selected.effect.frame)
ori.unfamiliar.not.selected.share.average <- ((study1.didnot.select.unfamiliar.share + study2.didnot.select.unfamiliar.share + study3.didnot.select.unfamiliar.share + study4.didnot.select.unfamiliar.share + study5.didnot.select.unfamiliar.share)/5)
ori.unfamiliar.not.selected.share <- round(c(study1.didnot.select.unfamiliar.share,study2.didnot.select.unfamiliar.share,study3.didnot.select.unfamiliar.share,study4.didnot.select.unfamiliar.share,study5.didnot.select.unfamiliar.share,ori.unfamiliar.not.selected.share.average),digits=2)

ori.unfamiliar.not.selected.meta.row <- c(ori.unfamiliar.not.selected.meta$beta,(ori.unfamiliar.not.selected.meta$se)^2,ori.unfamiliar.not.selected.meta$se)
ori.unfamiliar.not.selected.effect.frame <- rbind(ori.unfamiliar.not.selected.effect.frame,ori.unfamiliar.not.selected.meta.row)
ori.unfamiliar.not.selected.effect.frame$lower <- ori.unfamiliar.not.selected.effect.frame[,1] - 1.96*ori.unfamiliar.not.selected.effect.frame[,3]
ori.unfamiliar.not.selected.effect.frame$upper <- ori.unfamiliar.not.selected.effect.frame[,1] + 1.96*ori.unfamiliar.not.selected.effect.frame[,3]
ori.unfamiliar.not.selected.effect.frame <- round(ori.unfamiliar.not.selected.effect.frame,digits=2)
ori.unfamiliar.not.selected.effect.frame$out.text <- paste(ori.unfamiliar.not.selected.effect.frame$unfamiliar.not.selected.est,' [',ori.unfamiliar.not.selected.effect.frame$lower,',',ori.unfamiliar.not.selected.effect.frame$upper,']',sep='')
ori.unfamiliar.not.selected.effect.frame$share <- ori.unfamiliar.not.selected.share
#----5.5 Original analysis table------
# table 7
ori.conditional.effect.table <- cbind.data.frame(ori.unfamiliar.selected.effect.frame$out.text,ori.unfamiliar.not.selected.effect.frame$out.text)
names(ori.conditional.effect.table) <- c("Chose unfamiliar source","Avoided unfamiliar source")
rownames(ori.conditional.effect.table) <- c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5","Pooled")

xtable(ori.conditional.effect.table)




