require(dplyr)
require(tidyr)

catDat = read.delim("./Behavioral/catDat.txt")
# includes practice trials
# 512 trials per subject
# Practice: Trial = 1, SubTrial = 1...8
# First half: Trial = 2, SubTrial = 1...128
# Second half: Trial = 3, SubTrial = 128...256

expCatDat = catDat[catDat$Trial != 1,]  # takes out practice trials
expCatDat$Subject = factor(expCatDat$Subject)
expCatDat$TrialType = factor(expCatDat$TrialType)


# look at distribution of RTs
hist(expCatDat$TargetFace.RT[!(expCatDat$responseAccDat == 3)], breaks = 30) # looks really nicely distributed

# look at distribution of subject mean RTs (taking out misses)
noMiss = expCatDat[!(expCatDat$responseAccDat == 3),]

# tapply(Summary Variable, Group Variable, Function)
meanRT = tapply(noMiss$TargetFace.RT, noMiss$Subject, mean)
hist(meanRT) 
sort(meanRT)

# look at average RTs for each condition
tapply(noMiss$TargetFace.RT, noMiss$TrialType, mean)

# what about just correct trials?
corTrials = noMiss[noMiss$TargetFace.ACC==1,]
tapply(corTrials$TargetFace.RT, corTrials$TrialType, mean)

require(ggplot2)
ggplot(corTrials, aes(faceRace, TargetFace.RT, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(425,480)) +
  ggtitle("All blocks, correct trials")


############## Break down by block #################################################

# Look at RT by block: getting faster
tapply(corTrials$TargetFace.RT, corTrials$Trial, mean)

# Just block 1
ggplot(corTrials[corTrials$Trial == 2,], aes(faceRace, TargetFace.RT, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(425,480)) +
  ggtitle("Block 1")

# Just block 2
ggplot(corTrials[corTrials$Trial == 3,], aes(faceRace, TargetFace.RT, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(425,480)) +
  ggtitle("Block 2")



require(lme4)
require(lmerTest)

# check if subject and stimulus should be used as grouping variables
# intercept-only model

# no predictors- look at how intercept varies by face stimulus, word stimulus, and subject
m1 <- lmer(TargetFace.RT ~ 1 + (1|TargetPic) + (1|Subject) , data=corTrials) 
summary(m1)

# investigate whether random effects of each are different from 0
m2a <- lmer(TargetFace.RT ~ 1 + (1|TargetPic), data=corTrials)
anova(m1, m2a) # leave in subject as random effect

m2b <- lmer(TargetFace.RT ~ 1 + (1|Subject), data=corTrials)
anova(m1, m2b) # leave in TargetPic as random effect

# next step: include fixed effects (ie trial condition)

m4 <- lmer(TargetFace.RT ~ 1 + faceRace * FixArea + (1|Subject) + (1|TargetPic), data=corTrials)
summary(m4)
