require(dplyr)
require(tidyr)
require(ggplot2)

catDat = read.delim("./Analyses with full sample/2 Behavioral/catDat.txt")
# includes practice trials
# 256 trials per subject
# Practice: Trial = 1, SubTrial = 1...8
# First half: Trial = 2, SubTrial = 1...128
# Second half: Trial = 3, SubTrial = 128...256

# take out bad subs
badsubs = read.delim("./Analyses with full sample/Cat_badsubs.txt")
catDat = catDat[!(catDat$Subject %in% badsubs$Subject),]

expCatDat = catDat[catDat$Trial != 1,]  # takes out practice trials
expCatDat$Subject = factor(expCatDat$Subject)


# look at distribution of RTs
hist(expCatDat$TargetFace.RT[!(expCatDat$responseAccDat == 3)], breaks = 30) # looks really nicely distributed
hist(expCatDat$TargetFace.RT[(expCatDat$responseAccDat == 2)], breaks = 30) # correct trials
hist(expCatDat$TargetFace.RT[(expCatDat$responseAccDat == 1)], breaks = 30) # incorrect trials
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

tapply(corTrials$TargetFace.RT, corTrials$TrialType, sd)

corTrials$Fixation = corTrials$FixArea
ggplot(corTrials, aes(faceRace, TargetFace.RT, fill = Fixation)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(435, 490)) +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("grey40", "grey55"))

ggsave("./Analyses with full sample/2 Behavioral/Figures/Cat task (RT)_v2.png", height = 7, width = 6.2, unit="in")

# MLM analysis ------------------------------------------------------------



require(lme4)
require(lmerTest)

# check if subject and stimulus should be used as grouping variables
# intercept-only model

# no predictors- look at how intercept varies by face stimulus, word stimulus, and subject
m1 <- lmer(TargetFace.RT ~ 1 + (1|TargetPic) + (1|Subject), data=corTrials) 
m1a <- lmer(TargetFace.RT ~ 1 + (1|TargetPic) + (1|Subject), data=corTrials, REML=F) 

# investigate whether random effects of each are different from 0
m2a <- lmer(TargetFace.RT ~ 1 + (1|TargetPic), data=corTrials)
anova(m1, m2a) # subject is important

m2b <- lmer(TargetFace.RT ~ 1 + (1|Subject), data=corTrials)
anova(m1, m2b) # target face is important

# sink("./Analyses with full sample/6 For masters/Behavioral/Model output/2 Behavioral_Cat_interceptOnly.txt")
# summary(m1)
# coef(m1)
# summary(m1a)$AICtab
# sink()


# next step: include fixed effects (ie trial condition)

# maximal model (without interactions)- won't converge
m5 <- lmer(TargetFace.RT ~ faceRace * FixArea + 
             (faceRace * FixArea|Subject) + 
             (faceRace * FixArea|TargetPic), data=corTrials)
summary(m5)

# Write output to separate file
# sink("./Analyses with full sample/6 For masters/Behavioral/Model output/2.2 Behavioral_Cat_maximalModel.txt")
# "Doesn't converge"
# summary(m5)
# coef(m5)
# sink()

# take out interactions

m6 <- lmer(TargetFace.RT ~ faceRace * FixArea + 
             (faceRace + FixArea|Subject) + 
             (faceRace + FixArea|TargetPic), data=corTrials)


# sink("./Analyses with full sample/2 Behavioral/Model output (noBS)/2.3 Behavioral_Cat_takeOutInteractions.txt")
# "Doesn't converge"
# summary(m6)
# coef(m6)
# sink()


m7 = lmer(TargetFace.RT ~ faceRace * FixArea + 
            (faceRace + FixArea|Subject) + 
            (faceRace|TargetPic), data=corTrials)

sink("./Analyses with full sample/2 Behavioral/Model output (noBS)/2.4 Behavioral_Cat_raceFixBySubject_raceByStim.txt")
summary(m7)
coef(m7)
sink()


# let faceRace and fix vary just by subject
m8 = lmer(TargetFace.RT ~ faceRace * FixArea + 
            (faceRace + FixArea|Subject) + 
            (1|TargetPic), data=corTrials)

# sink("./Analyses with full sample/2 Behavioral/Model output (noBS)/2.5 Behavioral_Cat_raceFixBySubject.txt")
# summary(m8)
# coef(m8)
# sink()

# let slopes vary by subject, intercept vary by stimulus
m9 = lmer(TargetFace.RT ~ faceRace * FixArea + 
            (faceRace*FixArea|Subject) + 
            (1|TargetPic), data=corTrials)

# sink("./Analyses with full sample/6 For masters/Behavioral/Model output/2.6 Behavioral_Cat_allRandBySubject_randIntbyStim.txt")
# summary(m9)
# coef(m9)
# sink()


# let slopes vary by subject (without interaction), intercept vary by stimulus
m10 = lmer(TargetFace.RT ~ faceRace * FixArea + 
            (faceRace+FixArea|Subject) + 
            (1|TargetPic), data=corTrials)

# sink("./Analyses with full sample/6 For masters/Behavioral/Model output/2.7 Behavioral_Cat_slopesRandBySubject_randIntbyStim.txt")
# summary(m10)
# coef(m10)
# sink()
