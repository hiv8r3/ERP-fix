require(dplyr)
require(tidyr)

evalDat = read.delim("./March 2016/Behavioral/evalDat.txt")
# includes practice trials
# 512 trials per subject
# Practice: Trial = 1, SubTrial = 1...16
# First half: Trial = 2, SubTrial = 1...256
# Second half: Trial = 3, SubTrial = 256...512

expEvalDat = evalDat[evalDat$Trial != 1,]  # takes out practice trials
expEvalDat$Subject = factor(expEvalDat$Subject)

# look at distribution of RTs
hist(expEvalDat$TargetWord.RT[!(expEvalDat$responseAccDat == 3)], breaks = 30) # looks really nicely distributed

# look at distribution of subject mean RTs (taking out misses)
noMiss = expEvalDat[!(expEvalDat$responseAccDat == 3),]

# tapply(Summary Variable, Group Variable, Function)
meanRT = tapply(noMiss$TargetWord.RT, noMiss$Subject, mean)
hist(meanRT) 
sort(meanRT)

# look at subjects with short mean RT
hist(expEvalDat$TargetWord.RT[expEvalDat$Subject == 34], breaks = 20)
hist(expEvalDat$TargetWord.RT[expEvalDat$Subject == 47], breaks = 20)
hist(expEvalDat$TargetWord.RT[expEvalDat$Subject == 28], breaks = 20)
hist(expEvalDat$TargetWord.RT[expEvalDat$Subject == 50], breaks = 20)

# histograms for all subjects's RTs separately
for (i in unique(noMiss$Subject)) {
  hist(noMiss$TargetWord.RT[noMiss$Subject == i], main = i, breaks = 15)
}


# look at average RTs for each condition
tapply(noMiss$TargetWord.RT, noMiss$TrialType, mean)
# FUCK!!!

# even for just correct trials?
corTrials = noMiss[noMiss$TargetWord.ACC==1,]
tapply(corTrials$TargetWord.RT, corTrials$TrialType, mean)
# DOUBLE FUCK!!!

require(ggplot2)
ggplot(corTrials, aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(475, 530)) +
#  ggtitle("All blocks, correct trials") +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme(axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18),
        legend.title = element_blank(),
        strip.text = element_text(size=18),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=12))

# simple anovas for effect of valence 
wide = summarise(group_by(corTrials, faceRace, wordVal, FixArea, Subject), meanRT = mean(TargetWord.RT))

# effect of valence in Black trials
aov(meanRT ~ wordVal + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(wordVal)), # need to add error term for within subjects variables (ie repeated measures)
    data = wide[wide$faceRace == "Black",]) %>% 
  summary()

# effect of valence in White trials
aov(meanRT ~ wordVal + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(wordVal)), # need to add error term for within subjects variables (ie repeated measures)
    data = wide[wide$faceRace == "White",]) %>% 
  summary()

############## Break down by block #################################################

# Look at RT by block: getting faster
tapply(corTrials$TargetWord.RT, corTrials$Chunk, mean)

# Just block 1
ggplot(corTrials[corTrials$Chunk == 1,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(450, 550)) +
  ggtitle("Block 1")

# Just block 2
ggplot(corTrials[corTrials$Chunk == 2,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(450, 550)) +
  ggtitle("Block 2")

# Just block 3
ggplot(corTrials[corTrials$Chunk == 3,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(450, 550)) +
  ggtitle("Block 3")

# just block 4
ggplot(corTrials[corTrials$Chunk == 4,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(450, 550)) +
  ggtitle("Block 4")




require(lme4)
require(lmerTest)

# check if subject and stimulus should be used as grouping variables
# intercept-only model

# no predictors- look at how intercept varies by face stimulus, word stimulus, and subject
m1 <- lmer(TargetWord.RT ~ 1 + (1|PrimePic) +(1|TargetWord) + (1|Subject) , data=corTrials) 
summary(m1)

# investigate whether random effects of each are different from 0
m2a <- lmer(TargetWord.RT ~ 1 + (1|TargetWord) + (1|Subject), data=corTrials)
anova(m1, m2a) # can take out PrimePic as random effect

m2b <- lmer(TargetWord.RT ~ 1 + (1|PrimePic) + (1|Subject), data=corTrials)
anova(m1, m2b) # leave in TargetWord as random effect

m2c <- lmer(TargetWord.RT ~ 1 + (1|TargetWord) + (1|PrimePic), data=corTrials)
anova(m1, m2c) # should leave subject in as random effect

# next step: include fixed effects (ie trial condition)

m4 <- lmer(TargetWord.RT ~ 1 + faceRace * wordVal * FixArea + (1|Subject) + (1|TargetWord), data=corTrials)
summary(m4)
m4a = lmer(TargetWord.RT ~ 1 + faceRace * wordVal * FixArea + (1|Subject) + (1|TargetWord), data=corTrials, REML = F)

# sink("./March 2016/Model outputs/1 Behavioral_Eval_interceptOnlyModel.txt")
# summary(m4)
# coef(m4)
# summary(m4a)$AICtab
# sink()

# maximal model (without interactions)
m5 <- lmer(TargetWord.RT ~ 1 + faceRace * wordVal * FixArea + 
              (faceRace + wordVal + FixArea|Subject) + 
              (faceRace + wordVal + FixArea|TargetWord), data=corTrials)
summary(m5)
m5a = lmer(TargetWord.RT ~ 1 + faceRace * wordVal * FixArea + 
             (faceRace + wordVal + FixArea|Subject) + 
             (faceRace + wordVal + FixArea|TargetWord), data=corTrials, REML = F)

# Write output to separate file
# sink("./March 2016/Model outputs/2 Behavioral_Eval_maximalModelwithoutInteractions.txt")
# summary(m5)
# coef(m5)
# summary(m5a)$AICtab
# sink()

# take out some of the random slopes for target words
m6 = lmer(TargetWord.RT ~ faceRace * wordVal * FixArea + 
          (faceRace + wordVal + FixArea|Subject) + 
          (1|TargetWord), data=corTrials)
summary(m6)
m6a = lmer(TargetWord.RT ~ faceRace * wordVal * FixArea + 
            (faceRace + wordVal + FixArea|Subject) + 
            (1|TargetWord), data=corTrials, REML = F)

sink("./March 2016/Model outputs/1.3 Behavioral_Eval_randomSlopesJustForSubject.txt")
summary(m6)
coef(m6)
summary(m6a)$AICtab
sink()

# take out some of the random slopes for target words
m7 = lmer(TargetWord.RT ~ faceRace * wordVal * FixArea + 
            (wordVal|Subject) + 
            (wordVal|TargetWord), data=corTrials)
summary(m7)
m7a = lmer(TargetWord.RT ~ faceRace * wordVal * FixArea + 
             (wordVal|Subject) + 
             (wordVal|TargetWord), data=corTrials, REML = F)

sink("./March 2016/Model outputs/1.4 Behavioral_Eval_randomSlopeValence.txt")
summary(m7)
coef(m7)
summary(m7a)$AICtab
sink()
