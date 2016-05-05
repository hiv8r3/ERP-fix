require(dplyr)
require(tidyr)

# investigating how differences in P2 amplitude predict RT in categorizing faces by race
# create differences scores between fixations for each race

# read in behavioral data
catBeh = read.delim("./Behavioral/catDat.txt")
# includes practice trials
# 512 trials per subject
# Practice: Trial = 1, SubTrial = 1...8
# First half: Trial = 2, SubTrial = 1...128
# Second half: Trial = 3, SubTrial = 128...256

behave = catBeh[catBeh$Trial != 1,]  # takes out practice trials
behave$Subject = factor(behave$Subject)
behave$TrialType = factor(behave$TrialType)

# read in ERP data
catERP = read.delim("./ERP quant data/2 Cat dat/P2quant_AvgAmp_51subsAggregated.txt")

elec = c("CZ", "CPZ", "PZ")

catERP = catERP[catERP$Electrode %in% elec,]
catERP$Electrode = factor(catERP$Electrode)
catERP$Subject = factor(catERP$Subject)  



############################################################################################
################# DIFFERENCE SCORES BY FIXATION ############################################
############################################################################################

# create difference scores comparing fixation conditions
# BlackDiff = Black-fore - Black-eyes 
# WhiteDiff = White-fore - White-eyes

diffDatFix = data.frame(Subject = rep(1:51, each = 6),
                         Electrode = rep(c("CZ", "CPZ", "PZ"), 102),
                         DiffType = rep(c("BlackDiff", "WhiteDiff"), each = 3, 51),
                         AmpDiff = NA,
                         BehaveDiff = NA)

# loop to compute difference scores of peaks and average latencies
for (i in 1:51) {
  for (j in c("CZ", "CPZ", "PZ")) {
    P2temp = catERP[catERP$Subject == i & catERP$Electrode == j,]
    # Create difference score in amplitude between eyes and forehead fixations for Black faces
    diffDatFix$AmpDiff[diffDatFix$Subject == i & 
                       diffDatFix$Electrode == j & 
                       diffDatFix$DiffType == "BlackDiff"] = P2temp$AvgAmp[P2temp$Condition == "Black_fore"] - 
                                                             P2temp$AvgAmp[P2temp$Condition == "Black_eyes"]
    # Create difference score in amplitude between eyes and forehead fixations for White faces
    diffDatFix$AmpDiff[diffDatFix$Subject == i & 
                       diffDatFix$Electrode == j & 
                       diffDatFix$DiffType == "WhiteDiff"] = P2temp$AvgAmp[P2temp$Condition == "White_fore"] - 
                                                             P2temp$AvgAmp[P2temp$Condition == "White_eyes"]
    
    # Create difference score in RT for Black faces
    diffDatFix$BehaveDiff[diffDatFix$Subject == i & 
                          diffDatFix$Electrode == j & 
                          diffDatFix$DiffType == "BlackDiff"] = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_forehead"]) - 
                                                                mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_eyes"])
    # Create difference score in RT for Black faces
    diffDatFix$BehaveDiff[diffDatFix$Subject == i & 
                            diffDatFix$Electrode == j & 
                            diffDatFix$DiffType == "WhiteDiff"] = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_forehead"]) - 
                                                                  mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_eyes"])
    
  }
}


require(lme4)
require(lmerTest)

# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(BehaveDiff ~ 1 + (1|Subject) + (1|Electrode), data = diffDatFix)
summary(m1)

# see if ERPs predict RT
m2a = lmer(BehaveDiff ~ AmpDiff + (1|Subject) + (1|Electrode), data = diffDatFix)
summary(m2a)
#### Amp difference score (fixation) significantly predicts RT difference score (fixation) #####

m2 = lmer(BehaveDiff ~ AmpDiff*DiffType + (1|Subject) + (1|Electrode), data = diffDatFix)
summary(m2)
# significant interaction

# Look at diffTypes separately to break down interaction
m3a = lmer(BehaveDiff ~ AmpDiff + (1|Subject), data = diffDatFix[diffDatFix$DiffType == "BlackDiff",])
# doesn't converge (probably because there's only 3 data points per subject)

# run as repeated measures ANOVA
aov(BehaveDiff ~ AmpDiff + Error(Subject/(AmpDiff+Electrode)), data = diffDatFix[diffDatFix$DiffType == "BlackDiff",]) %>% 
  summary() # not significant
aov(BehaveDiff ~ AmpDiff + Error(Subject/(AmpDiff+Electrode)), data = diffDatFix[diffDatFix$DiffType == "WhiteDiff",]) %>% 
  summary() # marginally significant



############################################################################################
################# DIFFERENCE SCORES BY RACE ################################################
############################################################################################


# create difference scores comparing race conditions
# EyesDiff = Black-eyes - White-eyes 
# ForeDiff = Black-fore - White-fore

diffDatRace = data.frame(Subject = rep(1:51, each = 6),
                        Electrode = rep(c("CZ", "CPZ", "PZ"), 102),
                        DiffType = rep(c("EyesDiff", "ForeDiff"), each = 3, 51),
                        AmpDiff = NA,
                        BehaveDiff = NA)

# loop to compute difference scores of peaks and average latencies
for (i in 1:51) {
  for (j in c("CZ", "CPZ", "PZ")) {
    P2temp = catERP[catERP$Subject == i & catERP$Electrode == j,]
    # Create difference score in amplitude between eyes and forehead fixations for Black faces
    diffDatRace$AmpDiff[diffDatRace$Subject == i & 
                         diffDatRace$Electrode == j & 
                         diffDatRace$DiffType == "EyesDiff"] = P2temp$AvgAmp[P2temp$Condition == "Black_eyes"] - 
                                                               P2temp$AvgAmp[P2temp$Condition == "White_eyes"]
    # Create difference score in amplitude between eyes and forehead fixations for White faces
    diffDatRace$AmpDiff[diffDatRace$Subject == i & 
                         diffDatRace$Electrode == j & 
                         diffDatRace$DiffType == "ForeDiff"] = P2temp$AvgAmp[P2temp$Condition == "Black_fore"] - 
                                                               P2temp$AvgAmp[P2temp$Condition == "White_fore"]
    
    # Create difference score in RT for Black faces
    diffDatRace$BehaveDiff[diffDatRace$Subject == i & 
                            diffDatRace$Electrode == j & 
                            diffDatRace$DiffType == "EyesDiff"] = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_eyes"]) - 
                                                                  mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_eyes"])
    # Create difference score in RT for Black faces
    diffDatRace$BehaveDiff[diffDatRace$Subject == i & 
                            diffDatRace$Electrode == j & 
                            diffDatRace$DiffType == "ForeDiff"] = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_forehead"]) - 
                                                                  mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_forehead"])
    
  }
}


# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(BehaveDiff ~ 1 + (1|Subject) + (1|Electrode), data = diffDatRace)
summary(m1)

# see if ERPs predict RT
m2a = lmer(BehaveDiff ~ AmpDiff + (1|Subject) + (1|Electrode), data = diffDatRace)
summary(m2a)
#### Amp difference score (fixation) significantly predicts RT difference score (fixation) #####

m2 = lmer(BehaveDiff ~ AmpDiff*DiffType + (1|Subject) + (1|Electrode), data = diffDatRace)
summary(m2)
# no interaction

# Look at diffTypes separately to break down interaction
m3a = lmer(BehaveDiff ~ AmpDiff + (1|Subject), data = diffDatRace[diffDatRace$DiffType == "EyesDiff",])
# doesn't converge (probably because there's only 3 data points per subject)

# run as repeated measures ANOVA
aov(BehaveDiff ~ AmpDiff + Error(Subject/(AmpDiff+Electrode)), data = diffDatRace[diffDatRace$DiffType == "EyesDiff",]) %>% 
  summary() # significant
aov(BehaveDiff ~ AmpDiff + Error(Subject/(AmpDiff+Electrode)), data = diffDatRace[diffDatRace$DiffType == "ForeDiff",]) %>% 
  summary() # not significant


############################################################################################
################# INTERACTION CONTRAST ################################################
############################################################################################


# create interaction constrast scores
# Weights:
## Black-eyes: -1
## Black-fore: -.5
## White-eyes: .5
## White-fore: 1
weights = c(-1, -.5, .5, 1)

diffDatInt = data.frame(Subject = rep(1:51, each = 3),
                         Electrode = rep(c("CZ", "CPZ", "PZ"), 51),
                         AmpIntScore = NA,
                         BehaveIntScore = NA)

# loop to compute difference scores of peaks and average latencies
for (i in 1:51) {
  for (j in c("CZ", "CPZ", "PZ")) {
    P2temp = catERP[catERP$Subject == i & catERP$Electrode == j,]

    # Create interaction score in amplitude
    a1 = P2temp$AvgAmp[P2temp$Condition == "Black_eyes"]
    b1 = P2temp$AvgAmp[P2temp$Condition == "Black_fore"]
    c1 = P2temp$AvgAmp[P2temp$Condition == "White_eyes"]
    d1 = P2temp$AvgAmp[P2temp$Condition == "White_fore"]

    diffDatInt$AmpIntScore[diffDatInt$Subject == i & 
                          diffDatInt$Electrode == j] = sum(weights * c(a1, b1, c1, d1))
    
    
    # Create interaction score in RTs
    a2 = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_eyes"])
    b2 = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "Black_forehead"])
    c2 = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_eyes"])
    d2 = mean(behave$TargetFace.RT[behave$Subject == i & behave$TrialType == "White_forehead"])
    
    diffDatInt$BehaveIntScore[diffDatInt$Subject == i & 
                             diffDatInt$Electrode == j] = sum(weights * c(a2, b2, c2, d2))
  }
}

# run as repeated measures ANOVA
aov(BehaveIntScore ~ AmpIntScore + Error(Subject/(AmpIntScore+Electrode)), data = diffDatInt) %>% 
  summary() # not significant
