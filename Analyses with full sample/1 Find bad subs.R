require(dplyr)
require(tidyr)


# Look at behavioral accuracy (Eval) ---------------------------------------------


evalDat = read.delim("./Analyses with full sample/2 Behavioral/evalDat.txt")

expEvalDat = evalDat[evalDat$Task == "expBlockEval",]
expEvalDat$Subject = factor(expEvalDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea x WordVal)

# overall accuracy for each subject
acc = tapply(expEvalDat$TargetWord.ACC, expEvalDat$Subject, sum)/512

# any subjects more than 3 sds below mean?
m = mean(acc) # mean is 90.5%
s = sd(acc) # cutoff is 65.8%
acc < (m-3*s) # 61 and 47 had accuracies that were too low

badsubs = data.frame(Subject = c("47", "61"),
                     Reason = "<3 sds below mean for accuracy")

write.table(badsubs, "./Analyses with full sample/1 ERP quant data/Eval_badsubs.txt", sep="\t", row.names = F)


# Look at number of artifacts for avgMast---------------------------------------------

artdat = read.delim("./Analyses with full sample/1 ERP quant data/1 Individual time intervals/Eval_NumberOfAcceptedTrials_avgMast.txt")

# take out 47 and 61
artdat = artdat[artdat$Subject != 47 & artdat$Subject != 61,]

artdat[(artdat$numAccept/512 < .25),]
sort(artdat$numAccept/512)

mean(artdat$numAccept/512) # on average, 85.3% of trials were accepted

# Look at number of artifacts for nose---------------------------------------------

artdat = read.delim("./Analyses with full sample/1 ERP quant data/2 Group time intervals/Eval_NumberOfAcceptedTrials_Nose.txt")

# take out 47 and 61
artdat = artdat[artdat$Subject != 47 & artdat$Subject != 61,]

artdat[(artdat$numAccept/512 < .25),]
sort(artdat$numAccept/512)

mean(artdat$numAccept/512) # on average, 76.2% of trials were accepted

# Look at behavioral accuracy (Eval) ---------------------------------------------


catDat = read.delim("./Analyses with full sample/2 Behavioral/catDat.txt")

expCatDat = catDat[catDat$Task == "expBlockCat",]
expCatDat$Subject = factor(expCatDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea)

# overall accuracy for each subject
acc = tapply(expCatDat$TargetFace.ACC, expCatDat$Subject, sum)/256

# any subjects more than 3 sds below mean?
m = mean(acc) # mean is 90.5%
s = sd(acc) # cutoff is 65.8%
acc < (m-3*s) # 66 had accuracy that was too low

badsubs = data.frame(Subject = c("66"),
                     Reason = "<3 sds below mean for accuracy")

write.table(badsubs, "./Analyses with full sample/1 ERP quant data/Cat_badsubs.txt", sep="\t", row.names = F)


# Look at number of artifacts for avgMast---------------------------------------------

artdat = read.delim("./Analyses with full sample/1 ERP quant data/1 Individual time intervals/Cat_NumberOfAcceptedTrials_avgMast.txt")

# take out 66
artdat = artdat[artdat$Subject != 66,]

artdat[(artdat$numAccept/256 < .25),]
sort(artdat$numAccept/256)

mean(artdat$numAccept/256) # on average, 84.2% of trials were accepted

# Look at number of artifacts for Nose ---------------------------------------------

artdat = read.delim("./Analyses with full sample/1 ERP quant data/2 Group time intervals/Cat_NumberOfAcceptedTrials_Nose.txt")

# take out 66
artdat = artdat[artdat$Subject != 66,]

artdat[(artdat$numAccept/256 < .25),]
sort(artdat$numAccept/256)

mean(artdat$numAccept/256) # on average, 75.5% of trials were accepted
