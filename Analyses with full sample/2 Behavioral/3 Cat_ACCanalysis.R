require(dplyr)
require(tidyr)

catDat = read.delim("./Analyses with full sample/2 Behavioral/catDat.txt")

expCatDat = catDat[catDat$Task == "expBlockCat",]
expCatDat$Subject = factor(expCatDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea x WordVal)

# overall accuracy for each subject
acc = tapply(expCatDat$TargetFace.ACC, expCatDat$Subject, sum)/256

# take out bad subs
badsubs = read.delim("./Analyses with full sample/Cat_badsubs.txt")
expCatDat = expCatDat[!(expCatDat$Subject %in% badsubs$Subject),]

# overall accuracy for each subject
acc = as.data.frame(tapply(expCatDat$TargetFace.ACC, expCatDat$Subject, sum)/256)
acc$Subject = row.names(acc)
names(acc)[1] = "accRate"

# look at mean and sd accuracy for all conditions
acc2 = expCatDat %>% 
  group_by(TrialType, Subject) %>% 
  summarise(accRate = mean(TargetFace.ACC))

# look at average accuracy for each condition
tapply(acc2$accRate, acc2$TrialType, mean)

# look at sds for each condition
tapply(acc2$accRate, acc2$TrialType, sd)

# look at average accuracy for all conditions
tapply(expCatDat$TargetFace.ACC, expCatDat$TrialType, sum)/(64*length(unique(expCatDat$Subject)))

# make new data set with mean accuracy per condition for each subject
accRate = expCatDat %>% 
  group_by(Subject, faceRace, FixArea) %>%
  summarise(corCount = sum(TargetFace.ACC)) %>%
  mutate(accRate = corCount/64) %>%
  ungroup()

require(ggplot2)
# all trials
ggplot(accRate, aes(faceRace, accRate, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("Cat task (accuracy)")

ggsave("./Analyses with full sample/2 Behavioral/Cat task (Acc).png")

