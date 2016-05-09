require(dplyr)
require(tidyr)

catDat = read.delim("catDat.txt")

expCatDat = catDat[catDat$Task == "expBlockCat",] # take out practice trials
expCatDat$Subject = factor(expCatDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea)

# overall accuracy for each subject
acc = tapply(expCatDat$TargetFace.ACC, expCatDat$Subject, sum)/256
sort(acc) # lowest accuracy is 77%

# look at average accuracy for all conditions
tapply(expCatDat$TargetFace.ACC, expCatDat$TrialType, sum)/(64*length(unique(expCatDat$Subject)))

# make new data set with mean accuracy per condition for each subject
accRate = expCatDat %>% 
  group_by(Subject, faceRace, FixArea) %>%
  summarise(corCount = sum(TargetFace.ACC)) %>%
  mutate(accRate = corCount/64) %>%
  ungroup()

# all trials
ggplot(accRate, aes(faceRace, accRate, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("All blocks")

############# Break down by half #############

# same thing but group by halves (first half: Trial = 2; second half: Trial = 3)
accRateHalves = expCatDat %>% 
  group_by(Subject, Trial, faceRace, FixArea) %>%
  summarise(corCount = sum(TargetFace.ACC)) %>%
  mutate(accRate = corCount/32)

# First half
ggplot(accRateHalves[accRateHalves$Trial == 2,], aes(faceRace, accRate, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("First half")

# Second half
ggplot(accRateHalves[accRateHalves$Trial == 3,], aes(faceRace, accRate, fill = FixArea)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("Second half")
