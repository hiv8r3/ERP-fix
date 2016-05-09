require(dplyr)
require(tidyr)

evalDat = read.delim("./Behavioral/evalDat.txt")

expEvalDat = evalDat[evalDat$Task == "expBlockEval",]
expEvalDat$Subject = factor(expEvalDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea x WordVal)

# overall accuracy for each subject
acc = tapply(expEvalDat$TargetWord.ACC, expEvalDat$Subject, sum)/512

# look at average accuracy for all conditions
tapply(expEvalDat$TargetWord.ACC, expEvalDat$TrialType, sum)/(64*length(unique(expEvalDat$Subject)))

############## Break down by block #################################################

# look at error rates by block: stays fairly even
tapply(expEvalDat$TargetWord.ACC, expEvalDat$Chunk, sum)/(128*length(unique(expEvalDat$Subject)))

# look at average accuracy for Block 1- more Black-pos correct than Black-neg
tapply(expEvalDat$TargetWord.ACC[expEvalDat$Chunk == 1], expEvalDat$TrialType[expEvalDat$Chunk == 1], sum)/
  (16*length(unique(expEvalDat$Subject)))

# look at average accuracy for Block 4- more Black-neg correct than Black-pos
tapply(expEvalDat$TargetWord.ACC[expEvalDat$Chunk == 4], expEvalDat$TrialType[expEvalDat$Chunk == 4], sum)/
  (16*length(unique(expEvalDat$Subject)))


# make new data set with mean accuracy per condition for each subject
accRate = expEvalDat %>% 
          group_by(Subject, faceRace, wordVal, FixArea) %>%
          summarise(corCount = sum(TargetWord.ACC)) %>%
          mutate(accRate = corCount/64) %>%
          ungroup()


# all trials
ggplot(accRate, aes(faceRace, accRate, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("All blocks")


# same thing but group by halves (first half: Trial = 2; second half: Trial = 3)
accRateHalves = expEvalDat %>% 
  group_by(Subject, Trial, faceRace, wordVal, FixArea) %>%
  summarise(corCount = sum(TargetWord.ACC)) %>%
  mutate(accRate = corCount/32)

# First half
ggplot(accRateHalves[accRateHalves$Trial == 2,], aes(faceRace, accRate, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("First half")

# Second half
ggplot(accRateHalves[accRateHalves$Trial == 3,], aes(faceRace, accRate, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("Second half")
