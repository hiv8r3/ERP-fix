require(dplyr)
require(tidyr)
require(magrittr)

evalDat = read.delim("./Analyses with full sample/2 Behavioral/evalDat.txt")

# take out bad subs
badsubs = read.delim("./Analyses with full sample/Eval_badsubs.txt")
evalDat = evalDat[!(evalDat$Subject %in% badsubs$Subject),]

expEvalDat = evalDat[evalDat$Task == "expBlockEval",]
expEvalDat$Subject = factor(expEvalDat$Subject)

# 64 trials of each trial condition (FaceRace x FixArea x WordVal)

# overall accuracy for each subject
acc = as.data.frame(tapply(expEvalDat$TargetWord.ACC, expEvalDat$Subject, sum)/512)
acc$Subject = row.names(acc)
names(acc)[1] = "accRate"

# look at mean and sd accuracy for all conditions
acc2 = expEvalDat %>% 
  group_by(TrialType, Subject) %>% 
  summarise(accRate = mean(TargetWord.ACC))

# look at average accuracy for each condition
tapply(acc2$accRate, acc2$TrialType, mean)

# look at sds for each condition
tapply(acc2$accRate, acc2$TrialType, sd)


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


# all trials (correct)
ggplot(accRate, aes(faceRace, accRate, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~FixArea) +
  coord_cartesian(ylim=c(.75, 1)) +
  scale_fill_manual(values = c("darkorange","darkslategray")) +
  ggtitle("Eval task (accuracy rates)")

ggsave("./Analyses with full sample/2 Behavioral/Eval task (Acc).png")

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
