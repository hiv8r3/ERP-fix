require(dplyr)
require(tidyr)
require(ggplot2)


evalDat = read.delim("./Analyses with full sample/2 Behavioral/evalDat.txt")
# includes practice trials
# 512 trials per subject
# Practice: Trial = 1, SubTrial = 1...16
# First half: Trial = 2, SubTrial = 1...256
# Second half: Trial = 3, SubTrial = 256...512

# take out bad subs
badsubs = read.delim("./Analyses with full sample/Eval_badsubs.txt")
evalDat = evalDat[!(evalDat$Subject %in% badsubs$Subject),]

expEvalDat = evalDat[evalDat$Trial != 1,]  # takes out practice trials
expEvalDat$Subject = factor(expEvalDat$Subject)

# just look at correct trials (doesn't include misses)
corTrials = expEvalDat[expEvalDat$responseAccDat == 2,]


# Look across individuals -------------------------------------------------


# create performance bias score for each subject 

perf = group_by(corTrials, Subject, faceRace, wordVal) %>% 
  summarise(avgRT = mean(TargetWord.RT)) %>% 
  as.data.frame()

perf$Condition = paste(perf$faceRace, perf$wordVal, sep = "_")

perfBias = data.frame(Subject = NULL, perfBias = NULL)
for (i in unique(perf$Subject)) {
  temp = perf[perf$Subject == i,]
  a = (temp$avgRT[2] - temp$avgRT[1]) - (temp$avgRT[4] - temp$avgRT[3])
  b = data.frame(Subject = i, perfBias = a)
  perfBias = rbind(perfBias, b)
}

plot(perfBias$Subject, perfBias$perfBias)
hist(perfBias$perfBias, breaks = 20,
     xlab = "Performance Bias", main = "")

perfBias[order(perfBias$perfBias),]

# most biased
ggplot(corTrials[corTrials$Subject ==28,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Subject) +
  coord_cartesian(ylim=c(400, 500)) +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("grey55", "grey75")) +
  guides(fill=F)

ggplot(corTrials[corTrials$Subject ==58,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Subject) +
  coord_cartesian(ylim=c(475, 575)) +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("grey55", "grey75"))+
  guides(fill=F)

# least biased
ggplot(corTrials[corTrials$Subject ==4,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Subject) +
  coord_cartesian(ylim=c(440, 540)) +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("grey55", "grey75"))+
  guides(fill=F)

# middle
ggplot(corTrials[corTrials$Subject ==63,], aes(faceRace, TargetWord.RT, fill = wordVal)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Subject) +
  coord_cartesian(ylim=c(500, 600)) +
  labs(x="Race of face prime", y="Reaction Time (ms)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14)) +
  scale_fill_manual(values=c("grey55", "grey75"))+
  guides(fill=F)





# Look at effect across task ----------------------------------------------

# plot over course of task

corTrials$Condition = paste(corTrials$faceRace, corTrials$wordVal, sep = "_")

condense = select(corTrials, Condition, TargetWord.RT, SubTrial) %>% 
  group_by(SubTrial, Condition) %>% 
  summarise(avgRT = mean(TargetWord.RT)) %>% 
  as.data.frame()

ggplot(condense, aes(SubTrial, avgRT, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Reaction Time (ms)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("blue", "blue", "red", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(420,600)) +
  guides(fill=F)

m1 = lmer(TargetWord.RT ~ faceRace * wordVal * SubTrial + 
            (wordVal|Subject) + 
            (1|TargetWord), data=corTrials)

summary(m1)
