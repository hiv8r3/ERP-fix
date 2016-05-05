# Cat task
require(dplyr)
require(tidyr)
# Investigate whether the latency of the N170 has effect on effect of fixation
# MLM looking at peak amplitude of N170 (ignoring latency) finds main effect of race, no effect of fixation

# investigate hypothesis that the early the N170, the larger the effect of fixation

N170cat = read.delim("ERP quant data/2 Cat dat/N170quant_Peak_51subsAggregated.txt")
# add column for condition (helps with plotting)
N170cat = unite(N170cat, condition, faceRace, FixArea, sep = "_", remove = F)


# create difference scores comparing fixation conditions
# BlackDiff = Black-fore - Black-eyes 
# WhiteDiff = White-fore - White-eyes

diffDatCat = data.frame(Subject = rep(1:51, each = 8),
                     Electrode = rep(c("P8/T6", "TP7", "TP8", "P7/T5"), 102),
                     DiffType = rep(c("BlackDiff", "WhiteDiff"), each = 4, 51),
                     AvgLatency = NA,
                     PeakDiff = NA)

# loop to compute difference scores of peaks and average latencies
for (i in 1:51) {
  for (j in c("P8/T6", "TP7", "TP8", "P7/T5")) {
    N170temp = N170cat[N170cat$Subject == i & N170cat$Electrode == j,]
    # Create difference score in peaks between eyes and forehead fixations for Black faces
    diffDatCat$PeakDiff[diffDatCat$Subject == i & 
                       diffDatCat$Electrode == j & 
                       diffDatCat$DiffType == "BlackDiff"] = N170temp$Peak[N170temp$condition == "Black_fore"] - 
      N170temp$Peak[N170temp$condition == "Black_eyes"]
    # Create difference score in peaks between eyes and forehead fixations for White faces
    diffDatCat$PeakDiff[diffDatCat$Subject == i & 
                       diffDatCat$Electrode == j & 
                       diffDatCat$DiffType == "WhiteDiff"] = N170temp$Peak[N170temp$condition == "White_fore"] - 
      N170temp$Peak[N170temp$condition == "White_eyes"]
    
    # Compute average latency for eyes and forehead fixations for Black faces
    diffDatCat$AvgLatency[diffDatCat$Subject == i & 
                         diffDatCat$Electrode == j & 
                         diffDatCat$DiffType == "BlackDiff"] = (N170temp$Latency[N170temp$condition == "Black_fore"] + 
                                                               N170temp$Latency[N170temp$condition == "Black_eyes"])/2
    # Compute average latency eyes and forehead fixations for White faces
    diffDatCat$AvgLatency[diffDatCat$Subject == i & 
                         diffDatCat$Electrode == j & 
                         diffDatCat$DiffType == "WhiteDiff"] = (N170temp$Latency[N170temp$condition == "White_fore"] + 
                                                               N170temp$Latency[N170temp$condition == "White_eyes"])/2
    
  }
}

# If PeakDiff is negative, larger N170 to eyes than forehead
# If PeakDiff is positive, larger N170 to forehead than eyes



require(ggplot2)
ggplot(diffDatCat, aes(AvgLatency, PeakDiff, color = Electrode)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~DiffType) + 
  ggtitle("Cat task")

ggplot(diffDatCat, aes(AvgLatency, PeakDiff, color = DiffType)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~Electrode) +
  ggtitle("Cat task")

require(lme4)
require(lmerTest)
# looking at fixed effects, allowing varying intercept for subject
lmer(PeakDiff ~ AvgLatency * DiffType + (1|Subject), data = diffDatCat) %>%
  summary
# no effect of latency
# main effect of race of trial, p = .015
# significant interaction, p = .021

# just for Black trials
lmer(PeakDiff ~ AvgLatency + (1|Subject), data = diffDatCat[diffDatCat$DiffType == "BlackDiff",]) %>%
  summary
# no effect of latency

# just for White trials
lmer(PeakDiff ~ AvgLatency + (1|Subject), data = diffDatCat[diffDatCat$DiffType == "WhiteDiff",]) %>%
  summary
# marginal effect of latency