# Investigate whether the latency of the N170 has effect on effect of fixation
# MLM looking at peak amplitude of N170 (ignoring latency) finds main effect of race, no effect of fixation

# investigate hypothesis that the early the N170, the larger the effect of fixation

N170 = read.delim("ERP quant data/1 Eval dat/N170quant_Peak_51subsAggregated.txt")
# add column for condition (helps with plotting)
N170 = unite(N170, condition, faceRace, FixArea, sep = "_", remove = F)


# create difference scores comparing fixation conditions
# BlackDiff = Black-fore - Black-eyes 
# WhiteDiff = White-fore - White-eyes

diffDatEval = data.frame(Subject = rep(1:51, each = 8),
                      Electrode = rep(c("P8/T6", "TP7", "TP8", "P7/T5"), 102),
                      DiffType = rep(c("BlackDiff", "WhiteDiff"), each = 4, 51),
                      AvgLatency = NA,
                      PeakDiff = NA)

# loop to compute difference scores of peaks and average latencies
for (i in 1:51) {
  for (j in c("P8/T6", "TP7", "TP8", "P7/T5")) {
    N170temp = N170[N170$Subject == i & N170$Electrode == j,]
    # Create difference score in peaks between eyes and forehead fixations for Black faces
    diffDatEval$PeakDiff[diffDatEval$Subject == i & 
                       diffDatEval$Electrode == j & 
                       diffDatEval$DiffType == "BlackDiff"] = N170temp$Peak[N170temp$condition == "Black_fore"] - 
                                                            N170temp$Peak[N170temp$condition == "Black_eyes"]
    # Create difference score in peaks between eyes and forehead fixations for White faces
    diffDatEval$PeakDiff[diffDatEval$Subject == i & 
                       diffDatEval$Electrode == j & 
                       diffDatEval$DiffType == "WhiteDiff"] = N170temp$Peak[N170temp$condition == "White_fore"] - 
                                                            N170temp$Peak[N170temp$condition == "White_eyes"]

    # Compute average latency for eyes and forehead fixations for Black faces
    diffDatEval$AvgLatency[diffDatEval$Subject == i & 
                       diffDatEval$Electrode == j & 
                       diffDatEval$DiffType == "BlackDiff"] = (N170temp$Latency[N170temp$condition == "Black_fore"] + 
                                                            N170temp$Latency[N170temp$condition == "Black_eyes"])/2
    # Compute average latency eyes and forehead fixations for White faces
    diffDatEval$AvgLatency[diffDatEval$Subject == i & 
                       diffDatEval$Electrode == j & 
                       diffDatEval$DiffType == "WhiteDiff"] = (N170temp$Latency[N170temp$condition == "White_fore"] + 
                                                            N170temp$Latency[N170temp$condition == "White_eyes"])/2
    
  }
}

# If PeakDiff is negative, larger N170 to eyes than forehead
# If PeakDiff is positive, larger N170 to forehead than eyes

require(ggplot2)
ggplot(diffDatEval, aes(AvgLatency, PeakDiff, color = Electrode)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~DiffType) + 
  ggtitle("Eval task")

ggplot(diffDatEval, aes(AvgLatency, PeakDiff, color = DiffType)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~Electrode) +
  ggtitle("Eval task")
# interesting moderation by DiffType on relationship between Latency and differences 
# in peak amp between eyes/forehead conditions

require(lme4)
require(lmerTest)
lmer(PeakDiff ~ 1 + (1|Subject) + (1|Electrode), data = diffDatEval) %>%
  summary()

# looking at fixed effects, allowing varying intercept for subject
lmer(PeakDiff ~ AvgLatency * DiffType + (1|Subject), data = diffDatEval) %>%
  summary
# main effect of latency, p = .005
# main effect of race of trial, p = .01
# significant interaction, p = .010

# just for Black trials
lmer(PeakDiff ~ AvgLatency + (1|Subject), data = diffDatEval[diffDatEval$DiffType == "BlackDiff",]) %>%
  summary
# significant effect of latency
# beta = -.019
# p = .022

# just for White trials
lmer(PeakDiff ~ AvgLatency + (1|Subject), data = diffDatEval[diffDatEval$DiffType == "WhiteDiff",]) %>%
  summary
# no significant effect of latency

# take out weird person with really negative diff score
mean = mean(diffDatEval$PeakDiff[diffDatEval$DiffType =="WhiteDiff"])
sd = sd(diffDatEval$PeakDiff[diffDatEval$DiffType =="WhiteDiff"])

noOut = diffDatEval[diffDatEval$DiffType == "WhiteDiff" & diffDatEvalt$PeakDiff > mean-3*sd,]
lmer(PeakDiff ~ AvgLatency + (1|Subject), data = noOut) %>%
  summary
