# Put P2 data for both tasks together 

# read in data from evaluation block
eval = read.delim("ERP quant data/1 Eval dat/P2quant_AvgAmp_51subsAggregated.txt")
eval$Task = "Eval"

# read in data from categorization block
cat = read.delim("ERP quant data/2 Cat dat/P2quant_AvgAmp_51subsAggregated.txt")
cat$Task = "Cat"

# combine in data set
combined = rbind(eval, cat)

# select relevant electrodes
elec = c("CZ", "CPZ", "PZ")
combined = combined[combined$Electrode %in% elec,]
combined$Subject = factor(combined$Subject)
combined$Task = factor(combined$Task)

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Electrode), data = combined)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]

# Next step: Include fixed effects
m3 = lmer(AvgAmp ~ faceRace * FixArea * Task + (1|Subject) + (1|Electrode), data = combined)
summary(m3)
# main effect of race (Black is bigger)
# main effect of fix (eyes is bigger)
# main effect of task (cat is bigger)
# race x fix interaction is significant
# 3 way race x fix x task interaction is NOT significant