require(dplyr)

dat = read.delim("./march 2016/ERP quant data/2 Cat dat/P2quant_AvgAmp_51subsAggregated.txt")

elec = c("CZ", "CPZ", "PZ")

select = dat[dat$Electrode %in% elec,]
select$Electrode = factor(select$Electrode)
select$Subject = factor(select$Subject)   # I don't think subject needs to be factor to be used as a grouping variable

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Electrode), data = select)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]


# Next step: Include fixed effects
m3 = lmer(AvgAmp ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = select)
summary(m3)
# main effect of race (Black is bigger)
# main effect of fix (eyes is bigger)
# significant interaction

# Look at effect of fixation within Black faces
m3a = lmer(AvgAmp ~ FixArea + (1|Subject) + (1|Electrode), data = select[select$faceRace == "Black",])
summary(m3a)

# Look at effect of fixation within White faces
m3b = lmer(AvgAmp ~ FixArea + (1|Subject) + (1|Electrode), data = select[select$faceRace == "White",])
summary(m3b)

# Look at effect of race within eye fixation
m4a = lmer(AvgAmp ~ faceRace + (1|Subject) + (1|Electrode), data = select[select$FixArea == "eyes",])
summary(m4a)

# Look at effect of race within forehead fixation
m4a = lmer(AvgAmp ~ faceRace + (1|Subject) + (1|Electrode), data = select[select$FixArea == "fore",])
summary(m4a)

