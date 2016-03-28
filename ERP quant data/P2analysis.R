dat = read.delim("ERP quant data/P2quant_AvgAmp_51subsAggregated.txt")

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

# Compare to examine effect of subject
m2a = lmer(AvgAmp ~ 1 + (1|Electrode), data = select)
anova(m1,m2a)

# Compare to examine effect of electrode- not significant, but keep in anyway
# enough electrodes to use as grouping variable?? Or just fixed effect?
m2b = lmer(AvgAmp ~ 1 + (1|Subject), data = select)
anova(m1,m2b)

# Next step: Include fixed effects
m3 = lmer(AvgAmp ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = select)
summary(m3)
# main effect of race (Black is bigger)
# main effect of fix (eyes is bigger)

m4 = lmer(AvgAmp ~ faceRace * FixArea + Electrode + (1|Subject), data = select)   # doesn't seem to be effect of electrode
summary(m4)



# check out blups: at what electrode is intercept maximal?
