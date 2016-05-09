dat = read.delim("./March 2016/ERP quant data/1 Eval dat/P2quant_AvgAmp_51subsAggregated.txt")

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

# Compare to examine effect of subject
m2a = lmer(AvgAmp ~ 1 + (1|Electrode), data = select)
anova(m1,m2a)

# Compare to examine effect of electrode- not significant, but keep in anyway
# enough electrodes to use as grouping variable?? Or just fixed effect?
m2b = lmer(AvgAmp ~ 1 + (1|Subject), data = select)
anova(m1,m2b)

# Next step: Include fixed effects
m3 = lmer(AvgAmp ~ faceRace * FixArea + Electrode + (1|Subject), data = select)
summary(m3)
# main effect of race (Black is bigger)
# main effect of fix (eyes is bigger)
# fit with FIML in order to get AIC
m3a = lmer(AvgAmp ~ faceRace * FixArea + Electrode + (1|Subject), data = select, REML = F)


# sink(file = "./March 2016/Model outputs/2.1 P2_Eval_varyingInterceptModel_IndivAvgDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

m4 = lmer(AvgAmp ~ faceRace * FixArea + Electrode + (faceRace*FixArea|Subject), data = select)
summary(m4)
coef(m4)
# fit with FIML in order to get AIC
m4a = lmer(AvgAmp ~ faceRace * FixArea + Electrode + (faceRace*FixArea|Subject), data = select, REML = F)

# sink(file = "./March 2016/Model outputs/2.2 P2_Eval_MaximalModel_IndivAvgDat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()


anova(m3, m4)

