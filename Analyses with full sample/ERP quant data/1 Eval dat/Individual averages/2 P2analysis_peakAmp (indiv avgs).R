dat = read.delim("./Analyses with full sample/ERP quant data/1 Eval dat/Individual averages/AllSubs_IndivAverages_P2_peakAmp.txt")

elec = c("CZ", "C3", "C4", "CPZ", "CP3", "CP4", "PZ")

select = dat[dat$Electrode %in% elec,]
select$Electrode = factor(select$Electrode)

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(PeakAmp ~ 1 + (1|Subject) + (1|Electrode), data = select)
summary(m1)
# BLUPs show largest P2 peak at CZ
coef(m1)[2]

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/2 P2_peakAmp_InterceptOnly_IndivAvgDat.txt")
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Compare to examine effect of subject
m2a = lmer(PeakAmp ~ 1 + (1|Electrode), data = select)
anova(m1,m2a)

# Compare to examine effect of electrode
m2b = lmer(PeakAmp ~ 1 + (1|Subject), data = select)
anova(m1,m2b)

# Next step: Include fixed effects

# Varying intercept only
m3 = lmer(PeakAmp ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = select)
summary(m3)
# main effect of race (Black is bigger)
# main effect of fix (eyes is bigger)
# Race x Fix interaction
# fit with FIML in order to get AIC
m3a = lmer(PeakAmp ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = select, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/2.1 P2_peakAmp_varyingInterceptModel_IndivAvgDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# Maximal model
m4 = lmer(PeakAmp ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (faceRace*FixArea|Electrode), data = select)
summary(m4)
coef(m4)
# fit with FIML in order to get AIC
m4a = lmer(PeakAmp ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (faceRace*FixArea|Electrode), data = select, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/2.2 P2_peakAmp__MaximalModel_IndivAvgDat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

# still somewhat high. Let's try all random effects by subject but not by electrode
m6 = lmer(PeakAmp ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (1|Electrode), data = select)
summary(m6)
# fit with FIML in order to get AIC
m6a = lmer(PeakAmp ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (1|Electrode), data = select, REML = F)

# sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Peak amp analyses/2.4 P2_peakAmp_randomEffectsSubject_IndivAvgDat.txt")
# summary(m6)
# "________________________________________________________________________________________________"
# coef(m6)
# "________________________________________________________________________________________________"
# summary(m6a)$AICtab
# sink()



############### Repeated measures ANOVA ##########################

select$Subject = factor(select$Subject)   # Subject needs to be a factor for RMANOVA, but not MLM

aov1 = aov(PeakAmp ~ faceRace*FixArea + Error(Subject/(faceRace*FixArea)), data = select) 

sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Repeated measures ANOVA/2 P2_peakAmp_RManova_IndivAvgDat.txt")
"aov(PeakAmp ~ faceRace*FixArea + Error(Subject/(faceRace*FixArea)), data = select)"
summary(aov1)
sink()

# include electrode as fixed effect
aov2 = aov(PeakAmp ~ faceRace*FixArea + Electrode + Error(Subject/(faceRace*FixArea*Electrode)), data = select) 

sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Repeated measures ANOVA/2.2 P2_peakAmp_RManova_electrodeFixed_IndivAvgDat.txt")
"aov(PeakAmp ~ faceRace*FixArea + Electrode + Error(Subject/(faceRace*FixArea*Electrode)), data = select) "
summary(aov2)
sink()

# include electrode as fixed effect plus interactions
aov3 = aov(PeakAmp ~ faceRace*FixArea*Electrode + Error(Subject/(faceRace*FixArea*Electrode)), data = select) 

sink(file = "./Analyses with full sample/Model outputs/P2 analyses/Repeated measures ANOVA/2.3 P2_peakAmp_RManova_electrodeFixedWithInteractions_IndivAvgDat.txt")
"aov(PeakAmp ~ faceRace*FixArea*Electrode + Error(Subject/(faceRace*FixArea*Electrode)), data = select) "
summary(aov3)
sink()

