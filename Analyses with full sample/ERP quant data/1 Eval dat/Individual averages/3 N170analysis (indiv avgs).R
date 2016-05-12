require(magrittr)
datN170 = read.delim("Analyses with full sample/ERP quant data/1 Eval dat/N170quant_Peak_66subsAggregated.txt")

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(Peak ~ 1 + (1|Electrode) + (1|Subject), data = datN170)
summary(m1)
# BLUPs show largest N170 peak at P8/T6
coef(m1)[2]
# look just at position of electrode
lmer(Peak ~ Hemisphere + Coronal + (1|Subject), data = datN170) %>%
  summary()
# no effect

# sink(file = "Analyses with full sample/Model outputs/3 N170_Eval_InterceptOnly_IndivAvgDat.txt")
# summary(m1)
# "________________________________________________________________________________________________"
# coef(m1)
# sink()

# Compare to examine effect of subject- significant
m2a = lmer(Peak ~ 1 + (1|Electrode), data = datN170)
anova(m1,m2a)

# Compare to examine effect of electrode- significant
m2b = lmer(Peak ~ 1 + (1|Subject), data = datN170)
anova(m1,m2b)


# Add fixed effects

# Varying intercept model
m3 = lmer(Peak ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = datN170)
summary(m3)
m3a = lmer(Peak ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = datN170, REML = F)

# Write to output file
# sink(file = "./Analyses with full sample/Model outputs/3.1 N170_Eval_varyingInterceptModel_IndivAvgDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# Maximal model
m4 = lmer(Peak ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (faceRace*FixArea|Electrode), data = datN170)
summary(m4)
m4a = lmer(Peak ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (faceRace*FixArea|Electrode), data = datN170, REML = F)

# Write to output file
# sink(file = "./Analyses with full sample/Model outputs/3.2 N170_Eval_MaximalModel_IndivAvgDat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

# Effects vary by subject but not electrode
m5 = lmer(Peak ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (1|Electrode), data = datN170)
summary(m5)
m5a = lmer(Peak ~ faceRace * FixArea + (faceRace*FixArea|Subject) + (1|Electrode), data = datN170, REML = F)

# Write to output file
sink(file = "./Analyses with full sample/Model outputs/3.3 N170_Eval_randomEffectSubject_IndivAvgDat.txt")
summary(m5)
"________________________________________________________________________________________________"
coef(m5)
"________________________________________________________________________________________________"
summary(m5a)$AICtab
sink()

# Effects vary by subject (except interaction) but not electrode
m6 = lmer(Peak ~ faceRace * FixArea + (faceRace+FixArea|Subject) + (1|Electrode), data = datN170)
summary(m6)
m6a = lmer(Peak ~ faceRace * FixArea + (faceRace+FixArea|Subject) + (1|Electrode), data = datN170, REML = F)

# Write to output file
# sink(file = "./Analyses with full sample/Model outputs/3.4 N170_Eval_randomEffectSubjectWithoutInteraction_IndivAvgDat.txt")
# summary(m6)
# "________________________________________________________________________________________________"
# coef(m6)
# "________________________________________________________________________________________________"
# summary(m6a)$AICtab
# sink()
