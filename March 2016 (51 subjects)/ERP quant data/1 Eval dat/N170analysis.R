require(magrittr)
datN170 = read.delim("March 2016/ERP quant data/1 Eval dat/N170quant_Peak_51subsAggregated.txt")

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(Peak ~ 1 + (1|Subject) + (1|Electrode), data = datN170)
summary(m1)
# BLUPs show largest N170 peak at P8/T6
coef(m1)[2]
# look just at position of electrode
lmer(Peak ~ Hemisphere + Coronal + (1|Subject) + (1|Electrode), data = datN170) %>%
  summary()
# no effect

# Compare to examine effect of subject- significant
m2a = lmer(Peak ~ 1 + (1|Electrode), data = datN170)
anova(m1,m2a)

# Compare to examine effect of electrode- significant
m2b = lmer(Peak ~ 1 + (1|Subject), data = datN170)
anova(m1,m2b)


# Add fixed effects

# look at fixed effects of trial conditions
m3 = lmer(Peak ~ faceRace * FixArea + Electrode + (1|Subject), data = datN170)
summary(m3)
# main effect of faceRace (Black is more negative (bigger))
# Including Electrode as fixed effect or grouping variable doesn't change fixed effects
# In this case, having electrode as grouping variable increases AIC (worse model)
m3a = lmer(Peak ~ faceRace * FixArea + Electrode + (1|Subject), data = datN170, REML = F)

# Write to output file
# sink(file = "./March 2016/Model outputs/3.1 N170_Eval_varyingInterceptModel_IndivAvgDat.txt")
# summary(m3)
# "________________________________________________________________________________________________"
# coef(m3)
# "________________________________________________________________________________________________"
# summary(m3a)$AICtab
# sink()

# use electrode as fixed effect, not random because not enough levels

# look at fixed effects of trial conditions
m4 = lmer(Peak ~ faceRace * FixArea + Electrode + (faceRace*FixArea|Subject), data = datN170)
summary(m4)
m4a = lmer(Peak ~ faceRace * FixArea + Electrode + (faceRace*FixArea|Subject), data = datN170, REML = F)

# Write to output file
# sink(file = "./March 2016/Model outputs/3.2 N170_Eval_MaximalModel_IndivAvgDat.txt")
# summary(m4)
# "________________________________________________________________________________________________"
# coef(m4)
# "________________________________________________________________________________________________"
# summary(m4a)$AICtab
# sink()

# compare two models: significant
anova(m3, m4)

# looking just at hemisphere
m3a = lmer(Peak ~ Hemisphere + (1|Subject) + (1|Electrode), data = datN170)
summary(m3a) # not bigger on one side than the other

# Look at latency instead of peak?
lmer(Latency ~ 1 + (1|Subject) + (1|Electrode), data = datN170)%>%
  summary()

require(ggplot2)
temp = datN170[datN170$Subject == 6,]
ggplot(temp, aes(Latency, Source.File, color = Electrode, shape = Hemisphere)) +
  geom_point() +
  coord_cartesian(xlim=c(130, 220))
