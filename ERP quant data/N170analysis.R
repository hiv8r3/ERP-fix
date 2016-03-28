datN170 = read.delim("ERP quant data/N170quant_Peak_51subsAggregated.txt")

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(Peak ~ 1 + (1|Subject) + (1|Electrode), data = datN170)
summary(m1)

# Compare to examine effect of subject- significant
m2a = lmer(Peak ~ 1 + (1|Electrode), data = datN170)
anova(m1,m2a)

# Compare to examine effect of electrode- significant
m2b = lmer(Peak ~ 1 + (1|Subject), data = datN170)
anova(m1,m2b)


# Add fixed effects
# Next step: Include fixed effects
m3 = lmer(Peak ~ faceRace * FixArea + Hemisphere + (1|Subject) + (1|Electrode), data = datN170)
summary(m3)
# main effect of faceRace (Black is more negative (bigger))
# estimate for hemisphere is NOT whether one hemisphere has bigger N170s
# it's whether effect of faceRace and FixArea DIFFER across hemispheres