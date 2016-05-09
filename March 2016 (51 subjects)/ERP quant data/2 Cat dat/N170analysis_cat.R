require(magrittr)
N170cat = read.delim("March 2016/ERP quant data/2 Cat dat/N170quant_Peak_51subsAggregated.txt")

require(lme4)
require(lmerTest)
# Examine grouping factors using intercept only models
# grouping variables: subjects, electrodes
m1 = lmer(Peak ~ 1 + (1|Subject) + (1|Electrode), data = N170cat)
summary(m1)
# BLUPs show largest N170 peak at P8/T6
coef(m1)[2]
# effect of location? Nope.
lmer(Peak ~ Hemisphere*Coronal + (1|Subject) + (1|Electrode), data = N170cat) %>%
  summary()


# look at fixed effects of trial conditions
m3 = lmer(Peak ~ faceRace * FixArea + (1|Subject) + (1|Electrode), data = N170cat)
summary(m3)
# main effect of faceRace (Black is more negative (bigger))
