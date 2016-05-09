require(dplyr)
require(tidyr)
require(magrittr)

# Compares latencies for N170s and P2s in cat task

# first read in N170 dat
N170 = read.delim("ERP quant data/2 Cat dat/N170quant_Peak_51subsAggregated.txt") %>%
  select(-Hemisphere) %>%
  select(-Coronal)

# aggregate peak P2 data
# peak P2s are calculated within same window that average amp was calculated
# same windows as for eval task

# read in individual file
x = read.delim("ERP quant data/2 Cat dat/Quant files/Sub1_P2quant_peak.dat", header = T, sep = "\t") %>%
  rename(Peak = Amplitude) %>%
  rename(Electrode = Channel)
# add column for subject number
x$Subject = 1
# append the rest of the subjects to same data file using a loop
for (i in 2:51) {
  temp = read.delim(paste("ERP quant data/2 Cat dat/Quant files/Sub", i, "_P2quant_peak.dat", sep = ""), 
                    header = T, sep = "\t") %>%
    rename(Peak = Amplitude) %>%
    rename(Electrode = Channel)
  temp$Subject = i
  x = rbind(x, temp)
}

# add column specifying race of prime
x$faceRace = NA
x$faceRace[grep("Black", x$Source.File)] = "Black"
x$faceRace[grep("White", x$Source.File)] = "White"

# add column specifying fixation area
x$FixArea = NA
x$FixArea[grep("eyes", x$Source.File)] = "eyes"
x$FixArea[grep("fore", x$Source.File)] = "fore"


# Combine P2 and N170 data together
latencyDat = rbind(N170,x)

# add column for condition (helps with plotting)
latencyDat = unite(latencyDat, condition, faceRace, FixArea, sep = "_", remove = F)



require(lme4)
require(lmerTest)

# Look at intercept-only model
m1 = lmer(Latency ~ 1 + (1|Subject) + (1|Electrode) + (1|Source.File), data = latencyDat)
summary(m1)

# Look at contribution of condition
m2 = lmer(Latency ~ 1 + (1|Subject) + (1|Electrode), data = latencyDat)
anova(m1, m2)
# don't include condition as grouping variable


# Adding fixed effects:
lmer(Latency ~ Marker + (1|Subject) + (1|Electrode), data = latencyDat) %>%
  summary()
# main effect of marker (P2/N170): P2 is faster (about 10 ms), p = .003

require(ggplot2)
temp = latencyDat[latencyDat$Subject > 0 & latencyDat$Subject < 10,]
ggplot(temp, aes(Latency, condition, color = Electrode, shape = Marker)) +
  geom_point() +
  coord_cartesian(xlim=c(130, 220)) +
  facet_wrap(~Subject)


