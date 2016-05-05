require(magrittr)
require(dplyr)

dat = read.delim("./Behavioral/Merge51.txt") %>%
  select(Subject,
         DemoAge.RESP,
         DemoEthnicity.RESP,
         DemoGender.RESP,
         DemoRace.RESP,
         responseMappingRace,
         responseMappingVal,
         Block,
         Trial,
         SubTrial,
         Procedure.Trial.,
         faceRace,
         FixArea,
         wordVal,
         responseAccData,       # 1 = incorrect, 2 = correct, 3 = too slow
         TargetFace.ACC,
         TargetFace.RT,
         TargetWord.ACC,
         TargetWord.RT,
         Effort.RESP,
         Attentive.RESP,
         PrimePic,  # for eval block
         TargetPic, # for cat block
         TargetWord) %>%
  rename(Task = Procedure.Trial.)

evalDat = dat[grep("Eval", dat$Task),] %>%
  select(-contains("TargetFace")) %>%
  select(-TargetPic)
# Practice: Trial = 1, SubTrial = 1...16
# First half: Trial = 2, SubTrial = 1...256
# Second half: Trial = 3, SubTrial = 1...256

# Make SubTrial go from 1 to 512
evalDat$SubTrial[evalDat$Trial == 3] = evalDat$SubTrial[evalDat$Trial == 3] + 256

# Add column to separate task into 4 chunks
evalDat$Chunk = 0
evalDat$Chunk[evalDat$SubTrial <= 128] = 1
evalDat$Chunk[evalDat$SubTrial > 128 & evalDat$SubTrial <= 256] = 2
evalDat$Chunk[evalDat$SubTrial > 256 & evalDat$SubTrial <= 384] = 3
evalDat$Chunk[evalDat$SubTrial > 384] = 4

# make column defining type of trial
evalDat = unite(evalDat, TrialType, faceRace, FixArea, wordVal, remove = F)



## Do the same for cat data
catDat = dat[grep("Cat", dat$Task),] %>%
  select(-contains("TargetWord")) %>%
  select(-wordVal) %>%
  select(-PrimePic)
# Practice: Trial = 1, SubTrial = 1...8
# First half: Trial = 2, SubTrial = 1...128
# Second half: Trial = 3, SubTrial = 1...128

# Make SubTrial go from 1 to 256
catDat$SubTrial[catDat$Trial == 3] = catDat$SubTrial[catDat$Trial == 3] + 128

# make column defining type of trial
catDat = unite(catDat, TrialType, faceRace, FixArea, remove = F)

# response accuracy for Black eyes trials was coded wrong for some reason 
# ("correct response" as determined by E-Prime was wrong)
# need to change responseAccData and TargetFace.ACC columns for Black eyes trials
catDat = rename(catDat, TargetFace.ACC.wrong = TargetFace.ACC) %>%
  rename(responseAccDat.wrong = responseAccData)

catDat$responseAccDat = NA
# all non Black eyes trials stay the same
catDat$responseAccDat[catDat$TrialType != "Black_eyes"] = catDat$responseAccDat.wrong[catDat$TrialType != "Black_eyes"]
# for Black eyes trials, change 1 to 2 and 2 to 1
catDat$responseAccDat[catDat$responseAccDat.wrong == 1 & catDat$TrialType == "Black_eyes"] = 2
catDat$responseAccDat[catDat$responseAccDat.wrong == 2 & catDat$TrialType == "Black_eyes"] = 1
catDat$responseAccDat[catDat$responseAccDat.wrong == 3 & catDat$TrialType == "Black_eyes"] = 3

# same for TargetFace.ACC
catDat$TargetFace.ACC = NA
# all non Black eyes trials stay the same
catDat$TargetFace.ACC[catDat$responseAccDat == 1] = 0
catDat$TargetFace.ACC[catDat$responseAccDat == 2] = 1
catDat$TargetFace.ACC[catDat$responseAccDat == 3] = 0

write.table(evalDat, "./Behavioral/evalDat.txt", sep = "\t", row.names = F)
write.table(catDat, "./Behavioral/catDat.txt", sep = "\t", row.names = F)
