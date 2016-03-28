# Creates stim lists for 4 experiments
# Picks 30 numbers randomly from 60
# Sets those as prime filenames
# Sets the rest of the numbers as target filenames
# Exports to as text file
require(dplyr)
require(tidyr)
require(magrittr)

v60 = rep(1:60, each=1)

# generates prime and target lists for Black faces (Exps 1-4)
for (i in 1:4) {
  primeFilename = paste("blackPrimeList", i, ".txt", sep="") # make filename. Paste() combines strings together.
  targetFilename = paste("blackTargetList", i, ".txt", sep="")
  primeTemp = sample(v60, size = 30) 
  targetTemp = v60[-primeTemp]
  
  primeTemp = sort(primeTemp) %>%
    data.frame(1,.)
  primeTemp$Race = "Black"
  primeTemp$Filename = ".jpg"
  primeTemp = rename(primeTemp, num = .) %>%
    unite(col, c(Race, num, Filename), sep = "") %>%
    write.table(file = primeFilename, sep = "\t", row.names = FALSE)
  
  
  targetTemp = sort(targetTemp) %>%
    data.frame(1,.)
  targetTemp$Race = "Black"
  targetTemp$Filename = ".jpg"
  targetTemp = rename(targetTemp, num = .) %>%
    unite(col, c(Race, num, Filename), sep = "") %>%
    write.table(file = targetFilename, sep = "\t", row.names = FALSE)
}


# generates prime and target lists for White faces (Exps 1-4)
for (i in 1:4) {
  primeFilename = paste("whitePrimeList", i, ".txt", sep="") # make filename. Paste() combines strings together.
  targetFilename = paste("whiteTargetList", i, ".txt", sep="")
  primeTemp = sample(v60, size = 30) 
  targetTemp = v60[-primeTemp]
  
  primeTemp = sort(primeTemp) %>%
    data.frame(1,.)
  primeTemp$Race = "White"
  primeTemp$Filename = ".jpg"
  primeTemp = rename(primeTemp, num = .) %>%
    unite(col, c(Race, num, Filename), sep = "") %>%
    write.table(file = primeFilename, sep = "\t", row.names = FALSE)
  
  
  targetTemp = sort(targetTemp) %>%
    data.frame(1,.)
  targetTemp$Race = "White"
  targetTemp$Filename = ".jpg"
  targetTemp = rename(targetTemp, num = .) %>%
    unite(col, c(Race, num, Filename), sep = "") %>%
    write.table(file = targetFilename, sep = "\t", row.names = FALSE)
}