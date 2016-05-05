# Read in stim lists, match them with fixations and export as stim lists with fixation points
# for each face
require(dplyr)

# 1. Read in files

# files with fix points
BlackFix = read.delim("BlackFixPoints.txt", stringsAsFactors=F)
WhiteFix = read.delim("WhiteFixPoints.txt", stringsAsFactors=F)

# List1
PrimeWhite1 = read.delim("whitePrimeList1.txt", stringsAsFactors=F)
PrimeBlack1 = read.delim("blackPrimeList1.txt", stringsAsFactors=F)
TargetWhite1 = read.delim("whiteTargetList1.txt", stringsAsFactors=F)
TargetBlack1 = read.delim("blackTargetList1.txt", stringsAsFactors=F)

# List2
PrimeWhite2 = read.delim("whitePrimeList2.txt", stringsAsFactors=F)
PrimeBlack2 = read.delim("blackPrimeList2.txt", stringsAsFactors=F)
TargetWhite2 = read.delim("whiteTargetList2.txt", stringsAsFactors=F)
TargetBlack2 = read.delim("blackTargetList2.txt", stringsAsFactors=F)

# List3
PrimeWhite3 = read.delim("whitePrimeList3.txt", stringsAsFactors=F)
PrimeBlack3 = read.delim("blackPrimeList3.txt", stringsAsFactors=F)
TargetWhite3 = read.delim("whiteTargetList3.txt", stringsAsFactors=F)
TargetBlack3 = read.delim("blackTargetList3.txt", stringsAsFactors=F)

# List4
PrimeWhite4 = read.delim("whitePrimeList4.txt", stringsAsFactors=F)
PrimeBlack4 = read.delim("blackPrimeList4.txt", stringsAsFactors=F)
TargetWhite4 = read.delim("whiteTargetList4.txt", stringsAsFactors=F)
TargetBlack4 = read.delim("blackTargetList4.txt", stringsAsFactors=F)

# 2. Select only filenames in each stim list from whole fix list, write to text files

# List 1
WP1 = WhiteFix[WhiteFix$Filename %in% PrimeWhite1$col,]
write.table(WP1, file = "whitePrimeFixList1.txt", sep = "\t", row.names = F)

BP1 = BlackFix[BlackFix$Filename %in% PrimeBlack1$col,]
write.table(BP1, file = "blackPrimeFixList1.txt", sep = "\t", row.names = F)

WT1 = WhiteFix[WhiteFix$Filename %in% TargetWhite1$col,]
write.table(WT1, file = "whiteTargetFixList1.txt", sep = "\t", row.names = F)

BT1 = BlackFix[BlackFix$Filename %in% TargetBlack1$col,]
write.table(BT1, file = "blackTargetFixList1.txt", sep = "\t", row.names = F)


# List 2
WP2 = WhiteFix[WhiteFix$Filename %in% PrimeWhite2$col,]
write.table(WP2, file = "whitePrimeFixList2.txt", sep = "\t", row.names = F)

BP2 = BlackFix[BlackFix$Filename %in% PrimeBlack2$col,]
write.table(BP2, file = "blackPrimeFixList2.txt", sep = "\t", row.names = F)

WT2 = WhiteFix[WhiteFix$Filename %in% TargetWhite2$col,]
write.table(WT2, file = "whiteTargetFixList2.txt", sep = "\t", row.names = F)

BT2 = BlackFix[BlackFix$Filename %in% TargetBlack2$col,]
write.table(BT2, file = "blackTargetFixList2.txt", sep = "\t", row.names = F)



# List 3
WP3 = WhiteFix[WhiteFix$Filename %in% PrimeWhite3$col,]
write.table(WP3, file = "whitePrimeFixList3.txt", sep = "\t", row.names = F)

BP3 = BlackFix[BlackFix$Filename %in% PrimeBlack3$col,]
write.table(BP3, file = "blackPrimeFixList3.txt", sep = "\t", row.names = F)

WT3 = WhiteFix[WhiteFix$Filename %in% TargetWhite3$col,]
write.table(WT3, file = "whiteTargetFixList3.txt", sep = "\t", row.names = F)

BT3 = BlackFix[BlackFix$Filename %in% TargetBlack3$col,]
write.table(BT3, file = "blackTargetFixList3.txt", sep = "\t", row.names = F)



# List 4
WP4 = WhiteFix[WhiteFix$Filename %in% PrimeWhite4$col,]
write.table(WP4, file = "whitePrimeFixList4.txt", sep = "\t", row.names = F)

BP4 = BlackFix[BlackFix$Filename %in% PrimeBlack4$col,]
write.table(BP4, file = "blackPrimeFixList4.txt", sep = "\t", row.names = F)

WT4 = WhiteFix[WhiteFix$Filename %in% TargetWhite4$col,]
write.table(WT4, file = "whiteTargetFixList4.txt", sep = "\t", row.names = F)

BT4 = BlackFix[BlackFix$Filename %in% TargetBlack4$col,]
write.table(BT4, file = "blackTargetFixList4.txt", sep = "\t", row.names = F)

