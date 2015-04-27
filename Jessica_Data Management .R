# Getting started
# Started: April 24, 2015. Begin to assemble data, and improve legibility 
library(xlsx)

setwd("/Users/SamKrasnobrod/GitHub/OakResearchRepoSP2015") # Samuel
setwd("C:/GitHub/OakResearchRepoSP2015") # Robin

##########################################################################################################################                                                      

# This step takes a while to read in. 
datav2 <- read.xlsx("ChicoPhenv2.xlsx", sheetIndex=1, header=TRUE, rowIndex = c(1, 3:1000), colIndex=c(1:22))     

# Add a count of days from planting until bud burst (First recorded 1)
bb <- rep(NA, NROW(datav2))
for(i in 1:NROW(datav2)){
    x <- min(which(datav2[i,c(11:20)]==1))*7
    bb[i] <- ifelse(x<Inf, x, NA)
}
datav2$budburst <- bb
  
# Add a count of days from planting until full leafs (First recorded 5)
fl <- rep(NA, NROW(datav2))
for(i in 1:NROW(datav2)){
  x <- min(which(datav2[i,c(11:20)]==5))*7
  fl[i] <- ifelse(x<Inf, x, NA)
}
datav2$days.to.leaf <- fl



