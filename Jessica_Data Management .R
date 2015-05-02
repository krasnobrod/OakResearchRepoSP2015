# Getting started
# Started: April 24, 2015. Begin to assemble data, and improve legibility 
library(foreign)
library(lattice)


setwd("/Users/SamKrasnobrod/GitHub/OakResearchRepoSP2015") # Samuel
setwd("C:/GitHub/OakResearchRepoSP2015") # Robin

##########################################################################################################################                                                      

# This step takes a while to read in. 
#datav2 <- read.xlsx("ChicoPhenv2.xlsx", sheetIndex=1, header=TRUE, rowIndex = c(1, 3:3678), colIndex=c(1:22))     
datav2 <- read.csv("ChicoPhenv2.csv", header=TRUE, stringsAsFactors=FALSE)     

weeks <- which(grepl("^week", names(datav2), ignore.case=TRUE))

# Add a count of days from planting until bud burst (First recorded 1)
bb <- rep(NA, NROW(datav2))
for(i in 1:NROW(datav2)){
    x <- min(which(datav2[i,weeks]==1))
    bb[i] <- ifelse(x<Inf, x, NA)
}
datav2$budburst <- bb
  
# Add a count of weeks from planting until full leafs (First recorded 5)
fl <- rep(NA, NROW(datav2))
for(i in 1:NROW(datav2)){
  x <- min(which(datav2[i,weeks]==5))
  fl[i] <- ifelse(x<Inf, x, NA)
}
datav2$full.leaf <- fl


# Write the data as a text file - and produce SAS code to read in data. 
write.foreign(datav2, "ChicoPhen.txt", "ReadChicoPhen.sas", package="SAS")


## Use bb or fl against site in anova
hist(bb) #shows bb for weeks 1-8
shist(fl)
summary(aov(bb~datav2$site)) #anova bb against site
summary(aov(fl~datav2$site))
aov(bb~datav2$site)
view(datav2$site)

## Setup histogram with lattice for anova results