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
  x <- min(which(datav2[i,weeks]>0
           & !is.na(datav2[i,weeks])))
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

# Do some data cleaning to site name
# - If the site name starts with TAC, then replace it with "TAC" only, otherwise keep original value
datav2$site <- ifelse(grepl("^TAC", datav2$site, ignore.case=TRUE), "TAC", datav2$site)

# Only keep records with valid site names. 
site.factor <- as.factor(datav2$site) # converted site to a temp factor variable
bad.levels <- levels(site.factor)[1:13] # listed invalid site names
`%ni%` <- Negate(`%in%`) #Define a new function that is "not in"
datav3 <- subset(datav2, datav2$site %ni% bad.levels) #subset the data to only keep records without bad names


# merge on lat & long data by tree


# aggregate to average lat/long/elevation by site




# clean up
rm(bb, fl, site.factor, weeks, i, bad.levels, datav2, x);gc()

# Write the data as a text file - and produce SAS code to read in data. 
write.foreign(datav3, "ChicoPhen.txt", "ReadChicoPhen.sas", package="SAS")

#################################################################################################
## Use bb or fl against site in anova
library(lattice)
histogram(datav3$budburst) #shows bb for weeks 1-8
histogram(~full.leaf | site, data=datav3)

mean.bb <- tapply(datav3$budburst, datav3$site, mean, na.rm=TRUE)
dotplot(mean.bb)

mean.fl <- tapply(datav3$full.leaf, datav3$site, mean, na.rm=TRUE)
dotplot(mean.fl)

# Add lines for mean, 95% CI, color those with means that aren't in CI red
# make means stand out better
# horizonal axis 2, count on axis 4, 
boxplot(budburst~site, data=datav3, horizontal=TRUE, pch=".", col="gray80", 
        main="Budburst by site", xlab="weeks")


summary(aov(budburst~site, data=datav3)) #anova bb against site
summary(aov(full.leaf~site, data=datav3))


## Setup histogram with lattice for anova results