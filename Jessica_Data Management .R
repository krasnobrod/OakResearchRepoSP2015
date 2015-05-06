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


# Sort by lat (instead of site)



# clean up
rm(bb, fl, site.factor, weeks, i, bad.levels, datav2, x);gc()

# Write the data as a text file - and produce SAS code to read in data. 
write.foreign(datav3, "ChicoPhen.txt", "ReadChicoPhen.sas", package="SAS")

#################################################################################################
## Use bb or fl against site in anova
library(lattice)
histogram(datav3$budburst) #shows bb for weeks 1-8
histogram(~full.leaf | site, data=datav3)

# CALCULATE THE MEAN VALUE PER SITE
mean.bb <- tapply(datav3$budburst, datav3$site, mean, na.rm=TRUE)
mean.fl <- tapply(datav3$full.leaf, datav3$site, mean, na.rm=TRUE)


# OVERALL MEANS - TO BE PLOTTED AS THE VERTICAL LINE
mean.total.bb <- mean(datav3$budburst, na.rm=TRUE)
mean.total.fl <- mean(datav3$full.leaf, na.rm=TRUE)

# Calculate the lower and upper quartiles for each variable by site
q1.bb <- tapply(datav3$budburst, datav3$site, function(x)quantile(x,.25, na.rm=TRUE))
q3.bb <- tapply(datav3$budburst, datav3$site, function(x)quantile(x,.75, na.rm=TRUE))
# COPY/PASTE CHANGE BB TO FL
q1.fl <- tapply(datav3$full.leaf, datav3$site, function(x)quantile(x,.25, na.rm=TRUE))
q3.fl <- tapply(datav3$full.leaf, datav3$site, function(x)quantile(x,.75, na.rm=TRUE))
# create a vector of colors, where if the Q1 is above the mean or Q3 is below the mean color it red.
color.bb <- ifelse(q1.bb>mean.total.bb | q3.bb<mean.total.bb, "red", "gray90")
#copy/paste and change bb to fl
color.fl <- ifelse(q1.fl>mean.total.fl | q3.bb<mean.total.fl, "red", "gray90")


#### PLOT BUDBURST DISTRIBUTION BY SITE ###

png(filename="budburst.png", width=480, height=1100) # get this height looking good before copy 

boxplot(budburst~site, data=datav3, horizontal=TRUE, axes=FALSE, pch=".", col=color.bb, 
        main="Week of first Bud burst by site", xlab="weeks")
axis(2, las=2, at=1:length(mean.bb), label=names(mean.bb), cex.axis=0.28)
axis(1, at=1:10)
box()
abline(v=mean.total.bb, col="slateblue", lwd=2)
points(mean.bb, 1:length(mean.bb), col="blue", pch=16)

dev.off()

# Copy/paste all above code and change to fl
png(filename="full.leaf.png", width=480, height=1100) 

boxplot(full.leaf~site, data=datav3, horizontal=TRUE, axes=FALSE, pch=".", col=color.bb, 
        main="Week of first Full Leaf by site", xlab="weeks")
axis(2, las=2, at=1:length(mean.fl), label=names(mean.fl), cex.axis=.28)
axis(1, at=1:10)
box()
abline(v=mean.total.fl, col="slateblue", lwd=2)
points(mean.fl, 1:length(mean.fl), col="blue", pch=16)


######################################################################

summary(aov(budburst~site, data=datav3)) #anova bb against site
summary(aov(full.leaf~site, data=datav3))


## Setup histogram with lattice for anova results