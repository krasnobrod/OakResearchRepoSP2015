# Getting started
# Started: April 24, 2015. Begin to assemble data, and improve legibility
library(foreign)
library(lattice)
library(dplyr)

setwd("/Users/SamKrasnobrod/GitHub/OakResearchRepoSP2015") # Samuel
setwd("C:/GitHub/OakResearchRepoSP2015") # Robin

##########################################################################################################################
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
latlong      <- read.csv("Source data copy.csv", header=TRUE,  stringsAsFactors=FALSE)
latlong$site <- ifelse(grepl("^TAC", latlong$Site.abb., ignore.case=TRUE), "TAC", latlong$Site.abb.)
 # Create a new Tree ID
latlong$id <- with(latlong, paste(site ,  Acc..No., Tree.No., sep='-'))
datav3$id  <- datav3$Use.this.tree
latlong    <- latlong[,-c(1:4)]

# merge on matching  variables (site, tree id)
datav4 <- left_join (datav3, latlong)
datav4$sitelat <- reorder(datav4$site, datav4$Latitude, min)
datav4 <- datav4[,-c(5:7)]


# clean up
rm(bb, fl, site.factor, weeks, i, bad.levels, datav2, x, latlong, datav3);gc()

#################################################################################################
## Use bb or fl against site in anova

histogram(datav4$budburst) #shows bb for weeks 1-8
histogram(~full.leaf | site, data=datav4)

# OVERALL MEANS - TO BE PLOTTED AS THE VERTICAL LINE
mean.total.bb <- mean(datav4$budburst, na.rm=TRUE)
mean.total.fl <- mean(datav4$full.leaf, na.rm=TRUE)

# CALCULATE summary statistics PER SITE
# Also create a vector of colors, where if the Q1 is above the mean or Q3 is below the mean color it red.
# -- this is data management code using dplyr. High end novice stage learning, but easier
# -- sometimes to use.
bysite <- datav4 %>%
          group_by(sitelat) %>%
          summarise(mean.bb = mean(budburst, na.rm=TRUE),
                    mean.fl = mean(full.leaf, na.rm=TRUE),
                    q1.bb = quantile(budburst,.25, na.rm=TRUE),
                    q3.bb = quantile(budburst,.75, na.rm=TRUE),
                    q1.fl = quantile(full.leaf,.25, na.rm=TRUE),
                    q3.fl = quantile(full.leaf,.75, na.rm=TRUE),
                    minlat = round(min(Latitude, na.rm=TRUE), 3)) %>%
    mutate(color.bb = ifelse(q1.bb>mean.total.bb | q3.bb<mean.total.bb, "red", "gray90"),
           color.fl = ifelse(q1.fl>mean.total.fl | q3.fl<mean.total.fl, "red", "gray90"))



#### PLOT BUDBURST DISTRIBUTION BY SITE ###
y <- NROW(bysite)

png(filename="budburst.png", width=480, height=1100) # get this height looking good before copy
par(mar=c(5,4,4,5))
boxplot(budburst~sitelat, data=datav4, horizontal=TRUE, axes=FALSE,
        col=bysite$color.bb, cex=.2, ylab="Site",
        main="Week of first Bud burst by site", xlab="weeks")
axis(2, las=2, at=1:y, label=bysite$sitelat, cex.axis=0.6)
axis(4, las=2, at=1:y, label=bysite$minlat,cex.axis=0.6)
mtext(side=4, line=3, "Latitude")
axis(1, at=1:10)
box()
abline(v=mean.total.bb, col="slateblue", lwd=2)
points(bysite$mean.bb, 1:y, col="blue", pch=16)

dev.off()

# Copy/paste all above code and change to fl
png(filename="full.leaf.png", width=480, height=1100)
par(mar=c(5,4,4,5))
boxplot(full.leaf~sitelat, data=datav4, horizontal=TRUE, axes=FALSE,
        col=bysite$color.fl, cex=.2, ylab="Site",
        main="Week of first Full leaf by site", xlab="weeks")
axis(2, las=2, at=1:y, label=bysite$sitelat, cex.axis=0.6)
axis(4, las=2, at=1:y, label=bysite$minlat,cex.axis=0.6)
mtext(side=4, line=3, "Latitude")
axis(1, at=1:10)
box()
abline(v=mean.total.fl, col="slateblue", lwd=2)
points(bysite$mean.fl, 1:y, col="blue", pch=16)

dev.off()

######################################################################

summary(aov(budburst~site, data=datav4)) #anova bb against site
summary(aov(full.leaf~site, data=datav4))


## Setup histogram with lattice for anova results