library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)
library(gtools)
library(e1071)
library(foreign)
library(lme4)
library(ncdf4)
library(ggplot2)
library(vegan)


#for exporting large numbers
options(scipen=999)

#write Excel function needed for export
write.excel <- function (x,row.names=FALSE,col.names=TRUE,...) {
write.table (x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

################################################################################################
#### Load data
data <- read.csv(file="C:/Users/brian/Dropbox/research/fire permafrost/geospatial interaction study/data/ak/data_climate.csv")

#confirm factors
data$ecocode <- as.factor(data$ecocode)
data$anth <- as.factor(data$anth)

#subset to study region (Interior Alaska)
data <- subset(data, data$econame == "Interior Bottomlands" | data$econame == "Interior Highlands" |
  data$econame == "Interior Forested Lowlands and Uplands" |data$econame == "Yukon Flats" | 
  data$econame == "Ogilvie Mountains")



###############################################################################
#Re-extract climate variables
#Download current version from the BioClim website.  Version used in paper downloaded in 2021.

#Column names
names <- c("AnnualMeanTemp","MeanDirunalRange","Isothermality",
	"TempSeasonality","MaxTempWarmestMonth","MinTempColdestMonth","TempAnnualRange","MeanTempWettestQ",
	"MeanTempDriestQ","MeanTempWarmestQ","MeanTempColdestQ","AnnualPrecip","PrecipWettestM","PrecipDriestM",
	"PrecipSeasonality","PrecipWettestQ","PrecipDriestQ","PrecipWarmestQ","PrecipColdestQ")
store <- matrix(ncol=length(names), nrow=nrow(data))

#Locations to extract
coord <- cbind(data$x, data$y)

#Repeat extraction, change location to match 
#location of BioClim files
store[,1] <- raster::extract(raster("D:/AK climate resample/bio1.tif"), coord)
store[,2] <- raster::extract(raster("D:/AK climate resample/bio2.tif"), coord)
store[,3] <- raster::extract(raster("D:/AK climate resample/bio3.tif"), coord)
store[,4] <- raster::extract(raster("D:/AK climate resample/bio4.tif"), coord)
store[,5] <- raster::extract(raster("D:/AK climate resample/bio5.tif"), coord)
store[,6] <- raster::extract(raster("D:/AK climate resample/bio6.tif"), coord)
store[,7] <- raster::extract(raster("D:/AK climate resample/bio7.tif"), coord)
store[,8] <- raster::extract(raster("D:/AK climate resample/bio8.tif"), coord)
store[,9] <- raster::extract(raster("D:/AK climate resample/bio9.tif"), coord)
store[,10] <- raster::extract(raster("D:/AK climate resample/bio10.tif"), coord)
store[,11] <- raster::extract(raster("D:/AK climate resample/bio11.tif"), coord)
store[,12] <- raster::extract(raster("D:/AK climate resample/bio12.tif"), coord)
store[,13] <- raster::extract(raster("D:/AK climate resample/bio13.tif"), coord)
store[,14] <- raster::extract(raster("D:/AK climate resample/bio14.tif"), coord)
store[,15] <- raster::extract(raster("D:/AK climate resample/bio15.tif"), coord)
store[,16] <- raster::extract(raster("D:/AK climate resample/bio16.tif"), coord)
store[,17] <- raster::extract(raster("D:/AK climate resample/bio17.tif"), coord)
store[,18] <- raster::extract(raster("D:/AK climate resample/bio18.tif"), coord)
store[,19] <- raster::extract(raster("D:/AK climate resample/bio19.tif"), coord)
colnames(store) <- names

#hard coding of columns.  Double check things align.
colnames(data)[25:43] <- names
data[,25:43] <- store

#################################################################################
#create working copy of data
dat <- data

#create subcategories of repeat fires
nofire <- (subset(dat, dat$store == 0))
allfire <- (subset(dat, dat$store > 0))
onefire <- subset(dat, dat$store == 1)
twofire <- subset(dat, dat$store == 2)
threefire <- subset(dat, dat$store == 3)

#######
#ratios - how many of each?  also by ecoregion
nrow(allfire)/nrow(dat)*100		#points w/ fire
nrow(onefire)/nrow(allfire)*100	#percentage of fire: 1x
nrow(twofire)/nrow(allfire)*100	#percentage of fire: 2x
nrow(threefire)/nrow(allfire)*100	#percentage of fire: 3x

lst <- unique(dat$econame)

index <- 1:length(lst)
store <- matrix(ncol=5, nrow=length(lst))
store <- as.data.frame(store)


for (i in index) {
	temp <- subset(dat, dat$econame == lst[i])
	store[i,1] <- as.character(temp$econame[1])
	
	nofire.temp <- (subset(temp, temp$store == 0))
	allfire.temp <- (subset(temp, temp$store > 0))
	onefire.temp <- subset(temp, temp$store == 1)
	twofire.temp <- subset(temp, temp$store == 2)
	threefire.temp <- subset(temp, temp$store == 3)	

	store[i,2] <- nrow(allfire.temp)/nrow(temp)*100
	store[i,3] <- nrow(onefire.temp)/nrow(allfire.temp)*100
	store[i,4] <- nrow(twofire.temp)/nrow(allfire.temp)*100
	store[i,5] <- nrow(threefire.temp)/nrow(allfire.temp)*100
}

colnames(store) <- c("econame","burn_perc","onefire_perc","twofire_perc","threefire_perc")

#report stats (reburn stats relative to fire points only, not everywhere)
stats <- store; stats

########################################################
#Calculate first/last years and intervals

#One fire
last <- rep(NA, nrow(onefire))
first <- apply(cbind(onefire$Year1),1,min)
interval <- NA
onefire <- cbind(onefire, last, interval,first)

#two fires
last <- apply(cbind(twofire$Year1, twofire$Year2),1,max)
interval <- abs(twofire$Year1 - twofire$Year2)
first<- apply(cbind(twofire$Year1, twofire$Year2),1,min)
twofire <- cbind(twofire, last, interval, first)

#three fires
last <- apply(cbind(threefire$Year1, threefire$Year2, threefire$Year3),1,max)
first <- apply(cbind(threefire$Year1, threefire$Year2, threefire$Year3),1,min)
interval <- (last-first)/2	#approximate, just needs num as placeholder
threefire <- cbind(threefire, last, interval, first)

    ## removed from calculation ##  
#remove reburns >30 years from consideration. Only reduces total by 5.
#twofire <- subset(twofire, twofire$interval < 31)
#threefire <- subset(threefire, threefire$interval < 31)


###########################################################
#ECDF

#create working temp object
temp <- rbind(onefire,twofire,threefire)

#objects needed for code
name <- temp$econame
int <- temp$interval
df <- data.frame(name, int, stringsAsFactors=F)

#make list
lst <- unique(df$name)
df$int[is.na(df$int)] <- 35

#Create plot scale
#Note to change the x limits to get the entire record (xlim = c(1,32), though see text for reasons to only plot first half.
plot(ecdf(df[df$name==lst[1],]$int), pch=16,ylab="Proportion reburned",xlab="Interval (years)",
	ylim=c(0,.04), xlim=c(1,16), col="white",main="")

#draw lines
x <- 1
for (l in lst) {
	lines(ecdf(df$int),verticals=T,
      	pch=NA, lwd=4, col="black")
	lines(ecdf(df[df$name==l,]$int),verticals=T,
      	col=x+1,lty=3,pch=NA, lwd=3)
	x <- x + 1
}

legend("topleft", legend=c(lst,"Total"), col=c(2:(length(lst)+1),"black"), 
	lty=c(3,3,3,3,3,1), lwd=4, bty="n")

##############################
#What fraction are <x years 

temp <- rbind(twofire, threefire)
sum(temp$interval<20)/nrow(temp)
sum(temp$interval<10)/nrow(temp)
sum(temp$interval<5)/nrow(temp)


####################################################################################
# What covertypes are reburning?
# Load 1984 map from ABoVE
map <- raster("D:/AK Land Cover/above1984/Annual_Landcover_ABoVE_1691/data/subset/ras1.tif")

#Project and process for extraction of cover type data
spdat <- rbind(onefire, twofire, threefire)
coordinates(spdat) <- ~x+y

latlong = "+init=epsg:4326"
proj4string(spdat) = CRS(latlong)

#Extract
lc1984 <- raster::extract(map, spdat)

#Bind
spdat <- cbind(spdat, lc1984)
names(spdat)[48] <- "lc1984"			#note this MUST be the right column, double check it is really column 48

#	Original code names.  
#1	Evergreen Forest	1,4	Evergreen Forest, Woodland
#2	Deciduous Forest	2,3	Deciduous Forest, Mixed Forest
#3	Shrubland	5,6,7	Low Shrub, Tall Shrub, Open Shrub
#4	Herbaceous	8,9	Herbaceous, Tussock Tundra
#5	Sparsely Vegetated	10	Sparsely Vegetated
#6	Barren	14	Barren
#7	Fen	11	Fen
#8	Bog	12	Bog
#9	Shallows/Littoral	13	Shallows/Littoral
#10	Water	15	Water

#Get into working object
reburn.lc <- as.data.frame(spdat)

#simplify cover codes 
reburn.lc$lc1984[reburn.lc$lc1984 == 3] <- 4
reburn.lc$lc1984[reburn.lc$lc1984 == 6] <- 5
reburn.lc$lc1984[reburn.lc$lc1984 == 8 | reburn.lc$lc1984 == 9 | reburn.lc$lc1984 == 10] <- 7

#Copy object and subset to just reburns
reburn.mod <- reburn.lc							#Extra temp object for model below
reburn.lc <- subset(reburn.lc, reburn.lc$store > 1)

#Calculate reference number for total fires
#Subsets to 20 years with single fire
ref <- rbind(onefire, twofire, threefire)
refsub <- subset(ref, ref$interval < 21 & ref$store == 1)

#Cisualize
hist(onefire$first)
ref <- table(ref$first)


#Quick function for avoiding numeric(0) problems
isEmpty <- function(x) {
    return(length(x)==0)
}

###################################################

#Proportions overall
#LAST - 11 year

#Plotting
par(mfrow=c(3,1))

#New object for processing, isolate reburns and limit to single decade starting at full potentiality (1993)
temp <- reburn.lc
temp <- temp[temp$interval < 11,]
temp <- temp[temp$last > 1993,]

#Calculate basic stats
counts <- table(temp$lc1984, temp$last)

  ##  Removed ##
#t <- matrix(ncol=1, nrow=5, c(0,0,0,0,0)); colnames(t) <- 2016	#inserts missing years to align graph
#counts <- cbind(counts, t)


#Turn counts into proportions either total fire or total landscape
test <- subset(ref, names(ref)%in% colnames(counts) )
as.numeric(test)

#Two options for proportions - proportion of landscape or proportion of fires. Manuscipt uses propotion of fires.
cols <- 1:ncol(counts)
for (c in cols) {
	counts[,c] <- counts[,c]/(as.numeric(test[c]))			#total fire proportion - skews towards high values for low fire years (e.g. 2014)
	#counts[,c] <- counts[,c]/nrow(dat)*100				#total landscape

}

counts2 <- prop.table(counts, margin=2)	#convert to proportions

#Plot
bp <- barplot(counts, main="Year of reburn by original cover type, <10 year intervals",
  xlab="Reburn fire year",  ylim=c(0,0.04),
	col=c("darkgreen","lightgreen","brown","grey","blue"))

legend("topleft",c("Conifer","Deciduous","Grass/Shrub","Sparse Veg.","Wetlands"), 
	col=c("darkgreen","lightgreen","brown","grey","blue"), pch=15)


bp <- barplot(counts2, main="Proportion in each cover type",
  xlab="Reburn fire year", 
	col=c("darkgreen","lightgreen","brown","grey","blue"))


#Moving window approach to conifer proportions for regression
interval <- 10
int <- (1984+interval):(max(reburn.lc$last))
store <- matrix(ncol=3, nrow=length(int))
cnt <- 1

for (i in int) {
	temp <- reburn.lc[reburn.lc$last == i,]
	temp <- temp[temp$interval < (interval+1),]

	counts <- table(temp$lc1984, temp$last)
	counts2 <- prop.table(counts, margin=2)	#convert to proportions

	counts2 <- as.data.frame(counts2)
	t <- subset(counts2, counts2$Var1 == 1)
	
	store[cnt,1] <- ifelse(isEmpty(t$Freq),0,t$Freq)
	store[cnt,2] <- i
	store[cnt,3] <- sum(counts)

	cnt <- cnt+1
}

plot(store[,1]~store[,2], cex=store[,3]/(mean(store[,3])/1.5), lwd=2, main="", xlab= "", ylab="Prop. conifer")
mod <- lm(store[,1]~store[,2], weights=store[,3])
abline(mod)
summary(mod)
txt <- paste("r2","=",round(summary(mod)$r.squared,2), sep=" ")
text(x=2015,y=.2,labels=txt)

##############################################

#Same thing, but for two decades
#Could also be accomplished with changing subsetting above.

#2 decades
par(mfrow=c(3,1))
temp <- reburn.lc
temp <- temp[temp$interval < 21,]
temp <- temp[temp$last > 2003,]

counts <- table(temp$lc1984, temp$last)
#t <- matrix(ncol=1, nrow=5, c(0,0,0,0,0)); colnames(t) <- 2016	#inserts missing years to align graph
#counts <- cbind(counts, t)


#turn counts into proportions either total fire or total landscape
test <- subset(ref, names(ref)%in% colnames(counts) )
as.numeric(test)

#
cols <- 1:ncol(counts)
for (c in cols) {
	counts[,c] <- counts[,c]/as.numeric(test[c])			#total fire proportion - skews towards high values for low fire years (2014)
	#counts[,c] <- counts[,c]/nrow(dat)*100				#total landscape

}

counts2 <- prop.table(counts, margin=2)	#convert to proportions
bp <- barplot(counts, main="Year of reburn by original cover type, <20 year intervals",
  xlab="Reburn fire year", ylim=c(0,0.1),
	col=c("darkgreen","lightgreen","brown","grey","blue"))

bp <- barplot(counts2, main="Proportion in each cover type",
  xlab="Reburn fire year", 
	col=c("darkgreen","lightgreen","brown","grey","blue"))

# Conifer proportions
interval <- 20
int <- (1984+interval):(max(reburn.lc$last))
store <- matrix(ncol=3, nrow=length(int))
cnt <- 1

for (i in int) {
	temp <- reburn.lc[reburn.lc$last == i,]
	temp <- temp[temp$interval < (interval+1),]

	counts <- table(temp$lc1984, temp$last)
	counts2 <- prop.table(counts, margin=2)	#convert to proportions

	counts2 <- as.data.frame(counts2)
	t <- subset(counts2, counts2$Var1 == 1)
	
	store[cnt,1] <- ifelse(isEmpty(t$Freq),0,t$Freq)
	store[cnt,2] <- i
	store[cnt,3] <- sum(counts)

	cnt <- cnt+1
}

#plotting proportions
plot(store[,1]~store[,2], cex=store[,3]/(mean(store[,3])/1.5), lwd=2, main="20 year or less interval", xlab= "", ylab="Prop. conifer")
mod <- lm(store[,1]~store[,2], weights=store[,3])
summary(mod)
abline(mod)
txt <- paste("r2","=",round(summary(mod)$r.squared,2), sep=" ")
text(x=2015,y=.2,labels=txt)


###############################################################################
# Conditional Random Forest model
# Can take a significant amout of time
# results saved in separate CSV

library(party)
library(permimp)

#Index is number of iterations on model
index <- 1:20
store <- matrix(ncol=26, nrow=max(index))

for (i in index) {

	#Focus on two burns only for simplicity

	onefire.moddat <- subset(reburn.mod, reburn.mod$store == 1)
	twofire.moddat <- subset(reburn.mod, reburn.mod$store == 2)
	twofire.moddat <- subset(twofire.moddat, twofire.moddat$interval < 21)

	#sample down onefire to avoid overfitting one fire locations
	count <- nrow(twofire.moddat)
	onefire.sub <- sample_n(onefire.moddat, count)

	#bind modeling objects
	moddat.main <- rbind(onefire.sub, twofire.moddat)

	#create subset for splitting
	moddat <- moddat.main

	#Adjust aspect so it's no longer circular
	moddat$aspect <- (1 - cos((3.14/180)*(moddat$aspect-30)))/2
	moddat$aspect <- round(moddat$aspect*100)

	#get x vars: Make sure the column numbers are correct
	moddat <- cbind(moddat[,16], moddat[,20:41], moddat[,48])

	#Rename if necessary
	colnames(moddat)[1] <- "store"
	colnames(moddat)[24] <- "lc1984"

	#Factorize
	moddat$lc1984 <- as.factor(moddat$lc1984)

	#Save response variable
	store.num <- moddat$store

	#Data frame 
	moddat <- as.data.frame(moddat)

	#Build training set: 80% in this case
	fsub.train <- sample_n(moddat, nrow(moddat)*.8)
	ok <- complete.cases(fsub.train)
	fsub.train <- fsub.train[ok,]

	#Create test set: the remainder of the data
	fsub.test <- setdiff(moddat, fsub.train)

	#If regular random forests are desired.
	#mod <- randomForest(as.factor(fsub.train$store)~.,data=fsub.train, keep.forest=T, keep.inbag=T)

	#Conditional Forest: Create forest, 1000 iterations
	mod <- cforest(as.factor(fsub.train$store)~.,data=fsub.train, 
		controls=cforest_unbiased(ntree=1000, mtry=5))

	#Reporting code
	print(paste("Mod",i,sep=" "))

	#Test the outputs
	pred <- predict(mod, type="response")						#Predictions on training set, not currently used, could be deleted for speed
	pred.train <- predict(mod, newdata=fsub.test)					#Predictions on test set
	t <- confusionMatrix(pred.train, as.factor(fsub.test$store))		#Confusion matrix, not used except in diagnostics
	imp <- permimp(mod, conditional=T, threshold=0.95)				#Threshold determines extent of correlation adjustment, equivalent to RF

	#store
	store[i,] <- c(as.numeric(imp$values), nrow(fsub.train), nrow(fsub.test),t$overall[1])
	print(paste((i/max(index)*100),"%", sep=""))
}

#Label and save
nam <- c(names(imp$perTree), "Training n","Testing n","Accuracy")
colnames(store) <- nam
write.csv(store, file="")

#Graph variable importance from conditional forest.  use also one above, with the new perimp
#uses joyplot technique.

library(ggjoy)
library(reshape)

#Load file if not working off memory
store <- read.csv(file="")	

#Adjust and display summary
store <-as.data.frame(store)
summarylist <- summary(store); summarylist

#subset to just coefficients and format, double check column numbers
imps <- store[,2:23]*100
imps2 <- melt(imps)
imps2 <- as.data.frame(imps2)

#Display median values for reference, not necessary
aggregate(imps2$value, by=list(imps2$variable), FUN=median)

# Sort the participants by median intercept value
subject_order <- imps2 %>%
  group_by(variable) %>%
  summarise(median = median(value)) %>%
  ungroup() %>%
  mutate(imps2 = factor(variable, ordered=T) %>% 
  forcats::fct_reorder(median)) 

#Combine summary with main data
imps2 <- left_join(imps2, subject_order)
imps2$imps2 <- as.character(imps2$imps2)

#Label categories.  Purely for visualization, could be adjusted as desired.
imps2$type[imps2[,4]=="elevation"] <- "Topography"
imps2$type[imps2[,4]=="slope"] <- "Topography"
imps2$type[imps2[,4]=="aspect"] <- "Topography"
imps2$type[imps2[,4]=="AnnualMeanTemp"] <- "Temperature"
imps2$type[imps2[,4]=="MinTempColdestMonth"] <- "Temperature"
imps2$type[imps2[,4]=="MaxTempWarmestMonth"] <- "Temperature"
imps2$type[imps2[,4]=="MeanTempWettestQ"] <- "Temperature"
imps2$type[imps2[,4]=="MeanTempDriestQ"] <- "Temperature"
imps2$type[imps2[,4]=="MeanTempWarmestQ"] <- "Temperature"
imps2$type[imps2[,4]=="MeanTempColdestQ"] <- "Temperature"
imps2$type[imps2[,4]=="AnnualPrecip"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipWettestM"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipDriestM"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipWettestQ"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipDriestQ"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipWarmestQ"] <- "Precipitation"
imps2$type[imps2[,4]=="PrecipColdestQ"] <- "Precipitation"
imps2$type[imps2[,4]=="MeanDirunalRange"] <- "Variability"
imps2$type[imps2[,4]=="Isothermality"] <- "Variability"
imps2$type[imps2[,4]=="TempSeasonality"] <- "Variability"
imps2$type[imps2[,4]=="TempAnnualRange"] <- "Variability"
imps2$type[imps2[,4]=="PrecipSeasonality"] <- "Variability"

imps2$imps2 <- as.factor(imps2$imps2)

imps2 <- as.data.frame(imps2)
imps2 <- imps2[order(imps2$median),]
imps2$variable <- factor(imps2$variable, levels = unique(imps2$variable) )

#plot.  Code throughout heavily borrowed from here:
#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

p1 <- ggplot(imps2) +
  aes(x = value, y = variable, height = ..density..,
      group = variable, fill=as.factor(type)) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_fill_brewer(palette="BrBG")+
   labs(y = "", x = "Variable Importance (Change in accuracy)") +
  geom_density_ridges()+stat_density_ridges(quantile_lines = TRUE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept=0)


#For the threshold 0.95
p1 + 	annotate(geom="text", x=.12, y=22.5, label=paste(round(mean(store$Accuracy)*100),"%","Mean Accuracy",sep=" "),
              color="black") + 
	annotate(geom="text", x=.12, y=21.5, label=paste(round(mean(store$Testing.n)),"Test set n",sep=" "),
              color="black") +
	xlim(-.1,.1)

#For the threshold 1
p1 + 	annotate(geom="text", x=.4, y=21.5, label=paste(round(mean(store$Accuracy)*100),"%","Mean Accuracy",sep=" "),
              color="black") + 
	annotate(geom="text", x=.395, y=20.5, label=paste(round(mean(store$Testing.n)),"Test set n",sep=" "),
              color="black")

#######################################################################################################################################

# Code for null/neutral landscape comparison model.
# Separate from preceeding code.  Suggest reloading data.

# Load data
data <- read.csv(file="C:/Users/brian/Dropbox/research/fire permafrost/geospatial interaction study/data/ak/data_climate.csv")
deepstore <- data

nrow(data)
head(data)

data <- subset(data, data$econame == "Interior Bottomlands" | data$econame == "Interior Highlands" |
  data$econame == "Interior Forested Lowlands and Uplands" |data$econame == "Yukon Flats" | 
  data$econame == "Ogilvie Mountains")

dat <- data
head(dat)

#descriptive, includes years
nofire <- (subset(dat, dat$store == 0))
allfire <- (subset(dat, dat$store == 1 |dat$store == 2|dat$store == 3))
onefire <- subset(dat, dat$store == 1)
twofire <- subset(dat, dat$store == 2)
threefire <- subset(dat, dat$store == 3)

allfire <- rbind(onefire,twofire,threefire)


########################################################
#calculate years and intervals

#one fire
last <- rep(NA, nrow(onefire))
first <- apply(cbind(onefire$Year1),1,min)
interval <- NA
onefire <- cbind(onefire, last, interval,first)


#two fire
last <- apply(cbind(twofire$Year1, twofire$Year2),1,max)
interval <- abs(twofire$Year1 - twofire$Year2)
first<- apply(cbind(twofire$Year1, twofire$Year2),1,min)
twofire <- cbind(twofire, last, interval, first)

#three
#note to change median to max for last year if not comparing
last <- apply(cbind(threefire$Year1, threefire$Year2, threefire$Year3),1,max)
first <- apply(cbind(threefire$Year1, threefire$Year2, threefire$Year3),1,min)
interval <- (last-first)/2
threefire <- cbind(threefire, last, interval, first)

#histograms
par(mfrow=c(3,1))
hist(twofire$first, breaks=23, xlim=c(1984,2016), main="Year of Initial Fire: Reburn locations only (any interval)", xlab="")
hist(twofire$last, breaks=23, xlim=c(1984,2016), main="Year of Second Fire", xlab="")
hist(onefire$first, breaks=23, xlim=c(1984, 2016), main="Overall Fire Occurence", xlab="")

###########################################################
##########


#calculate overall fire rate
temp <- data
nrow(temp)
temp <- subset(temp, temp$store > 0)


temp$Year1[temp$Year1 == 0] <- NA
temp$Year2[temp$Year2 == 0] <- NA
temp$Year3[temp$Year3 == 0] <- NA

first <- apply(cbind(temp$Year1, temp$Year2, temp$Year3, temp$Year4, temp$Year5),1,min, na.rm=T)
last <- apply(cbind(temp$Year1, temp$Year2, temp$Year3, temp$Year4, temp$Year5),1,max, na.rm=T)

baseline_fire <- cbind(temp,first,last)


#######################################################
# Interval graphing
n <- 20			#max interval size
n1 <- 10 			#min interval size
index <- (1984+(n-1)):(max(twofire$last))
cnt <- 1:length(index)

code <- unique(twofire$econame)
cntcode <- 1:length(code)
count <- 1
nullcount <-1
m <- 10000			#numeber of iterations on null model

#storage
store <- matrix(nrow=length(index), ncol = length(unique(twofire$econame))+1)
store_baseline <- matrix(nrow=length(index), ncol = length(unique(twofire$econame))+1)
store_null <- matrix(nrow=length(index), ncol = 2+length(cntcode)*2)
store_null_temp <- matrix(ncol=1,nrow=m)
store_null_quantiles <- matrix(ncol=max(cntcode)*2+2, nrow=length(index))

#difference storage
store_null_diff <- matrix(nrow=length(index), ncol=2+length(cntcode)*2)
store_null_diff_quantiles <- matrix(nrow=length(index), ncol=2+length(cntcode)*2)

#Loop through simulations
for (i in cnt) {
	#fires that happened that year that also had a fire in interval
	#temp <- subset(twofire, twofire$first >= (index[i]-(n-1)) & twofire$first < (index[i]-n1)) #subset fires that had a first burn for that interval
	temp <- subset(twofire, twofire$interval >= n1 & twofire$interval < (n+1))
 	temp <- subset(temp, temp$last == index[i])								 #subset to those with last fires in the year of interest
	store[i,1] <- nrow(temp)											 #store
	print(index[i])

	#all fires that happened that year, note this includes reburns to be conservative (max number)
	temp2 <- subset(baseline_fire, baseline_fire$Year1 == index[i] | baseline_fire$Year2 == index[i] | baseline_fire$Year3 == index[i])  
	store_baseline[i,1] <- nrow(temp2)										#store

	#all fires in that interval
	temp.1 <- subset(baseline_fire, baseline_fire$Year1 >= (index[i]-(n-1)) & baseline_fire$Year1 <= (index[i]-n1))
	temp.2 <- subset(baseline_fire, baseline_fire$Year2 >= (index[i]-(n-1)) & baseline_fire$Year2 <= (index[i]-n1))
	temp.3 <- subset(baseline_fire, baseline_fire$Year3 >= (index[i]-(n-1)) & baseline_fire$Year3 <= (index[i]-n1))
	temp.4 <- subset(baseline_fire, baseline_fire$Year4 >= (index[i]-(n-1)) & baseline_fire$Year4 <= (index[i]-n1))
	temp.5 <- subset(baseline_fire, baseline_fire$Year5 >= (index[i]-(n-1)) & baseline_fire$Year5 <= (index[i]-n1))

	datatemp <- rbind(temp.1, temp.2, temp.3, temp.4, temp.5)			#all fires in the interval

	#define null area
	nullarea <- data										#the background area to calculate % against.  data is the full (subset) ecoregion, very large, not all areas will burn anyway.

	#m is the number of iterations to run
	index2 <- 1:m
	store_null_temp <- matrix(ncol=1,nrow=m)

	for (k in index2) {

		#all fire, fraction of area burned in the interval* 100
		g <- runif(nrow(datatemp)/nrow(nullarea)*10000,1,10000)		#percentage area burned in the interval relative to total burnable area
		g <- round(g, digits=0)									

		#fires in that year, fraction, * 100
		h <- runif(nrow(temp2)/nrow(nullarea)*10000,1,10000)
		h <- round(h, digits=0)

		l <- length(intersect(g,h))							#number of cells reburned
		
		store_null_temp[k] <- (l/length(h))*100					#fraction of reburns / fraction of landscape burned

	}

	#make zeros from the NaN's from the divisors if needed
	store_null_temp[is.nan(store_null_temp)] <- 0

	#record average and SD
	store_null[i,1] <- mean(store_null_temp, na.rm=T)
	store_null[i,2] <- sd(store_null_temp, na.rm=T)

	#store quantiles
	store_null_quantiles[i,1] <- quantile(store_null_temp, c(0.25,0.75),na.rm=T)[[1]]
	store_null_quantiles[i,2] <- quantile(store_null_temp, c(0.25,0.75),na.rm=T)[[2]]

	#fraction burned in specific year (last) vs. fraction in interval up to that point compared to mean - the difference in reburn obs. vs. modeled
	diffv <- nrow(temp)/nrow(temp2)*100 - store_null_temp 

	store_null_diff[i,1] <- mean(diffv, na.rm=T)
	store_null_diff[i,2] <- sd(diffv, na.rm=T)

	store_null_diff_quantiles[i,1] <- quantile(diffv, c(0.25,0.75),na.rm=T)[[1]]
	store_null_diff_quantiles[i,2] <- quantile(diffv, c(0.25,0.75),na.rm=T)[[2]]

	count <- count+1
	print((count-1)/max(cnt))
}

#name columns - note that ecoregions are not done here, so extra columns.
store[1,]
colnames(store) <- c("overall",paste("x",code, sep=""))
colnames(store_baseline) <- c("overall",paste(code))
colnames(store_null) <- c("overall", "overallsd", paste("mean",code,sep=""),paste("sd",code,sep=""))

#remove NA's, as zero
store_null[is.na(store_null[,1])] <- 0
mean(store_null[,1], na.rm=T)

#dataframe
store <- as.data.frame(store)
store_baseline <- as.data.frame(store_baseline)
store_null <- as.data.frame(store_null)
store_baseline_raw <- store_baseline


#plot 
#par(mfrow=c(2,1))

obs <- 100*(store$overall/store_baseline$overall)

#dynamic labeling
lab <- paste("Proportion of Short Interval Fires (<", n, " years)", sep="")
scl <- max(c(obs,store_null_quantiles[,2]),na.rm=T)

plot(obs~index, 
	ylab = lab, 
	pch=16, cex=2, ylim=c(0,scl),  xlim=c(1993, 2016))
mod <- lm(obs~index)
summary(mod)
abline(mod, lwd=3)

points(index, store_null$overall, pch=16, col="darkred", cex=2)
modnull <- lm(store_null$overall~index)
summary(modnull)
abline(modnull, lwd=2, col="darkred")


#examine normality of residuals and lack of autocorrleation
#Theil-Sen median slope estimate
#obs[4] <- 0				#for the 0-10 lag
Box.test(obs, lag=1)			#no serial autocorr for 0-10
fit <- mblm(obs~index)
summary(fit)
confint(fit, level=0.9)
abline(fit, lwd=3)

null <- store_null$overall
Box.test(store_null$overall, lag=1)	#no serial autocorr for 0-10
fit <- mblm(null~index)
summary(fit)
confint(fit, level=0.9)
abline(fit, lwd=3, col="darkred")


arrows(index, store_null_quantiles[,1], index, store_null_quantiles[,2], col="darkred",length=0.05, angle=90, code=3, lwd=2)
legend("topleft", legend=c("Observed","Null Model"), col=c("black","darkred"), bg="white",pch=16)

#ANCOVA
obsi <- rep(1,length(obs))
obsi <- cbind(obs, obsi)
nulli <- rep(2,length(store_null$overall))
nulli <- cbind(store_null$overall, nulli)

comp <- rbind(obsi, nulli)

indexi <- c(index,index)
mod <- lm(comp[,1]~indexi + comp[,2])
mod2 <- lm(comp[,1]~indexi * comp[,2])
summary(mod)
summary(mod2)
anova(mod, mod2)



#bootstrapped difference.  This is the difference in means of the null sample and the observed value, confidence interval as derived from the quantiles (0.05, 0.095)
plot(store_null_diff[,1]~index, pch=16, col="darkred", ylab="Difference (Obs - Predicted)", ylim=c(-20,20))
abline(h=0)
arrows(index, store_null_diff_quantiles[,1], index, store_null_diff_quantiles[,2], 
 	length=0.05, angle=90, code=3, col="darkred",lwd=2)



g <- subset(twofire, twofire$last == 2014)
g <- subset(g, g$interval>10)
h <- subset(allfire, allfire$first == 2014 | allfire$last==2014)









