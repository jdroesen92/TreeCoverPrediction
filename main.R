#Careli Caballero
#Jan Droesen
#Team: CarJan
#01/13/2016

## libraries
library(raster)
library(rgdal)
library(graphics)

## load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")

#processing data and plot bands against tree cover

#Erase tree cover higher than 100%
vcfGewata[vcfGewata > 100] <- NA

#make bricks and assign names to each brick layer
bandsgewata <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
alllayers <- addLayer(bandsgewata, vcfGewata)
names(bandsgewata) <- c("BlueBand", "GreenBand", "RedBand", "NIR", "SWIR", "MIR")
names(alllayers) <- c("BlueBand", "GreenBand", "RedBand", "NIR", "SWIR", "MIR", "VCF")

#plot all bands with VCF
par(mfrow = c(3,2))
plot(alllayers[[1:6]], alllayers[[7]], xlab = names(alllayers), ylab = ("Tree cover (%)"))

#modelblue <- lm(vcfbands$vcf2000Gewata ~ vcfbands$BlueBand, data=vcfbands)
#summary(modelblue)$r.squared
#modelgreen <- lm(vcfbands$vcf2000Gewata ~ vcfbands$GreenBand, data=vcfbands)
#modelred <- lm(vcfbands$vcf2000Gewata ~ vcfbands$RedBand, data=vcfbands)
#modelNIR <- lm(vcfbands$vcf2000Gewata ~ vcfbands$NIR, data=vcfbands)
#modelSWIR <- lm(vcfbands$vcf2000Gewata ~ vcfbands$SWIR, data=vcfbands)
#modelMIR <- lm(vcfbands$vcf2000Gewata ~ vcfbands$MIR, data=vcfbands)


#process data for linear regression model
dfgewata <- as.data.frame(getValues(alllayers))
#erase NAś
dfgewata <- na.omit(dfgewata)

#multiple regression model with all bands
modelMultipleAll <- lm(VCF ~ BlueBand + GreenBand + RedBand + NIR + SWIR + MIR, data = dfgewata)
#check for significant bands
summary(modelMultipleAll)

#erase non-significant band(s) and run linear regression model
modelMultiple <- lm(VCF ~ BlueBand + GreenBand + RedBand + NIR + SWIR, data = dfgewata)
#predicted values
predTC <- predict(bandsgewata, model=modelMultiple, na.rm=TRUE)

#comparing predicted and VCF tree cover

#get the difference between VCF and predicted
raserDif <- vcfGewata - predTC

#calculate RMSE separately for each of the classes
calcRMSE <- function(x,y){
	sqrt(mean((x-y)^2))
}
polyError <- calcRMSE(vcfGewata,predTC)

#visualize difference beteen VCF and predicted
colfunc <- colorRampPalette(c("white", "red"))
par(mfrow = c(2,2))
extentGewata <- extent(vcfGewata)
plot(vcfGewata, main="Tree cover (VCF)", zlim=c(0,100), ext=extentGewata)
plot(predTC, main="Tree cover predicted (LM)", zlim=c(0,100)) #add 0-100
plot(raserDif, main="Difference in tree cover", col=colfunc(255),  zlim=c(0,50))
plot(polyError, main="RMSE of VCF and LM",col=colfunc(255), zlim=c(0,50))
#compare VCF and LM with the land cover types
load("data/trainingPoly.rda")
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, polyError, field='Code')

#calculate error per land cover class
errorTraining <- zonal(polyError, classes, fun='mean', digits=0)
errorTraining
dferrorTraining <- as.data.frame(errorTraining)

#which class corresponds to highest error
mergedPolyError <- merge(x = trainingPoly, y = dferrorTraining, by.x = "Code", by.y = "zone")

#visualize errors per land cover type
x <- mergedPolyError$Class
y <- mergedPolyError$mean
par(mfrow=c(1,1))
plot(x,y, ylim= c(0,10), "s", ylab="RMSE")