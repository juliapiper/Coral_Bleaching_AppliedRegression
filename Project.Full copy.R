setwd("/Users/jules/Downloads")
cb2016<- read.table(file = "cb2016ALL.csv", header = TRUE, sep=",")
cb2016 <- cb2016[cb2016$Percent_Bleaching != 0,] #remove all zeros
dim(cb2016)

########### Variable Removal examine ######
cb2016 <- cb2016[cb2016$Percent_Cover != -9999,] #remove all missing
dim(cb2016)
#write.csv(cb2016, file = "/Users/jules/Desktop/hmm.csv", row.names = TRUE)
#no change
#cb2016 <- cb2016[cb2016$Substrate_Name != -9999,] #remove all mising
#dim(cb2016) #remove 200 variables -> NOT remvoing variables and just removing column  complete increases r2 by .3

#intro stats 
colnames(cb2016)
head(cb2016)
dim(cb2016)
summary(cb2016)

#summary statistics 
summary(cb2016)

#standard deviation 
sapply(cb2016, function(x) if(is.numeric(x)) sd(x))

## Histogram for totals of categorical variables 
hist(cb2016$Percent_Bleaching, col="tomato3", 
     main ="Histogram of Percent Bleaching",
     ylab = "Count",
     xlab = "Percent Bleaching",
     breaks = 50)


## Create a Pie Chart and Box Plot of Categorical Variables ##
library(viridisLite)
library(viridis)

#Ocean Name
ocean_freq <- table(cb2016$Ocean_Name)
ocean_perc <- round(ocean_freq/sum(ocean_freq)*100, 1)
ocean_colors <- magma(length(ocean_freq))
pie(ocean_freq, main="Ocean Names", col=ocean_colors,
    labels=paste(names(ocean_freq), " (", ocean_perc, "%)", sep=""),
    cex=0.8, font=2)

boxplot(cb2016$Percent_Bleaching~cb2016$Ocean_Name,
        main ="Ocean Name",
        ylab =" Percent Bleaching",
        xlab=" Ocean",
        col= ocean_colors)

#Exposure
exp_freq <- table(cb2016$Exposure)
exp_perc <- round(exp_freq/sum(exp_freq)*100, 1)
pie(exp_freq, main="Exposure", col=inferno(length(exp_freq)),
    labels=paste(names(exp_freq), " (", exp_perc, "%)", sep=""),
    cex=0.8, font=2)

boxplot(cb2016$Percent_Bleaching~cb2016$Exposure,
        main ="Exposure",
        ylab =" Percent Bleaching",
        xlab=" Exposure Type",
        col= inferno(3)) 



########## Correlation  Graphs ############
#Checking Original 
num_only<-subset(cb2016, select = c("Percent_Bleaching","Latitude_Degrees", "Longitude_Degrees", "Distance_to_Shore", "Turbidity", "Cyclone_Frequency", "Depth_m", "Percent_Cover", "ClimSST", "Temperature_Kelvin", "Temperature_Mean", "Temperature_Minimum", "Temperature_Maximum", "Temperature_Kelvin_Standard_Deviation", "Windspeed", "SSTA", "SSTA_Standard_Deviation", "SSTA_Minimum", "SSTA_Maximum", "SSTA_Frequency", "SSTA_Frequency_Standard_Deviation", "SSTA_FrequencyMax", "SSTA_FrequencyMean", "SSTA_DHW", "SSTA_DHW_Standard_Deviation", "SSTA_DHWMax", "SSTA_DHWMean", "TSA", "TSA_Minimum", "TSA_Maximum", "TSA_Mean", "TSA_Frequency", "TSA_Frequency_Standard_Deviation", "TSA_FrequencyMax", "TSA_FrequencyMean", "TSA_DHW", "TSA_DHW_Standard_Deviation", "TSA_DHWMax", "TSA_DHWMean"))
sapply(num_only,class)
#pairs(num_only) #round(cor(num_only),digits =2)

library(corrplot)
round(cor(num_only), digit = 2)
cb.cor = cor(num_only)
corrplot(cb.cor, tl.cex = 0.4, tl.col="navy")

#Removal of Summary 
num_only<-subset(cb2016, select = c("Percent_Bleaching","Latitude_Degrees", "Longitude_Degrees", "Distance_to_Shore", "Turbidity", "Cyclone_Frequency", "Depth_m", "Percent_Cover", "ClimSST", "Temperature_Kelvin",  "Windspeed", "SSTA", "SSTA_Frequency", "SSTA_DHW", "TSA", "TSA_Frequency", "TSA_DHW"))
sapply(num_only,class)
#pairs(num_only) #round(cor(num_only),digits =2)

library(corrplot)
round(cor(num_only), digit = 2)
cb.cor = cor(num_only)
corrplot(cb.cor, tl.cex = 0.6, tl.col="navy")

###chosen data (Numerical only)
cbNUMONLY<-subset(cb2016, select = c("Latitude_Degrees" , "Longitude_Degrees","Percent_Bleaching",  "Cyclone_Frequency", "Turbidity" + "Depth_m", "Percent_Cover", "Temperature_Kelvin","SSTA_DHW"))
sapply(cbNUMONLY,class)

#Latitude_Degrees +  Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover  + Temperature_Kelvin +  SSTA_DHW

library(corrplot)
round(cor(cbNUMONLY), digit = 2)
cb.cor = cor(cbNUMONLY)
corrplot(cb.cor, tl.cex = 0.4, tl.col="navy")

############### LINEAR REGRESSION ###########

#Example model no categorical 
fitNC<- lm(Percent_Bleaching ~ Latitude_Degrees + Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover +  Temperature_Kelvin + SSTA_DHW, data = cb2016)
summary(fitNC) #.387

#Example model with dummy variables
Sometimes <- ifelse(cb2016$Exposure == "Sometimes", 1, 0)
Exposed <- ifelse(cb2016$Exposure == "Exposed", 1, 0)

#Oceans- Pacific as default
Indian <- ifelse(cb2016$Ocean_Name == "Indian", 1, 0)
Atlantic <- ifelse(cb2016$Ocean_Name == "Atlantic", 1, 0)
ArabianGulf <- ifelse(cb2016$Ocean_Name == "Arabian Gulf", 1, 0)
RedSea <- ifelse(cb2016$Ocean_Name == "Red Sea", 1, 0)

X <- cbind( Sometimes, Exposed, Indian, Atlantic, ArabianGulf, RedSea, cb2016$Distance_to_Shore,cb2016$Turbidity, cb2016$Cyclone_Frequency,  cb2016$Depth_m, cb2016$Temperature_Kelvin,  cb2016$Percent_Cover, cb2016$SSTA_DHW)

# Fitting modelw/ dummy variable
fit<- lm(Percent_Bleaching ~  Sometimes + Exposed + Indian + Atlantic + ArabianGulf + RedSea + Latitude_Degrees +  Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover  + Temperature_Kelvin +  SSTA_DHW, data = cb2016)
summary(fit) #0.4837

layout(matrix(c(1,2,3,4),2,2)) 
plot(fit)

#check assumptions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit)
plot(fitted(fit),residuals(fit))
abline(a=0, b=0)

###### Log Transformation ####
Cb_Log <- cb2016
Cb_Log$Percent_Bleaching[Cb_Log$Percent_Bleaching == 0] <- 0.000001 #shouldnt do anything
Cb_Log$Log_Percent_Bleaching <- log(Cb_Log$Percent_Bleaching + 1)
(dim(Cb_Log))

###compare histograms:
#w/ Log
hist(Cb_Log$Log_Percent_Bleaching, col="tomato3", 
     main ="Histogram of the Log of Percent Bleaching",
     ylab = "Count",
     xlab = "Log of Percent Bleaching",
     breaks = 100)

#w/out 
hist(cb2016$Percent_Bleaching, col="tomato3", 
     main ="Histogram of Percent Bleaching",
     ylab = "Count",
     xlab = "Percent Bleaching",
     breaks = 100)

#Example Model: w/ LOG categorical (but no dummy variables created yet)
fitL2<- lm(Log_Percent_Bleaching ~ Latitude_Degrees + Longitude_Degrees + Ocean_Name + Exposure + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover + Temperature_Kelvin +   SSTA_DHW, data = Cb_Log)
summary(fitL2) #4148

#check assumptions
layout(matrix(c(1,2,3,4),2,2)) 
plot(fitL2)

#large Residual vs Fitted
#check assumptions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit)
plot(fitted(fit),residuals(fit))
abline(a=0, b=0)


### run fitted model without categorical (?)
#fitNC<- lm(Log_Percent_Bleaching ~ Latitude_Degrees + Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover +  Temperature_Kelvin +  SSTA_DHW, data = Cb_Log)
#summary(fitNC) #0.3438

#layout(matrix(c(1,2,3,4),2,2)) 
#plot(fitNC)


###### Create Dummy Variables for categorical data


#Exposure- Sheltered is the default 
Sometimes <- ifelse(Cb_Log$Exposure == "Sometimes", 1, 0)
Exposed <- ifelse(Cb_Log$Exposure == "Exposed", 1, 0)

#Oceans- Pacific as default
Indian <- ifelse(Cb_Log$Ocean_Name == "Indian", 1, 0)
Atlantic <- ifelse(Cb_Log$Ocean_Name == "Atlantic", 1, 0)
ArabianGulf <- ifelse(Cb_Log$Ocean_Name == "Arabian Gulf", 1, 0)
RedSea <- ifelse(Cb_Log$Ocean_Name == "Red Sea", 1, 0)


# Combining dummy variables with other predictor variables
X <- cbind( Sometimes, Exposed, Indian, Atlantic, ArabianGulf, RedSea, Cb_Log$Distance_to_Shore, Cb_Log$Turbidity, Cb_Log$Cyclone_Frequency,  Cb_Log$Depth_m, Cb_Log$Temperature_Kelvin, Cb_Log$Windspeed, Cb_Log$Percent_Cover, Cb_Log$SSTA_DHW)

# Fitting modelw/ dummy variable
fitC<- lm(Log_Percent_Bleaching ~  Sometimes + Exposed + Indian + Atlantic + ArabianGulf + RedSea + Latitude_Degrees +  Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover  + Temperature_Kelvin +  SSTA_DHW, data = Cb_Log)
summary(fitC) #0.4135

layout(matrix(c(1,2,3,4),2,2)) 
plot(fitC)

################# Outlier Removal 
setwd("/Users/jules/Downloads")
cb2016<- read.table(file = "cb2016ALL.csv", header = TRUE, sep=",")
cb2016 <- cb2016[cb2016$Percent_Bleaching != 0,] #remove all zeros
#cb2016 <- cb2016[cb2016$Percent_Cover != 0,] #remove all missing
dim(cb2016)

zero_cover_rows <- which(cb2016$Percent_Cover == 0)

# print row numbers
print(zero_cover_rows+722)


#note that numbers start at 723
#influential observations removed:
#1077=355, 771=49,772=50,773=51,811=89,812=90, 863=141 =7 outliers
#774=52,772=50,1082=360, 813=91, 810=88, 916=194,#4501 = 6 outliers 
#1093=371 1094=372 1081=359, 917=195, 815=93, 816=94 ###4698 6 outliers
#1219=497, 759=37, 737=15 #4709
#cb2016 <- cb2016[-c(355,49,51,89,90,141,50,52,360,91,88,194,371,372,359,195,93,94),]
cb2016 <- cb2016[-c(355,49,51,50,89,90,141),] #7
#cb2016 <- cb2016[-c(355,49,51,50,89,90,141,52,50,360,91,88,194),] #13 

dim(cb2016)

Cb_Log <- cb2016
Cb_Log$Percent_Bleaching[Cb_Log$Percent_Bleaching == 0] <- 0.000001 #shouldnt do anything
Cb_Log$Log_Percent_Bleaching <- log(Cb_Log$Percent_Bleaching + 1)
(dim(Cb_Log))


#Exposure- Sheltered is the default 
Sometimes <- ifelse(Cb_Log$Exposure == "Sometimes", 1, 0)
Exposed <- ifelse(Cb_Log$Exposure == "Exposed", 1, 0)

#Oceans- Pacific as default
Indian <- ifelse(Cb_Log$Ocean_Name == "Indian", 1, 0)
Atlantic <- ifelse(Cb_Log$Ocean_Name == "Atlantic", 1, 0)
ArabianGulf <- ifelse(Cb_Log$Ocean_Name == "Arabian Gulf", 1, 0)
RedSea <- ifelse(Cb_Log$Ocean_Name == "Red Sea", 1, 0)


# Combining dummy variables with other predictor variables
X <- cbind( Sometimes, Exposed, Indian, Atlantic, ArabianGulf, RedSea, Cb_Log$Distance_to_Shore, Cb_Log$Turbidity, Cb_Log$Cyclone_Frequency,  Cb_Log$Depth_m, Cb_Log$Temperature_Kelvin, Cb_Log$Windspeed, Cb_Log$Percent_Cover, Cb_Log$SSTA_DHW)

# Fitting modelw/ dummy variable
fitC<- lm(Log_Percent_Bleaching ~  Sometimes + Exposed + Indian + Atlantic + ArabianGulf + RedSea + Latitude_Degrees +  Longitude_Degrees + Turbidity + Cyclone_Frequency+ Depth_m +  Percent_Cover  + Temperature_Kelvin +  SSTA_DHW, data = Cb_Log)
summary(fitC) #0.4135

layout(matrix(c(1,2,3,4),2,2)) 
plot(fitC)

