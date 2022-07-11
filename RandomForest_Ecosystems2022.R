#The following R code details the Random Forest analysis presented in 
#"Topography mediates the response of soil CO2 efflux to precipitation 
#over days, seasons, and years"

#AUTHORS: Marissa Kopp [1], Jason Kaye [1], Yuting He Smeglin [2],  
#Thomas Adams [3], Edward Primka IV [1,4], Brosi Bradley [1], Yuning Shi [3], 
#David Eissenstat [1] 

#AUTHORS AFFILIATIONS: [1] Department of Ecosystem Science and Management, 
#Pennsylvania State University, University Park, PA, USA; 
#[2] Department of Meteorology and Atmospheric Science, Pennsylvania State University, 
#University Park, PA, USA; [3] Department of Plant Science, Pennsylvania State University,
#University Park, PA, USA; [4] Department of Natural Resource Ecology and Management, 
#Oklahoma State University, Stillwater, OK, 74078, USA

#CORRESPONDING AUTHOR CONTACT: mkk5565@psu.edu; 
#116 ASI Building, University Park, PA, 16802

#Acknowledgements: This code is modified from Debashish Saha et al. 2021
#Available: https://datadryad.org/stash/dataset/doi:10.5061/dryad.bnzs7h493 

##############################################################################

###load packages
library(ggplot2)
library(cowplot)
library(randomForest)
library(reprtree)
library(party)
library(ggpubr)
library(Metrics)

########Step 0: Clean and split data into convergent and non-convergent

#Import data and check structure of dataset
str(RsForRandomForest)
RsForRandomForest$year <- as.factor(RsForRandomForest$year)
RsForRandomForest$Chamber <- as.factor(RsForRandomForest$Chamber)
names(RsForRandomForest)[6] <- "Ppt_3wk"
names(RsForRandomForest)[15] <- "Planform_Curv"
names(RsForRandomForest)[16] <- "Profile_Curv"
names(RsForRandomForest)[18] <- "SoilC_O"
names(RsForRandomForest)[19] <- "SoilC_Surface"
names(RsForRandomForest)[20] <- "SoilC_Deep"
names(RsForRandomForest)[21] <- "Clay_Surface"
names(RsForRandomForest)[22] <- "Clay_Deep"

#Drop missing values
RsForRandomForest_naOMIT <- RsForRandomForest[!is.na(RsForRandomForest$CO2),]

#Split data into chambers from convergent areas 
convergent <- subset(RsForRandomForest_naOMIT, Chamber == "Upper_SW" |
                       Chamber == "Lower_NW" |
                       Chamber == "Lower_SW" |
                       Chamber == "Lower_SE")

#Split data into chambers from nonconvergent areas 
nonconvergent <- subset(RsForRandomForest_naOMIT, Chamber == "Upper_NW" |
                          Chamber == "Upper_NE" |
                          Chamber == "Upper_SE" |
                          Chamber == "Lower_NE")

#########Step 1a: Train the overall model (convergent and nonconvergent combined)

#Randomly split the overall data into 70% for training, 30% for validating
set.seed (502)
train <- sample(2, nrow(RsForRandomForest_naOMIT), replace = TRUE, prob = c(0.7,0.3))
TrainSet <- RsForRandomForest_naOMIT[train==1,] #To train the RF model
ValidSet <- RsForRandomForest_naOMIT[train==2,] #To validate the RF model
summary(TrainSet)
summary(ValidSet)

#The Standard Random Forest model was run with 10 different seeds to estimate 
#uncertainty of variable importance due to bootstrapping

###Set seed to replicate random generation
set.seed (502)
#set.seed (124)
#set.seed (101)
#set.seed (1001)
#set.seed (2346)
#set.seed (801)
#set.seed (1021)
#set.seed (5001)
#set.seed (556)
#set.seed (12346)

fit_rf <- randomForest(CO2 ~ Ppt_3wk + AvAirTemp + Elevation +
                         Planform_Curv + Profile_Curv + Soil_Depth + 
                         SoilC_O + SoilC_Surface + SoilC_Deep + 
                         Clay_Surface + Clay_Deep, data=TrainSet, 
                       keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)

#The summary of the Standard Random Forest model
fit_rf
#Variable importance measures
importance (fit_rf)
#Looking at the OOB error reduction with the tree size
plot (fit_rf) 
#Plotting the variable importance
varImpPlot(fit_rf)

#########Step 2: Evaluate the model

#Generate predictions of validation data using Random Forest Model
prediction.validation <- predict(fit_rf_con, ValidSet_con) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validation, ValidSet_con$CO2, method = "pearson", alpha=.05)

Validation_con <- cbind(prediction.validation, ValidSet_con)
names(Validation_con)[1] <- "Prediction"

validation_con.lm <- lm(CO2~Prediction, data=Validation_con)
summary(validation_con.lm)

##########################################################################

#########Step 1b: Train the convergent model

#Randomly split the convergent data into 70% for training, 30% for validating
set.seed (502)
train_con <- sample(2, nrow(convergent), replace = TRUE, prob = c(0.7,0.3))
TrainSet_con <- convergent[train_con==1,]
ValidSet_con <- convergent[train_con==2,]
summary(TrainSet_con)
summary(ValidSet_con)

#The Standard Random Forest model was run with 10 different seeds to estimate 
#uncertainty of variable importance due to bootstrapping

###Set seed to replicate random generation
set.seed (502)
#set.seed (124)
#set.seed (101)
#set.seed (1001)
#set.seed (2346)
#set.seed (801)
#set.seed (1021)
#set.seed (5001)
#set.seed (556)
#set.seed (12346)

fit_rf_con <- randomForest(CO2 ~ Ppt_3wk + AvAirTemp + Elevation +
                             Planform_Curv + Profile_Curv + Soil_Depth + 
                             SoilC_O + SoilC_Surface + SoilC_Deep + 
                             Clay_Surface + Clay_Deep, data=TrainSet_con, 
                           keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)

#The summary of the Standard Random Forest model
fit_rf_con
#Variable importance measures
importance (fit_rf_con)
#Looking at the OOB error reduction with the tree size
plot (fit_rf_con) 
#Plotting the variable importance
varImpPlot(fit_rf_con)

#########Step 1c: Train the nonconvergent model

#########Step 2: Evaluate the model

#Generate predictions of validation data using Random Forest Model
prediction.validation <- predict(fit_rf_con, ValidSet_con) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validation, ValidSet_con$CO2, method = "pearson", alpha=.05)

Validation_con <- cbind(prediction.validation, ValidSet_con)
names(Validation_con)[1] <- "Prediction"

validation_con.lm <- lm(CO2~Prediction, data=Validation_con)
summary(validation_con.lm)
