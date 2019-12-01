# ************************************************
# The Neural Network used to predict impactful attacks is defined within this file.
# TODO : More explanation
# ************************************************

# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

# ************************************************
#   Global Environment variables
# ************************************************
OUTPUT_FIELD      <- "Successful"             # Field name of the output class to predict

OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection

# These are the data preparation values
HOLDOUT           <- 70                   # % split to create TRAIN dataset

BASICNN_HIDDEN    <- 10                   # 10 hidden layer neurons
BASICNN_EPOCHS     <- 100                  # Maximum number of training epocs
DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE 

#Filename of the dataset
DATASET_FILENAME = "data/globalterrorismdb_0718dist.csv"

# ************************************************

neural_network<-function(train,test, plot=TRUE){
  
  myTitle<-paste("Preprocessed Dataset. MLP. Hidden=",BASICNN_HIDDEN,sep="")
  print(myTitle)

  N_DEEP_Initialise()
  
  mlp_classifier<-N_DEEP_TrainClassifier(train=train,
                                         fieldNameOutput=OUTPUT_FIELD,
                                         hidden=BASICNN_HIDDEN,
                                         stopping_rounds=DEEP_STOPPING,
                                         stopping_tolerance=DEEP_TOLERANCE,
                                         activation=DEEP_ACTIVATION,
                                         reproducible=DEEP_REPRODUCABLE)
  
  plot(mlp_classifier,metric="classification_error")
  
  # Evaluate the deep NN
  measures<-N_EVALUATE_DeepNeural(test=test,
                                  fieldNameOutput=OUTPUT_FIELD,
                                  deep=mlp_classifier,
                                  plot=plot,
                                  myTitle = myTitle)
  
  return(measures)
} #endof mlpNeural()


# ************************************************
# main() :
# main entry point to our program
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function(){
  # Load the dataset
  global_terrorism <- read.csv(DATASET_FILENAME)
  
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  filteredDataset <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  
  filteredDataset <- data.frame(
    "Kill_Count" = filteredDataset$nkill,
    "Wounded_Count" = filteredDataset$nwound,
    "Property_Damage_Extent" = filteredDataset$propextent,
    "Country" = filteredDataset$country,
    "Country_Txt" = filteredDataset$country_txt,
    "Region" = filteredDataset$region_txt,
    "Attack_Type" = filteredDataset$attacktype1_txt,
    "Weapon_Type" = filteredDataset$weaptype1_txt,
    "Perpetrators_Number" = filteredDataset$nperps,
    "Successful" = filteredDataset$success
  )
  
  # get 15 countries most often victims of an attack
  top15Countries <- filteredDataset %>% 
    group_by(Country) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    slice(1:15)
  
  countryVect <- top15Countries$Country
  
  # filter out data to have only top 15 countries
  filteredDataset <- filteredDataset[filteredDataset$Country %in% countryVect,]
  
  killedThresh <- computeRoundedMean(filteredDataset, 'Kill_Count')
  woundedThresh <- computeRoundedMean(filteredDataset, 'Wounded_Count')

  filteredDataset <- computeImpactfulField(filteredDataset, killedThresh, woundedThresh)
  
  perpetrator_mean <- computeRoundedMean(filteredDataset, "Perpetrators_Number")
  
  filteredDataset <- filteredDataset %>% 
    mutate(Perpetrators_Number = case_when(is.na(Perpetrators_Number) 
                                           | Perpetrators_Number < 0 ~ perpetrator_mean, 
                                           TRUE ~ as.double(Perpetrators_Number)))
  
  
  filteredDataset <- select(filteredDataset,-c("Property_Damage_Extent", "Country"))
  
  transformedNumeric <- transformNumeric(filteredDataset)
  
  cr<-cor(select(filteredDataset, c("Kill_Count", "Wounded_Count", "Perpetrators_Number", "Successful", "Impactful")), use="everything")
  NPLOT_correlagram(cr)
  
  attacks_categorical <- data.frame(
    #"Country" = filteredDataset$Country_Txt,
    "Region" = filteredDataset$Region,
    "Attack_Type" = filteredDataset$Attack_Type,
    "Weapon_Type" = filteredDataset$Weapon_Type
  )

  transformedCategorical<-oneHotEncoding(attacks_categorical)
  
  #readyForNN<-cbind(transformedCategorical, select(transformedNumeric, -c("Kill_Count", "Wounded_Count")))
  readyForNN<-cbind(transformedCategorical, select(transformedNumeric, -c("Impactful")))
  
  
  names(readyForNN)[names(readyForNN) == 'Weapon_Type_Vehiclenottoincludevehicleborneexplosivesiecarortruckbombs'] <- 'Weapon_Type_Vehicle_Not_Bomb'
  
  dataset_split<-NPREPROCESSING_splitdataset(readyForNN)
  measures<- neural_network(train=dataset_split$train, test=dataset_split$test)
}

gc() # garbage collection to automatically release memory
# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# clears the console area
cat("\014")

print("START Neural Network")

# specify libraries to be loaded by pacman
# TODO Remove unused
myLibraries<-c(
  "readr",
  "ggplot2",
  "dplyr",
  "tidyverse",
  "DT",
  "highcharter",
  "treemap",
  "viridis",
  "h2o",
  "keras",
  "pROC",
  "formattable",
  "anchors",
  "tensorflow",
  "pgirmess",
  "corrplot")

library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)      

# load functions from the labs, all code written by Prof. Nick Ryman-Tubb
source("scripts/labFunctions.R")

source("scripts/oneHotEncoding.R")
source("scripts/preprocessingFunctions.R")

set.seed(123)

main()

print("end of program")

