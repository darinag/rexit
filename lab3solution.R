# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  COM3018 / COMM053
#
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 16 OCTOBER 2019
#
# UPDATE
# 1.00      15/2/2019    Initial Version
# 1.01      25/2/2019    Updates for MANM module
# 1.02      16/10/2019   COM3018 / COMM053 2019
# 1.03      22/10/2019   Added PerformanceAnalytics as a required library
# 1.04      24/10/2019   Bug fix which fields were originally determined to be SYMBOLIC?
# ************************************************
# R Script For lab 3,

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables
# - i.e. available to all functions
# Good practice to place "constants" in named variables
# I use UPPERCASE to identify these in my code

DATASET_FILENAME = "globalterrorismdb_0616dist.csv"          # Name of input dataset file
# DATASET_FILENAME = "preprocessed.csv"
  
OUTPUT_FIELD      <- "Status"             # Field name of the output class to predict

#DATASET_FILENAME  <- "zoocust1.csv"          # Name of input dataset file
#OUTPUT_FIELD      <- "churned"             # Field name of the output class to predict

#DATASET_FILENAME  <- "adultSalaryData.csv"          # Name of input dataset file
#OUTPUT_FIELD      <- "Salary"             # Field name of the output class to predict

HOLDOUT           <- 70                   # % split to create TRAIN dataset

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
                                          # Set to negative means analyse but do not replace outliers

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCREET_BINS     <- 5                    # Number of empty bins to determine discreet
MAX_LITERALS      <- 1000000000                    # Maximum numner of hotcoding new fields

# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.51.4
# pROC	                 1.15.3
# formattable 	         0.2.01
# stats                  3.6.1
# PerformanceAnalytics   1.5.3
#install.packages('dplyr')


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "dplyr",
               "stats",
               "anchors")




# User defined functions are next

# ************************************************
# myModelFormula() :
#
# Create formula for column names & given output
#
# INPUT   :   Data frame - dataset         - data
#         :   String     - fieldNameOutput - name of the output field
#
# OUTPUT  :   Formula    - R formula object
#
# ************************************************
myModelFormula<-function(dataset,fieldNameOutput){
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  output<-paste(fieldNameOutput,"~")
  formular=as.formula(paste(output,inputs))
  return(formular)
} #endof myModelFormula()

# ************************************************
# myEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
myEvaluateClassifier<-function(probs,testing_data,threshold) {

  predictedClass<-ifelse(probs<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]

  results<-NcalcConfusion(expectedClass=expectedClass,predictedClass=predictedClass)

  return(results)
} #endof myEvaluateClassifier()


# ************************************************
# myPerformancePlot() :
#
# Use dataset to generate predictions from model
# as classifier at range of thresholds values
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# Uses   library(pROC)
# ************************************************
myPerformancePlot<-function(probs,testing_data){

  toPlot<-data.frame()

  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-myEvaluateClassifier(probs=probs,testing_data=testing_data,threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }

  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1

  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))

  # ************************************************
  # Plot threshold graph

  # Sensitivity
  plot(toPlot$x,toPlot$tpr,
       type="l",lwd=3, col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main="Threshold Perfomance Loan Classifier Model")

  # Plot the specificity (1-FPR)
  lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)

  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)

  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
  mindist<-toPlot$x[which(toPlot$distance==min(toPlot$distance))]
  abline(v=mindist,col="green",lty=3,lwd=2)

  # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  indexToBest<-which(toPlot$youdan==max(toPlot$youdan))
  maxYoudan<-toPlot$x[indexToBest]
  abline(v=maxYoudan,col="purple",lty=3,lwd=2)

  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))

  # ************************************************
  # ROC graph using a library

  rr<-pROC::roc(response=testing_data[,OUTPUT_FIELD],
          predictor=probs,
          plot=TRUE,auc=TRUE, auc.polygon=TRUE,
          percent=TRUE, grid=TRUE,print.auc=TRUE,
          main="ROC Loan Classifier Model",
          xlab="Specificity (1-FPR) %",
          ylab="Sensitivity (TPR) %")

  # Selects the "best" threshold based on distance
  analysis<-coords(rr, x="best",
                   best.method="closest.topleft",
                   ret=c("threshold",
                         "specificity",
                         "sensitivity"))

  fpr<-round(100.0-analysis["specificity"],digits=2)

  #Add crosshairs to the graph
  abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
  abline(v=analysis["specificity"],col="red",lty=3,lwd=2)

  #Annote with text
  annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L),
                  " TPR: ",round(analysis["sensitivity"],digits=2L),
                  "% FPR: ",fpr,"%",sep="")

  text(x=analysis["specificity"],
       y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
       col="red",annotate)

  #Use the "best" distance threshold to evaluate classifier
  results<-myEvaluateClassifier(probs=probs,
                                testing_data=testing_data,
                                threshold=analysis["threshold"])

  #Use the Youdan threshold to evaluate classifier
  #results<-myEvaluateClassifier(probs=probs,
  #                              testing_data=testing_data,
  #                              threshold=maxYoudan)

  return(results)
} #endof myPerformancePlot()

# ************************************************
# myModelling() :
# Create a logistic regression classifier and evaluate
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************
myModelling<-function(training_data,testing_data){

  formular<-myModelFormula(dataset=training_data,fieldNameOutput=OUTPUT_FIELD)

  #Build a logistic regression classifier on training dataset
  logisticModel<-stats::glm(formular,data=training_data,family=quasibinomial)

  # Get probabilities of being class 1 from the classifier
  probabilities<-predict(logisticModel, testing_data,type="response")

  #Evaluate the classifier on test dataset
  threshold<-0.7
  results<-myEvaluateClassifier(probs=probabilities,
                                testing_data=testing_data,
                                threshold=threshold)

  # This outputs our results into the "Viewer" in RStudio
  NprintMeasures(results)

  # Plot FPR/TPR through threshold range
  results<-myPerformancePlot(probs=probabilities,testing_data=testing_data)

  NprintMeasures(results)

  # Optional output of strengths
  importance<-as.data.frame(caret::varImp(logisticModel, scale = TRUE))
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  barplot(t(importance[order(importance$Overall),,drop=FALSE]),las=2, border = 0, cex.names =0.8)

  print("evaluation complete")
}

# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){

  print("Inside main function")

  # ************************************************
  # This reads in a CSV file called "UCI-G"
  # German Credit Score dataset
  # The first row of this file has the field names of each column
  # Note: "Status" is 1 for good loan, 2 for loan defaulted/bad
  # This is a discreet numeric that will later be converted to good=0, bad=1
  
  DATASET_FILENAME = "globalterrorismdb_0616dist.csv"  
  global_terrorism <- read.csv(DATASET_FILENAME)
  
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  after_1997 <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  
  
  post_feature_selection <- data.frame(#"Event_Id" = after_1997$eventid,
    #"Year" = after_1997$iyear,
    #"Month" = after_1997$imonth,
    #"Day" = after_1997$iday,
    #"Country" = after_1997$country,
    #"Country" = after_1997$country_txt,
    #"Region" = after_1997$region,
    "Region" = after_1997$region_txt,
    #"City" = after_1997$city,
    #"Vicinity" = after_1997$vicinity,
    #"Latitude" = after_1997$latitude,
    #"Longtitude" = after_1997$longitude,
    #"Criteria1" = after_1997$crit1,
    #"Criteria2" = after_1997$crit2,
    #"Criteria3" = after_1997$crit3,
    #"Multiple_Attacks" = after_1997$multiple,
    #"Related_Attacks" = after_1997$related,
    "Attack_Type" = after_1997$attacktype1_txt,
    #"Successful" = after_1997$success,
    #"Suicide" = after_1997$suicide,
    "Weapon_Type" = after_1997$weaptype1_txt,
    #"Target_Type" = after_1997$targtype1,
    #"Group_Name" = after_1997$gname,
    #"Perpetrators_Number" = after_1997$nperps,
    #"Motive" = after_1997$motive,
    "Kill_Count" = after_1997$nkill,
    #"Perpetrator_Kill_Count" = after_1997$nkillter,
    "Wounded_Count" = after_1997$nwound
    #"Perpetrators_Wounded_Count" = after_1997$nwoundte,
    #"Property_Damage" = after_1997$property,
    #"Property_Damage_Extent" = after_1997$propextent,
    #"Property_Damage_Value" = after_1997$propvalue,
    #"Nationality_Interconnection" = after_1997$INT_IDEO
  )
  
  # Filter data for moments calculations
  filtered_killed <- post_feature_selection %>% filter(!is.na(Kill_Count) & Kill_Count>=0 ) 
  filtered_wounded_no_na <- post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count >= 0)
  filtered_wounded_no_na_no_zero <- post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count>0)
  
  # Thresholds
  killed_threshold <- round(mean(filtered_killed$Kill_Count)) 
  wounded_threshold <- round(mean(filtered_wounded_no_na_no_zero$Wounded_Count))
  
  # Transform Dataset
  loans <- replace.value(post_feature_selection, c("Kill_Count"), from=NA, to=as.double(killed_threshold))
  loans <- replace.value(loans, c("Wounded_Count"), from=NA, to=as.double(wounded_threshold))
  loans <- subset(loans, !is.na(Country))
  loans <- subset(loans, !is.na(Region))
  
  #countries<- loans %>% group_by(Country) %>% summarize(count=n())
  regions<- loans %>% group_by(Region) %>% summarize(count=n())
  attack_type <- loans %>% group_by(Attack_Type) %>% summarize(count=n())
  weapon_type <- loans %>% group_by(Weapon_Type) %>% summarize(count=n())

  #DATASET_FILENAME = "UCI-G.csv"
  #loans<-NreadDataset(DATASET_FILENAME)
 
  NPREPROCESSING_prettyDataset(loans)

  # ************************************************
  # Determine if the field appears numeric or symbolic

  field_types<-NPREPROCESSING_initialFieldType(loans)

  # ************************************************
  # View the field types on the console

  numeric_fields<-names(loans)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)

  symbolic_fields<-names(loans)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)

  # ************************************************
  # Determine if the numeric fields might be discreet numeric

  field_types1<-NPREPROCESSING_discreetNumeric(dataset=loans,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)

  results<-data.frame(field=names(loans),initial=field_types,types1=field_types1)
  print(formattable::formattable(results))

  # ************************************************
  # This is a sub-set frame of just the ordinal fields

  #ordinals<-loans[,which(field_types1==TYPE_ORDINAL)]

  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%

  #ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)

  # ************************************************
  # z-scale
  #zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))

  # In the choosen classifier, the input values need to be scaled to [0.0,1.0]
  #ordinalReadyforML<-Nrescaleentireframe(zscaled)

  # ************************************************
  # We now have a frame of just the numeric fields, nice and ready for the ML
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset=loans,field_types=field_types1)

  #print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))

  # number of non-numeric fields before transformation
  # 241019 which fields are either SYMBOLIC or DISCREET
  nonNumericbefore<-length(which(field_types1!=TYPE_ORDINAL))

  # How many fields have be generated through the 1-hot-encoding process
  nonNumerictranformed<-ncol(catagoricalReadyforML)
  print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))

  # Output the names of the encoded fields (literals)
  print(formattable::formattable(data.frame(field=1:nonNumerictranformed,encoded=names(catagoricalReadyforML))))

  # ************************************************
  # Combine the two sets of data that are read for ML
  combinedML<-catagoricalReadyforML

  # Are any of the fields redundant?
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=0.95)

  # The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))

  # ************************************************
  # **** Create a TRAINING dataset using HOLDOUT% (e.g. 70) of the records

  #Randomise the entire data set
  # combinedML<-combinedML[order(runif(nrow(combinedML))),]
  # 
  # # Create a TRAINING dataset using first HOLDOUT% of the records
  # # and the remaining 30% is used as TEST
  # # use ALL fields (columns)
  # training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  # training_data <- combinedML[1:training_records,]
  # testing_data = combinedML[-(1:training_records),]
  # 
  # # ************************************************
  # 
  # myModelling(training_data = training_data, testing_data = testing_data)

} #endof main()

# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#This [optionally] sets working directory
#setwd("/Users/nickryman-tubb/Documents/My Stuff/University of Surrey/UOS Teaching/MANM354 - 2019/Labs/lab3 - preprocessing/code")

#Load additional R script files provide for this lab
source("lab3dataPrep.R")

set.seed(123)

# ************************************************
main()

print("end")

