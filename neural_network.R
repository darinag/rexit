
# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

# ************************************************
#   Global Environment variables

DATASET_FILENAME = "globalterrorismdb_0718dist.csv"


# Load the libraries used in the project
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(highcharter)
library(treemap)
library(viridis)
library(h2o)
library(keras)
library(pROC)
library(formattable)
               

# Load the dataset
global_terrorism <- read.csv(DATASET_FILENAME)

# Includes only incidents after 1997, where all incidents represent an act of terrorism 
after_1997 <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]

refined <- data.frame(
  "Country" = after_1997$country,
  "Criteria1" = after_1997$crit1,
  "Criteria2" = after_1997$crit2,
  "Criteria3" = after_1997$crit3,
  "Attack_Type" = after_1997$attacktype1,
  "Successful" = after_1997$success,
  "Suicide" = after_1997$suicide,
  "Weapon_Type" = after_1997$weaptype1,
  "Target_Type" = after_1997$targtype1,
  "Perpetrators_Number" = after_1997$nperps
)

########NEURAL NETWORK#########
OUTPUT_FIELD      <- "Successful"             # Field name of the output class to predict

# These are the data preparation values

HOLDOUT           <- 70                   # % split to create TRAIN dataset

# Cutoff values - you can experiment with these

CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off

# Indicates the type of each field

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

MAX_LITERALS      <- 55                    # Maximum numner of 1-hot-encoding fields

# These are the supervised model constants

PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RULES_FILENAME    <- "rules.txt"          # Name of text file with rules saved
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

BASICNN_HIDDEN    <- 10                   # 10 hidden layer neurons
BASICNN_EPOCHS    <- 100                  # Maximum number of training epocs

# See https://cran.r-project.org/web/packages/h2o/h2o.pdf

DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE 

NPREPROCESSING_splitdataset<-function(combinedML){
  
  # **** Create a TRAINING dataset using HOLDOUT % of the records
  
  combinedML<-combinedML[order(runif(nrow(combinedML))),]
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  
  train <- 1:training_records
  test <- -train
  
  training_data <- combinedML[train,]
  testing_data = combinedML[test,]
  
  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}

NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  
  return(results)
} #endof NEvaluateClassifier()

N_DEEP_Initialise<-function(reproducible=FALSE){
  
  library(h2o)
  
  print("Initialise the H2O server")
  #Initialise the external h20 deep learning local server if needed
  #130517NRT - set nthreads to -1 to use maximum so fast, but set to 1 to get reproducable results
  #080819NRT - use reproducable parameter
  if (reproducible==TRUE)
    nthreads<-1
  else
    nthreads<- -1
  
  h2o.init(max_mem_size = "5g",nthreads = nthreads)
  
  h2o.removeAll() # 261019NRT clean slate - just in case the cluster was already running
  #h2o.no_progress()
}

N_DEEP_TrainClassifier<- function(train,
                                  fieldNameOutput,
                                  hidden,
                                  stopping_rounds,
                                  stopping_tolerance,
                                  activation,
                                  reproducible){
  
  positionOutput<-which(names(train)==fieldNameOutput)
  
  #Creates the h2o training dataset
  train[fieldNameOutput] <- lapply(train[fieldNameOutput] , factor) #Output class has to be a R "factor"
  
  train_h2o <- as.h2o(train, destination_frame = "traindata")
  
  # Create validation dataset for early stopping
  splits <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%
  
  #This lists all the input field names ignoring the fieldNameOutput
  predictors <- setdiff(names(train_h2o), fieldNameOutput)
  
  # Deep training neural network
  # Updated 13/5/17 - set reproducible = TRUE so that the same random numbers are used to initalise
  # 281019NRT - added validation dataset for early stopping
  
  deep<-h2o::h2o.deeplearning(x=predictors,
                              y=fieldNameOutput,
                              training_frame = nntrain,
                              validation_frame=nnvalid,
                              epochs=BASICNN_EPOCHS,
                              hidden=hidden,
                              adaptive_rate=TRUE,
                              stopping_rounds=stopping_rounds,
                              stopping_tolerance=stopping_tolerance,
                              stopping_metric = "misclassification",
                              fast_mode=FALSE,
                              activation=activation,
                              seed=1234,
                              l1 = 1e-2,
                              l2 = 1e-2,
                              variable_importances = TRUE,
                              reproducible = TRUE)
  return(deep)
}

N_EVALUATE_DeepNeural<-function(test,fieldNameOutput, deep,plot,myTitle){
  
  #Creates the h2o test dataset
  test[fieldNameOutput] <- lapply(test[fieldNameOutput] , factor) #Output class has to be a R "factor"
  test_h2o <- as.h2o(test, destination_frame = "testdata")
  
  pred <- h2o::h2o.predict(deep, test_h2o)
  
  test_predicted<-as.vector(pred$p1)  #Returns the probabilities of class 1
  
  positionClassOutput<-which(names(test)==fieldNameOutput)
  
  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predicted,
                                plot=plot,
                                title=myTitle)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=myTitle)
  
  return(measures)
}

N_MLP_TrainClassifier<- function(train,
                                 fieldNameOutput,
                                 hidden,
                                 plot
){
  
  positionClassOutput<-which(names(train)==fieldNameOutput)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  x<-as.matrix(train_inputs)
  y<-keras::to_categorical(train_expected,num_classes = 2)
  
  mlp_classifier = keras_model_sequential()
  
  # add layers, first layer needs input dimension
  mlp_classifier %>%
    keras::layer_dense(input_shape = ncol(x), units=ncol(x), activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = hidden, activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = 2, activation = "softmax")
  
  # add a loss function and optimizer
  mlp_classifier %>%
    keras::compile(
      loss = "categorical_crossentropy",
      optimizer = "adagrad",
      metrics = "accuracy"
    )
  
  # train model with our training data set
  fit = mlp_classifier %>%
    keras::fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 5,
      validation_split = 0.2,
      epochs = BASICNN_EPOCHS,
      callbacks = c(
        callback_early_stopping(monitor = "val_loss", patience = 8, mode = "auto")),
      verbose=0, view_metrics=0
    )
  
  # Plot the neural network error (loss) udring training
  if (plot==TRUE)
    print(plot(fit))
  
  return(mlp_classifier)
}

N_evaluate_MLP<-function(test,fieldNameOutput,mlp_classifier,plot,myTitle){
  
  positionClassOutput<-which(names(test)==fieldNameOutput)
  
  # test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedAllClassProbs<-predict(mlp_classifier,as.matrix(test_inputs))
  
  # Probabilities for just class 1
  testPredictedClassProbs<-testPredictedAllClassProbs[,2]
  
  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=testPredictedClassProbs,
                                plot=plot,
                                title=myTitle)
  if (plot==TRUE)
    NprintMeasures(results=measures,title=myTitle)
  
  return(measures)
}

NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  
  # Helper local scope function
  getFirst<-function(values){
    if (length(values)>1){
      return(values[1])
    } else
      return(values)
  }
  
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  indexToBest<-getFirst(which(toPlot$youdan==max(toPlot$youdan)))
  maxYoudan<-toPlot$x[indexToBest]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  #241019 select just the first min distance, as might be more
  mindist<-getFirst(toPlot$x[which(toPlot$distance==min(toPlot$distance))])
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if (crosspoint<1)
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more
    abline(v=mindist,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),
           col=c("blue","red","green","purple"),
           lty=c(1,1,3,3),
           lwd=2)
    
    text(x=0,y=50, adj = c(-0.2,2),cex=1,
         col="black",
         paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))
    
    # ************************************************
    # ROC graph using a library
    
    rr<-pROC::roc(response=test_expected,
                  predictor=test_predicted,
                  plot=TRUE,
                  auc=TRUE,
                  auc.polygon=TRUE,
                  percent=TRUE,
                  grid=TRUE,
                  print.auc=TRUE,
                  main=paste("ROC for Classifier Model",title),
                  xlab="Specificity (1-FPR) %",
                  ylab="Sensitivity (TPR) %")
    
    # Selects the "best" threshold based on distance
    analysis<-coords(rr, x="best",transpose = FALSE,
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
    
  } # endof if plotting
  
  # Select the threshold - I have choosen distance
  
  myThreshold<-mindist      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  
  return(results)
} #endof myPerformancePlot()

NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
} #endof NcalcConfusion()

NcalcMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}

NprintMeasures<-function(results,title){
  
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-title
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}


original<-NPREPROCESSING_splitdataset(refined)


mlpNeural<-function(train,test, plot=TRUE){
  
  myTitle<-paste("Preprocessed Dataset. MLP. Hidden=",BASICNN_HIDDEN,sep="")
  print(myTitle)
  
  # Set to TRUE to use the h2o library
  # otheriwse FALSE to try to use the Keras library
  
  if (TRUE) {
    N_DEEP_Initialise()
    
    mlp_classifier<-N_DEEP_TrainClassifier(train=train,
                                           fieldNameOutput=OUTPUT_FIELD,
                                           hidden=BASICNN_HIDDEN,
                                           stopping_rounds=DEEP_STOPPING,
                                           stopping_tolerance=DEEP_TOLERANCE,
                                           activation=DEEP_ACTIVATION,
                                           reproducible=DEEP_REPRODUCABLE)
    
    plot(mlp_classifier,metric="classification_error")
    
    # Evaluate the deep NN as we have done previously
    measures<-N_EVALUATE_DeepNeural(test=test,
                                    fieldNameOutput=OUTPUT_FIELD,
                                    deep=mlp_classifier,
                                    plot=plot,
                                    myTitle = myTitle)
  } else {
    
    mlp_classifier<-N_MLP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=BASICNN_HIDDEN,
                                          plot=plot)
    
    measures<-N_evaluate_MLP(test=test,
                             fieldNameOutput=OUTPUT_FIELD,
                             mlp_classifier=mlp_classifier,
                             plot=plot,
                             myTitle=myTitle)
  } #endof if()
  
  return(measures)
} #endof mlpNeural()

measures<- mlpNeural(train=original$train, test=original$test)

