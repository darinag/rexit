# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  COM3018 
#
# Group: REXIT
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 03 DECEMBER 2019
#
# ************************************************
# R Script For Coursework Assignment

# ************************************************
#   Global Environment variables
# ************************************************
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCREET_BINS     <- 0                     # Number of empty bins to determine discreet
MAX_LITERALS      <- 55                    # Maximum numner of hotcoding new fields

# computeImpactfulField(dataset, killed_threshold, wounded_threshold) :
#
# Compute and add Impactful field to the dataframe and transform Kill Count
#
# INPUT: data Frame - refined dataset
#        threshold for Kill Count - Generated with computeRoundedMean
#        threshold for Wounded Count - Generated with computeRoundedMean
#
# OUTPUT : data Frame - transformed dataset
# 
# ************************************************
computeImpactfulField <- function(dataset, killed_threshold, wounded_threshold) {
  
  dataset <- replace.value(dataset, c("Kill_Count"), from=NA, to=as.double(killed_threshold))
  dataset <- replace.value(dataset, c("Wounded_Count"), from=NA, to=as.double(wounded_threshold))
  
  dataset$Impactful=0
  dataset_refined <- dataset %>% mutate(Impactful 
            = case_when(Kill_Count > killed_threshold
                        | Wounded_Count > wounded_threshold 
                        | Property_Damage_Extent == 1 
                        | Property_Damage_Extent == 2 ~ 1, 
                        TRUE  ~ 0))
  dataset_refined;
}

# ************************************************
# computeRoundedMean(dataset, colname) :
#
# Compute rounded means for fields that cannot contain continuous values
# To be used as a thresholds or mean
#
# INPUT: data Frame - refined dataset with no missing values
#        column name of the field containing the values to be thransformed
#
# OUTPUT : threshold - the computed mean
# 
# ************************************************
computeRoundedMean <- function(dataset, colname) {
  columnIndex <- which(colnames(dataset) == colname)
  filtered_dataset <- dataset[!is.na(dataset[,columnIndex]), ]
  filtered_dataset <- filtered_dataset[filtered_dataset[,columnIndex]>=0, ]
  threshold <- round(mean(filtered_dataset[,columnIndex]))
  
  threshold;
}

# ************************************************
# correlationAndCovarianceMatrix(dataset) :
#
# Correlation and Covariance matrices for numeric fields
# 
#
# INPUT: data Frame - refined dataset with no missing values
#
#
# OUTPUT: Correlation and Covariance matrices displayed in the console
# 
# ************************************************
correlationAndCovarianceMatrix <- function(dataset) {
  
  # Dataset for Correlation Matrix
  dataset_prepped <- data.frame("Kill_Count" = dataset$Kill_Count,
                                   "Wounded_Count" = dataset$Wounded_Count,
                                   "Perpetrator_Count" = dataset$Perpetrators_Number)
  
  filtered_99 <- replace.value(dataset_prepped, c("Perpetrator_Count"), from=-99, to=as.double(NA))
  filtered_9 <- replace.value(filtered_99, c("Perpetrator_Count"), from=-9, to=as.double(NA))
  filtered_ready <- filtered_9 %>% filter(!is.na(Perpetrator_Count))
  
  #Correlation and Covariance Matrices
  correlation_matrix <- cormat(filtered_ready)
  covariance_matrix <- cov(filtered_ready)
  
  print(correlation_matrix)
  print(covariance_matrix)
  
}

# ************************************************
# transformNumeric(dataset) :
#
# Check data types, remove outliers from numeric data and transform them with feature scaling
#
# INPUT: data Frame - refined dataset that must only contain fields of numeric type with removed missing values
#
# OUTPUT : transformed data Frame, ready for modelling and further transformation
# 
# ************************************************
transformNumeric <- function(dataset) {
  
  field_types<-NPREPROCESSING_initialFieldType(dataset)
  
  # View the field types on the console
  numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  # Determine if the numeric fields might be discreet numeric
  field_types1<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)
  
  results<-data.frame(field=names(dataset),initial=field_types,types1=field_types1)
  print(formattable::formattable(results))
  
  # find ordinals
  numerics<-data.frame(dataset[,which(field_types==TYPE_NUMERIC)])
  
  # Test if any values are outliers and replace with mean values
  numerics<-NPREPROCESSING_outlier(ordinals=numerics,confidence=OUTLIER_CONF)
 
  colnames <- c(colnames(numerics))
  
  # z-scale
  scaled <- as.data.frame(scale(numerics,center=TRUE, scale=TRUE))
  ReadyforML<-Nrescaleentireframe(scaled)
  as.data.frame(ReadyforML)

}

# ************************************************
# oneHotEncoding(attacks) :
#
# Perform one-hot-encoding on categorical fields
# 
#
# INPUT: data Frame - refined dataset with no missing values
#
#
# OUTPUT: one-hot-encoded categorical fields
# 
# ************************************************
oneHotEncoding <- function (attacks) {
  
  field_types<-NPREPROCESSING_initialFieldType(attacks)
  
  results<-data.frame(field=names(attacks),initial=field_types,types1=field_types)
  print(formattable::formattable(results))
  
  # perform 1 hot encoding on symbolic fields
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset=attacks,field_types=field_types)
  
  print(formattable::formattable(data.frame(fields=names(catagoricalReadyforML))))
  
  # number of symbolic fields before transformation
  nonNumericbefore<-length(which(field_types==TYPE_SYMBOLIC))
  
  # number of fields generated by 1-hot-encoding
  nonNumerictranformed<-ncol(catagoricalReadyforML)
  print(paste("Before encoding=",nonNumericbefore,"After",nonNumerictranformed))
  
  # Output the names of the encoded fields (literals)
  print(formattable::formattable(data.frame(field=1:nonNumerictranformed,encoded=names(catagoricalReadyforML))))
  
  # The dataset for ML information
  print(paste("Fields=",ncol(catagoricalReadyforML)))
  
  catagoricalReadyforML
}