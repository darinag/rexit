# ************************************************
# computeImpactfulField() :
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
# computeRoundedMean() :
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
  #filtered_dataset <- dataset %>% filter(!is.na(filtered_dataset[colname, ]) & colname>=0 ) 
  columnIndex <- which(colnames(dataset) == colname)
  filtered_dataset <- dataset[!is.na(dataset[,columnIndex]), ]
  filtered_dataset <- filtered_dataset[filtered_dataset[,columnIndex]>=0, ]
  threshold <- round(mean(filtered_dataset[,columnIndex]))
  
  threshold;

}

# ************************************************
# correlationAndCovarianceMatrix() :
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
# transformNumeric() :
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
  
  # ************************************************
  # View the field types on the console
  
  numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  # ************************************************
  # Determine if the numeric fields might be discreet numeric
  
  field_types1<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)
  
  results<-data.frame(field=names(dataset),initial=field_types,types1=field_types1)
  print(formattable::formattable(results))
  
  # find ordinals
  numerics<-data.frame(dataset[,which(field_types==TYPE_NUMERIC)])
  
  # Test if any values are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  numerics<-NPREPROCESSING_outlier(ordinals=numerics,confidence=OUTLIER_CONF)
 
  colnames <- c(colnames(numerics))

  
  # ************************************************
  # z-scale
  #zscaled<-as.data.frame(scale(perps_preprocessed,center=TRUE, scale=TRUE))
  scaled <- as.data.frame(scale(numerics,center=TRUE, scale=TRUE))
  ReadyforML<-Nrescaleentireframe(scaled)
  as.data.frame(ReadyforML)

}


