MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "dplyr",
               "anchors",
               "corrplot",
               "outliers",
               "eqs2lavaan",
               "ggplot2",
               "tidyverse",
               "corrplot",
               "pgirmess",
               "float",
               "BBmisc")


OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers

TYPE_DISCREET     <- "DISCREET"           # d is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCREET_BINS     <- 5 

DATASET_FILENAME <- "globalterrorismdb_0718dist.csv"





library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("lab3dataPrep.R")
source("lab2functions.R")
source("preprocessingFunctions.R")


  
  main_dataset <- read.csv(DATASET_FILENAME)

  after_1997 <- main_dataset[main_dataset$iyear >= 1997 & main_dataset$doubtterr == 0,]
  post_feature_selection <- data.frame("Event_Id" = after_1997$eventid,
                                             "Year" = after_1997$iyear,
                                             "Month" = after_1997$imonth,
                                             "Day" = after_1997$iday,
                                             "Country" = after_1997$country,
                                             "Country_txt" = after_1997$country_txt,
                                             "Region" = after_1997$region,
                                             "Region_txt" = after_1997$region_txt,
                                             "City" = after_1997$city,
                                             "Vicinity" = after_1997$vicinity,
                                             "Latitude" = after_1997$latitude,
                                             "Longtitude" = after_1997$longitude,
                                             "Criteria1" = after_1997$crit1,
                                             "Criteria2" = after_1997$crit2,
                                             "Criteria3" = after_1997$crit3,
                                             "Multiple_Attacks" = after_1997$multiple,
                                             "Related_Attacks" = after_1997$related,
                                             "Attack_Type" = after_1997$attacktype1,
                                             "Attack_Type_txt" = after_1997$attacktype1_txt,
                                             "Successful" = after_1997$success,
                                             "Suicide" = after_1997$suicide,
                                             "Weapon_Type" = after_1997$weaptype1,
                                             "Weapon_Type_txt" = after_1997$weaptype1_txt,
                                             "Target_Type" = after_1997$targtype1,
                                             "Group_Name" = after_1997$gname,
                                             "Perpetrators_Number" = after_1997$nperps,
                                             "Motive" = after_1997$motive,
                                             "Kill_Count" = after_1997$nkill,
                                             "Perpetrator_Kill_Count" = after_1997$nkillter,
                                             "Wounded_Count" = after_1997$nwound,
                                             "Perpetrators_Wounded_Count" = after_1997$nwoundte,
                                             "Property_Damage" = after_1997$property,
                                             "Property_Damage_Extent" = after_1997$propextent,
                                             "Property_Damage_Value" = after_1997$propvalue,
                                             "Nationality_Interconnection" = after_1997$INT_IDEO
)
  


  
  field_types<-NPREPROCESSING_initialFieldType(post_feature_selection)
  # ************************************************
  # View the field types on the console

  
  numeric_fields<-names(post_feature_selection)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  
  symbolic_fields<-names(post_feature_selection)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  # Determine if the numeric fields might be discreet numeric
  
  numeric_field_types<-NPREPROCESSING_discreetNumeric(dataset=post_feature_selection,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)
  
  type_results<-data.frame(field=names(post_feature_selection),initial=field_types,types1=numeric_field_types)
  print(formattable::formattable(type_results))
  
  
 
 
  dataset_stats <- NPREPROCESSING_prettyDataset(post_feature_selection)
 
  ##Group dataset by various fileds for data exploration purposes
  #nkilled <- post_feature_selection %>% group_by(Kill_Count) %>% summarize(count=n())
  #nwounded<- post_feature_selection %>% group_by(Wounded_Count) %>% summarize(count=n())
  #nproperty<- post_feature_selection %>% group_by(Property_Damage) %>% summarize(count=n())
  #nproperty_extent<- post_feature_selection %>% group_by(Property_Damage_Extent) %>% summarize(count=n())
  #nproperty_value<- post_feature_selection %>% group_by(Property_Damage_Value) %>% summarize(count=n())
  #nattack_type<- post_feature_selection %>% group_by(Attack_Type) %>% summarize(count=n())
  #ncountries<- post_feature_selection %>% group_by(Country) %>% summarize(count=n())
  nweapon_type <- post_feature_selection %>% group_by(Weapon_Type) %>% summarize(count=n())
  #nperp_number <-post_feature_selection %>% group_by(Perpetrators_Number) %>% summarize(count=n())

  
  
  #prop_dmg_yes <- post_feature_selection %>% filter(Property_Damage == 1)
  #filtered_property_damage <- post_feature_selection %>% filter(!is.na(Property_Damage_Value) & Property_Damage_Value > 0)
  #filtered_property_damage_extent_not_na <- post_feature_selection %>% filter(Property_Damage_Extent != 4)
  #filtered_property_damage_extent_catastrophic <- post_feature_selection %>% filter(Property_Damage_Extent == 1)
  #filtered_property_damage_extent_major <- post_feature_selection %>% filter(Property_Damage_Extent == 2)
  #filtered_property_damage_extent_minor <- post_feature_selection %>% filter(Property_Damage_Extent == 3)
  
  # Filter data for moments calculations
  
 
  post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count>0) -> filtered_wounded_no_na_no_zero
  
  
  # Thresholds
  
  number_perp_threshold <- mean(filtered_perp_no_na$nperps)
  
  # Transform Dataset
  
  
  nakilled <- dataset %>% group_by(Kill_Count) %>% summarize(count=n())
  nawounded <- dataset %>% group_by(Wounded_Count) %>% summarize(count=n())
  
  killed_threshold <- computeRoundedMean(post_feature_selection, "Kill_Count")
  wounded_threshold <- computeRoundedMean(post_feature_selection, "Wounded_Count")
  perpetrator_mean <- computeRoundedMean(post_feature_selection, "Perpetrators_Number")
  
  dataframe <- computeImpactfulField(post_feature_selection, killed_threshold, wounded_threshold)
  
  correlationAndCovarianceMatrix(dataframe)
  
  

  #plot(dataframe$Perpetrators_Number, dataframe$Kill_Count, xlab="Perpetrator Count", ylab="Victim Count", main="Perpetrator/Kill Count With Outliers")
  #abline(lm( Perpetrators_Number ~ Kill_Count, data=dataframe), col="blue", lwd=3, lty=2)
  ## 
  ## plot(filtered_dataset_for_cormat$Perpetrator_Count, filtered_dataset_for_cormat$Kill_Count, xlab="Perpetrator Count", ylab="Victim Count", main="Zoomed Distribution", xlim=c(0,5000), ylim=c(0,1000))
  ## abline(lm( Perpetrator_Count ~ Kill_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  ## 
  ## plot(filtered_dataset_for_cormat$Perpetrator_Count, filtered_dataset_for_cormat$Wounded_Count, xlab="Perpetrator Count", ylab="Wounded Count", main="Wounded/Kill Count With Outliers")
  ## abline(lm( Perpetrator_Count ~ Wounded_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  ## 
  ##x = filtered_dataset_for_cormat$Perpetrator_Count
  ##OutVals = boxplot(x)
  ##outliers = which( x %in% OutVals)
  #
  #
  #test_df <- impactful_new %>% mutate(Perpetrators_Number = case_when(is.na(Perpetrators_Number) | Perpetrators_Number < 0 ~ perp_count_mean, TRUE ~ as.double(Perpetrators_Number)))
  #log_transform <- test_df %>% mutate(Perpetrators_Number = case_when(!is.na(Perpetrators_Number) ~ log(Perpetrators_Number)))
  #
  #
  #rescaled_values_perps <- ƒ_df %>% mutate(Perpetrators_Number = case_when(!is.na(Perpetrators_Number) ~ Nrescale(Perpetrators_Number)))
  #
  #
  ##Outliers from Perpetrators Number
  #outlier_values <- boxplot.stats(filtered_dataset_for_cormat$Perpetrator_Count)$out
  
  
  #Min = min(filtered_dataset_for_cormat$Perpetrator_Count)
  #Max = max(filtered_dataset_for_cormat$Perpetrator_Count)
  #Median = median(filtered_dataset_for_cormat$Perpetrator_Count)
  #IQRange = IQR(filtered_dataset_for_cormat$Perpetrator_Count)
  #Lower_quantile = IQRange - 4
  #Upper_quantile = IQRange + 4
  #Lower_Outlier_Limit = Lower_quantile - 1.5 * IQRange
  #Upper_Outlier_Limit = Upper_quantile + 1.5 * IQRange
  
  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  
##############
  ########3
  ######
  
  #Remove fileds that do not interest us from the dataframe
  refined_dataframe <- data.frame("Country_txt" = dataframe$Country_txt,
                                  "Country" = dataframe$Country,
                                  "Region" = dataframe$Region_txt,
                                  "Attack_Type" = dataframe$Attack_Type_txt,
                                  "Suicide" = dataframe$Suicide,
                                  "Weapon_Type_txt" = dataframe$Weapon_Type_txt,
                                  "Perpetrators_Number" = dataframe$Perpetrators_Number,
                                  "Impactful" = dataframe$Impactful,
                                  "Killed" = dataframe$Kill_Count)
  
  refined_dataframe <- refined_dataframe[!is.na(refined_dataframe$Country),]
  refined_dataframe <- refined_dataframe %>% mutate(Perpetrators_Number = case_when(is.na(Perpetrators_Number) | Perpetrators_Number < 0 ~ perpetrator_mean, TRUE ~ as.double(Perpetrators_Number)))
  transformNumeric(refined_dataframe)
  
  
  
  # remove fileds that do not interest us from the dataframe
  
  
  
  # find ordinals
  ordinals<-refined_dataframe[,which(field_types1==TYPE_ORDINAL)]
  
  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)
  sorted<-unique(sort(test[,2],decreasing=TRUE))
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values","Perpetrators Number"),bty="n")
  
  # replace NA from perp field
  perps_preprocessed <- ordinals %>% mutate(dataframe.Perpetrators_Number = case_when(is.na(dataframe.Perpetrators_Number) | dataframe.Perpetrators_Number < 0 ~ perpetrator_mean, TRUE ~ as.double(dataframe.Perpetrators_Number)))
  
  
  # ************************************************
  # z-scale
  #zscaled<-as.data.frame(scale(perps_preprocessed,center=TRUE, scale=TRUE))
  scaled <- as.data.frame(scale(perps_preprocessed$dataframe.Perpetrators_Number,center=TRUE, scale=TRUE))
  
  
  ReadyforML<-Nrescaleentireframe(scaled)
  
  
  
  
  
  
  
  
  
  ####
  ####
  ####
  
  dataset_refined <- dataframe %>% mutate(Perpetrators_Number = case_when(Perpetrators_Number < 0 | is.na(Perpetrators_Number) ~ perpetrator_mean, TRUE ~ as.double(Perpetrators_Number)))
  log_transform <- dataset_refined %>% mutate(Perpetrators_Number = case_when(!is.na(Perpetrators_Number) ~ log(Perpetrators_Number)))
  
  
  perps_grouped<- no_outliers %>% group_by(Perpetrators_Number) %>% summarize(count=n())
  
  
  rescaled_values <- log_transform %>% mutate(Perpetrators_Number = case_when(!is.na(Perpetrators_Number) ~ scale(Perpetrators_Number))) 
  
  
  plot(rescaled_values$Perpetrators_Number)
  
  
  impactful_grouped<- impactful_new %>% group_by(Impactful) %>% summarize(count=n())
  NPREPROCESSING_prettyDataset(no_outliers)
  
  # Check covariance and correlation after outliers have been modified
  no_outliers_filtered <- no_outliers %>% filter(!is.na(Perpetrators_Number))
  perp_count_field <- no_outliers$Perpetrators_Number
  kill_count_field <- no_outliers$Kill_Count
  wounded_count_field <- no_outliers$Wounded_Count
  
  plot(no_outliers$Perpetrators_Number, no_outliers$Kill_Count, xlab="Perpetrator Count", ylab="Victim Count", main="Perpetrator/Kill Count With Outliers")
  abline(lm( Perpetrator_Count ~ Kill_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  
  
  correlation_dataframe <- data.frame("Perpetrators_Count"= perp_count_field,
                                      "Kill_Count" = kill_count_field)
                                      
  
  correlation_matrix_new <- cor.test(perp_count_field, kill_count_field, method="spearman", exact=FALSE)
  
  model <- lm(Perpetrators_Number ~ Kill_Count + Wounded_Count, data = no_outliers_filtered)
  summary(model)
  
  
  
  Nrescale(no_outliers$Perpetrators_Number)
  

  write.csv(impactful_new,'preprocessed.csv')

  
 #Subset the data where number of killed is null to further analyse whether this data is useful for analysis
  killed_is_null <- subset(post_feature_selection, is.na(Kill_Count))
  wounded_is_null<- subset(post_feature_selection, is.na(Wounded_Count))
  wounded <- unique(killed_is_null$Wounded_Count)
  
  successful_count <- post_feature_selection %>% group_by(Successful) %>% summarize(count=n())
  unique(post_feature_selection$Country)
  

set.seed(123)

# ************************************************
#main()

print("end")