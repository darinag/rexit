library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

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
               "ggplot",
               "tidyverse")

OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers

TYPE_DISCREET     <- "DISCREET"           # d is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

DISCREET_BINS     <- 5 

NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}



library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


  
  main_dataset <- read.csv(DATASET_FILENAME)

  after_1997 <- main_dataset[main_dataset$iyear >= 1997 & main_dataset$doubtterr == 0,]
  post_feature_selection <- data.frame("Event_Id" = after_1997$eventid,
                                             "Year" = after_1997$iyear,
                                             "Month" = after_1997$imonth,
                                             "Day" = after_1997$iday,
                                             "Country" = after_1997$country,
                                             #"Country_txt" = after_1997$country_txt,
                                             "Region" = after_1997$region,
                                            #"Region_txt" = after_1997$region_txt,
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
                                             #"Attack_Type_txt" = after_1997$attacktype1_txt,
                                             "Successful" = after_1997$success,
                                             "Suicide" = after_1997$suicide,
                                             "Weapon_Type" = after_1997$weaptype1,
                                             #"Weapon_Type_txt" = after_1997$weapon_type_txt,
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
  
  post_feature_selection$Impactful=0

  
  field_types<-NPREPROCESSING_initialFieldType(dataset)
  
  # ************************************************
  # View the field types on the console
  
  numeric_fields<-names(dataset)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  
  symbolic_fields<-names(dataset)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  # Determine if the numeric fields might be discreet numeric
  
  numeric_field_types<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                               field_types=field_types,
                                               cutoff=DISCREET_BINS)
  
  type_results<-data.frame(field=names(dataset),initial=field_types,types1=numeric_field_types)
  print(formattable::formattable(type_results))
  
  
 
 
  #dataset_stats <- NPREPROCESSING_prettyDataset(post_feature_selection)
 
  #Group dataset by various fileds for data exploration purposes
  nkilled <- post_feature_selection %>% group_by(Kill_Count) %>% summarize(count=n())
  nwounded<- post_feature_selection %>% group_by(Wounded_Count) %>% summarize(count=n())
  nproperty<- post_feature_selection %>% group_by(Property_Damage) %>% summarize(count=n())
  nproperty_extent<- post_feature_selection %>% group_by(Property_Damage_Extent) %>% summarize(count=n())
  nproperty_value<- post_feature_selection %>% group_by(Property_Damage_Value) %>% summarize(count=n())
  attack_type_explore<- post_feature_selection %>% group_by(Attack_Type) %>% summarize(count=n())
  countries<- post_feature_selection %>% group_by(Country) %>% summarize(count=n())
  attack_type <- post_feature_selection %>% group_by(Attack_Type) %>% summarize(count=n())
  weapon_type <- post_feature_selection %>% group_by(Weapon_Type) %>% summarize(count=n())
  perp_number <-post_feature_selection %>% group_by(Perpetrators_Number) %>% summarize(count=n())

  
  
  
  post_feature_selection %>% filter(Property_Damage == 1) -> prop_dmg_yes
  post_feature_selection %>% filter(!is.na(Property_Damage_Value) & Property_Damage_Value > 0) -> filtered_property_damage
  post_feature_selection %>% filter(Property_Damage_Extent != 4) -> filtered_property_damage_extend_not_na
  post_feature_selection %>% filter(Property_Damage_Extent == 1) -> filtered_property_damage_extend_catastrophic
  post_feature_selection %>% filter(Property_Damage_Extent == 2) -> filtered_property_damage_extend_major
  post_feature_selection %>% filter(Property_Damage_Extent == 3) -> filtered_property_damage_extend_minor
  
  # Filter data for moments calculations
  filtered_killed <- post_feature_selection %>% filter(!is.na(Kill_Count) & Kill_Count>=0 ) 
  filtered_wounded_no_na <- post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count >= 0)
  filtered_perp_no_na <- after_1997 %>% filter(!is.na(nperps) & nperps>=0)
  post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count>0) -> filtered_wounded_no_na_no_zero
  
  
  # Thresholds
  killed_threshold <- round(mean(filtered_killed$Kill_Count)) 
  wounded_threshold <- round(mean(filtered_wounded_no_na_no_zero$Wounded_Count))
  number_perp_threshold <- mean(filtered_perp_no_na$nperps)
  
  # Transform Dataset
  dataset <- replace.value(post_feature_selection, c("Kill_Count"), from=NA, to=as.double(killed_threshold))
  dataset <- replace.value(dataset, c("Wounded_Count"), from=NA, to=as.double(wounded_threshold))
  
  
  
  # Keep only top 10 Countries
  # Subset data to only include countries with top 10 attack occurrences
  dataset %>% 
    arrange(desc(Country)) %>%
    group_by(Country) %>% slice()
  
  
  nakilled <- dataset %>% group_by(Kill_Count) %>% summarize(count=n())
  nawounded <- dataset %>% group_by(Wounded_Count) %>% summarize(count=n())
  

  dataset$Impactful=0
  
  
  idx_killed <- which(colnames(post_feature_selection)=="Kill_Count")
  idx_wounded <- which(colnames(post_feature_selection)=="Wounded_Count")
  idx_prop_ext <- which(colnames(post_feature_selection)=="Property_Damage_Extent")
  idx_impact <- which(colnames(post_feature_selection)=="Impactful")

  
  
  # Dataset for Correlation Matrix
  dataset_for_cormat <- data.frame("Kill_Count" = dataset$Kill_Count,
                                          "Wounded_Count" = dataset$Wounded_Count,
                                          "Perpetrator_Count" = dataset$Perpetrators_Number)
  
  filter_no99 <- replace.value(dataset_for_cormat, c("Perpetrator_Count"), from=-99, to=as.double(NA))
  filter_no99 <- replace.value(filter_no99, c("Perpetrator_Count"), from=-9, to=as.double(NA))
  filtered_dataset_for_cormat <- filter_no99 %>% filter(!is.na(Perpetrator_Count))
  testing_dataset_for_regression <- dataset_for_cormat %>% filter(is.na(Perpetrator_Count))
  
  library(corrplot)
  library(rquery)
  library(pgirmess)
  #Correlation matrix
  correlation_matrix <- cormat(filtered_dataset_for_cormat)
  covariance_matrix <- cov(filtered_dataset_for_cormat)
  
  
  
  
  
  plot(filtered_dataset_for_cormat$Perpetrator_Count, filtered_dataset_for_cormat$Kill_Count, xlab="Perpetrator Count", ylab="Victim Count", main="Perpetrator/Kill Count With Outliers")
  abline(lm( Perpetrator_Count ~ Kill_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  
  plot(filtered_dataset_for_cormat$Perpetrator_Count, filtered_dataset_for_cormat$Kill_Count, xlab="Perpetrator Count", ylab="Victim Count", main="Zoomed Distribution", xlim=c(0,5000), ylim=c(0,1000))
  abline(lm( Perpetrator_Count ~ Kill_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  
  plot(filtered_dataset_for_cormat$Perpetrator_Count, filtered_dataset_for_cormat$Wounded_Count, xlab="Perpetrator Count", ylab="Wounded Count", main="Wounded/Kill Count With Outliers")
  abline(lm( Perpetrator_Count ~ Wounded_Count, data=filtered_dataset_for_cormat), col="blue", lwd=3, lty=2)
  
  #x = filtered_dataset_for_cormat$Perpetrator_Count
  #OutVals = boxplot(x)
  #outliers = which( x %in% OutVals)
  
  perp_count_mean = round(mean(filtered_dataset_for_cormat$Perpetrator_Count))
  
  outlier_values <- boxplot.stats(filtered_dataset_for_cormat$Perpetrator_Count)$out
  
  IQRange = IQR(filtered_dataset_for_cormat$Perpetrator_Count)
  boxplot()
  
  Min = min(filtered_dataset_for_cormat$Perpetrator_Count)
  Max = max(filtered_dataset_for_cormat$Perpetrator_Count)
  Median = median(filtered_dataset_for_cormat$Perpetrator_Count)
  IQRange = IQR(filtered_dataset_for_cormat$Perpetrator_Count)
  Lower_quantile = IQRange - 4
  Upper_quantile = IQRange + 4
  Lower_Outlier_Limit = Lower_quantile - 1.5 * IQRange
  Upper_Outlier_Limit = Upper_quantile + 1.5 * IQRange
  
  
  no_outliers <- impactful_new %>% mutate(Perpetrators_Number = case_when(Perpetrators_Number > 14 | Perpetrators_Number < -6 | is.na(Perpetrators_Number) ~ perp_count_mean, TRUE ~ as.double(Perpetrators_Number)))
  perps_grouped<- no_outliers %>% group_by(Perpetrators_Number) %>% summarize(count=n())
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
  

 
  
  

  

  
  impactful_new <- dataset %>%mutate(Impactful = case_when(Kill_Count > killed_threshold | Wounded_Count > wounded_threshold | Property_Damage_Extent == 1 | Property_Damage_Extent == 2 ~ 1, TRUE  ~ 0))
  write.csv(impactful_new,'preprocessed.csv')

  
 #Subset the data where number of killed is null to further analyse whether this data is useful for analysis
  killed_is_null <- subset(post_feature_selection, is.na(Kill_Count))
  wounded_is_null<- subset(post_feature_selection, is.na(Wounded_Count))
  wounded <- unique(killed_is_null$Wounded_Count)
  
  successful_count <- post_feature_selection %>% group_by(Successful) %>% summarize(count=n())
  unique(post_feature_selection$Country)
  
 
  
  



#Load additional R script files provide for this lab
source("lab3dataPrep.R")
source("lab2functions.R")

set.seed(123)

# ************************************************
#main()

print("end")