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
               "dplyr")

OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

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



  

  
  main_dataset <- read.csv(DATASET_FILENAME)

  after_1997 <- main_dataset[main_dataset$iyear >= 1997,]
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

  
 
 
  #dataset_stats <- NPREPROCESSING_prettyDataset(post_feature_selection)
# Group dataset by number of victims (how many attacks per number of killed)
  nkilled <- post_feature_selection %>% group_by(Kill_Count) %>% summarize(count=n())
  nwounded<- post_feature_selection %>% group_by(Wounded_Count) %>% summarize(count=n())
  nproperty<- post_feature_selection %>% group_by(Property_Damage) %>% summarize(count=n())
  nproperty_extent<- post_feature_selection %>% group_by(Property_Damage_Extent) %>% summarize(count=n())
  nproperty_value<- post_feature_selection %>% group_by(Property_Damage_Value) %>% summarize(count=n())
  
  nimpactful <- post_feature_selection %>% group_by(Impactful) %>% summarize(count=n())
  
  
  
  post_feature_selection %>% filter(Property_Damage == 1) -> prop_dmg_yes
  post_feature_selection %>% filter(!is.na(Property_Damage_Value) & Property_Damage_Value > 0) -> filtered_property_damage
  post_feature_selection %>% filter(Property_Damage_Extent != 4) -> filtered_property_damage_extend_not_na
  post_feature_selection %>% filter(Property_Damage_Extent == 1) -> filtered_property_damage_extend_catastrophic
  post_feature_selection %>% filter(Property_Damage_Extent == 2) -> filtered_property_damage_extend_major
  post_feature_selection %>% filter(Property_Damage_Extent == 3) -> filtered_property_damage_extend_manor
  
  post_feature_selection %>% filter(!is.na(Kill_Count)) -> filtered_killed_no_na
  post_feature_selection %>% filter(!is.na(Kill_Count) & Kill_Count>0 ) -> filtered_killed_no_na_no_zero
  post_feature_selection %>% filter(!is.na(Wounded_Count)) -> filtered_wounded_no_na
  post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count>0) -> filtered_wounded_no_na_no_zero
  
  
  
  
  mean(filtered_killed_no_na_no_zero$Kill_Count) -> killed_threshold
  mean(filtered_wounded_no_na_no_zero$Wounded_Count) -> wounded_threshold
  

  post_feature_selection$Impactful=0
 # 
 # for (i in 1:nrow(post_feature_selection)) {
 #   if ((post_feature_selection$Killed_Count[i,] > killed_threshold) || (post_feature_selection$Wounded_Count > wounded_threshold) || (post_feature_selection$Property_Damage_Extent==1) || (post_feature_selection$Property_Damage_Extent==2)) {
 #     post_feature_selection$Impactfu[i]=1
 #   }
 # }
  
#for (i in 1:nrow(post_feature_selection)) {
#  if (!is.na(post_feature_selection$Killed_Count[i])) {
#    post_feature_selection$Impactfu[i]=1
#  }
#}
 
#if (post_feature_selection$Kill_Count > killed_threshold) {
#  post_feature_selection$impactful
#}
  
  idx_killed <- which(colnames(post_feature_selection)=="Kill_Count")
  idx_wounded <- which(colnames(post_feature_selection)=="Wounded_Count")
  idx_prop_ext <- which(colnames(post_feature_selection)=="Property_Damage_Extent")
  idx_impact <- which(colnames(post_feature_selection)=="Impactful")
  
  for (i in 1:nrow(post_feature_selection)){
    for (j in 1:ncol(post_feature_selection)){
      if (j==idx_killed){
        if (!is.na(post_feature_selection[i,j]) & post_feature_selection[i,j] > killed_threshold){
          post_feature_selection[i, idx_impact]=1
        }
      }
      else if (j==idx_wounded){
        if (!is.na(post_feature_selection[i,j]) & post_feature_selection[i,j] > wounded_threshold){
          post_feature_selection[i, idx_impact]=1
        }
      }
      else if (j==idx_prop_ext){
        if (!is.na(post_feature_selection[i,j]) & (post_feature_selection[i,j] == 1 | post_feature_selection[i,j] == 2)){
          post_feature_selection[i, idx_impact]=1
        }
      }
    }
  }
  
  post_feature_selection_new <- na.omit(post_feature_selection, cols="Kill_Count")
  post_feature_selection_new %>% mutate(Impactful = case_when(Kill_Count > killed_threshold) ~ 1)
  
  #bfiltered_property_damage
# Subset the data where number of killed is null to further analyse whether this data is useful for analysis
  killed_is_null <- subset(post_feature_selection, is.na(Kill_Count))
  wounded_is_null<- 
  wounded <- unique(killed_is_null$Wounded_Count)
  
  successful_count <- post_feature_selection %>% group_by(Successful) %>% summarize(count=n())
  unique(post_feature_selection$Country)
  
  


# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)



#Load additional R script files provide for this lab
source("lab3dataPrep.R")

set.seed(123)

# ************************************************
#main()

print("end")