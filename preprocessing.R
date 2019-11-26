
main_dataset <- read.csv(DATASET_FILENAME)

after_1997 <- main_dataset[main_dataset$iyear >= 1997,]
post_feature_selection <- data.frame("Event_Id" = after_1997$eventid,
                                             "Year" = after_1997$iyear,
                                             "Month" = after_1997$imonth,
                                             "Day" = after_1997$iday,
                                             "Country" = after_1997$country,
                                             "Region" = after_1997$region,
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
                                             "Successful" = after_1997$success,
                                             "Suicide" = after_1997$suicide,
                                             "Weapon_Type" = after_1997$weaptype1,
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

library(dplyr)

# Group dataset by number of victims (how many attacks per number of killed)
nkilled <- post_feature_selection %>% group_by(Kill_Count) %>% summarize(count=n())

# Subset the data where number of killed is null to further analyse whether this data is useful for analysis
killed_is_null <- subset(post_feature_selection, is.na(Kill_Count))

successful_count <- post_feature_selection %>% group_by(Successful) %>% summarize(count=n())


#unique(post_feature_selection$Country)