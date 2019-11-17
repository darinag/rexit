
rm(list=ls())

Sys.setenv(TZ="Europe/London")

DATASET_FILENAME = "globalterrorismdb_0718dist.csv"
SPLIT_TRAIN       <-70

MYLIBRARIES<-c("quantmod",
               "timeSeries",
               "formattable",
               "rnn",
               "h2o")

main<-function(){
  
  data <- read.csv(DATASET_FILENAME)
  
  no_doubt <- data[data$doubtterr==0,]
  
  
  after_1997 <- data[no_doubt$iyear >= 1997,]
  str(after_1997)
  standardDev <- sd(after_1997$imonth)
  print(standardDev)
  plot(after_1997$imonth)
  
  refined <- data.frame("Event_Id" = after_1997$eventid,
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
}

gc()

if(!is.null(dev.list())) dev.off()
graphics.off()

assign("last.warning", NULL, envir = baseenv())

cat("\014")

#Load additional R script
# source("5labfunctions.R")

set.seed(123)

main()

print("end")