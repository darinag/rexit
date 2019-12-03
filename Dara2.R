# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

# ************************************************
#   Global Environment variables

DATASET_FILENAME = "globalterrorismdb_0616dist.csv"

# Load the libraries used in the project
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(highcharter)
library(itertools)

# User defined functions 
# ************************************************
# ************************************************
# main() :
# main entry point to our program
#
# INPUT: None
# OUTPUT :None
# ************************************************

  
  # Load the dataset
  
global_terrorism <- read.csv(DATASET_FILENAME)
  
# Includes only incidents after 1997, where all incidents represent an act of terrorism 
after_1997 <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  

refined <- data.frame(#"Event_Id" = after_1997$eventid,
                        "Year" = after_1997$iyear,
                        #"Month" = after_1997$imonth,
                        #"Day" = after_1997$iday,
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
                        #"Property_Damage_Value" = after_1997$propvalue,
                        "Nationality_Interconnection" = after_1997$INT_IDEO
  )
  

refined <- data.frame(
  "Country" = after_1997$country,
  "Criteria1" = after_1997$crit1,
  "Criteria3" = after_1997$crit3,
  "Attack_Type" = after_1997$attacktype1,
  "Successful" = after_1997$success,
  "Suicide" = after_1997$suicide,
  "Weapon_Type" = after_1997$weaptype1,
  "Target_Type" = after_1997$targtype1,
  "Perpetrators_Number" = after_1997$nperps
)


  
  require(kohonen)
  require(RColorBrewer)
  library(RCurl)
  df <- after_1997
  colnames(after_1997)
  
  df.measure1 <- c("nkill", "country", "weaptype1")
  #df.SOM1 <- som(scale(df[df.measure1]), grid = somgrid(6,4, "rectangular"))
  df.SOM1 <- som(scale(df[df.measure1]), grid = somgrid(20,20, "hexagonal"))
  #plot(df.SOM1, type="property")
  
  #plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4], palette.name=coolBlueHotRed)
    
  
  colours <- function(n, alpha = 1) {
    rev(heat.colors(n, alpha))
  }

  
  #plot(df.SOM1, type = "counts", palette.name = colours, heatkey = TRUE)
 
  # plot(df.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
  #plot(df.SOM1, main = "Default SOM Plot")
  

  
df <- refined 

install.packages(missForest) 
library(missForest)

# Generate 5% missing values at random
bank.mis <- prodNA(df, noNA = 0.05) 

library(Amelia)
#missmap(bank.mis)
#imputed_fields <- c('Country', 'Criteria1', 'Criteria3', 'Attack_Type', 'Suicide', 'Weapon_Type', 'Target_Type', 'Perpetrators_Number')
#amelia_bank <- amelia(bank.mis, m=3, parallel = "multicore",noms=imputed_fields)


# ************************************************

gc() # garbage collection to automatically release memory
# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())
# clears the console area
cat("\014")


set.seed(123)

x <- data.frame("SN" = 1:5, "Age" = c(21,15,14, 12,10), "Name" = c("John","Dora","Meli", "PESHI", "LQLQ"))
ageidx <- grep("Age", colnames(x))

for (i in 1:nrow(x)){
  for (j in 1:ncol(x)){
    print(j)
    if (j == ageidx){
      
      if (x[i,j] < 15 ){
        x$Age[i] = 1000
      }
    }
  }
}

grep("Age", colnames(x))

# ************************************************

print("end")