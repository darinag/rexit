
# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

# ************************************************
#   Global Environment variables

DATASET_FILENAME = "C:/Users/cc01030/OneDrive - University of Surrey/Documents/GitHub/rexit/globalterrorismdb_0718dist.csv"


# Load the libraries used in the project
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(highcharter)





# User defined functions 
# ************************************************



# ************************************************
# main() :
# main entry point to our program
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function(){
  
  # Load the dataset
  global_terrorism <- read.csv(DATASET_FILENAME)
  
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  after_1997 <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  
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
  
  #print(str(refined))
  
  
  # ************************************************
  # Plot missing values count for each variable of the 'refined' dataset
  
  #options(repr.plot.width=6, repr.plot.height=8)
  #missing_data <- refined %>% summarise_all(funs(sum(is.na(.))/n()))
  #missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
  
  #plot_missing <- ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  #geom_bar(stat = "identity", fill = "lightblue", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()+ 
  #ggtitle("Missing values in refined dataset") + labs(x = "Variables", y = "% of missing values")
  
  #print(plot_missing)
  
  # ************************************************
  
  # ************************************************
  # Data Cleaning
  
  #data_cleaned <- global_terrorism[, -which(colMeans(is.na(global_terrorism)) > 0.50)]
  #print(dim(data_cleaned))
  
  # ************************************************  
  

  
  # ************************************************
  # Basic Data Visualisation
  
  # Number of terror attacks (1997-2017)
  
  after_1997$iyear <- factor(after_1997$iyear) # Convert to factor to plot on chart
  #print(str(after_1997$iyear))
  
  # Use as.data.frame() function to obtain the frequency count per year
  attacks_by_year <- as.data.frame(table(after_1997$iyear))
  names(attacks_by_year) <- c("Year", "Total")
  
  plot_attacks_by_year <- hchart(after_1997$iyear, name = "Attacks Count", color = "lightblue") %>%
  hc_title(text = "Terror incidents by year (1997-2017)") %>%
  hc_add_theme(hc_theme_flatdark())
  
  print(plot_attacks_by_year)
  
  # Plot the distribution of attack types
  #plot_attack_type <- ggplot(data = after_1997, aes(x = after_1997$attacktype1_txt)) + 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #theme_gray() +
  #geom_histogram(color = "white", fill = "lightblue", stat = "count") +
  #labs(title='Terrorism attack type distribution')
  
  #print(plot_attack_type)
  
  print("end of main")
}

# ************************************************

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")


set.seed(123)

# ************************************************
main()

print("end")

