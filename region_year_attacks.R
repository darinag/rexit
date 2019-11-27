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
  
  
  tmp <- after_1997[1:100,]
  
  library("reshape2")
  library("ggplot2")
  library("tidyverse")
  
  #NUMBER OF KILLED PER YEAR PER REGION
  after_1997 %>% filter(nkill > 0) -> after
  after %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkill)) %>% ungroup() -> dfyr
  colnames(dfyr)<-c("Year","Region","Killed")
  ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
    geom_line() + geom_point() + theme_bw()
  
  #TYPE OF ATTACK PER YEAR PER NUMBER OF EVENTS
  after_1997 %>% group_by(iyear,attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> dfya
  colnames(dfya)<-c("Year","Type of attack","Number of events")
  ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
    geom_line() + geom_point() + theme_bw()
  
  dfw_us %>% group_by(iyear,region_txt) %>% summarise(nwounds = sum(nwoundus)) %>% ungroup() -> dfyr
  colnames(dfyr)<-c("Year","Region","Wounded")
  ggplot(data = dfyr, aes(x = Year, y = Wounded, colour = Region)) +       
    geom_line() + geom_point() + theme_bw()
  
  
  ########HIGHCHART Attempt
  #year_area_number of attacks
 # data(after_1997)
   # hc1 <- highchart() %>%
   # hc_xAxis(categories = after_1997$iyear) %>%
 #   hc_add_series(name = "North America", data = filter(after_1997$region==1)) %>%
 #   hc_add_series(name = "East Asia", data = filter(after_1997$region==4))
 # hc1
  
  # data(citytemp)
  # hc1 <- highchart() %>%
  #   hc_xAxis(categories = citytemp$month) %>%
  #   hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>%
  #   hc_add_series(name = "London", data = citytemp$london)
  # hc1
  # 
  # citytemp2 <- citytemp %>%
  #   tidyr::gather(key = city, value = temperature, tokyo, london)
  # 
  # hchart(citytemp2, type = 'line', hcaes(y = temperature, group = city, x = month))
  # 
  # 
  # tmp <- after_1997[1:100,]
  # 
  # citytemp3 <- tmp %>%
  #   tidyr::gather(key = region_txt, value = attacktype1_txt, region_txt)
  # 
  # hchart(citytemp2, type = 'line', hcaes(y = temperature, group = city, x = month))
  # 
  # 
  # citytemp2 <- tmp %>% 
  #   dplyr::filter(region_txt %in% c('East Asia', 'North America')) # filter to just east asia and north america
  # 
  # hc2 <- hchart(citytemp2, type = 'line', hcaes(y = attacktype1_txt, group = region_txt, x = weaptype1))
  # hc2
  
  
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