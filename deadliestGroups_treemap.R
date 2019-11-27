
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
library(treemap)
library(viridis)

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

plot_attack_type <- after_1997 %>% 
  filter(!is.na(attacktype1_txt)) %>%
  count(attacktype1_txt) %>%
  arrange(n) %>%
  hchart(type = 'treemap', hcaes(x = attacktype1_txt, value = n, color = n)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
  hc_title(text = "Attacks Types")

#print(plot_attack_type)

df <- aggregate(refined$Kill_Count, by=list(Groups=refined$Group_Name), FUN=sum)
df <- df[order(df$x),]
df <- df[!is.na(df$x),]
df <- tail(df, 20)
tm <- treemap(df, index = c("Groups"),
              vSize = "x",
              type = "comp",
              draw = FALSE)

deadliest_plot <- df %>% 
  filter(!is.na(x)) %>%
  hchart(type = 'treemap', hcaes(x = Groups, value = x, color = x)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
  hc_title(text = "Deadliest Terrorist Groups")
print(deadliest_plot)
# highchart(height = 800) %>% 
#   hc_add_series_treemap(tm, allowDrillToNode = TRUE,
#                         layoutAlgorithm = "squarified",
#                         name = "tmdata") %>% 
#   hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
#   hc_title(text = "Deadliest Terrorist Groups") %>% 
#   hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
#                              Pop: {point.value:,.0f}<br>")


print(plot_deadliest_orgs)

# 
