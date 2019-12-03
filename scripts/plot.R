# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRATICAL BUSINESS ANALYTICS 2019
#  COM3018 
#
# Group: REXIT
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 03 DECEMBER 2019
#
# ************************************************
# R Script For Coursework Assignment


# ************************************************
# plotAttacksOverTime() :
# plot terrorist attacks over time
#
# INPUT: terrorism dataset
# OUTPUT: None
# ************************************************
plotAttacksOverTime <- function(data) {
  # Number of terror attacks (1997-2017)
  data$iyear <- factor(data$iyear) # Convert to factor to plot on chart
  
  # Use as.data.frame() function to obtain the frequency count per year
  attacks_by_year <- as.data.frame(table(data$iyear))
  names(attacks_by_year) <- c("Year", "Total")
  
  plot_attacks_by_year <- hchart(data$iyear, 
                                 name = "Attacks Count", 
                                 color = "lightblue") %>%
    hc_title(text = "Terror incidents by year (1997-2017)") %>%
    hc_yAxis(title = list(text = "Frequency")) %>%
    hc_add_theme(hc_theme_flatdark())
  
  print(plot_attacks_by_year)
}

# ************************************************
# plotAttackTypesBarChart() :
# plot the count of each attack type in a bar chart
#
# INPUT: terrorism dataset
# OUTPUT: None
# ************************************************
plotAttackTypesBarChart <- function(data) {
  # Plot the distribution of attack types as a bar chart
  plot_attack_type_bar <-data %>%
    filter(!is.na(attacktype1_txt)) %>%
    count(attacktype1_txt)%>%
    arrange(n)%>%
    hchart(type = "bar",color = "lightblue", hcaes(x = attacktype1_txt, y = n)) %>%
    hc_title(text = "Distribution of attack types") %>%
    hc_xAxis(title = list(text = "Type of Attack")) %>%
    hc_yAxis(title = list(text = "Count")) %>%
    hc_add_theme(hc_theme_flatdark())
  
  print(plot_attack_type_bar)
}

# ************************************************
# plotAttackTypesTreemap() :
# plot the count of each attack type in a treemap
#
# INPUT: terrorism dataset
# OUTPUT: None
# ************************************************
plotAttackTypesTreemap <- function(data){
  
  plot_attack_type <- data %>% 
    filter(!is.na(attacktype1_txt) & attacktype1_txt != "Unknown") %>%
    count(attacktype1_txt) %>%
    arrange(n) %>%
    hchart(type = 'treemap', hcaes(x = attacktype1_txt, value = n, color = n)) %>%
    hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
    hc_title(text = "Distribution of different attack types")
  
  print(plot_attack_type)
}

# ************************************************
# plotDeadliestTerrOrgsBarChart() :
# plot the number of killed people per terrorist organization
# in a bar chart
#
# INPUT: terrorism dataset
# OUTPUT: None
# ************************************************
plotDeadliestTerrOrgsBarChart <- function(data) {
  
  df <- data[!is.na(data$nkill),]
  df <- aggregate(df$nkill, by=list(Groups=df$gname), FUN=sum)
  df <- df[order(df$x),]
  df <- tail(df, 20)
  
  
  df <- df %>% 
    rename(
      Killed = x 
    )
  
  plot_deadliest_bar_chart <- df%>%
    filter(Groups != "Unknown") %>%
    hchart(type = "bar", hcaes(x = Groups, y = Killed)) %>%
    hc_title(text = "Deadliest Terrorist Organizations since 1997") %>%
    hc_xAxis(title = list(text = "Terrorist Groups")) %>%
    hc_yAxis(title = list(text = "Lives Taken"))
  
  print(plot_deadliest_bar_chart)
}

# ************************************************
# plotMostActiveTerrOrgsTreemap() :
# plot the number of attacks for each terrorist organizatoin
# in a treemap
#
# INPUT: terrorism dataset
# OUTPUT: None
# ************************************************
plotMostActiveTerrOrgsTreemap <- function(data) {
  plot_groups <- data %>% 
    filter(!is.na(gname)) %>%
    filter(gname != "Unknown") %>%
    count(gname) %>%
    arrange(n) %>%
    tail(20) %>%
    hchart(type = 'treemap', hcaes(x = gname, value = n, color = n)) %>%
    hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
    hc_title(text = "Most active terrorist groups")
  
  print(plot_groups)
}

# ************************************************
# plotFatalitiesByAttackType() :
# plot the total number of confirmed fatalities for each attack type
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotFatalitiesByAttackType <- function(data) {
  plot_deadliest_attacks <- data %>%
    filter(!is.na(attacktype1_txt) & attacktype1_txt != "Unknown") %>%
    mutate(Attack_Type = reorder(attacktype1_txt, nkill, sum, na.rm = T), Fatalties = nkill) %>%
    group_by(Attack_Type) %>%
    summarise(Fatalties = sum(Fatalties, na.rm = T)) %>%
    ggplot(aes(Attack_Type, Fatalties)) +
    geom_bar(stat = "identity", fill = "firebrick2")+
    coord_flip()+
    labs(title = "Fatalities per attack type")
  
  print(plot_deadliest_attacks)
}

# ************************************************
# plotKilledByWeaponType() :
# plot the total number of confirmed fatalities for each attack type
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotMostLethalWeapons <- function(data) {
  plot_weapon_lethality <- data %>%
    filter(weaptype1_txt != "Unknown") %>%
    select(iyear, Weapon_Type = weaptype1_txt, nkill)%>%
    group_by(iyear, Weapon_Type)%>%
    summarise(casulties = n())%>%
    top_n(n=5, wt=casulties) %>%
    mutate(percent_deaths = (casulties/sum(casulties)*100))
  
  # Visualise by year / weapon type
  plot_weapon_lethality <- ggplot(data=plot_weapon_lethality, aes(x=iyear, y=casulties, col= Weapon_Type, group= Weapon_Type)) +
    geom_line(size=1.5, alpha=0.5) + 
    ggtitle('Terrorism lethality by weapon type') +
    labs(x = "Year", y = "Casualties") +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  print(plot_weapon_lethality)
}


# ************************************************
# plotCasualtiesByRegion() :
# plot the total number of confirmed fatalities by region throughout the years 
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotCasualtiesByRegion <- function(data) {
  data %>% filter(nkill > 0) -> filtered
  filtered %>% 
    group_by(iyear,region_txt) %>% 
    summarise(nkills = sum(nkill)) %>% 
    ungroup() -> dfyr
  
  colnames(dfyr)<-c("Year","Region","Victims")
  plot_casualties_region <- ggplot(data = dfyr, aes(x = Year, y = Victims, colour = Region)) +
    ggtitle('Number of casualties in varios regions') +
    geom_line() + 
    geom_point() + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  print(plot_casualties_region)
}


# ************************************************
# plotWoundedByRegion() :
# plot the total number of wounded people in various regions
# throughout the years 
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotWoundedByRegion <- function(data) {
  data %>% filter(nwound > 0) -> filtered
   filtered %>% 
     group_by(iyear,region_txt) %>% 
     summarise(nwound = sum(nwound)) %>% 
     ungroup() -> df
  
   colnames(df)<-c("Year","Region","Wounded")
   plot_wounded_region <- ggplot(data = df, aes(x = Year, y = Wounded, colour = Region)) +
     ggtitle('Number of people wounded in varios regions') +
     geom_line() + 
     geom_point() + 
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5, size = 14))
   
   print(plot_wounded_region)

}


# ************************************************
# plotWoundedByAttackType() :
# plot the number of confirmed non-fatal injuries for each attack type
# throughout the years 
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotWoundedByAttackType <- function(data) {
  
  plot_attacks_wounded <- data %>%
    filter(!is.na(attacktype1_txt) & attacktype1_txt != "Unknown") %>%
    mutate(Type = reorder(attacktype1_txt, nwound, sum, na.rm = T), Year = iyear, Wounded = nwound) %>%
    group_by(Year, Type) %>%
    summarise(Wounded = sum(Wounded, na.rm = T)) %>%
    ggplot(aes(Year, Type)) +
    geom_tile(aes(fill = Wounded))+
    scale_fill_viridis_c()+
    labs(title = "Wounded frequency by attack type", x = "Year", y = "Attack Method")+
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_attacks_wounded <- ggplotly(plot_attacks_wounded)
  
  print(plot_attacks_wounded)
}


# ************************************************
# plotAttackTypesByYear() :
# plot the number of terrorist incidents by attack type throughout the years 
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotAttackTypesByYear <- function(data) {
  
  data %>%
  filter(!is.na(attacktype1_txt) & attacktype1_txt != "Unknown") %>% 
  group_by(iyear, attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> df
  
  colnames(df)<-c("Year","Attack Type","Number of attacks")
  plot_attacks_by_year <- ggplot(data = df, aes(x = Year, y = `Number of attacks`, colour = `Attack Type`)) +
    geom_line() + geom_point() + theme_bw() + 
    labs(title = "Frequency of attack types since 1997") +
    theme(plot.title = element_text(hjust = 0.5, size = 15))
  
  print(plot_attacks_by_year)
}


# ************************************************
# plotWoundedKilledExtent() :
# plot the 
#
# INPUT:  terrorism dataset
# OUTPUT: None
# ************************************************
plotExtentPropertyDamage <- function(data){
  
  plot_extent_property_damage <- data %>%
    filter(!is.na(propextent_txt) & propextent_txt != "") %>%
    group_by(iyear, propextent_txt) %>%
    summarise(
      number_of_events = length(iyear)
    ) %>%
    ggplot(aes(iyear, number_of_events, fill = propextent_txt)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    guides(fill = guide_legend(title = "Extent of property damage")) +
    labs(title = "Extent of property damage over the years", x = "Year", y = "Number of incidents") +
    theme(plot.title = element_text(hjust = 0.5, size = 14))

  print(plot_extent_property_damage)
  
}

# ************************************************
# plotMissingValues() :
# Plot percentage of missing values for each variable 
# in the dataset above certain percentage threshold
#
# INPUT:  terrorism dataset, percentage threshold
# OUTPUT: None
# ************************************************
plotMissingValues <- function(data, THRESHOLD_MISSING_VALUES) {
  
  options(repr.plot.width=6, repr.plot.height=8)
  missing_data <- data %>% summarise_all(funs(sum(is.na(.))/n()))
  missing_data <- gather(missing_data, key = "fields", value = "percent_missing") 
  
  # Filter by percentage threshold
  missing_data <- missing_data %>% filter(percent_missing > THRESHOLD_MISSING_VALUES)
  
  plot_missing <- ggplot(missing_data, aes(x = reorder(fields, desc(percent_missing)), y = percent_missing)) +
    geom_bar(stat = "identity", fill = "lightblue", aes(color = I('white')), size = 0.1) + 
    coord_flip() +
    ggtitle(paste("Missing values in filtered dataset (Above:",THRESHOLD_MISSING_VALUES*100,"%)")) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "Field Name", y = "% of missing values")
  
  print(plot_missing)
}

