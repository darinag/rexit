# ************************************************
# plotAttacksOverTime() :
# plot terrorist attacks over time
#
# INPUT: terrorism dataset
# OUTPUT :None
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
    hc_add_theme(hc_theme_flatdark())
  
  print(plot_attacks_by_year)
}

# ************************************************
# plotAttackTypesBarChart() :
# plot the count of each attack type in a bar chart
#
# INPUT: terrorism dataset
# OUTPUT :None
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
# OUTPUT :None
# ************************************************
plotAttackTypesTreemap <- function(data){
  
  plot_attack_type <- data %>% 
    filter(!is.na(attacktype1_txt)) %>%
    count(attacktype1_txt) %>%
    arrange(n) %>%
    hchart(type = 'treemap', hcaes(x = attacktype1_txt, value = n, color = n)) %>%
    hc_colorAxis(stops = color_stops(colors = viridis(10))) %>%
    hc_title(text = "Attack Types")
  
  print(plot_attack_type)
}

# ************************************************
# plotDeadliestTerrOrgsBarChart() :
# plot the number of killed people per terrorist organization
# in a bar chart
#
# INPUT: terrorism dataset
# OUTPUT :None
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
# OUTPUT :None
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

