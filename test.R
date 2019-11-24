
rm(list=ls())

Sys.setenv(TZ="Europe/London")

DATASET_FILENAME = "globalterrorismdb_0616dist.csv"

MYLIBRARIES<-c("quantmod",
               "timeSeries",
               "formattable",
               "rnn",
               "h2o",
               "dplyr",
               "stats",
               "highcharter")

main<-function(){
  
  data <- read.csv(DATASET_FILENAME)
  #df <- data[data$iyear >= 1997,]
  library(dplyr)
  library(stats)
  library(highcharter)
  
  datatets <- filter(data, iyear>1997)
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  after_1997 <- data[data$iyear >= 1997 & data$doubtterr== 0,]
  
  datatets <- factor(after_1997) # Convert to factor to plot on chart
  #df_killed <- filter(df, nkill > 0)
  
  plot_attacks_by_cntry <- datatets%>%
    count(country_txt)%>%
    arrange(n)%>%
    hchart(type = "treemap", hcaes(x = country_txt, value = n, color = n))
  print(plot_attacks_by_cntry)
  
  #mpgman3 <- df %>% 
   # group_by(country_txt) %>% 
    #summarise(n = n(), unique = length(unique(model))) %>% 
    #arrange(-n, -unique) %>% 
  ## Observations: 15
  ## Variables: 3
  ## $ manufacturer <chr> "dodge", "toyota", "volkswagen", "ford", "chevrol...
  ## $ n            <int> 37, 34, 27, 25, 19, 18, 14, 14, 13, 9, 8, 5, 4, 4, 3
  ## $ unique       <int> 4, 6, 4, 4, 4, 3, 2, 2, 3, 1, 1, 1, 1, 1, 1
  
  #hchart(mpgman3, "treemap", hcaes(x = country_txt, value = nkill))
  #plot(mpgman3)

  
  #counts <- table(df$nkills)
  #barplot(counts, main="Nkills", horiz=TRUE)
  #print(range(df$nkill))

  # df_filtered<-df[!(is.na(df$nkill)),]
  # df_naonly <- df[is.na(df$nkill),]
  
}



gc()
if(!is.null(dev.list())) dev.off()
graphics.off()
assign("last.warning", NULL, envir = baseenv())
cat("\014")
set.seed(123)
main()
print("end")