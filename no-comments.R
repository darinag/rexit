
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
  
  
  df <- data[data$iyear >= 1997,]
  View(df$nkill)
  print(range(df$nkill))
  #todo rename
  df_filtered<-df[!(is.na(df$nkill)),]
  df_naonly <- df[is.na(df$nkill),]

  
  df_filtered <- df_filtered[df$property != -9,]
  #str(after_1997)
  #standardDev <- sd(after_1997$imonth)
  #print(standardDev)
  #plot(after_1997$imonth)
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