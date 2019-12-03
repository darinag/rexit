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

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables

DATASET_FILENAME = "globalterrorismdb_0718dist.csv"          # Name of input dataset file

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric

DISCREET_BINS     <- 5                    # Number of empty bins to determine discreet

SOM_EPOCHS        <- 2000                 # SOM training training epochs
SOM_LEARN_RATE    <- c(0.05,0.01)         # SOM learning rate - see documentation
SOM_GRIDSIZE      <- 10                   # SOM a 20x20 grid of neurons
SOM_TYPE          <- "hexagonal"          # SOM neuron grid shape (also "rectangular")


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "dplyr",
               "stats",
               "anchors",
               "kohonen")



# ************************************************
# main() :
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){
  # Read dataset
  global_terrorism <- read.csv(DATASET_FILENAME)
  
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  after_1997 <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  
  post_feature_selection <- data.frame(
    "Region" = after_1997$region,
    "Attack_Type" = after_1997$attacktype1,
    "Perpetrators_Number" = after_1997$nperps,
    "Kill_Count" = after_1997$nkill,
    "Wounded_Count" = after_1997$nwound
  )
  
  install.packages("anchors")
  library(anchors)
  install.packages("dplyr")
  library(dplyr)
  
  # Filter data for calculations
  filtered_killed <- post_feature_selection %>% filter(!is.na(Kill_Count) & Kill_Count>=0 ) 
  filtered_wounded_no_na_no_zero <- post_feature_selection %>% filter(!is.na(Wounded_Count) & Wounded_Count>0)
  
  
  # Calculate thresholds
  killed_threshold <- round(mean(filtered_killed$Kill_Count)) 
  wounded_threshold <- round(mean(filtered_wounded_no_na_no_zero$Wounded_Count))
  perpetrator_mean <- computeRoundedMean(post_feature_selection, "Perpetrators_Number")
  
  # Transform Dataset
  df <- replace.value(post_feature_selection, c("Kill_Count"), from=NA, to=as.double(killed_threshold))
  df <- replace.value(df, c("Wounded_Count"), from=NA, to=as.double(wounded_threshold))
  df <- subset(df, !is.na(Region))
  df <- subset(df, (Attack_Type != 9))
  df <- df %>% mutate(Perpetrators_Number = case_when(is.na(Perpetrators_Number) | Perpetrators_Number < 0 ~ perpetrator_mean, TRUE ~ as.double(Perpetrators_Number)))
  
  north_america <- subset(df, (Region==1))
  central_america <- subset(df, (Region==2))
  south_america <- subset(df, (Region==3))
  east_asia <- subset(df, (Region==4))
  south_east_asia <- subset(df, (Region==5))
  south_asia <- subset(df, (Region==6))
  central_asia <- subset(df, (Region==7))
  western_europe <- subset(df, (Region==8))
  eastern_europe <- subset(df, (Region==9))
  middle_east <- subset(df, (Region==10))
  sub_saharan_africa <- subset(df, (Region==11))
  australia_oceania <- subset(df, (Region==12))
  
  # Function to prepare the dataframe and build a SOM model.
  # Parameters: the desired region df and the region name
  SOM <- function(region_df, region_df_name){
    som_df <- region_df[ , -which(colnames(middle_east)=="Region")]
    som_df_name <- region_df_name
    unscaled_som_df <- region_df[ , -which(colnames(middle_east)=="Region")]
    
    library(outliers)
    # Transform all numeric fields 
    som_df <- transformNumeric(som_df)
    
    # Scale the dataframe before feeding it into the SOM model
    data_train_matrix <- as.matrix(scale(som_df))
    
    # Creates the SOM neuron
    som_grid = kohonen::somgrid(SOM_GRIDSIZE, SOM_GRIDSIZE, SOM_TYPE)
    
    # Build SOM model
    som_model <- kohonen::som(data_train_matrix,
                              grid=som_grid,
                              rlen=SOM_EPOCHS,
                              alpha=SOM_LEARN_RATE,
                              keep.data = TRUE)
    
    # Progression of the learning process
    plot(som_model, type="changes")
    
    # The number of instances into the cells are used to identify high-density areas
    plot(som_model, type="count")
    
    # Neighbour distance plot. Called “U-Matrix” (unified distance matrix)
    plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
    
    # Codebook vectors. This chart represents the vector of weights in a pie chart for each cell of the map.
    plot(som_model, type="codes") 
    
    # Mapping plot
    plot(som_model, type="mapping")
    
    # Pallete function used for plots
    coolBlueHotRed <- function(n, alpha = 1) {
      rainbow(n, end=4/6, alpha=alpha)[n:1]
    }
    
    # Store SOM model codes in a variable  
    som_codes<-data.frame(som_model$codes)
    
    # Iterate over all features and generate heatmaps for each of them. 
    # Store them in the current folder
    for (i in 1:ncol(som_df)){
      
      # Restart plots
      while (!is.null(dev.list()))  dev.off()
      
      # Create plot variable name based on the feature number and region name 
      jpeg_name = paste("rplot_", som_df_name, "_", i, ".jpg", sep="")
      
      # Create a jpg file
      jpeg(jpeg_name)
      
      # Get the unscaled value of the feature to print it as a scale on the heatmap
      var_unscaled <- aggregate(as.numeric(unscaled_som_df[,i]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
      
      # PLot the heatmap for the current feraure index i
      plot(som_model, 
           type = "property",
           property = var_unscaled, 
           main=names(som_df)[i],
           palette.name=coolBlueHotRed
      )
      dev.off()
    }
    
    # Build SOM model for e.g. south asia
    # Feel free to experiment with different regions
    SOM(south_asia, "south_asia")
    
    dev.off()
  }
  
} 


# clears the console area
cat("\014")

#library(pacman)
#pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# load functions 
source("labFunctions.R")
source("preprocessingFunctions.R")


set.seed(123)
main()

gc() # garbage collection to automatically release memory
# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# clears the console area
cat("\014")
