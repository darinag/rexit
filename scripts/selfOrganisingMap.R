# ************************************************
# Self organizing map for the Global Terrorism dataset that 
# that generates heatmaps of feature distribution
# 
# TODO : More explanation
# ************************************************

# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

# ************************************************
#   Global Environment variables

DATASET_FILENAME = "data/globalterrorismdb_0718dist.csv"

SOM_EPOCHS        <- 100                  # SOM training training epochs
SOM_LEARN_RATE    <- c(0.05,0.01)         # SOM learning rate - see documentation
SOM_GRIDSIZE      <- 20                   # SOM a 20x20 grid of neurons
SOM_TYPE          <- "hexagonal"          # SOM neuron grid shape (also "rectangular")
SOM_FIELD         <- 2                    # SOM analysis of specified field number (here "Amount")

# ************************************************
# main() :
# main entry point to our program
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function() { 
  # Load the dataset
  global_terrorism <- read.csv(DATASET_FILENAME)
  
  # Includes only incidents after 1997, where all incidents represent an act of terrorism 
  filteredDataset <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]
  
  #TODO DO for whole preprocessed dataset 
  readyForSOM <- data.frame("Weapon_Type" = filteredDataset$weaptype1,
                           "Country" = filteredDataset$country,
                           "Attack_Type" = filteredDataset$attacktype1,
                           "Killed" = filteredDataset$nkill)
  
  readyForSOM<-readyForSOM[!is.na(readyForSOM$Weapon_Type),]
  readyForSOM<-readyForSOM[!is.na(readyForSOM$Country),]
  readyForSOM<-readyForSOM[!is.na(readyForSOM$Attack_Type),]
  readyForSOM<-readyForSOM[!is.na(readyForSOM$Killed),]
  
  data_train_matrix <- as.matrix(scale(readyForSOM))
  
  #Creates the SOM neuron map
  som_grid = kohonen::somgrid(SOM_GRIDSIZE, SOM_GRIDSIZE, SOM_TYPE)
  
  som_model <- kohonen::som(data_train_matrix,
                            grid=som_grid,
                            rlen=SOM_EPOCHS,
                            alpha=SOM_LEARN_RATE,
                            keep.data = TRUE,
                            dist.fcts="euclidean" )
  
  plot(som_model, type="changes")
  som_codes<-data.frame(som_model$codes)
  som.hc <- cutree(hclust(dist(som_codes)), 8)
  kohonen::add.cluster.boundaries(som_model, som.hc) 
  
  plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
  plot(som_model, type="count") 
  
  coolBlueHotRed <- function(n, alpha = 1) {
    rainbow(n, end=4/6, alpha=alpha)[n:1]
  }
  
  var <- 4 #todo plot each feature
  plot(som_model, 
       type = "property",
       property = som_codes[,var], 
       main=names(data.frame(som_model$data))[var],
       palette.name=coolBlueHotRed
  ) 
}

gc() # garbage collection to automatically release memory
# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# clears the console area
cat("\014")

print("START Self Organising Maps")

# specify libraries to be loaded by pacman
myLibraries<-c(
  "readr",
  "ggplot2",
  "dplyr",
  "tidyverse",
  "DT",
  "plotly",
  "highcharter",
  "viridis",
  "som",
  "kohonen"
)

library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)      

# load functions from the labs, all code written by Prof. Nick Ryman-Tubb
source("scripts/labFunctions.R")

set.seed(123)

main()

print("end of program")

