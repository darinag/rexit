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

# Clear all objects in "global environment"
rm(list=ls())

# Set the timezone
Sys.setenv(TZ="Europe/London")

#   Global Environment variables
DATASET_FILENAME = "data/globalterrorismdb_0718dist.csv"
THRESHOLD_MISSING_VALUES = 0.25

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
  filteredDataset <- global_terrorism[global_terrorism$iyear >= 1997 & global_terrorism$doubtterr== 0,]

  # Data visualization
  plotAttacksOverTime(filteredDataset)
  plotAttackTypesBarChart(filteredDataset)
  plotAttackTypesTreemap(filteredDataset)
  plotDeadliestTerrOrgsBarChart(filteredDataset)
  plotMostActiveTerrOrgsTreemap(filteredDataset)
  plotMissingValues(filteredDataset, THRESHOLD_MISSING_VALUES)
  #plotFatalitiesByAttackType(filteredDataset)
  plotMostLethalWeapons(filteredDataset)
  plotCasualtiesByRegion(filteredDataset)
  plotWoundedByRegion(filteredDataset)
  plotWoundedByAttackType(filteredDataset)
  plotAttackTypesByYear(filteredDataset)
  plotExtentPropertyDamage(filteredDataset)
  
    print("end of main")
}

# specify libraries to be loaded by pacman
# TODO Remove unused
myLibraries<-c("readr",
               "ggplot2",
               "dplyr",
               "tidyverse",
               "plotly",
               "DT",
               "highcharter",
               "treemap",
               "viridis")

library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)

# Load libraries

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

#load functions for plotting
source("scripts/plot.R")
set.seed(123)

main()

print("end")

