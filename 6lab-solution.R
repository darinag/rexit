# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  2019 PRATICAL BUSINESS ANALYTICS
#
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 8 November 2019
#
# UPDATE
# 1.00      13/5/2017  KOHONEN LIBRARY  UPDATED TO V.3.0.2/FUNCTIONS CHANGED
# 1.01      8/3/2019   Update to use factoextra, gridExtra library
# 1.02      8/11/2019  PBA, igraph library dependency
#
# ************************************************
# For lab 6 - Unsupervised Learning Experiments
# ************************************************

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global variables - i.e. available to all functions
# ************************************************
# Good practice to place CONSTANTS in variables
# I use UPPERCASE to distinguisg these in the code

# Either choose UCI-G.scv, with "Status"
# or "adultData.csv" with "Salary"

DATASET_FILENAME  <- "UCI-G.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "Status"

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

# Cutoff values - you can experiment with these

CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
                                          # Set to negative means analyse but do not replace outliers
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

MAX_LITERALS      <-50                    # Maximum numner of hotcoding new fields

SOM_EPOCHS        <- 100                  # SOM training training epochs
SOM_LEARN_RATE    <- c(0.05,0.01)         # SOM learning rate - see documentation
SOM_GRIDSIZE      <- 20                   # SOM a 20x20 grid of neurons
SOM_TYPE          <- "hexagonal"          # SOM neuron grid shape (also "rectangular")
SOM_FIELD         <- 2                    # SOM analysis of specified field number (here "Amount")

# Define and then load the libraries used in this project
# You can find out the version of a package loaded into your system using, packageVersion()

MYLIBRARIES<-c(
  "outliers",
  "kohonen",
  "cluster",
  "ggplot2",
  "formattable",
  "factoextra",
  "gridExtra",
  "igraph")

# ************************************************
# coolBlueHotRed()
# INPUT: int    - n     - Point colour
#        double - alpha - transparency
#
# OUTPUT : Converted colour to blue->Red
# ************************************************
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# ************************************************
# encodeOrderedSymbols()
#
# Encodes symbols in a field to 1/n
# See Lecture 3, slide 48, Ordered Catagorical encoding
#
# INPUT:    Data Frame     - originalDataset - dataset to preprocess
#           String         - fieldName       - name of the field
#           String Vector  - orderedSymbols  - list of symbols in order to be encoded
#
# OUTPUT :  Data Frame - updated dataset
# 111119NRT - minor bug with length() compare
# ************************************************
encodeOrderedSymbols<-function(dataset, fieldName, orderedSymbols){

  NPREPROCESSING_setInitialFieldType(fieldName, TYPE_IGNORE)

  field<-which(names(dataset)==fieldName)

  for (eachSymbol in 1:length(orderedSymbols)){
    records<-which(dataset[,field]==orderedSymbols[eachSymbol])
    if (length(records)>0){
      dataset[records,field]<-(eachSymbol-1)/(length(orderedSymbols)-1)
    }
  }
  dataset[,field]<-as.numeric(dataset[,field])
  return(dataset)
}

# ************************************************
# main() :
# main entry point to execute our ML data analytics
#
# INPUT: None
# OUTPUT :None
# ************************************************
main<-function(){

# ************************************************
# GERMAN CREDIT SCORE EXAMPLE
# The first row of this file has the field names of each column
# Note: "Status" is 1 for good loan, 2 for loan defaulted/bad.
# Converted to good=0, bad=1

originalDataset<-NreadDataset(DATASET_FILENAME)

# ************************************************
# Output summary statistics to Viewer
NPREPROCESSING_prettyDataset(originalDataset)

# ************************************************
# Pre-process the dataset to scaled numeric [0,1]
#newDataset<-NPREPROCESSING_dataset(dataset=originalDataset, scaleFlag=SCALE_DATASET)

# ************************************************
# Oh, a problem - as preprocessing has determined some
# numeric fields are discreet; we'd prefer them to be left
# as numeric.
# Set these fields to the type we want

NPREPROCESSING_setInitialFieldType("Employment",    TYPE_ORDINAL)
NPREPROCESSING_setInitialFieldType("PerCentIncome", TYPE_ORDINAL)
NPREPROCESSING_setInitialFieldType("ResidentYears", TYPE_ORDINAL)
NPREPROCESSING_setInitialFieldType("Credits",       TYPE_ORDINAL)

# Oh, a problem - Some of our fields are actually ORDERED symbols
# Write our own function to deal with this

orderedSavings<-c("UNKNOWN","lt100","100to500","500to1000","gt1000")

newDataset<-encodeOrderedSymbols(originalDataset,
                                      "Savings", orderedSavings)

orderedChecking<-c("lt0","NONE","0to200","gt200")
newDataset<-encodeOrderedSymbols(newDataset,
                                      "Checking", orderedChecking)

# ************************************************
# Finally, preprocess the dataset ready for clustering
# Pre-process the dataset to scaled numeric [0,1]
newDataset<-NPREPROCESSING_dataset(dataset=newDataset, scaleFlag=SCALE_DATASET)

# ************************************************
# Remove the target field
positionOutput<-which(names(newDataset)==OUTPUT_FIELD)
predictors<-newDataset[,-positionOutput]

# ************************************************
# Clustering k-means.  Just pick 4 clusters
# Use ?kmeans for more help

modelKmeans <- kmeans(predictors, centers=4, nstart=25)

# ************************************************
# Output k-means result

print(str(modelKmeans))

# ************************************************
# cluster plot of the 4 clusters (uses ggplot2 library)

p<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point")
print(p)

# ************************************************
# BUT we just guessed at k=4
# Maybe take a look at different k

results<-list()
r<-1
for (k in 2:5){
  modelKmeans <- kmeans(predictors, k, nstart=25)
  results[[r]]<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point") + ggtitle(paste("k=",k))
  r<-r+1
}
gridExtra:: grid.arrange(grobs=results, nrow = 2)

# ************************************************
# Here are some approachs to help determine k

# Elbow method to find optimal number for k
# 1. Compute k-means for range values of k.
# 2. Calculate the total within-cluster sum of square (wss)
# 3. Plot the curve of wss against k.
# The location of a bend (knee) in the plot is generally considered as
# indicator of the appropriate number of clusters

p<-factoextra::fviz_nbclust(predictors, kmeans, method = "wss",k.max = 10)
print(p)

# ************************************************
# Average silhouette method to find optimal number for k
# Determines how well each object lies within its cluster

p<-factoextra::fviz_nbclust(predictors, kmeans, method = "silhouette",k.max = 15)
print(p)

# ************************************************
# Gap statistic method to find optimal number for k
# Compares the total intracluster variation for different values of k
# with their expected values when there is no obvious clustering in the data

#gap_stat <- cluster::clusGap(predictors, FUN = kmeans, nstart = 25, K.max = 30, B = 10)
#p<-factoextra::fviz_gap_stat(gap_stat)
#print(p)

# ************************************************
# Pick K from analysis of the above
# Final cluster analysis

kmeansClusters<-10
modelKmeans <- kmeans(predictors, kmeansClusters, nstart=25)

# cluster plot
p<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point")
print(p)


# ************************************************
#Output summary statistics for each cluster
#Calculates % of BAD loans in each cluster

for (k in 1:kmeansClusters){
  clusterRecord<-originalDataset[which(modelKmeans$cluster==k),]

  amountBad<-sum(clusterRecord$Amount[which(clusterRecord$Status==2)])
  debtRatio<-round(amountBad/sum(clusterRecord$Amount),digits=2)
  print(paste("cluster=",k,"debt ratio=",debtRatio))
  tableTitle<-paste("Clus=",k,"(",debtRatio,")",sep="")

  NPREPROCESSING_prettyDataset(clusterRecord,tableTitle)
}

print("KNN Complete")

# ************************************************
# Hierarchical clustering
# Compute hclust and we then choose how many clusters (cut the tree)
# Impossible to visualise more than 200 end points (records)

#I've chosen to select same number of clusters
hc.cut <- hcut(predictors[1:200,], k = 10, hc_method = "complete")

# Visualize dendrogram
p<-factoextra::fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
print(p)

p<-fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE,type="circular")
print(p)

#needs igraph
p<-factoextra::fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE,type="phylogenic")
print(p)

# Visualize clusters
hc.cut <- hcut(predictors, k = 3, hc_method = "complete")
p<-factoextra::fviz_cluster(hc.cut, ellipse.type = "convex")
print(p)

# ************************************************
# There are no obvious relationships here....

p<-ggplot(predictors, aes(x=Amount, y = Age)) + geom_point(color = factor(hc.cut$cluster))
print(p)

p<-ggplot(predictors, aes(x=Employment, y = Age)) + geom_point(color = factor(hc.cut$cluster))
print(p)
# ************************************************
# Clustering SOM

#Training data is input as a scaled matrix
data_train_matrix <- as.matrix(scale(predictors))

#Creates the SOM neuron map
som_grid = kohonen::somgrid(SOM_GRIDSIZE, SOM_GRIDSIZE, SOM_TYPE)

#Update 13/5/17 - removed n.hood parameter as the Kohonen library changed, this is replaved by dist.fcts="euclidean"
som_model <- kohonen::som(data_train_matrix,
                          grid=som_grid,
                          rlen=SOM_EPOCHS,
                          alpha=SOM_LEARN_RATE,
                          keep.data = TRUE,
                          dist.fcts="euclidean" )

par(mar = rep(4, 4))

plot(som_model, type="changes")

plot(som_model, type="count")

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")

#Update 13/5/17 create a data.frame() called som_codes as the Kohonen library changed
som_codes<-data.frame(som_model$codes)
som.hc <- cutree(hclust(dist(som_codes)), 20)
kohonen::add.cluster.boundaries(som_model, som.hc)
plot(som_model, type="quality", palette.name = coolBlueHotRed)

#Experiment here to see the different variables
#Update 13/5/17 use the som_codes data.frame() as the Kohonen library changed
var <- SOM_FIELD
plot(som_model, type = "property",
     property = som_codes[,var],
     main=names(data.frame(som_model$data))[var],
     palette.name=coolBlueHotRed)

print("SOM Complete")

} #endof main()


# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("START Lab 6 - Unsupervised Learning")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("6dataPrepFunctions.R")

set.seed(123)

main()

print("end")


