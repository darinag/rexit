data <- read.csv("globalterrorismdb_0718dist.csv")

MYLIBRARIES<-c("ggplot2",      # data visualization
               "scales",       # date/time scales for plots
               "dplyr",        # data wrangling
               "tidyr",        # reshaping data
               "stringr",
               "tidyverse",
               "tm",           # for text mining
               "SnowballC",    # for text stemming
               "RColorBrewer", # color palettes
               "wordcloud")    # word-cloud generator

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

columns <- c("eventid",
             "iday",
             "imonth",
             "iyear",
             "crit1",
             "crit2",
             "crit3",
             "doubtterr",
             "multiple",
             "related",
             "country",
             "region",
             "city",
             "vicinity",
             "latitude",
             "longitude",
             "attacktype1",
             "success",
             "suicide",
             "weaptype1",
             "targtype1",
             "gname",
             "nperps",
             "motive",
             "nkill",
             "nkillter",
             "nwound",
             "nwoundte",
             "property",
             "propextent",
             "propvalue",
             "INT_IDEO")


colIndexes <- sapply(columns, function(x) {
    which(names(data)==x)
  }
)

data <- data[,colIndexes]
no_doubt <- data[data$doubtterr==0,]
df <- no_doubt[no_doubt$iyear >= 1997,]
gname <- df$gname
#motive <- df$motive

regex <- '(\\([A-Z]+\\))'


createWordCloud <- function(text, myStopwords) {
  docs <- Corpus(VectorSource(text))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, myStopwords) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

createWordCloud(text, c("unknown"))
