data <- read.csv("globalterrorismdb_0718dist.csv")

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
after_1997 <- no_doubt[no_doubt$iyear >= 1997,]
