library(stringr)
library(dplyr)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/ISLS_BIGRAM_DF"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save bigram frequency
bigram_count <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(bigram_count) <- c("key")

#start function which loops through files and joint 
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  tb <- read.csv(x)
  tb <- tb[,2:3]
  names(tb) = c("key", conf_year)
  bigram_count <- full_join(bigram_count, tb, by = "key")
  assign("bigram_count", bigram_count, envir = .GlobalEnv)
})

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords")

# Write out the bigram table
bigram_count[is.na(bigram_count)] <- 0
bigram_count <- filter(bigram_count, key != 0)
bigram_count$tFreq <- rowSums(bigram_count[,2:ncol(bigram_count)])
bigram_count <- arrange(bigram_count, desc(tFreq))
write.csv(bigram_count, file = "ISLS_bigrams_1995_2021.csv", row.names = FALSE)
