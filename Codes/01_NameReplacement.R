library(dplyr)
library(tidyr)
library(stringr)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/00_RawData"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

nameReplace <- read.csv("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/01_CleanedData/NameReplacement/name_replacement.csv")

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/01_CleanedData")

#start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  #Upload data
  df <- read.csv(x)
  
  #Replace names with its standard forms
  for (i in 1:nrow(nameReplace)) {
    df$dc.contributor.author.. <- gsub(nameReplace[i,1], nameReplace[i,2], df$dc.contributor.author..)
  }
  
  #Write out the cleaned data after name replacement
  write.csv(df, file = paste(paste(conf_name, year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
})