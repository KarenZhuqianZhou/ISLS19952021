library(dplyr)
library(tidyr)
library(stringr)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/01_CleanedData"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  #Upload data
  df <- read.csv(x)
  df <- select(df, 'id', 'dc.contributor.author..') 
  df <- cbind(df, str_split_fixed(df$'dc.contributor.author..', "\\|\\|", 30)) #Divide authors into different columns
  df[df[]==""] <- NA
  df <- select(df, -'dc.contributor.author..')
  
  #Create the articleID-author long table
  df <- gather(df, Order, Author, 2:31)
  df <- select(df, -Order)
  df <- na.omit(df)
  df$Author <- gsub(",", "_", df$Author) #replace comma with underscore
  df$Author <- gsub(" ", "_", df$Author) #replace space with underscore
  df <- unique(df)
  
  #Set the working directory
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors")
  #Write out the article author long table
  write.csv(df, file = paste(paste(conf_name, "articleAuthors", year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
  
  #Create CoAuthor Adjacency Matrices
  df$n <- 1
  df <- spread(df, Author, n)
  df <- select(df, -id)
  df[is.na(df)] <- 0
  df <- as.matrix(df)
  df <- t(df) %*% df
  
  #Set the working directory
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA")
  #Write out the coauthor matrices
  write.csv(df, file = paste(paste(conf_name, "CoAuthor", year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
})
