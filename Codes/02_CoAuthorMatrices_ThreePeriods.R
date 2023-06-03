library(dplyr)
library(tidyr)
library(stringr)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  df <- read.csv(x)
  
  assign(paste(conf_name, year_name, sep = "_"), df, envir = .GlobalEnv)
})

CSCL_01 <- rbind(CSCL_1995, CSCL_1997, CSCL_1999, CSCL_2002, CSCL_2003)
CSCL_02 <- rbind(CSCL_2005, CSCL_2007, CSCL_2009, CSCL_2011)
CSCL_03 <- rbind(CSCL_2013, CSCL_2015, CSCL_2017, CSCL_2019)

ICLS_01 <- rbind(ICLS_1996, ICLS_1998, ICLS_2000, ICLS_2004)
ICLS_02 <- rbind(ICLS_2006, ICLS_2008, ICLS_2010, ICLS_2012)
ICLS_03 <- rbind(ICLS_2014, ICLS_2016, ICLS_2018, ICLS_2020)

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors_ThreePeriods")
#Write out the article-author long table
write.csv(CSCL_01, "CSCL_01.csv", row.names = FALSE)
write.csv(CSCL_02, "CSCL_02.csv", row.names = FALSE)
write.csv(CSCL_03, "CSCL_03.csv", row.names = FALSE)
write.csv(ICLS_01, "ICLS_01.csv", row.names = FALSE)
write.csv(ICLS_02, "ICLS_02.csv", row.names = FALSE)
write.csv(ICLS_03, "ICLS_03.csv", row.names = FALSE)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors_ThreePeriods"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA_ThreePeriods")

#start function which loops through files
lapply(files, function(x) {
  order_name = str_sub(x,nchar(x)-5, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  df <- read.csv(x)
  
  #Create CoAuthor Adjacency Matrices
  df$n <- 1
  df <- spread(df, Author, n)
  df <- select(df, -id)
  df[is.na(df)] <- 0
  df <- as.matrix(df)
  df <- t(df) %*% df
  
  write.csv(df, file = paste(paste(conf_name, "CoAuthor", order_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
})
