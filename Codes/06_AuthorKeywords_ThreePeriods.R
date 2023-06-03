library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/articleBigrams"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  df <- read.csv(x)
  
  assign(paste(conf_name, "arKw", year_name, sep = "_"), df, envir = .GlobalEnv)
})

CSCL_arKw_01 <- rbind(CSCL_arKw_1995, CSCL_arKw_1997, CSCL_arKw_1999, CSCL_arKw_2002, CSCL_arKw_2003)
CSCL_arKw_02 <- rbind(CSCL_arKw_2005, CSCL_arKw_2007, CSCL_arKw_2009, CSCL_arKw_2011)
CSCL_arKw_03 <- rbind(CSCL_arKw_2013, CSCL_arKw_2015, CSCL_arKw_2017, CSCL_arKw_2019)

ICLS_arKw_01 <- rbind(ICLS_arKw_1996, ICLS_arKw_1998, ICLS_arKw_2000, ICLS_arKw_2004)
ICLS_arKw_02 <- rbind(ICLS_arKw_2006, ICLS_arKw_2008, ICLS_arKw_2010, ICLS_arKw_2012)
ICLS_arKw_03 <- rbind(ICLS_arKw_2014, ICLS_arKw_2016, ICLS_arKw_2018, ICLS_arKw_2020)

# The list of article-author tables
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors_ThreePeriods"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

auTopKw <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auTopKw) <- "Author"

setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/authorKeywords_ThreePeriods")

#Start function which loops through files
lapply(files, function(x) {
  order_name = str_sub(x,nchar(x)-5, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_order = paste(conf_name, order_name, sep = "_")
  
  #Upload data
  arAu <- read.csv(x)
  
  #Join arKw and arAu to get auKw where numeric values mean the number of papers written by the author containing the keyword
  arKw <- get(paste(conf_name, "arKw", order_name, sep = "_"))
  auKw <- left_join(arAu, arKw, by = "id")
  auKw <- select(auKw, -id)
  auKw <- auKw %>% group_by(Author) %>% summarise_each(funs(sum))
  assign(paste(conf_name, "auKw", order_name, sep = "_"), auKw, envir = .GlobalEnv)
  
  #Write out the auKw tables
  write.csv(auKw, file = paste(paste(conf_name, "authorKeywords", order_name, sep = "_") ,".csv", sep = ""), row.names = FALSE)
})
