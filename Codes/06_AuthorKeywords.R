library(dplyr)
library(tidyr)
library(stringr)
library(tidyselect)

## Create the articleID and Keyword table

#Upload the list of top 250 bigrams as keywords. Nonsensical bigrams have been removed.
kw <- read.csv("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/ISLS_bigramsTop250_1995_2021.csv")

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/01_CleanedData"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/articleBigrams")

#Start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  #Upload data
  df <- read.csv(x)
  df <- select(df, 'id', 'bigrams')
  
  #Create a data frame to save articles' keywords
  arKw <- as.data.frame(matrix(nrow = nrow(df), ncol = 1+nrow(kw)))
  names(arKw) <- c("id", kw$key)
  arKw$id = df$id
  
  #Check if the article's abstract contains some of the top 250 bigrams
  for (i in 1:nrow(arKw)) {
    for (j in 2:ncol(arKw)) {
      arKw[i,j] = grepl(names(arKw)[j],df$bigrams[i]) 
    }
  }
  assign(paste(conf_name, "arKw", year_name, sep = "_"), arKw, envir = .GlobalEnv)
  
  #Write out the articleID-Top250keywords table
  write.csv(arKw, file = paste(paste(conf_name, "arKw", year_name, sep = "_") ,".csv", sep = ""), row.names = FALSE)
})

# The list of article-author tables
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/02_ArticleAuthors"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

auTopKw <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auTopKw) <- "Author"

setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/authorKeywords")

#Start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  #Upload data
  arAu <- read.csv(x)
  
  #Join arKw and arAu to get auKw where numeric values mean the number of papers written by the author containing the keyword
  arKw <- get(paste(conf_name, "arKw", year_name, sep = "_"))
  auKw <- left_join(arAu, arKw, by = "id")
  auKw <- select(auKw, -id)
  auKw <- auKw %>% group_by(Author) %>% summarise_each(funs(sum))
  assign(paste(conf_name, "auKw", year_name, sep = "_"), auKw, envir = .GlobalEnv)
  
  #Write out the auKw tables
  write.csv(auKw, file = paste(paste(conf_name, "authorKeywords", year_name, sep = "_") ,".csv", sep = ""), row.names = FALSE)
  
  #Create a new column showing the most frequent keyword of the author
  TopKw <- auKw %>%  mutate(topKw = "",)
  for (i in 1:nrow(TopKw)) {
    TopKw$topKw[i] = names(auKw[i,2:ncol(auKw)])[auKw[i,2:ncol(auKw)] == max(auKw[i,2:ncol(auKw)])][1]
  }
  TopKw <- TopKw %>% select(Author, topKw)
  
  #Join author top keywords across years
  names(TopKw) = c("Author", conf_year)
  auTopKw <- full_join(auTopKw, TopKw, by = "Author")
  assign("auTopKw", auTopKw, envir = .GlobalEnv)
})

auDegr <- read.csv("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorCentrality_Combined/ISLS_auDegr_1995_2021.csv")
auDegr <- select(auDegr, Author, tDegr)
auTopKw <- left_join(auDegr, auTopKw, by = "Author")
auTopKw[is.na(auTopKw)] <- ""

setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords")
write.csv(auTopKw, file = "AuthorTopOneKeywords_1995_2021.csv", row.names = FALSE)
