library(dplyr)
library(igraph)
library(stringr)

#create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Start function which loops through files
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  degr_name = paste(conf_name, "authorDegree", year_name, sep = "_")
  betw_name = paste(conf_name, "authorBetweenness", year_name, sep = "_")
  
  sna_table <- read.csv(x)

  # Create adjacency list 
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  
  # Get the degree and betweeness centralities of each author
  degr <- degree(new_net)
  betw <- betweenness(new_net, directed = FALSE)
  
  # Write out the centralities
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorDegr")
  write.csv(degr, file = paste(degr_name, ".csv", sep = ""))
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorBetw")
  write.csv(betw, file = paste(betw_name, ".csv", sep = ""))
})

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorDegr"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save author centralities
auDegr <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auDegr) <- c("Author")

#start function which loops through files and joint 
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  tb <- read.csv(x)
  names(tb) = c("Author", conf_year)
  auDegr <- full_join(auDegr, tb, by = "Author")
  assign("auDegr", auDegr, envir = .GlobalEnv)
})

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorDegr"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save author centralities
auDegr <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auDegr) <- c("Author")

#start function which loops through files and joint 
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  tb <- read.csv(x)
  names(tb) = c("Author", conf_year)
  auDegr <- full_join(auDegr, tb, by = "Author")
  assign("auDegr", auDegr, envir = .GlobalEnv)
})

p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorBetw"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save author centralities
auBetw <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auBetw) <- c("Author")

#start function which loops through files and joint 
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  tb <- read.csv(x)
  names(tb) = c("Author", conf_year)
  auBetw <- full_join(auBetw, tb, by = "Author")
  assign("auBetw", auBetw, envir = .GlobalEnv)
})

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/07_AuthorCentrality/07_AuthorCentrality_Combined")

# Write out the centrality tables
auDegr[is.na(auDegr)] <- 0
auDegr <- filter(auDegr, Author != 0)
auDegr$tDegr <- rowSums(auDegr[,2:ncol(auDegr)])
auDegr <- arrange(auDegr, desc(tDegr))
write.csv(auDegr, file = "ISLS_auDegr_1995_2021.csv", row.names = FALSE)

auBetw[is.na(auBetw)] <- 0
auBetw <- filter(auBetw, Author != 0)
auBetw$tBetw <- rowSums(auBetw[,2:ncol(auBetw)])
auBetw <- arrange(auBetw, desc(tBetw))
write.csv(auBetw, file = "ISLS_auBetw_1995_2021.csv", row.names = FALSE)