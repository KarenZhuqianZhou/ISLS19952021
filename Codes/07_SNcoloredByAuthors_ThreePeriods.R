library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

# The keyword of interest
input = "computer science"
input = sub(" ", ".", input)

#Create list with file names
p1 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA_ThreePeriods"
files <- list.files(path=p1, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)
p2 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/06_Keywords/authorKeywords_ThreePeriods/"

#start function which loops through files, creates graph objects, and summary metrics
lapply(files, function(x) {
  order_name = str_sub(x,nchar(x)-5, nchar(x)-4)
  conf_name = str_sub(x,nchar(p1)+2,nchar(p1)+5)
  auKw = paste(p2,conf_name,"_authorKeywords_",order_name,".csv", sep = "")
  
  # Import data 
  sna_table <- read.csv(x)
  rownames(sna_table) <- colnames(sna_table)
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  
  # Get authors who used a certain keyword
  auKw <- read.csv(auKw)
  auKw <- select(auKw, Author, all_of(input))
  V(new_net)$color <- ifelse(auKw[,2] > 0, "red", "grey")
  
  # Plot
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Graphs/Keywords_ThreePeriods")
  plot_name = paste(paste(conf_name, order_name, input, sep = "_"), '.png', sep = "")
  plot_title = paste(input, "in", conf_name, order_name, sep=" ")
  png(plot_name,width=1000,height=1000)
  plot(new_net, vertex.size=5, vertex.label=NA, main=plot_title)
  
  dev.off()
})
