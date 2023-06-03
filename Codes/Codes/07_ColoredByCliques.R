library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

# The keyword of interest
kw = "problem-based learning"
kw = sub(" ", ".", kw)
kw = sub("-", ".", kw)

#Create list with file names
p1 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA"
files <- list.files(path=p1, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)
p2 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/Others/CommKw/"
p3 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/05_SNAmembership/"

setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Graphs/CommKeywords")

#start function which loops through files, creates graph objects, and summary metrics
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p1)+2,nchar(p1)+5)
  coKw = paste(p2,conf_name,"_coKw_",year_name,".csv", sep = "")
  coRk = paste(p3,conf_name,"_commR_",year_name,".csv", sep = "")
  
  # Import data 
  sna_table <- read.csv(x)
  rownames(sna_table) <- colnames(sna_table)
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  
  # Get communities who used a certain keyword
  coKw <- read.csv(coKw)
  coKw <- select(coKw, CommR, all_of(kw))
  coRk <- read.csv(coRk)
  coRk <- left_join(coRk, coKw, by = "CommR")
  V(new_net)$color <- ifelse(coRk[,3] > 0, "red", "grey")
  
  # Plot
  plot_name = paste(paste(conf_name, year_name, kw, sep = "_"), '.png', sep = "")
  plot_title = paste(kw, "in", conf_name, year_name, sep=" ")
  png(plot_name,width=1000,height=1000)
  plot(new_net, layout = layout_with_mds, vertex.size=5, vertex.label=NA, main=plot_title)
  
  dev.off()
})
