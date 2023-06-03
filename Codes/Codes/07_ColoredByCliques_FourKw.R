library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

# The keyword of interest
kw = c("climate change", "computational thinking", "virtual reality", "augmented reality")
kw = sub(" ", ".", kw)
kw = sub("-", ".", kw)

#Create list with file names
p1 = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA/ICLS_2014_2020"
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
  coRk[is.na(coRk)] <- 0
  V(new_net)$color <- ifelse(coRk[,3] > 0, "#0089E2",
                             ifelse(coRk[,4] > 0, "#89E200",
                                    ifelse(coRk[,5] > 0, "#FF0081", 
                                           ifelse(coRk[,6] > 0, "orange", "white"))))
  
  # Plot
  plot_name = paste(paste(conf_name, year_name, sep = "_"), '.png', sep = "")
  png(plot_name,width=1000,height=1000)
  plot(new_net, layout = layout_with_mds, vertex.size=5, vertex.label=NA)
  
  dev.off()
})


library(igraph)
set.seed(8675309)

g <- sample_gnp(200, p = 0.01)
V(g)$name <- paste0("Node", 1:vcount(g))
V(g)$shape <- sample(c("circle","square"), vcount(g), replace = T)
clu <- components(g)
grps <- groups(clu)

lapply(grps, function(x) plot(induced_subgraph(g, x)))