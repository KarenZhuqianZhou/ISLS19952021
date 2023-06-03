library(stringr)
library(igraph)
library(CINNA)
library(dplyr)

#Create list with file names
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/03_SNA"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files, creates graph objects, and summary metrics
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  
  # Import data 
  sna_table <- read.csv(x)
  rownames(sna_table) <- colnames(sna_table)
  
  # Create adjacency list and graph and assign it to Global Environment so loop doesn't overwrite
  my_adj_list <- sna_table %>% as.matrix()
  new_net <- graph_from_adjacency_matrix(my_adj_list, weighted=NULL, mode = "undirected") %>% simplify()
  
  # Graph Summary Measures
  Author_count = V(new_net) %>% length
  Edge_count = E(new_net) %>% length
  Diameter = diameter(new_net, directed = F)
  Avg_Degree = mean(degree(new_net)) %>% round(3)
  Deg_Centrlz = centr_degree(new_net)$centralization
  
  Edge_density = edge_density(new_net) %>% round(3)
  Mean_distance = mean_distance(new_net) %>% round(3)
  Top_pagerank = page.rank(new_net) %>% .[[1]] %>% sort %>% tail(1) %>% names
  Avg_Clustering_Coeff = transitivity(new_net, type = "average") %>% round(3)
  Components = components(new_net)[[3]] 
  Authors_LCC = max(components(new_net)[[2]])
  
  # Largest Connected Component Summary Measures
  LCC = giant_component_extract(new_net, directed = FALSE)
  LCC <- graph.data.frame(LCC[2])
  LCC_auth_count = V(LCC) %>% length
  LCC_frac_size = (LCC_auth_count/Author_count) %>% round(3)
  
  LCC_edge_density = edge_density(LCC) %>% round(3)
  LCC_diameter = diameter(LCC, directed = F)
  
  LCC_top_eigencentrality = eigen_centrality(LCC) %>% .[[1]] %>% sort %>% tail(1) %>% names
  LCC_mean_distance = mean_distance(LCC) %>% round(3)
  LCC_top_pagerank = page.rank(LCC) %>% .[[1]] %>% sort %>% tail(1) %>% names
  
  #Set the working directory
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/04_SNAmetrics")
  
  # Write out SNA metrics
  table <- cbind(conf_name, year_name,Author_count, Edge_count, Edge_density,Diameter,Avg_Degree, Deg_Centrlz, Mean_distance,Top_pagerank,Avg_Clustering_Coeff, Components, Authors_LCC, LCC_auth_count, LCC_frac_size,LCC_edge_density,LCC_diameter,LCC_top_eigencentrality,LCC_mean_distance,LCC_top_pagerank) %>% tbl_df
  write.csv(table, file = paste(paste(conf_name, "metrics", year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
  
  # Clustering
  cluster <- new_net %>% cluster_fast_greedy()
  comm <- cluster %>% membership()
  
  # Membership List
  membership <- comm %>% print() %>% as.data.frame()
  names(membership) <- c("Comm")
  membership$Author <- rownames(membership)
  membership$Comm <- as.factor(membership$Comm)
  
  #Get the size of each community
  size <- as.data.frame(table(membership$Comm))
  
  # Rank each community from the largest to the smallest
  size$Rank <- nrow(size) - rank(size$Freq, ties.method = "random") + 1
  names(size)[1] <- "Comm"
  
  # Change the random numbering of each community into its rank
  membership <- left_join(membership, size, by = "Comm")
  membership <- select(membership, Author, Rank)
  names(membership)[2] <- "CommR"
  
  #Set the working directory
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/05_SNAmembership")
  
  # Write out the ranked membership table
  write.csv(membership, file = paste(paste(conf_name, "commR", year_name, sep = "_"), '.csv', sep = ""), row.names = FALSE)
  
  # Plot
  setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Graphs")
  plot_name = paste(paste(conf_name, "SN", year_name, sep = "_"), '.png', sep = "")
  plot_title = paste("communities of", conf_name, "in", year_name, sep=" ")
  colors <- comm %>% max() %>% rainbow
  png(plot_name,width=1000,height=1000)
  plot(new_net, vertex.size=5, vertex.color=colors[comm], vertex.label=membership$CommR, main=plot_title)
  dev.off()
})

#Combine SNA membership into one table
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/05_SNAmembership"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save author membership
auMemb <- as.data.frame(matrix(nrow = 1, ncol = 1))
names(auMemb) <- c("Author")

#start function which loops through files and joint 
lapply(files, function(x) {
  year_name = str_sub(x,nchar(x)-7, nchar(x)-4)
  conf_name = str_sub(x,nchar(p)+2,nchar(p)+5)
  conf_year = paste(conf_name, year_name, sep = "_")
  
  tb <- read.csv(x)
  names(tb) = c("Author", conf_year)
  auMemb <- full_join(auMemb, tb, by = "Author")
  assign("auMemb", auMemb, envir = .GlobalEnv)
})

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/05_SNAmembership/05_SNAmembership_Combined")

# Write out the membership table
auMemb[is.na(auMemb)] <- 0
auMemb <- filter(auMemb, Author != 0)
write.csv(auMemb, file = "ISLS_auMemb_1995_2021.csv", row.names = FALSE)


#Combine SNA metrics into one table
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/04_SNAmetrics"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#start function which loops through files and combines summary metrics
metrics_names <- read.csv("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/04_SNAmetrics/CSCL_metrics_1995.csv")
metrics <- as.data.frame(matrix(nrow = 1, ncol = ncol(metrics_names)))
names(metrics) = names(metrics_names)

lapply(files, function(x) {
  tb <- read.csv(x)
  metrics <- rbind(metrics, tb)
  assign("metrics", metrics, envir = .GlobalEnv)
})

metrics <- na.omit(metrics)

#Add a column showing the number of publications each year
p = "C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/00_RawData"
files <- list.files(path=p, pattern="\\d.csv", full.names=TRUE, recursive=FALSE)

#Create a data frame to save paper counts
paper_count <- as.data.frame(matrix(nrow = 1, ncol = 2))
names(paper_count) <- c("year_name", "Paper_count")

#start function which loops through files and counts the number of papers
lapply(files, function(x) {
  year_name = as.integer(str_sub(x,nchar(x)-7, nchar(x)-4))
  tb <- read.csv(x)
  paper_count <- rbind(paper_count, c(year_name, nrow(tb)))
  assign("paper_count", paper_count, envir = .GlobalEnv)
})

paper_count <- na.omit(paper_count)

metrics <- left_join(metrics, paper_count)

#Set the working directory
setwd("C:/Users/zkr/Documents/01_Research&Project/LearningAnalytics/2022_ISLS/Data/04_SNAmetrics/04_SNAmetrics_combined")

# Write out the metrics table
write.csv(metrics, file = "ISLS_metrics_1995_2021.csv", row.names = FALSE)
