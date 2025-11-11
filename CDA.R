###CDA
# This code was meant to run a community detection analysis (CDA) on a behavioral data set.

#Load libraries
library(igraph)
library(ggcorrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library

######################################
### Data Prep
#Load data
data <- read.csv("normalized_imputed_pooled.csv")
tasks_del <- read.csv("tasks_threshold_70_to_delete.csv")

#Remove designated tasks in order to make sure network uses subjects as nodes
data <- data[, !(names(data) %in% tasks_del$Task)]

# Convert to long format
data_long <- data %>%
  pivot_longer(-ID, names_to = "Task", values_to = "Value")

# Convert to wide format with IDs as columns
data_cda <- data_long %>%
  pivot_wider(names_from = ID, values_from = Value)

data_cda <- data_cda %>% select(-Task)

write.csv(data_cda, "cda_data.csv", row.names = FALSE)


######################################
### CDA
# Correlations
corr_matrix = round(cor(data_cda, method = "spearman", use = "pairwise"),3)

#Plot the correlations
corr_matrix_plot <- ggcorrplot(corr_matrix, show.legend = FALSE, lab = FALSE) + 
  theme_minimal() +                          # Use a minimal theme
  theme(
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank(),      # Remove minor grid lines
    panel.background = element_blank(),      # Remove the panel background
    axis.title = element_blank(),            # Remove axis titles
    axis.text = element_blank(),             # Remove axis text
    axis.ticks = element_blank()             # Remove axis ticks
  )

corr_matrix_thres_pos <- as.matrix(corr_matrix)

# Threshold
corr_matrix_thres_pos[corr_matrix < 0]  = 0
ggcorrplot(corr_matrix_thres_pos)
View(corr_matrix_thres_pos)

write.csv(corr_matrix_thres_pos, "thresholded_corr_matrix_pos.csv")

#Network creation
mat_pos = as.matrix(corr_matrix_thres_pos)
g_pos <- graph_from_adjacency_matrix(mat_pos, "undirected", weighted=TRUE, diag = FALSE)

set.seed(111)

#Louvain Clustering
louvain_clusters_pos <- cluster_louvain(g_pos)
head(louvain_clusters_pos)

constraint.scores = constraint(g_pos, nodes = V(g_pos), weights = E(g_pos)$weight)
edge.attributes(g_pos)

# Visualize groups
png(filename=paste("louvain_clusters.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
plot(g_pos, vertex.label=V(g_pos)$name, edge.width=E(g_pos)$weight*25, vertex.color = louvain_clusters_pos$membership, 
     vertex.label.family = "Arial", vertex.label.color="black", vertex.label.cex=c(1.2))
dev.off()

# Find summary stats of clustering
modularity(louvain_clusters_pos) 
length(louvain_clusters_pos) # n clusters
sizes(louvain_clusters_pos)

# Store subject group membership
names <- louvain_clusters_pos$names
membership <- louvain_clusters_pos$membership
louvain_tmp <- data.frame(names,membership)

louvain <- data %>%
  left_join(louvain_tmp, by = c("ID" = "names"))

write.csv(louvain, "cda_results.csv", row.names = FALSE)