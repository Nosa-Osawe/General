library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

data("USArrests")
view(USArrests)# Compute PCA with ncp = 3
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc,
          cex = 0.7, # Label size
          palette = "jco", # Color palette see ? ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco", # Rectangle color
          labels_track_height = 0.8) # Augment the room forlabels

fviz_cluster(res.hcpc,
             repel = TRUE, # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco", # Color palette see ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map")


# Principal components + tree
plot(res.hcpc, choice = "3D.map")

res.hcpc

# To display quantitative variables that describe the most each cluster, type this:
  res.hcpc$desc.var$quanti
  head(res.hcpc$data.clust, 10)
# to show principal dimensions that are the most associated with clusters
  
  res.hcpc$desc.axes$quanti
  
  # representative individuals of each clusters
  res.hcpc$desc.ind$para  
  #For each cluster, the top 5 closest individuals to the cluster center is
  #shown. The distance between each individual and the cluster center is
  #provided.
  
  #####################################################################################
  
  data(tea)
view(tea)  

# Performing MCA
res.mca <- MCA(tea,
               ncp = 20, # Number of components kept
               quanti.sup = 19, # Quantitative supplementary variables
               quali.sup = c(20:36), # Qualitative supplementary variables
               graph=FALSE)

colnames(tea[19])
# apply hierarchical clustering on the results of the MCA
res.hcpc <- HCPC (res.mca, graph = FALSE, max = 3)
fviz_dend(res.hcpc, show_labels = FALSE)

#Description by variables
res.hcpc$desc.var$test.chi2
# Description by variable categories
res.hcpc$desc.var$category


# Description by principal components
res.hcpc$desc.axes
# Description by Individuals
res.hcpc$desc.ind$para
