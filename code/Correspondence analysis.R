install.packages("gplots")


library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

data(housetasks)
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
# For a small contingency table, you can use the Chi-square test to evaluate
# whether there is a significant dependence between row and column categories
chisq <- chisq.test(housetasks)
chisq

# compute correspondence analysis

res.ca <- CA(housetasks, graph = FALSE)
res.ca
eig.val <- get_eigenvalue(res.ca)
eig.val

# check the umber of dimensiions to use using a scree plot
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))

# draw the biplotof rows and columns variables.
# repel= TRUE to avoid text overlapping (slow if many point)
fviz_ca_biplot(res.ca, repel = TRUE)

row <- get_ca_row(res.ca)
row

# Coordinates
head(row$coord)
# Cos2: quality on the factore map


head(row$cos2)  #The values of the cos2 are comprised between 0 and 1. 
              #The sum of the cos2 for
              #rows on all the CA dimensions is equal to one.
# Contributions to the principal components
head(row$contrib)

fviz_ca_row(res.ca, repel = TRUE) # vizual representation of row points

fviz_ca_row(res.ca, col.row="steelblue", shape.row = 15) # new color of row points 

# Color by cos2 values: quality on the factor map
# Color by cos2 values: quality on the factor map
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

corrplot(row$cos2, is.corr=FALSE)
# Cos2 of rows on Dim.1 and Dim.2
fviz_cos2(res.ca, choice = "row", axes = 1:2) 

#all row points except Official are well represented by the first
#two dimensions. This implies that the position of the point corresponding
#the item Official on the scatter plot should be interpreted with some
#caution.

#highlight the most contributing row points for each dimension:
corrplot(row$contrib, is.corr=FALSE)
# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)

fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
col <- get_ca_col(res.ca) #to extract the results for column variables.
col


# Coordinates of column points
head(col$coord)
# Quality of representation
head(col$cos2)
# Contributions
head(col$contrib)

fviz_ca_col(res.ca, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

#The R code below creates a barplot of columns cos2:
  fviz_cos2(res.ca, choice = "col", axes = 1:2)
  # contrubutions of the first two dimenstions
  fviz_contrib(res.ca, choice = "col", axes = 1:2)
  
  #Symmetric biplot
  fviz_ca_biplot(res.ca, repel = TRUE)
#in order to interpret the distance between column points and
  #row points, the simplest way is to make an asymmetric

  fviz_ca_biplot(res.ca,
                 map ="rowprincipal", arrow = c(FALSE, TRUE),
                 repel = TRUE)

# contribution biplot
  fviz_ca_biplot
  
  
  # Dimension description
  res.desc <- dimdesc(res.ca, axes = c(1,2))
  
  head(res.desc[[1]]$row, 4)

  # Description of dimension 1 by column points
  head(res.desc[[1]]$col, 4)

  # Description of dimension 2 by row points
  res.desc[[2]]$row
  # Description of dimension 1 by column points
  res.desc[[2]]$col
  
  
  ########################################################################################
  data(children)
  head(children)
  CA(X, ncp = 5, row.sup = NULL, col.sup = NULL,
     graph = TRUE)
  
  res.ca <- CA (children, row.sup = 15:18, col.sup = 6:8,
                graph = FALSE)
  fviz_ca_biplot(res.ca, repel = TRUE)
 # hide supplementary rows and columns using the argument invisible:
  fviz_ca_biplot(res.ca, repel = TRUE,
                 invisible = c("row.sup", "col.sup"))
 # Predicted results (coordinates and cos2) for the supplementary rows:
  res.ca$row.sup
  fviz_ca_row(res.ca, repel = TRUE)
  res.ca$col.sup  
  fviz_ca_col(res.ca, repel = TRUE)
  
  
  # Visualize rows with cos2 >= 0.8
  fviz_ca_row(res.ca, select.row = list(cos2 = 0.8), repel = TRUE)
  
  # Top 5 active rows and 5 suppl. rows with the highest cos2
  fviz_ca_row(res.ca, select.row = list(cos2 = 5))
  
  # Select by names
  name <- list(name = c("employment", "fear", "future"))
  fviz_ca_row(res.ca, select.row = name)
  
  # Top 5 contributing rows and columns
  fviz_ca_biplot(res.ca, select.row = list(contrib = 5), select.col =
                   list(contrib = 5), repel = TRUE) +
    theme_minimal()
  
  
  
  