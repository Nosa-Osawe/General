library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

data(wine)
colnames(wine)
View(wine)

res.mfa <- MFA(wine,
               group = c(2, 5, 3, 10, 9, 2),
               type = c("n", "s", "s", "s", "s", "s"),
               name.group = c("origin","odor","visual",
                              "odor.after.shaking",
                              "taste","overall"),
               num.group.sup = c(1, 6),
               graph = FALSE) # n= categorical variable (no standardization)
                              # s= standardize the continuous variables
res.mfa
# Eigen value: The proportion of variances retained by the  dimensions (axes)
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)
fviz_screeplot(res.mfa)

group <- get_mfa_var(res.mfa, "group")
group

#The different components can be accessed as follow:
  # Coordinates of groups
  head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the dimensions
head(group$contrib)

# plot of variables
# red color = active groups of variables
# green color = supplementary groups of variables
fviz_mfa_var(res.mfa, "group")


# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)

# extract results of qualtitative variables
quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var
# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)

# Correlation between quantitative variables and dimensions
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE)


#change also the legend position from "right" to "bottom"
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco") # variables are coloured by groups

# Contributions to dimension 2
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")# Contributions to dimension 2

fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")

# most contributing variables
fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))
# To create a bar plot of variables cos2, type this:
  fviz_cos2(res.mfa, choice = "quanti.var", axes = 1)
  
# results for individuals
    ind <- get_mfa_ind(res.mfa)
    ind
#color individuals by their cos2 values
fviz_mfa_ind(res.mfa, col.ind = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                invisible = "quali.var", # make qualitative variables invisible
                 repel = TRUE)

fviz_mfa_ind(res.mfa,
             habillage = "Label", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)

fviz_ellipses(res.mfa, c("Label", "Soil"), repel = TRUE)
# or 
fviz_ellipses(res.mca, 1:2, geom = "point")

#The graph of partial individuals represents each wine viewed by each group and
# its barycenter. To plot the partial points of all individuals, type this:
  fviz_mfa_ind(res.mfa, partial = "all")
# Visualize partial points 
  fviz_mfa_ind(res.mfa, 
               partial = c("1DAM", "1VAU", "2ING"), 
               labels = 3) # change label size from 'labels' function
  fviz_mfa_axes(res.mfa)

