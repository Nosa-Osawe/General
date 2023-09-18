library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

data(wine)
df <- wine[,c(1,2, 16, 22, 29, 28, 30,31)]
head(df[, 1:7], 4)
#To see the structure of the data
str(df)
view(df)
res.famd <- FAMD(df, graph = FALSE)
print(res.famd)

eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)



var <- get_famd_var(res.famd)
var

# The different components can be accessed as follow:
  # Coordinates of variables
  head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the dimensions
head(var$contrib)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)

# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

# the results for quantitative variables
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var

# plots quantitative variables
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("blue", "#E7B800", "#FC4E07"),
              repel = TRUE)

# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# result of qualitative variables
quali.var <- get_famd_var(res.famd, "quali.var")

fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#To get the results for individuals, type this:
ind <- get_famd_ind(res.famd)
ind
# plot individuals
fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              invisible = "quali.var", # invisible function removed the qualitatives
              repel = TRUE)

fviz_mfa_ind(res.famd,
             habillage = "Label", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE)

# based on multiple variables
fviz_ellipses(res.famd, c("Label", "Soil"), repel = TRUE)
fviz_ellipses(res.famd, c("Label", "Soil"), ellipse.type = "confidence",  repel = TRUE)
#   Or
fviz_ellipses(res.famd, 1:2, geom = "point")


