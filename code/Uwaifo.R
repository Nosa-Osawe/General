library("FactoMineR")
library("factoextra")
library(tidyverse)
library("corrplot")
library(gplots)

uwaifo <- read.csv("C:\\Users\\user\\Documents\\GitHub\\General\\data\\UWAIFO_not_standardized.csv")
colnames(uwaifo)
view(uwaifo)
row.names(uwaifo) <- uwaifo[,1]
uwaifo <- uwaifo[,-1]

res.uwaifo <- MFA(uwaifo,
               group = c(2, 20, 6),
               type = c("n", "s", "s"),
               name.group = c("origin","metals","anti-biotics"),
               graph = FALSE) # n= categorical variable (no standardization)
# s= standardize the continuous variables
res.uwaifo

eig.val_uwaifo <- get_eigenvalue(res.uwaifo)
head(eig.val_uwaifo)
fviz_screeplot(res.uwaifo)

group_uwaifo <- get_mfa_var(res.uwaifo, "group")
group_uwaifo

#The different components can be accessed as follow:
# Coordinates of groups
head(group_uwaifo$coord)
# Cos2: quality of representation on the factore map
head(group_uwaifo$cos2)
# Contributions to the dimensions
head(group_uwaifo$contrib)

# plot of variables
# red color = active groups of variables
# green color = supplementary groups of variables
fviz_mfa_var(res.uwaifo, "group")


# Contribution to the first dimension
fviz_contrib(res.uwaifo, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.uwaifo, "group", axes = 2)
# Contribution to the second dimension
fviz_contrib(res.uwaifo, "group", axes = 3)

# extract results of quantitative variables
quanti.var_uwaifo <- get_mfa_var(res.uwaifo, "quanti.var")
quanti.var_uwaifo

# Coordinates
head(quanti.var_uwaifo$coord)
# Cos2: quality on the factore map
head(quanti.var_uwaifo$cos2)
# Contributions to the dimensions
head(quanti.var_uwaifo$contrib)

fviz_mfa_var(res.uwaifo, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE)

#change also the legend position from "right" to "bottom"
fviz_mfa_var(res.uwaifo, "quanti.var", palette = "jco",
             col.var.sup = FALSE, repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco") # variables are coloured by groups

# Contributions to dimension 2
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")# Contributions to dimension 2

# Contributions to dimension 3
fviz_contrib(res.uwaifo, choice = "quanti.var", axes = 3, top = 20,
             palette = "jco")# Contributions to dimension 2

# most contributing variables
fviz_mfa_var(res.uwaifo, "quanti.var", col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo, choice = "quanti.var", axes = 1)

# To create a bar plot of variables cos2, type this:
fviz_cos2(res.uwaifo, choice = "quanti.var", axes = 2)

# results for individuals
ind <- get_mfa_ind(res.uwaifo)
ind


#color individuals by their cos2 values
fviz_mfa_ind(res.uwaifo, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quali.var", # make qualitative variables invisible
             repel = TRUE)




fviz_mfa_ind(res.uwaifo,
             habillage = "stations", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)








