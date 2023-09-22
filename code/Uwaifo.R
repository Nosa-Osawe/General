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




