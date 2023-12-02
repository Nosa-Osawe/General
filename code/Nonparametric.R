###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### November 26, 2018
### Updated: 10/26/2021
###

###
### Nonparametric.R
### (Nonparametric Analyses of Differences)
###
library(ggplot2)
library(ggthemes)
library(scales)


##
## One Factor
##

## Mann-Whitney U test
# df has subjects (S), one between-Ss factor (X) w/levels (a,b), and continuous response (Y)
library(coin)
df <- read.csv("data/Nonparametric/1F2LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a 2-level factor
wilcox_test(Y ~ X, data=df, distribution="exact")


## Wilcoxon signed-rank test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b), and continuous response (Y)
library(coin)
df <- read.csv("data/Nonparametric/1F2LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X = factor(df$X) # X is a 2-level factor
wilcoxsign_test(Y ~ X | S, data=df, distribution="exact")


## Kruskal-Wallis test
# df has subjects (S), one between-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(coin)
df <- read.csv("data/Nonparametric/1F3LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a 3-level factor
kruskal_test(Y ~ X, data=df, distribution="asymptotic")

# Post hoc Mann-Whitney U tests
# df has subjects (S), one between-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
ab = wilcox.test(df[df$X == "a",]$Y, df[df$X == "b",]$Y) # a vs. b
ac = wilcox.test(df[df$X == "a",]$Y, df[df$X == "c",]$Y) # a vs. c
bc = wilcox.test(df[df$X == "b",]$Y, df[df$X == "c",]$Y) # b vs. c
p.adjust(c(ab$p.value, ac$p.value, bc$p.value), method="holm")


## Friedman test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(coin)
df <- read.csv("data/Nonparametric/1F3LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X = factor(df$X) # X is a 3-level factor
friedman_test(Y ~ X | S, data=df, distribution="asymptotic")

# Post hoc Wilcoxon signed-rank tests
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(reshape2) # for dcast
df2 <- dcast(df, S ~ X, value.var="Y") # make wide-format table
ab = wilcox.test(df2$a, df2$b, paired=TRUE) # a vs. b
ac = wilcox.test(df2$a, df2$c, paired=TRUE) # a vs. c
bc = wilcox.test(df2$b, df2$c, paired=TRUE) # b vs. c
p.adjust(c(ab$p.value, ac$p.value, bc$p.value), method="holm")



##
## Multiple Factors
## 

## Aligned Rank Transform (between-Ss)
# df has subjects (S), two between-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(ARTool)
df <- read.csv("data/Nonparametric/2F2LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
m = art(Y ~ X1*X2, data=df)
anova(m)

## Post hoc ART-C contrast tests
library(dplyr) # for %>% pipe
art.con(m, ~ X1*X2, adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

## Or, use post hoc Mann-Whitney U tests
# df has subjects (S), two between-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
aa_ab = wilcox.test(df[df$X1 == "a" & df$X2 == "a",]$Y, df[df$X1 == "a" & df$X2 == "b",]$Y)
aa_ba = wilcox.test(df[df$X1 == "a" & df$X2 == "a",]$Y, df[df$X1 == "b" & df$X2 == "a",]$Y)
aa_bb = wilcox.test(df[df$X1 == "a" & df$X2 == "a",]$Y, df[df$X1 == "b" & df$X2 == "b",]$Y)
ab_ba = wilcox.test(df[df$X1 == "a" & df$X2 == "b",]$Y, df[df$X1 == "b" & df$X2 == "a",]$Y)
ab_bb = wilcox.test(df[df$X1 == "a" & df$X2 == "b",]$Y, df[df$X1 == "b" & df$X2 == "b",]$Y)
ba_bb = wilcox.test(df[df$X1 == "b" & df$X2 == "a",]$Y, df[df$X1 == "b" & df$X2 == "b",]$Y)
p.adjust(c(aa_ab$p.value, aa_ba$p.value, aa_bb$p.value, ab_ba$p.value, ab_bb$p.value, ba_bb$p.value), method="holm")

## Interaction contrasts (between-Ss)
# Interaction contrasts (Marascuilo & Levin 1970, Boik 1979) are a different type of contrast test 
# that work with ART. In the output, A-B : C-D is interpreted as answering whether the A vs. B 
# difference in condition C is itself significantly different than the A vs. B difference in 
# condition D. It is a "difference of differences" test.
library(phia) # for testInteractions
testInteractions(artlm(m, "X1:X2"), pairwise=c("X1","X2"), adjustment="holm") # m is the art model
# These can similarly be conducted using art.con and interaction=TRUE
art.con(m, ~ X1*X2, adjust="holm", interaction=TRUE)

# Create a boxplot
# http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=X1, y=Y, fill=X2)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font styles for the legend
  theme(legend.title = element_text(face="bold", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  theme(legend.text  = element_text(face="plain", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  # create the boxplots with a little bit of overlap
  geom_boxplot(position=position_dodge(0.65), alpha=0.25) + 
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,15,by=5), labels=seq(0,15,by=5), limits=c(0,15), oob=rescale_none) + 
  # set the name, labels, and colors for the boxes
  scale_fill_manual(name="X2", labels=c("a","b"), values=c("#69b3a2","#404080"))


## Aligned Rank Transform (within-Ss)
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(ARTool)
df <- read.csv("data/Nonparametric/2F2LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
m = art(Y ~ X1*X2 + (1|S), data=df) # S is a random factor
anova(m)

## Post hoc ART-C contrast tests
library(dplyr) # for %>% pipe
art.con(m, ~ X1*X2, adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

## Or, use post hoc Wilcoxon signed-rank tests
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(reshape2) # for dcast
df2 <- dcast(df, S ~ X1 + X2, value.var="Y") # make wide-format table
aa_ab = wilcox.test(df2$a_a, df2$a_b, paired=TRUE)
aa_ba = wilcox.test(df2$a_a, df2$b_a, paired=TRUE) 
aa_bb = wilcox.test(df2$a_a, df2$b_b, paired=TRUE)
ab_ba = wilcox.test(df2$a_b, df2$b_a, paired=TRUE)
ab_bb = wilcox.test(df2$a_b, df2$b_b, paired=TRUE)
ba_bb = wilcox.test(df2$b_a, df2$b_b, paired=TRUE)
p.adjust(c(aa_ab$p.value, aa_ba$p.value, aa_bb$p.value, ab_ba$p.value, ab_bb$p.value, ba_bb$p.value), method="holm")

## Interaction contrasts (within-Ss)
# Interaction contrasts (Marascuilo & Levin 1970, Boik 1979) are a different type of contrast test 
# that work with ART. In the output, A-B : C-D is interpreted as answering whether the A vs. B 
# difference in condition C is itself significantly different than the A vs. B difference in 
# condition D. It is a "difference of differences" test.
library(phia) # for testInteractions
testInteractions(artlm(m, "X1:X2"), pairwise=c("X1","X2"), adjustment="holm") # m is the art model
# These can similarly be conducted using art.con and interaction=TRUE
art.con(m, ~ X1*X2, interaction=TRUE, adjust="holm")

# Create a boxplot
# http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=X1, y=Y, fill=X2)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font styles for the legend
  theme(legend.title = element_text(face="bold", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  theme(legend.text  = element_text(face="plain", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  # create the boxplots with a little bit of overlap
  geom_boxplot(position=position_dodge(0.65), alpha=0.25) + 
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,20,by=5), labels=seq(0,20,by=5), limits=c(0,20), oob=rescale_none) + 
  # set the name, labels, and colors for the boxes
  scale_fill_manual(name="X2", labels=c("a","b"), values=c("#69b3a2","#404080"))
