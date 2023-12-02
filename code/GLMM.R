###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### March 12, 2019
### Updated: 03/14/2022
###

###
### GLMM.R
### (Generalized Linear Mixed Models)
###
library(ggplot2)
library(ggthemes)
library(scales)
library(plyr)


#
# Normal
#
library(lme4) # for glmer, lmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_normal.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
#m = glmer(Y ~ X + (1|S), data=df, family=gaussian) #deprecated
m = lmer(Y ~ X + (1|S), data=df) # equivalent, use instead
Anova(m, type=3, test.statistic="F")

## Normal post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="kenward-roger", type="Score"))


#
# Binomial
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_binomial.csv")
df$S = factor(df$S)
df$X = factor(df$X)
df$Y = factor(df$Y) # nominal response
contrasts(df$X) <- "contr.sum"
m = glmer(Y ~ X + (1|S), data=df, family=binomial)
Anova(m, type=3)

## Binomial post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and dichotomous response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Multinomial
#
library(mlogit) # for mlogit.data
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_multinomial.csv")
df$S = factor(df$S)
df$X = factor(df$X)
df$Y = factor(df$Y) # nominal response

# use the multinomial-Poisson (m-P) transformation (Baker 1994)
df2 <- as.data.frame(mlogit.data(df, choice="Y", shape="wide", id.var="S"))
df2$S = factor(df2$S)
contrasts(df2$X) <- "contr.sum"
contrasts(df2$alt) <- "contr.sum"

m = glmer(Y ~ X*alt + (1|S), data=df2, family=poisson) # m-P
a = Anova(m, type=3)
a[grep(":alt", rownames(a)),] # X:alt is X

## Multinomial post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and polytomous response (Y)
# df2 was produced by mlogit.data and has a logical response (Y) and alt factor
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova

df3 <- df2[df2$X == "a" | df2$X == "b",] # a vs. b
df3$X = factor(df3$X)
contrasts(df3$X) <- "contr.sum"
m = glmer(Y ~ X*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
ab = a[grep(":alt", rownames(a)),] 

df3 <- df2[df2$X == "a" | df2$X == "c",] # a vs. c
df3$X = factor(df3$X)
contrasts(df3$X) <- "contr.sum"
m = glmer(Y ~ X*alt + (1|S), data=df3, family=poisson) 
a = Anova(m, type=3)
ac = a[grep(":alt", rownames(a)),] 

df3 <- df2[df2$X == "b" | df2$X == "c",] # b vs. c
df3$X = factor(df3$X)
contrasts(df3$X) <- "contr.sum"
m = glmer(Y ~ X*alt + (1|S), data=df3, family=poisson)  
a = Anova(m, type=3)
bc = a[grep(":alt", rownames(a)),]

p.adjust(c(ab$`Pr(>Chisq)`, ac$`Pr(>Chisq)`, bc$`Pr(>Chisq)`), method="holm")


#
# Ordinal
#
library(ordinal) # for clmm
library(RVAideMemoire) # for Anova.clmm
df <- read.csv("data/GLMM/1F3LWs_ordinal.csv")
df$S = factor(df$S)
df$X = factor(df$X)
df$Y = ordered(df$Y) # ordinal response
contrasts(df$X) <- "contr.sum"
df2 <- as.data.frame(df) # Anova.clmm fails without this
m = clmm(Y ~ X + (1|S), data=df2, link="logit") # or "probit"
Anova.clmm(m)

## Ordinal post hoc test
# df has subjects (S), one factor (X) w/levels (a,b,c), and ordinal response (Y)
library(multcomp) # for adjusted
library(emmeans) # for as.glht, pairs, emmeans
summary(as.glht(pairs(emmeans(m, ~ X))), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Poisson
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_poisson.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
m = glmer(Y ~ X + (1|S), data=df, family=poisson)
Anova(m, type=3)

## Poisson post hoc test
# df has subjects (S), one factor (X) w/levels (a,b,c), and count response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Zero-Inflated Poisson
#
library(glmmTMB) # for glmmTMB
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_zipoisson.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
m = glmmTMB(Y ~ X + (1|S), data=df, family=poisson, ziformula=~X)
Anova(m, type=3)

## Zero-Inflated Poisson post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and count response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Negative Binomial
#
library(lme4) # for glmer.nb
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_negbin.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
m = glmer.nb(Y ~ X + (1|S), data=df)
Anova(m, type=3)

## Negative Binomial post hoc test
# df has subjects (S), one factor (X) w/levels (a,b,c), and count response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Zero-Inflated Negative Binomial
#
library(glmmTMB) # for glmmTMB
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_zinegbin.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
m = glmmTMB(Y ~ X + (1|S), data=df, family=nbinom2, ziformula=~X)
Anova(m, type=3)

## Zero-Inflated Poisson post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and count response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


#
# Gamma, including Exponential
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/1F3LWs_gamma.csv")
df$S = factor(df$S)
df$X = factor(df$X)
contrasts(df$X) <- "contr.sum"
m = glmer(Y ~ X + (1|S), data=df, family=Gamma)
# family=Gamma(link="log") is often used
Anova(m, type=3)

## Gamma post hoc test
# df has subjects (S), one factor (X) w/levels (a,b,c), and continuous response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm"))
# the following also performs the equivalent contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))



###
### Multiple Within-Ss Factors
###


#
# Normal
#
library(lme4) # for glmer, lmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_normal.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
#m = glmer(Y ~ X1*X2 + (1|S), data=df, family=gaussian) #deprecated
m = lmer(Y ~ X1*X2 + (1|S), data=df) # equivalent, use instead
Anova(m, type=3, test.statistic="F")

## Normal post hoc test
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="kenward-roger", type="Score"))

# Interaction plot
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
df2 <- ddply(df, ~ X1*X2, function(d) # make a summary data table
  c(NROW(d$Y),
    sum(is.na(d$Y)),
    sum(!is.na(d$Y)),
    mean(d$Y, na.rm=TRUE),
    sd(d$Y, na.rm=TRUE),
    median(d$Y, na.rm=TRUE),
    IQR(d$Y, na.rm=TRUE)))
colnames(df2) <- c("X1","X2","Rows","NAs","NotNAs","Mean","SD","Median","IQR")
ggplot(data=df2, aes(x=X1, y=Mean, color=X2, group=X2)) + theme_minimal() + 
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
  # create the plot lines, points, and error bars
  geom_line(aes(linetype=X2), position=position_dodge(0.05)) + 
  geom_point(aes(shape=X2, size=X2), position=position_dodge(0.05)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=position_dodge(0.05), width=0.1) + 
  # place text labels on each bar
  geom_text(aes(label=sprintf("%.2f (±%.2f)", Mean, SD)), position=position_dodge(0.05), hjust=0.0, vjust=-1.0, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,18,by=2), labels=seq(0,18,by=2), limits=c(0,18), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))


#
# Binomial
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_binomial.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
df$Y = factor(df$Y) # nominal response
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmer(Y ~ X1*X2 + (1|S), data=df, family=binomial)
Anova(m, type=3)

## Binomial post hoc test
# df has subjects (S), two factors (X1,X2) each w/levels (a,b), and dichotomous response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Barplot
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://stackoverflow.com/questions/51892875/how-to-increase-the-space-between-grouped-bars-in-ggplot2
df2 <- as.data.frame(xtabs(~ X1+X2+Y, data=df))
df2$X12 = with(df2, interaction(X1, X2))
df2$X12 = factor(df2$X12, levels=c("a.a","a.b","b.a","b.b"))
df2 <- df2[order(df2$X1, df2$X2),] # sort df2 alphabetically by X1, X2
ggplot(data=df2, aes(y=Freq, x=X12, fill=Y)) + theme_minimal() + 
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
  # create the barplots side-by-side
  geom_col(width=0.75, position=position_dodge(width=0.75), alpha=0.60) +
  # place text labels on each bar
  geom_text(aes(label=Freq), position=position_dodge(width=0.75), vjust=1.2, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1.X2", y="Count") + 
  # change the order of the bars on the x-axis
  scale_x_discrete(limits=c("a.a","a.b","b.a","b.b")) + 
  # set the scale, breaks, and labels on the y-axis
  scale_y_continuous(breaks=seq(0,12.5,by=5), labels=seq(0,12.5,by=5), limits=c(0,12.5), oob=rescale_none) + 
  # set the name, labels, and colors for the boxes
  scale_fill_manual(name="Y", labels=c("x","y"), values=c("#69b3a2","#404080"))


#
# Multinomial
#
library(mlogit) # for mlogit.data
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_multinomial.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
df$Y = factor(df$Y) # nominal response

# use the multinomial-Poisson (m-P) transformation (Baker 1994)
df2 <- as.data.frame(mlogit.data(df, choice="Y", shape="wide", id.var="S"))
df2$S = factor(df2$S)
contrasts(df2$X1) <- "contr.sum"
contrasts(df2$X2) <- "contr.sum"
contrasts(df2$alt) <- "contr.sum"

m = glmer(Y ~ X1*X2*alt + (1|S), data=df2, family=poisson) # m-P
a = Anova(m, type=3)
a[grep(":alt", rownames(a)),] # X1:alt is X1, etc.

## Multinomial post hoc test
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and polytomous response (Y)
# df2 was produced by mlogit.data and has a logical response (Y) and alt factor
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df2$X12 = with(df2, interaction(X1,X2)) # make combined factor for contrasts

df3 <- df2[df2$X12 == "a.a" | df2$X12 == "a.b",] # aa vs. ab
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
aa_ab = a[grep(":alt", rownames(a)),]

df3 <- df2[df2$X12 == "a.a" | df2$X12 == "b.a",] # aa vs. ba
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
aa_ba = a[grep(":alt", rownames(a)),]

df3 <- df2[df2$X12 == "a.a" | df2$X12 == "b.b",] # aa vs. bb
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
aa_bb = a[grep(":alt", rownames(a)),]

df3 <- df2[df2$X12 == "a.b" | df2$X12 == "b.a",] # ab vs. ba
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
ab_ba = a[grep(":alt", rownames(a)),]

df3 <- df2[df2$X12 == "a.b" | df2$X12 == "b.b",] # ab vs. bb
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
ab_bb = a[grep(":alt", rownames(a)),]

df3 <- df2[df2$X12 == "b.a" | df2$X12 == "b.b",] # ba vs. bb
df3$X12 = factor(df3$X12)
contrasts(df3$X12) <- "contr.sum"
m = glmer(Y ~ X12*alt + (1|S), data=df3, family=poisson)
a = Anova(m, type=3)
ba_bb = a[grep(":alt", rownames(a)),]

p.adjust(c(
  aa_ab$`Pr(>Chisq)`, 
  aa_ba$`Pr(>Chisq)`, 
  aa_bb$`Pr(>Chisq)`, 
  ab_ba$`Pr(>Chisq)`, 
  ab_bb$`Pr(>Chisq)`, 
  ba_bb$`Pr(>Chisq)`), method="holm")

# Barplot
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://stackoverflow.com/questions/51892875/how-to-increase-the-space-between-grouped-bars-in-ggplot2
df2 <- as.data.frame(xtabs(~ X1+X2+Y, data=df)) # build a freq table
df2$X12 = with(df2, interaction(X1, X2))
df2$X12 = factor(df2$X12, levels=c("a.a","a.b","b.a","b.b"))
df2 <- df2[order(df2$X1, df2$X2),] # sort df2 alphabetically by X1, X2
ggplot(data=df2, aes(x=X12, y=Freq, fill=Y)) + theme_minimal() + 
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
  # create the barplots side-by-side
  geom_col(width=0.75, position=position_dodge(width=0.75), alpha=0.60) +
  # place text labels on each bar
  geom_text(aes(label=Freq), position=position_dodge(width=0.75), vjust=1.2, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1.X2", y="Count") + 
  # change the order of the bars on the x-axis
  scale_x_discrete(limits=c("a.a","a.b","b.a","b.b")) + 
  # set the scale, breaks, and labels on the y-axis
  scale_y_continuous(breaks=seq(0,10,by=2), labels=seq(0,10,by=2), limits=c(0,10), oob=rescale_none) + 
  # set the name, labels, and colors for the boxes
  scale_fill_manual(name="Y", labels=c("x","y","z"), values=c("#69b3a2","#404080","#e69f00"))


#
# Ordinal
#
library(ordinal) # for clmm
library(RVAideMemoire) # for Anova.clmm
df <- read.csv("data/GLMM/2F2LWs_ordinal.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
df$Y = ordered(df$Y) # ordinal response
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
df2 <- as.data.frame(df) # Anova.clmm fails without this
m = clmm(Y ~ X1*X2 + (1|S), data=df2, link="logit") # or "probit"
Anova.clmm(m)

## Ordinal post hoc test
# df has subjects (S), two factors (X1,X2) each w/levels (a,b), and ordinal response (Y)
library(multcomp) # for adjusted
library(emmeans) # for as.glht, pairs
summary(as.glht(pairs(emmeans(m, ~ X1*X2))), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Ordinal histograms-as-barplots
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://stackoverflow.com/questions/51892875/how-to-increase-the-space-between-grouped-bars-in-ggplot2
df2 <- as.data.frame(xtabs(~ X1+X2+Y, data=df)) # build a freq table
df2$X12 = with(df2, interaction(X1, X2))
df2$X12 = factor(df2$X12, levels=c("a.a","a.b","b.a","b.b"))
df2 <- df2[order(df2$X1, df2$X2),] # sort df2 alphabetically by X1, X2
ggplot(data=df2, aes(x=Y, y=Freq, fill=X12)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=14, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font styles for the legend
  theme(legend.title = element_text(face="bold", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  theme(legend.text  = element_text(face="plain", color="black", size=14, hjust=0.5, vjust=0.0, angle=0)) +
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # use a bar plot to just plot the value for each 1-7 in (X1,X2)
  geom_col(width=0.95, alpha=0.60) + 
  #geom_bar(stat="identity", width=0.95, alpha=0.60) + # equivalent
  # place text labels on each bar
  geom_text(aes(label=Freq), vjust=1.2, color="black", size=3.5) +
  # create a grid of plots by (X1,X2), one for each histogram
  #facet_grid(X12 ~ .) +  # 4x1 stack
  #facet_grid(. ~ X12) +  # 1x4 row
  facet_grid(X2 ~ X1) +   # 2x2 grid
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080", "#e69f00", "darkred")) + 
  # set the labels for the title, each axis, and the legend
  labs(title="Responses by X1, X2", x="Likert (1-7)", y="Count") + 
  guides(fill=guide_legend(title="X1.X2")) + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=seq(1,7,by=1)) +
  scale_y_continuous(breaks=seq(0,5,by=1), minor_breaks=seq(0,5,by=1), labels=seq(0,5,by=1), limits=c(0,5))


#
# Poisson
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_poisson.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmer(Y ~ X1*X2 + (1|S), data=df, family=poisson)
Anova(m, type=3)

## Poisson post hoc test
# df has subjects (S), two factors (X1,X2) each w/levels (a,b), and count response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Interaction plot
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
df2 <- ddply(df, ~ X1*X2, function(d) # make a summary data table
  c(NROW(d$Y),
    sum(is.na(d$Y)),
    sum(!is.na(d$Y)),
    mean(d$Y, na.rm=TRUE),
    sd(d$Y, na.rm=TRUE),
    median(d$Y, na.rm=TRUE),
    IQR(d$Y, na.rm=TRUE)))
colnames(df2) <- c("X1","X2","Rows","NAs","NotNAs","Mean","SD","Median","IQR")
ggplot(data=df2, aes(x=X1, y=Mean, color=X2, group=X2)) + theme_minimal() + 
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
  # create the plot lines, points, and error bars
  geom_line(aes(linetype=X2), position=position_dodge(0.05)) + 
  geom_point(aes(shape=X2, size=X2), position=position_dodge(0.05)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=position_dodge(0.05), width=0.1) + 
  # place text labels on each bar
  geom_text(aes(label=sprintf("%.2f (±%.2f)", Mean, SD)), position=position_dodge(0.05), hjust=0.0, vjust=-1.0, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,9,by=2), labels=seq(0,9,by=2), limits=c(0,9), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))


#
# Zero-Inflated Poisson
#
library(glmmTMB) # for glmmTMB
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_zipoisson.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmmTMB(Y ~ X1*X2 + (1|S), data=df, family=poisson, ziformula=~X1*X2)
Anova(m, type=3)

## Zero-Inflated Poisson post hoc test
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and count response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, fill=X1, color=X2)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # remove the legend
  theme(legend.position="none") + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(binwidth=1, breaks=seq(-0.5, 10.5, by=1), alpha=0.25) + 
  stat_bin(aes(y=..count.., label=..count..), binwidth=1, geom="text", vjust=-0.5) + 
  # create stacked plots by X, one for each histogram
  facet_grid(X1+X2 ~ .) + 
  # determine the outline and fill color values of each histogram
  scale_color_manual(values=c("darkblue","darkred")) + 
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(-0.5, 10.5, by=1), labels=seq(0, 11, by=1), limits=c(-0.5, 10.5)) +
  scale_y_continuous(breaks=seq(0,10,by=2), labels=seq(0,10,by=2), limits=c(0,10))


#
# Negative Binomial
#
library(lme4) # for glmer.nb
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_negbin.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmer.nb(Y ~ X1*X2 + (1|S), data=df)
Anova(m, type=3)

## Negative Binomial post hoc test
# df has subjects (S), two factors (X1,X2) each w/levels (a,b), and count response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Interaction plot
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
df2 <- ddply(df, ~ X1*X2, function(d) # make a summary data table
  c(NROW(d$Y),
    sum(is.na(d$Y)),
    sum(!is.na(d$Y)),
    mean(d$Y, na.rm=TRUE),
    sd(d$Y, na.rm=TRUE),
    median(d$Y, na.rm=TRUE),
    IQR(d$Y, na.rm=TRUE)))
colnames(df2) <- c("X1","X2","Rows","NAs","NotNAs","Mean","SD","Median","IQR")
ggplot(data=df2, aes(x=X1, y=Mean, color=X2, group=X2)) + theme_minimal() + 
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
  # create the plot lines, points, and error bars
  geom_line(aes(linetype=X2), position=position_dodge(0.05)) + 
  geom_point(aes(shape=X2, size=X2), position=position_dodge(0.05)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=position_dodge(0.05), width=0.1) + 
  # place text labels on each bar
  geom_text(aes(label=sprintf("%.2f (±%.2f)", Mean, SD)), position=position_dodge(0.05), hjust=0.0, vjust=-1.0, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,30,by=5), labels=seq(0,30,by=5), limits=c(0,30), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))


#
# Zero-Inflated Negative Binomial
#
library(glmmTMB) # for glmmTMB
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_zinegbin.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmmTMB(Y ~ X1*X2 + (1|S), data=df, family=nbinom2, ziformula=~X1*X2)
Anova(m, type=3)

## Zero-Inflated Negative Binomial post hoc test
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and count response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, fill=X1, color=X2)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # remove the legend
  theme(legend.position="none") + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(binwidth=1, breaks=seq(-0.5, 24.5, by=1), alpha=0.25) + 
  stat_bin(aes(y=..count.., label=..count..), binwidth=1, geom="text", vjust=-0.5) + 
  # create stacked plots by X, one for each histogram
  facet_grid(X1+X2 ~ .) + 
  # determine the outline and fill color values of each histogram
  scale_color_manual(values=c("darkblue","darkred")) + 
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(-0.5, 24.5, by=1), labels=seq(0, 25, by=1), limits=c(-0.5, 24.5)) +
  scale_y_continuous(breaks=seq(0,8,by=2), labels=seq(0,8,by=2), limits=c(0,8))


#
# Gamma, including Exponential
#
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/GLMM/2F2LWs_gamma.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = glmer(Y ~ X1*X2 + (1|S), data=df, family=Gamma)
# family=Gamma(link="log") is often used
Anova(m, type=3)

## Gamma post hoc test
# df has subjects (S), two factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X1*X2, adjust="tukey", mode="linear.predictor", type="Score"))

# Interaction plot
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
df2 <- ddply(df, ~ X1*X2, function(d) # make a summary data table
  c(NROW(d$Y),
    sum(is.na(d$Y)),
    sum(!is.na(d$Y)),
    mean(d$Y, na.rm=TRUE),
    sd(d$Y, na.rm=TRUE),
    median(d$Y, na.rm=TRUE),
    IQR(d$Y, na.rm=TRUE)))
colnames(df2) <- c("X1","X2","Rows","NAs","NotNAs","Mean","SD","Median","IQR")
ggplot(data=df2, aes(x=X1, y=Mean, color=X2, group=X2)) + theme_minimal() + 
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
  # create the plot lines, points, and error bars
  geom_line(aes(linetype=X2), position=position_dodge(0.05)) + 
  geom_point(aes(shape=X2, size=X2), position=position_dodge(0.05)) + 
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), position=position_dodge(0.05), width=0.1) + 
  # place text labels on each bar
  geom_text(aes(label=sprintf("%.2f (±%.2f)", Mean, SD)), position=position_dodge(0.05), hjust=0.0, vjust=-1.0, color="black", size=3.5) +
  # set the labels for the title and each axis
  labs(title="Y by X1, X2", x="X1", y="Y") + 
  # set the ranges and value labels for each axis
  scale_x_discrete(labels=c("a","b")) + 
  scale_y_continuous(breaks=seq(0,9,by=2), labels=seq(0,9,by=2), limits=c(0,9), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))

