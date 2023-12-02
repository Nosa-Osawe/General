###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### November 13, 2019
### Updated: 2/15/2022
###

###
### Parametric.R
### (Parametric Analyses of Differences)
###
library(ggplot2)
library(ggthemes)
library(scales)
library(plyr)


##
## One Factor
##

## Independent-samples t-test
# df has subjects (S), one between-Ss factor (X) w/levels (a,b), and continuous response (Y)
df <- read.csv("data/Parametric/1F2LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a 2-level factor
t.test(Y ~ X, data=df, var.equal=TRUE) # use var.equal=FALSE if heteroscedastistic


## Paired-samples t-test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b), and continuous response (Y)
library(reshape2) # for dcast
df <- read.csv("data/Parametric/1F2LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X = factor(df$X) # X is a 2-level factor
df2 <- dcast(df, S ~ X, value.var="Y") # make wide-format table
t.test(df2$a, df2$b, paired=TRUE) # homoscedasticity is irrelevant for a paired-samples t-test


## One-way ANOVA
# df has subjects (S), one between-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
df <- read.csv("data/Parametric/1F3LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a 3-level factor
m = aov(Y ~ X, data=df) # fit model
anova(m)

## One-way ANOVA post hoc test
# df has subjects (S), one between-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(multcomp) # for glht, mcp
summary(glht(m, mcp(X="Tukey")), test=adjusted(type="holm")) # m is from aov
# the following also performs the same contrast tests
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X)), test=adjusted(type="holm"))
# or, using the Tukey HSD correction instead of Holm's
summary(emmeans(m, pairwise ~ X, adjust="tukey", mode="linear.predictor", type="Score"))


## One-way repeated measures ANOVA
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(ez) # for ezANOVA
df <- read.csv("data/Parametric/1F3LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X = factor(df$X) # X is a 3-level factor
m = ezANOVA(dv=Y, within=c(X), wid=S, type=3, data=df) # use c() for >1 factors
m$Mauchly # p<.05 indicates a sphericity violation
m$ANOVA # use if no violation
# if there is a sphericity violation, report the Greenhouse-Geisser or Huynh-Feldt correction
p = match(m$Sphericity$Effect, m$ANOVA$Effect) # positions of within-Ss effects in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[p] # Greenhouse-Geisser DFs
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[p]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[p] # Huynh-Feldt DFs
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[p]
m$Sphericity # show results

# the following also performs the equivalent repeated measures ANOVA, but does not address sphericity
m = aov(Y ~ X + Error(S/X), data=df)
summary(m)

## One-way repeated measures ANOVA post hoc test
# df has subjects (S), one within-Ss factor (X) w/levels (a,b,c), and continuous response (Y)
library(reshape2) # for dcast
df2 <- dcast(df, S ~ X, value.var="Y") # make wide-format table
ab = t.test(df2$a, df2$b, paired=TRUE) # a vs. b
ac = t.test(df2$a, df2$c, paired=TRUE) # a vs. c
bc = t.test(df2$b, df2$c, paired=TRUE) # b vs. c
p.adjust(c(ab$p.value, ac$p.value, bc$p.value), method="holm")



##
## Multiple Between-Ss Factors
##

## Factorial ANOVA
# df has subjects (S), two between-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(ez) # for ezANOVA
df <- read.csv("data/Parametric/2F2LBs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
m = ezANOVA(dv=Y, between=c(X1,X2), wid=S, type=3, data=df) # use c() for >1 factors
m$Levene # if p<.05, we have a violation of homoscedasticity, so use a White-adjusted ANOVA
m= ezANOVA(dv=Y, between=c(X1,X2), wid=S, type=3, data=df, white.adjust=TRUE) # heteroscedastic
m$ANOVA

# the following also performs the same factorial ANOVA
m = aov(Y ~ X1*X2, data=df)
anova(m)

## Linear Model (LM)
# df has subjects (S), two between-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
df <- read.csv("data/Parametric/2F2LBs.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
m = lm(Y ~ X1*X2, data=df)
anova(m)

## Factorial between-Ss post hoc tests
# df has subjects (S), two between-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm")) # m is from aov or lm
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
  scale_y_continuous(breaks=seq(0,20,by=5), labels=seq(0,20,by=5), limits=c(0,20), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))



##
## Multiple Within-Ss Factors
##

## Factorial repeated measures ANOVA
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(ez) # for ezANOVA
df <- read.csv("data/Parametric/2F2LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
m = ezANOVA(dv=Y, within=c(X1,X2), wid=S, type=3, data=df) # use c() for >1 factors
m$Mauchly # p<.05 indicates a sphericity violation
m$ANOVA # use if no violation
# if there is a sphericity violation, report the Greenhouse-Geisser or Huynh-Feldt correction
p = match(m$Sphericity$Effect, m$ANOVA$Effect) # positions of within-Ss effects in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[p] # Greenhouse-Geisser DFs
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[p]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[p] # Huynh-Feldt DFs
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[p]
m$Sphericity # show results

# the following also performs the equivalent repeated measures ANOVA, but does not address sphericity
m = aov(Y ~ X1*X2 + Error(S/(X1*X2)), data=df)
summary(m)

## Factorial repeated measures ANOVA post hoc test
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(reshape2) # for dcast
df2 <- dcast(df, S ~ X1 + X2, value.var="Y") # make wide-format table
aa_ab = t.test(df2$a_a, df2$a_b, paired=TRUE) # aa vs. ab
aa_ba = t.test(df2$a_a, df2$b_a, paired=TRUE) # aa vs. ba
aa_bb = t.test(df2$a_a, df2$b_b, paired=TRUE) # aa vs. bb
ab_ba = t.test(df2$a_b, df2$b_a, paired=TRUE) # ab vs. ba
ab_bb = t.test(df2$a_b, df2$b_b, paired=TRUE) # ab vs. bb
ba_bb = t.test(df2$b_a, df2$b_b, paired=TRUE) # ba vs. bb
p.adjust(c(aa_ab$p.value, aa_ba$p.value, aa_bb$p.value, ab_ba$p.value, ab_bb$p.value, ba_bb$p.value), method="holm")


## Linear Mixed Model (LMM)
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova
df <- read.csv("data/Parametric/2F2LWs.csv")
df$S = factor(df$S) # Subject id is nominal
df$X1 = factor(df$X1) # X1 is a 2-level factor
df$X2 = factor(df$X2) # X2 is a 2-level factor
contrasts(df$X1) <- "contr.sum"
contrasts(df$X2) <- "contr.sum"
m = lmer(Y ~ X1*X2 + (1|S), data=df)
Anova(m, type=3, test.statistic="F")

## Linear Mixed Model (LMM) post hoc tests
# df has subjects (S), two within-Ss factors (X1,X2) each w/levels (a,b), and continuous response (Y)
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ X1*X2)), test=adjusted(type="holm")) # m is from lmer
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
  scale_y_continuous(breaks=seq(0,20,by=5), labels=seq(0,20,by=5), limits=c(0,20), oob=rescale_none) + 
  # set the name, labels, and colors for the traces
  scale_color_manual(name="X2", labels=c("a","b"), values=c("red", "blue")) +
  # set the size and shape of the points
  scale_size_manual(values=c(4,4)) +
  scale_shape_manual(values=c(16,10)) +
  # set the linetype of the lines
  scale_linetype_manual(values=c("solid", "longdash"))

