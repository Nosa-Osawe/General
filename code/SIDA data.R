rm(list = ls()) # clears R's head
# Load useful packages
install.packages("PerformanceAnalytics")

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggstance)
library(huxtable)
library(corrplot)
library(jtools)
library(caret)
library(PerformanceAnalytics)
################################################################################

# Load data
sida <- read.csv("C:\\Users\\user\\Desktop\\data analytics\\R\\TEXTBOOKS\\dataset\\sidaT.csv")

View(sida) # view data
str(sida)
attach(sida)
# lets compare mean/median herbivory in the Focal and NN plant

shapiro.test(sida$percHerbPlant) # test of normality
shapiro.test(sida$NNpercHerbPlant) # data is not normally distributed
hist(sida$NNpercHerbPlant)


# check for outliers
fph<- ggplot(sida, aes(percHerbPlant)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "percHerbPlant") +
  theme_bw()+
  coord_flip()

nph<- ggplot(sida, aes(NNpercHerbPlant)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "NNpercHerbPlant", x= "") +
  theme_bw()+
  coord_flip() # coord_flip changes the orientation of the plot

nlv<- ggplot(sida, aes(numLeaves)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "numleaves", x= "") +
  theme_bw()+
  coord_flip() 


grid.arrange(fph, nph, nrow=1)

wilcox.test(percHerbPlant ,NNpercHerbPlant, paired = FALSE)
wilcox.test(NNpercHerbPlant, percHerbPlant , paired = FALSE)
# Result
# data:  NNpercHerbPlant and percHerbPlant
# W = 273.5, p-value = 0.007978

#lets try to predict percentage herbivory by creating
# a multiple regression model



# outlier detection

ggplot(sida, aes(focalPlantCover)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "focalPlantCover") +
  theme_bw()+
  coord_flip()

ggplot(sida, aes(otherPlantCover)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "otherPlantCover") +
  theme_bw()+
  coord_flip()

ggplot(sida, aes(numPlantsinQuad)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "numPlantsinQuad") +
  theme_bw()+
  coord_flip()

ggplot(sida, aes(ht_cm)) + 
  geom_boxplot(fill= "white", color = "black", outlier.size = 3) + 
  labs(title = "ht_cm") +
  theme_bw()+
  coord_flip()

# Correlation plot.

corrplot(cor(sida %>% 
               select(focalPlantCover,otherPlantCover,numPlantsinQuad, ht_cm,
                      numLeaves, numLeavesHerb, percHerbPlant,
                      NNdist, NNht_cm, NNnumLeaves, NNpercHerbPlant)),
         method ='color' ,
         title = 'plot of correlation',
         addCoef.col='black')


corrplot(cor(sida %>% 
               select(focalPlantCover,otherPlantCover,numPlantsinQuad, ht_cm,
                      numLeaves, numLeavesHerb, percHerbPlant,
                      NNdist, NNht_cm, NNnumLeaves, NNpercHerbPlant)),
         method ='color' ,
         title = 'plot of correlation',
         addCoef.col='black',
         type="upper", order="hclust")




chart.Correlation(sida %>% 
                    select(focalPlantCover,otherPlantCover,numPlantsinQuad, ht_cm,
                           numLeaves, numLeavesHerb, percHerbPlant,
                           NNdist, NNht_cm, NNnumLeaves, NNpercHerbPlant), 
                  histogram=TRUE, pch=19)

# Normality test

shapiro.test(as.numeric(sida$ht_cm))
hist(as.integer(sida$ht_cm))
qqnorm(as.numeric(sida$ht_cm))
qqline(as.numeric(sida$ht_cm))

qqnorm(as.numeric(sida$percLf1))
qqline(as.numeric(sida$percLf1)) # not a normal distribution at all

qqnorm(as.numeric(sida$percLf2))
qqline(as.numeric(sida$percLf2))# not a normal distribution at all

qqnorm(as.numeric(sida$percLf3))
qqline(as.numeric(sida$percLf3))# not a normal distribution at all

qqnorm(as.numeric(sida$percLf4))
qqline(as.numeric(sida$percLf4)) # not a normal distribution at all

qqnorm(as.numeric(sida$percLf5))
qqline(as.numeric(sida$percLf5)) # not a normal distribution at all

qqnorm(as.numeric(sida$percLf6))
qqline(as.numeric(sida$percLf6)) # not a normal distribution at all

qqnorm(as.numeric(sida$percLf7))
qqline(as.numeric(sida$percLf7))  # not a normal distribution at all

qqnorm(as.numeric(sida$percLf8))
qqline(as.numeric(sida$percLf8))  # not a normal distribution at all

qqnorm(as.numeric(sida$percLf9))
qqline(as.numeric(sida$percLf9))  # not a normal distribution at all

qqnorm(as.numeric(sida$percLf10))
qqline(as.numeric(sida$percLf10))   # not a normal distribution at all


head(sida %>% 
      select(percLf1:percLf10), 20)

######################################################################################

# lets build a multiple regression model to predict percHerbPlant

lm1 <-lm(percHerbPlant~numLeavesHerb + numPlantsinQuad+ numLeaves+ ht_cm)
lm2 <-lm(percHerbPlant~numLeavesHerb + ht_cm + numPlantsinQuad)

# lets build a multiple regression model to predict numleaveHerb
lm3 <- lm(numLeavesHerb~percHerbPlant + ht_cm + numLeaves)
lm4 <- lm(numLeavesHerb~ percHerbPlant + ht_cm + NNnumLeaves + NNht_cm)
export_summs(lm1,lm2)
export_summs(lm3, lm4)


# building Scatterplot Matrices
sida %>% 
  select(focalPlantCover,otherPlantCover,numPlantsinQuad, ht_cm,
         numLeaves, numLeavesHerb, percHerbPlant,
         NNdist, NNht_cm, NNnumLeaves, NNpercHerbPlant) %>% pairs()
