# Sherlin Whaley - 2019
# Bethel Tech - Powered by Woz U
# Predicting Student Grades



# Loading Packages
library(IDPmisc)
library(dplyr)
library(rcompanion)
library(car)
library(readr)


# Data Wrangling

Grades = data_set_2[1:901, 10:21]


# Normal Histogram for Att8Est is slightly positvely skewed.  
plotNormalHistogram(Grades2$Att8Est, xlab = "Att8Est Grades")

# Att8Est is now more normally distributed.
Grades$Att8EstSQRT <- sqrt(Grades$Att8Est)
plotNormalHistogram(Grades$Att8EstSQRT, xlab = "Att8EstSQRT Transformed")



#Normal Histogram for Att8Act is Normally Distributed
plotNormalHistogram(Grades2$Att8Act, xlab = "Att8Act Grades")



#Normal Histogram for Att8Diff is Normally Distributed
plotNormalHistogram(Grades2$Att8Diff, xlab = "Att8Diff Grades")



#Normal Histogram for EngEst is Normally Distributed
plotNormalHistogram(Grades2$EngEst, xlab = "EngEst Grades")



#Normal Histogram for EngAct is Normally Distributed
plotNormalHistogram(Grades2$EngAct, xlab = "EngAct Grades")



#Normal Histogram for EngDiff is Normally Distributed
plotNormalHistogram(Grades2$EngDiff, xlab = "EngDiff Grades")



#Normal Histogram for MathsEst is Normally Distributed
plotNormalHistogram(Grades2$MathsEst, xlab = "MathsEst Grades")


#Normal Histogram for MathsAct is Normally Distributed
plotNormalHistogram(Grades2$MathsAct, xlab = "MathsAct Grades")




#Normal Histogram for MathsDiff is Normally Distributed
plotNormalHistogram(Grades2$MathsDiff, xlab = "MathsDiff Grades")


#Normal Histogram for EbaccEst is Normally Distributed
plotNormalHistogram(Grades2$EbaccEst, xlab = "EbaccEst Grades")



#Normal Histogram for EbaccAct is slightly positively skewed.
plotNormalHistogram(Grades2$EbaccAct, xlab = "EbaccAct Grades")

# EbaccAct is now more normally distributed.
Grades$EbaccActSQRT <- sqrt(Grades$EbaccAct)
plotNormalHistogram(Grades$EbaccActSQRT, xlab = "EbaccActSQRT Transformed")



#Normal Histogram for EbaccDiff is Normally Distributed
plotNormalHistogram(Grades2$EbaccDiff, xlab = "EbaccDiff Grades")


#Dropped skewed variables
Grades2 = subset(Grades, select = -c(Att8Est, EbaccAct))


# Step-Wise Regression

#Backwards Elimination
FitAll = lm(Att8Act ~ ., data = Grades2)

summary(FitAll)

step(FitAll, direction = 'backward')



fitsome = glm(formula = Att8Act ~ Att8Diff + EngAct + EngDiff + MathsAct + 
               MathsDiff + EbaccEst + EbaccDiff + EbaccActSQRT, data = Grades2, 
             na.action = na.exclude)

summary(fitsome)


#Forward Selection
fitstart = lm(Att8Act ~ 1, data = Grades2)

summary(fitstart)


step(fitstart, direction = 'forward' ,scope=formula(FitAll))

fitsome2 = lm(formula = Att8Act ~ Att8Diff + EngEst + EngAct + EngDiff + 
                MathsEst + MathsAct + MathsDiff + EbaccEst + EbaccDiff + 
                Att8EstSQRT + EbaccActSQRT, data = Grades2)

summary(fitsome2)


#Combined Forward Selection and Backward Elimination
step(fitstart, direction = 'both' ,scope=formula(FitAll))

fitsome3 = glm(formula = Att8Act ~ EbaccActSQRT + EngAct + MathsAct + Att8Diff + 
                MathsDiff + EngDiff + EbaccEst + EbaccDiff, data = Grades2)

summary(fitsome3)

# Testing for Assumptions

#Fligner's test shows non-signifigance, therefore, we can proceed with the basic anova.
fligner.test(Att8Act ~ PP, data=Grades2A)

ANOVA1 <- aov(Grades2A$Att8Act ~ Grades2A$PP)

summary(ANOVA1)

#Anova shows signifigance meaning that there is a correlation between the actual grades and the pupil premium.  



#Fligner's test shows non-signifigance, therefore, we can proceed with the basic anova.
fligner.test(Att8Act ~ EAL, data = Grades2A)

ANOVA2 <- aov(Grades2A$Att8Act ~ Grades2A$EAL)

summary(ANOVA2)

#Anova shows signifigance meaning that there is a correlation between the actual grades 
#and students that have English as a second language.  




#Fligner's test shows non-signifigance, therefore, we can proceed with the basic anova.
fligner.test(Att8Act ~ SEN, data = Grades2A)

ANOVA3 <- aov(Grades2A$Att8Act ~ Grades2A$SEN)

summary(ANOVA3)

#Anova shows signifigance meaning that there is a correlation between the actual grades 
# and students that have special educational needs.

# write_csv(Grades2, path = "Grades3.csv")

