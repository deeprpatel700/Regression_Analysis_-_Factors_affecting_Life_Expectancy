
# Name: Deep Patel
# Project: Effects of Economic and Social Factors on Life Expectancy

# Loading Clean Dataset
setwd("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6357 Linear Models\\6357 Project")
getwd()
life_exp<- read.csv('LifeExpectancy_CLEAN.csv')

# Loading Necessary Libraries
library(dplyr)
library(class)
library(ISLR)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(ALSM)
library(MASS)


life_select<- life_exp[,c('Lifeexpectancy','AdultMortality','Alcohol','HepatitisB','Measles',
                          'BMI','Polio','Totalexpenditure','Diphtheria', 'under.fivedeaths',
                          'thinness1.19years','thinness5.9years', 'Population', 'infantdeaths',
                          'Incomecompositionofresources','Schooling',
                          'Country_Status','log_PE','log_HIV.AIDS','log_GDP')]

colnames(life_select)
names(life_select)[15]<- 'Income.Comp'

#-------------------------------------------------------------------------------
# Full Model (19 predictors & first stepwise regression)
full.model  <- lm(Lifeexpectancy ~ ., data = life_select)
fullmodel.start <- lm(Lifeexpectancy ~ 1, data = life_select)
summary(full.model)


step(full.model,direction = "backward")  
step(fullmodel.start, direction = "forward", scope = formula(full.model))
step(fullmodel.start, direction = "both",    scope = formula(full.model))
#-------------------------------------------------------------------------------

# Correlation Matrix & Heatmap after taking needed logs of select predictors
life_cor2<- cor(life_select)
ggcorrplot(life_cor2, type='lower', lab=T)

#From heatmap, notice that predictors Adult Mortality, BMI, Income.Comp,
#  Schooling, log_PE, log_HIV.AIDS, and log_GDP shows moderate to high correlation. 
# Anything below 0.5 is discarded as it represents weak or no correlation.

#-------------------------------------------------------------------------------
# Scatterplots
attach(life_select)
par(mfrow=c(2,3))
plot(Alcohol,Lifeexpectancy, pch=3, col='darkgreen', 
     main='Alcohol vs. Life Expectancy')
plot(infantdeaths,Lifeexpectancy, pch=12, col='red', 
     main='Infant Deaths vs. Life Expectancy')
plot(Income.Comp,Lifeexpectancy, pch=2, col='brown', 
     main='Income Composition of Resources vs. Life Expectancy')
plot(log_PE,Lifeexpectancy, pch=9, col='deeppink', 
     main='log(Percentage Expenditure) vs. Life Expectancy')
plot(log_GDP,Lifeexpectancy, pch=19, col='purple',
     main='log(GDP) vs. Life Expectancy')
plot(BMI,Lifeexpectancy, pch= 24, col='sienna1',
     main='BMI vs. Life Expectancy')

#Extra plots
plot(AdultMortality,Lifeexpectancy, pch=1, col='dodgerblue',
     main='Adult Mortality vs. Life expectancy')
plot(Schooling,Lifeexpectancy, pch= 21, col='navyblue',
     main='Schooling vs. Life expectancy')
plot(log_HIV.AIDS,Lifeexpectancy, pch=17, col='orange2',
     main='log(HIV.AIDS) vs. Life expectancy')
plot(HepatitisB,Lifeexpectancy)
plot(Measles,Lifeexpectancy)
plot(under.fivedeaths,Lifeexpectancy)
plot(Polio,Lifeexpectancy)
plot(Totalexpenditure,Lifeexpectancy)
plot(Diphtheria,Lifeexpectancy)
plot(Population,Lifeexpectancy)
plot(thinness1.19years,Lifeexpectancy)
plot(thinness5.9years,Lifeexpectancy)

#-------------------------------------------------------------------------------

# 6 predictors selected- Reduced Model 1
#(from Stpewise backward regression & corr coeff of 0.5 and above)
life_select3<- life_select[,c('Lifeexpectancy','AdultMortality',
                              'Income.Comp','Schooling',
                              'log_PE','log_HIV.AIDS','log_GDP')]

colnames(life_select)
# Second Step wise regression (to make sure it doesn't eliminate any more predictors)
reduced_model1<- lm(Lifeexpectancy~.,data=life_select3)
red1_start<- lm(Lifeexpectancy~1, data=life_select3)
summary(reduced_model1)
shapiro.test(resid(reduced_model1))
par(mfrow=c(1,2))
plot(reduced_model1)

step(reduced_model1, direction = "backward") 
step(red1_start, direction = "forward", scope = formula(reduced_model1))
step(red1_start, direction = "both",    scope = formula(reduced_model1))


#checking multicollinearity in reduced model1- remove Income.Comp & log_GDP?
vif(reduced_model1) 

#-------------------------------------------------------------------------------
# 5 predictors- removed Income.Comp
life_select4<- life_select[,c('Lifeexpectancy','AdultMortality',
                              'Schooling','log_PE','log_HIV.AIDS','log_GDP')]
reduced_model2<- lm(Lifeexpectancy~.,data=life_select4)
summary(reduced_model2)
plot(reduced_model2)
shapiro.test(resid(reduced_model2))
vif(reduced_model2)
#---------------------------------------------------------------------------
# 4 predictors- removed Income.Comp & log(GDP)
life_select5<- life_select[,c('Lifeexpectancy','AdultMortality','Schooling',
                              'log_PE')]
reduced_model3<- lm(life_select5)
summary(reduced_model3)
plot(reduced_model3)
shapiro.test(resid(reduced_model3))
vif(reduced_model3)
#----------------------------------------------------------------------------

#Trying BOXCOX Transformation- Not sure if I did it correctly
rm2<- boxcox(Lifeexpectancy~.,data=life_select5)
rm2_table<- cbind(rm2$x,rm2$y)
sorted<- rm2_table[order(-rm2$y),]
head(sorted)
post_boxcox<- lm(Lifeexpectancy^(1.2)~.,data=life_select5)
plot(post_boxcox)
shapiro.test(resid(post_boxcox))
boxplot.stats(reduced_model3$residuals)$out


#Looking for studentized deleted residuals (outliers)
res<- rstudent(reduced_model3)
plot(rstudent(reduced_model3),type = "o",xlab = "Case Index")
text(rstudent(reduced_model3), labels=rownames(life_select5), cex=0.5, font=2)
title("(a) Studentized Deleted residuals")
order(-rstudent(reduced_model3))
-rstudent(reduced_model3)[c(244,275,709,765,607,526,266,402,819,274, 
                            863, 302, 637, 249, 386,536,
                            539,741,738,736,487,909,621,14,910,490)]

# H0= outliers; Ha= no outliers
qt(1-(0.05/(2*911)),911-5-1)  #t-statistic using bonferroni outlier procedure
qt(0.9875,905) #critical value for alpha=0.05
# No outliers as |t-stat|>t-critical value


hat=hatvalues(reduced_model3); hat
plot(hatvalues(reduced_model3),type = "o")
abline(h=2*(5)/911)   #2p/n
text(hatvalues(mylm), labels=rownames(df), cex=0.9, font=2)
title("(b) Leverage Values")

h=2*(5)/911; h  #rule of thumb
hat.df<- as.data.frame(hat)
hat_new<- filter(hat.df, hat>0.01097695)
# 84 observations are outlying according to rule of thumb

dffits.df<- as.data.frame(dffits(reduced_model3))
dffits_thres<- 2*sqrt(5/911)
good_dffits<- filter(dffits.df,abs(dffits(reduced_model3)) <dffits_thres)
good_list<-as.vector(good_dffits)
# shows 85 observations are influential 

DB<- as.data.frame(dfbetas(reduced_model3))
DB_thres<- 2/sqrt(911)
good_DB<- filter(DB,abs(log_HIV.AIDS) > DB_thres)
#Find and try deleting those 85 observations according to DFFITS
life_after<- life_select5[c(rownames(good_dffits)),]
life_afterlm<- lm(Lifeexpectancy~.,data=life_after)
par(mfrow=c(1,2))
plot(life_afterlm)
shapiro.test(resid(life_afterlm)) #This WORKS! p-value>0.05 (barely)


#-------------------------------------------------------------------

# Does Life expectancy significantly differ in developing countries and developed?
life_select6<- life_after[,c('Lifeexpectancy','AdultMortality','Schooling',
                              'log_PE','log_HIV.AIDS','Country_Status')]

#Full model- Includes developing countries
dev.lm<- lm(Lifeexpectancy~.,data=life_select6) 
anova(dev.lm) # Full model- includes developing countries
anova(reduced_model3) # Reduced model- does not include developing countries
anova(reduced_model3,dev.lm)
qf(0.95,1,906)

# |F-stat| < critical value. 
# Also p-value greater than 0.05 - Fail to Reject Null hypothesis.
# Conclusion- There is not significant evidence there is a difference in 
# life expectancy between developing and developed countries. 
#-------------------------------------------------------------------------------



