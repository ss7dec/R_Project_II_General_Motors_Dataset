####################### PROJECT - IIB  - General Motors Dataset #####################

# Data collected for several hundred used General Motors (GM) cars allows us to develop a
# multivariate regression model to determine car values based on a variety of characteristics such
# as mileage, make, model, cruise control, and so on.

# Develop a Multivariate Regression Model for the data collected for several hundred used
# * General Motors (GM) car values based on a variety of characteristics such as:
# * Price: suggested retail price for the used GM car
# * Mileage: number of miles the car has been driven
# * Make: manufacturer of the car such as Cadillac, Pontiac, and Chevrolet
# * Cylinder: number of cylinders in the engine
# * Liter: a more specific measure of engine size
# * Cruise: indicator variable representing whether the car has cruise control (1 = cruise)
# * Sound: indicator variable representing whether the car has upgraded speakers (1 = upgraded)
# * Leather: indicator variable representing whether the car has leather seats (1 = leather)


(package="datasets")
options(stringsAsFactors=FALSE) # to prevent strings or characters being converted to Factors
options(scipen=999)

library(data.table)
library(readxl)
library(gdata)
library(xlsx)
library(XLConnect)
library(corrplot)
library(gclus)
library(caTools)
library(Hmisc)
library(e1071)
library(lmtest)
library(car)
library(MASS)
library(ggpubr)
library(caret)
library(DAAG)
library(Metrics)
library(usdm)
library(earth)
library(gvlma)

######################################################

# To Import .csv file in R environment, there are different functions as given below:--

General_Motors<-fread("E://Back up 1/ACADGILD/Project - II/data.csv",header = TRUE, sep=",",stringsAsFactors =FALSE)
View(General_Motors)

General_Motors<-read.csv("E://Back up 1/ACADGILD/Project - II/data.csv",header = TRUE, sep=",", stringsAsFactors=FALSE)
View(General_Motors)

General_Motors<-read.table("E://Back up 1/ACADGILD/Project - II/data.csv",header = TRUE, sep=",",stringsAsFactors =FALSE)
View(General_Motors)

# To understand the given dataset/table in R environment, a few Basic Codes needs 
# to be executed as follows:------------

dim(General_Motors)

class(General_Motors) # to understand the structure of the given datset

str(General_Motors) # To understand the data structures of the given variables in the dataset

summary(General_Motors) # To understand the Descriptive Statistics of the given variables in the dataset

################################################################################

# Before performing any Predictive Modelling Technique, a few basic tests for the 
# Sample Dataset needs to be carried out as follows:-

# I) Normality Test - To check/assess for Normality of Data:

# If the sample size is large enough (n > 30), we can ignore the distribution 
# of the data and use Parametric tests.
# The central limit theorem tells us that the sampling distribution tends to 
# be normal if the sample is large enough (n > 30).

# However, to be consistent, normality can be checked by visual inspection
# using the following techniques:
# a) Normal plots (Histogram)
# b) Q-Q plot (quantile-quantile plot) or 
# c) Significance tests

# In the situations where the assumptions are violated, Non-Paramatric Tests 
# are recommended.

# a) Normal Plots (Histogram):
hist(General_Motors$Cylinder)
kurtosis(General_Motors$Cylinder)
skewness(General_Motors$Cylinder)

hist(General_Motors$Liter)
kurtosis(General_Motors$Liter)
skewness(General_Motors$Liter)

hist(General_Motors$Cruise)
kurtosis(General_Motors$Cruise)
skewness(General_Motors$Cruise)

hist(General_Motors$Sound)
kurtosis(General_Motors$Sound)
skewness(General_Motors$Sound)

hist(General_Motors$Leather)
kurtosis(General_Motors$Leather)
skewness(General_Motors$Leather)

hist(General_Motors$Price)
kurtosis(General_Motors$Price)
skewness(General_Motors$Price)


hist(General_Motors$Mileage)
kurtosis(General_Motors$Mileage)
skewness(General_Motors$Mileage)


# b) Q-Q plot (quantile-quantile plot) 
# To assess the Outliers in the given dataset--
layout(matrix(c(1),1)) # Layout of the plots

qqPlot(General_Motors, main="QQ Plot") #qq plot for studentized residuals 


qqplot(x = General_Motors$Cylinder, y = General_Motors$Price, main="QQ Plot")
qqplot(x = General_Motors$Liter, y = General_Motors$Price, main="QQ Plot")
qqplot(x = General_Motors$Cruise, y = General_Motors$Price, main="QQ Plot")
qqplot(x = General_Motors$Sound, y = General_Motors$Price, main="QQ Plot")
qqplot(x = General_Motors$Leather, y = General_Motors$Price, main="QQ Plot")
qqplot(x = General_Motors$Mileage, y = General_Motors$Price, main="QQ Plot")

require(ggpubr) 
ggqqplot(General_Motors$Cylinder)
ggqqplot(General_Motors$Liter)
ggqqplot(General_Motors$Cruise)
ggqqplot(General_Motors$Sound)
ggqqplot(General_Motors$Leather)
ggqqplot(General_Motors$Price)
ggqqplot(General_Motors$Mileage)

# c) Significance Test----
# In significance test, we compare the sample distribution to a normal one 
# in order to ascertain whether data show or not a serious deviation from normality.

# There are several methods for Normality test such as: 
# i) Kolmogorov-Smirnov (K-S) normality test and 
# ii) Shapiro-Wilk's test.

# Shapiro-Wilk's method is widely recommended for normality test and 
# it provides better power than K-S. It is based on the correlation 
# between the data and the corresponding normal scores.

# The Null hypothesis of such tests conveys that ???sample distribution is normal???. 
# If the test is significant, the distribution is non-normal.

# The R function shapiro.test() can be used to perform the Shapiro-Wilk 
# test of normality for one variable (univariate).

shapiro.test(General_Motors$Cylinder)

shapiro.test(General_Motors$Liter)

shapiro.test(General_Motors$Cruise)

shapiro.test(General_Motors$Sound)

shapiro.test(General_Motors$Leather)

shapiro.test(General_Motors$Price)

shapiro.test(General_Motors$Mileage)


# p value is less than 0.5 (i.e. Significance Value or Alpha value), hence
# we reject Null Hypothesis and instead accept Alternate Hypothesis.

# Null Hypothesis - The samples are taken from Normally Distributed Data.
# Alternate Hypotheis - The Samples are NOT taken from Normally Distributed Data

# To understand the Confidence Levels---

t_crit_val_Cylinder <- quantile(General_Motors$Cylinder,probs = c(0.684,0.945,0.99))
t_crit_val_Cylinder

t_crit_val_Liter <- quantile(General_Motors$Liter,probs = c(0.684,0.945,0.99))
t_crit_val_Liter

t_crit_val_Cruise <- quantile(General_Motors$Cruise,probs = c(0.684,0.945,0.99))
t_crit_val_Cruise

t_crit_val_Sound <- quantile(General_Motors$Sound,probs = c(0.684,0.945,0.99))
t_crit_val_Sound

t_crit_val_Leather <- quantile(General_Motors$Leather,probs = c(0.684,0.945,0.99))
t_crit_val_Leather

t_crit_val_Price <- quantile(General_Motors$Price,probs = c(0.684,0.945,0.99))
t_crit_val_Price

t_crit_val_Mileage <- quantile(General_Motors$Mileage,probs = c(0.684,0.945,0.99))
t_crit_val_Mileage


# II) Density Plot---

# Density Plot is used to check if the Response/Dependant i.e. y variable is 
# close to normality or not

library(e1071)
#par(mfrow=c(1, 3))  # divide graph area in 3 columns

plot(density(General_Motors$Cylinder), main="Density Plot: Advertise", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Cylinder), 2)))  # density plot for Cylinder
polygon(density(General_Motors$Cylinder), col="maroon")

plot(density(General_Motors$Liter), main="Density Plot: Liter", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Liter), 2)))  # density plot for Liter
polygon(density(General_Motors$Liter), col="yellow")

plot(density(General_Motors$Cruise), main="Density Plot: Cruise", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Cruise))))  # density plot for Cruise
polygon(density(General_Motors$Cruise), col="Sea Green")

plot(density(General_Motors$Sound), main="Density Plot: Sound", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Sound))))  # density plot for Sound
polygon(density(General_Motors$Sound), col="Navy Blue")

plot(density(General_Motors$Leather), main="Density Plot: Leather", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Leather))))  # density plot for Leather
polygon(density(General_Motors$Leather), col="Orange")

plot(density(General_Motors$Mileage), main="Density Plot: Mileage", ylab="Price", sub=paste("Skewness:", round(e1071::skewness(General_Motors$Mileage))))  # density plot for Mileage
polygon(density(General_Motors$Mileage), col="Magenta")

# III) To generate Box-plots-----

boxplot(Price~Cylinder,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Cylinder", ylab="Price")

boxplot(Price~Liter,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Liter", ylab="Price")


boxplot(Price~Cruise,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Cruise", ylab="Price")


boxplot(Price~Sound,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Sound", ylab="Price")


boxplot(Price~Leather,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Leather", ylab="Price")

boxplot(Price~Mileage,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Mileage", ylab="Price")

boxplot(Price~Make,data=General_Motors, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="General_Motors_Data", 
        xlab="Make", ylab="Price")

boxplot(General_Motors$Price)

# IV) Scatter Plots---------
# To generate a Scatter Plot----
plot(General_Motors$Cylinder,General_Motors$Price,xlab="Cylinder",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Cylinder))

# To generate a Scatter Plot----
plot(General_Motors$Liter,General_Motors$Price,xlab="Liter",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Liter))

# To generate a Scatter Plot----
plot(General_Motors$Cruise,General_Motors$Price,xlab="Cruise",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Cruise))

# To generate a Scatter Plot----
plot(General_Motors$Sound,General_Motors$Price,xlab="Sound",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Sound))

# To generate a Scatter Plot----
plot(General_Motors$Leather,General_Motors$Price,xlab="Leather",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Leather))

# To generate a Scatter Plot----
plot(General_Motors$Mileage,General_Motors$Price,xlab="Mileage",ylab="General_Motors_Price")

# To generate a linear regression model of the two variables 
#with the lm function, and then draw a trend line with abline.
abline(lm(General_Motors$Price ~ General_Motors$Mileage))

# Basic Scatterplot Matrix with all the variables in the given dataset----
pairs(~Price+Cylinder+Liter+Cruise+Sound+Leather+Mileage,data=General_Motors, 
      main="Simple Scatterplot Matrix")

# Correlation:
# To get Correlations & to compute Correlation Matrix--- --------------
abs(cor(General_Motors$Cylinder,General_Motors$Price))
abs(cor(General_Motors$Liter,General_Motors$Price))
abs(cor(General_Motors$Cruise,General_Motors$Price))
abs(cor(General_Motors$Sound,General_Motors$Price))
abs(cor(General_Motors$Leather,General_Motors$Price))
abs(cor(General_Motors$Mileage,General_Motors$Price))

# Significant correlations between Cylinder and Price (0.80) as well as
# Liter and Price (0.76)

summary(General_Motors)

Gen_Motors_Mx <- subset(General_Motors, select = -c(Make))
View(Gen_Motors_Mx)

Mx<-cor(Gen_Motors_Mx,method=c("pearson", "kendall", "spearman"))
Mx
Ro<-(round(Mx,2))
Ro

# Significant correlations between the following variables viz.:-
# Cylinder & Price (0.81), Liter and Price (0.76), Liter and Cylinder (0.95)

rcorr(as.matrix(Gen_Motors_Mx)) 


# Correlogram is used for generating Correlation Matrix.
# Corrplot() is used to plot the graph of Correlation Matrix

corrplot(Mx, method="pie")
corrplot(Mx, method="circle")
corrplot(Mx, method="square")


corrplot(Mx, method="color")
corrplot(Mx, method="shade")
corrplot.mixed(Mx, tl.pos = "lt", diag = "u")


corrplot(Mx, method="number")

corrplot.mixed(Mx, tl.pos = "lt")
corrplot.mixed(Mx, tl.pos = "lt", diag = "u")
corrplot.mixed(Mx, tl.pos = "lt", diag = "l")
corrplot.mixed(Mx, tl.pos = "n")

# Positive correlations are displayed in blue and negative correlations in 
# red color. Color intensity and the size of the circle are proportional to 
# the correlation coefficients.

# Reordering the correlation matrix---
# correlogram with hclust reordering
corrplot(Mx, order="hclust")

# Combining correlogram with the significance test &
# Computing the p-value of correlations

# To compute the matrix of p-value, a custom R function is used :


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(Gen_Motors_Mx)
p.mat
head(p.mat[, 1:7])


# To Add significance level to the correlogram

# Specialized the Insignificant values or to Strike-Off Insignificant Values 
# according to the significant level---------

corrplot(Mx, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# OR----
# Leave blank on no significant coefficient
corrplot(Mx, type="full", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

# To Customize the correlogram ----------------------

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Mx, method="color", col=col(200),  
         type="full", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="maroon", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# Thus the correlation of variables within the given sampling dataset  has been 
# checked for and noted


####################

# To create Training (Development) and Test (Validation) data from the 
# original dataset----

## Set the seed to make the dataset partition reproductible----
set.seed(123)

# Sample Indexes
# Split dataset into 70% and 30%
indexes = sample(1:nrow(General_Motors), size=0.3*nrow(General_Motors))
test_data = General_Motors[indexes,]
dim(test_data)  
train_data = General_Motors[-indexes,]
dim(train_data) 

# OR-------
require(caTools)
sample = sample.split(General_Motors$Price, SplitRatio = .70)
train_data = subset(General_Motors, sample == TRUE)
test_data  = subset(General_Motors, sample == FALSE)
dim(train_data)
dim(test_data)

# OR ---

require(caret)
inTraining <- createDataPartition(General_Motors$Price, p = .70, list = FALSE)
train_data <- General_Motors[ inTraining,]
test_data  <- General_Motors[-inTraining,]
dim(train_data)
dim(test_data)


# Variable Selection for Linear Regression:---
# By using using the stepAIC( ) function from the MASS package, stepwise selection 
# can be performed comprising of 3 types namely:-
# a) Forward Selection
# b) Backward Elimination
# c) Forward-Backward Selection / Step-wise Regression

# a) Forward Selection -
# Step() can be used to perform variable selection. 
# To perform forward selection we need to begin by specifying a starting model 
# and the range of models which we want to examine in the search. 

null=lm(Price~1, data=train_data)
null

full=lm(Price~., data=train_data)
full

# To perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward")

# AIC value is 5732.91

# By doing so, it tells R to start with the null model and search through models 
# lying in the range between the null and full model using the forward selection 
# algorithm.

# b) Backward Elimination ---
step(full, data=train_data, direction="backward")

# c) Forward-Backward Selection / Step-wise Regression

step(null, scope = list(upper=full), data=train_data, direction="both")

# Herein, algorithms give rise to results that are equivalent to the 
# forward selection 

# By applying lm() to all the variables in the given dataset in order to
# do the fitting of the data-points---

require(MASS)
fit <- lm(Price~ Mileage + Cylinder + Liter + Cruise + Sound + Leather , data=train_data)
fit

fit_1 <- lm(Price~ Mileage + Cylinder + Liter + Leather , data=train_data)
fit_1

# AIC (Akaike's information criterion):
# It is a measure of measures of the goodness of fit of an estimated 
# statistical model and can also be used for model selection.

AIC_forward_step <- stepAIC(fit, direction="forward") # AIC value is 6045.44
AIC_forward_step$anova

AIC_forward_step <- stepAIC(fit_1, direction="forward") # AIC value is 6049.007
AIC_forward_step$anova

AIC_backward_step <- stepAIC(fit, direction="backward") # AIC value is 6045.44
AIC_backward_step$anova

AIC_stepwise <- stepAIC(fit, direction="both") # AIC value is 6005.71
AIC_stepwise$anova # display results

# BIC (Bayesian information criterion):
# Similar to AIC, this test is a measure of measures of the goodness of fit 
# of an estimated statistical model and can also be used for model selection.

BIC_forward_step<- BIC(fit) # BIC value is 7047.68
BIC_forward_step

# Detection of Outliers within the given dataset ----

# Bonferroni Outlier Test is used which reports the 
# Bonferroni p-values for Studentized residuals in linear and generalized 
# linear models

outlierTest(fit,cutoff = Inf,n.max = Inf)
leveragePlots(fit) # leverage plots

#######################

# Estimated Simple Regression Equation-----

# To Apply the simple linear regression model for the data set - General_Motors, 
# and to estimate the Vehicle/Motor Price,lm function is applied as follows:- 

# a) Model Preparation/Building of Algorithm for the Linear Regression Model: 

#df <- data.frame(matrix(unlist(l), nrow=132, byrow=T),stringsAsFactors=FALSE)

Gen_Motors_Model_1=lm(Price~.,train_data)
Gen_Motors_Model_1
summary(Gen_Motors_Model_1)

plot(Gen_Motors_Model_1)


# To predict the Motor Price:-----
SalesPred_Mod_1<-predict(Gen_Motors_Model_1,test_data)

#Summary
summary(SalesPred_Mod_1)

# ----OR-----

require(earth)
SalesPred_Mod_1_A <-earth(formula=Price ~ Cylinder + Liter + Cruise + Sound + Leather + Mileage,data = General_Motors)
evimp(SalesPred_Mod_1_A)

#b) Alternative model/Alternative Algorithm for the Linear Regression Model:---
Gen_Motors_Model_2 = lm(Price ~  Cylinder + Liter, data=train_data)
Gen_Motors_Model_2

# Thereafter, the parameters of the estimated regression equation with the
# coefficients function are extracted as follows:

coeffs = coefficients(Gen_Motors_Model_2); coeffs
summary(Gen_Motors_Model_2)

# To predict the Motor Price:-----
SalesPred_Mod_2<-predict(Gen_Motors_Model_2,test_data)

#Summary
summary(Gen_Motors_Model_2)
plot(Gen_Motors_Model_2)

# Based on the outcome i.e. Summary of 2 algorithms i.e. Gen_Motors_Model_1 and Gen_Motors_Model_2,
# we can decipher as follows:

# a) Gen_Motors_Model_1

# Multiple R-squared:  0.8744 and	Adjusted R-squared:  0.8715 
# Residual standard error: 3415
# p-value: < 0.00000000000000022


# b) Gen_Motors_Model_2

# R-square:  0.6411 and	Adjusted R-square:  0.639
# Residual standard error: 5724
# p-value: < 0.00000000000000022

# a) Based on the outcome, we can see that the 1st algorithm (Gen_Motors_Model_1) consisting of all the 
# variables, R square=0.874 and Adjusted R square=0.871.

#  Based on the outcome, we can see that the 2nd algorithm (Gen_Motors_Model_2) consisting of selected 
# variables, R square=0.64 and Adjusted R square=0.63.

# So we can observe that R square and Adjusted R square values Decrease while 
# opting for selected variables, which is NOT a healthy sign for a robust model.
# Higher the R2 and Adjusted R2 the better, hence we retain the first alogritm (Gen_Motors_Model_1)

# c) There is no change in p value for both the proposed models i.e. 
# p-value: < 0.00000000000000022.

# d) Residual standard error: 3415 (Gen_Motors_Model_1) and
#    Residual standard error: 5724 (Gen_Motors_Model_2)

# Residual error INCREASES upon specified or select variables which is again NOT
# a healthy sign for a good, robust model. Moreover, the standard error should be
# closer to zero the better.

# Based on the outcomes of the aforesaid 2 algorithms, it is found that the
# first algorithm  is considered to be a robust and better algorithm for
# the proposed model.
# Hence, we retain only the 1st algorithm (i.e. Gen_Motors_Model_1) and discard the 2nd
# algorithm for the proposed model(i.e.Gen_Motors_Model_2).

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(Gen_Motors_Model_1)

# To revert back to the default method of Graphs/Plots:
layout(matrix(c(1),1))

# k- Fold Cross validation: For this purpose, we perform MSE(Mean Square Error) calculation.
# It is important to rigorously test the models performance as much as possible. 
# One way is to ensure that the model equation you have will perform well, when it 
# is built on a different subset of training data and predicted on the remaining 
# data.
# For this purpose, Split your data into k-mutually exclusive random sample 
# portions. Keeping each portion as test data, we build the model on the remaining
# (k-1 portion) data and calculate the mean squared error of the predictions. This
# is done for each of the k-random sample portions. Then finally, the average of 
# these mean squared errors (for k portions) is computed. We can use this metric 
# to compare different linear models.

# By doing this, we need to check two things:

# i)  If the model's prediction accuracy isn't varying too much for any one 
# particular sample, and
# ii) If the lines of best fit don't vary too much with respect the the slope 
# and level.
# In other words, they should be parallel and as close to each other as possible. 

# Calculating MSE (Mean Standard Error)
# x <- test$x
# p <- predict(model, data.frame(x)) # or: predict(model, test)

sales <- test_data$General_Motors
p <- predict(Gen_Motors_Model_1, data.frame(General_Motors))
p<-predict(Gen_Motors_Model_1,test_data)
p


#mse(actual, predicted)
pd <- predict(Gen_Motors_Model_1 , train_data)
#mse(training.data$pd)
require(Metrics)
mse(train_data$Price,pd)


# Calculating MSE (Mean Standard Error)
# x <- test$x
# p <- predict(model, data.frame(x)) # or: predict(model, test)

sales <- test_data$sales_dataset
p <- predict(dvdmod1, data.frame(sales_dataset))
p<-predict(dvdmod1,test_data)
p

#MSE (Mean Standard Error)

mean((test_data$Price - predict.lm(Gen_Motors_Model_1, test_data)) ^ 2)

#mse(actual, predicted)
pd <- predict(Gen_Motors_Model_1 , train_data)
#mse(training.data$pd)
mse(train_data$Price,pd)

#  To check for the Validity of Assumptions of Linear Regression:------------
# 1) To rule out Heteroscedascity &  establish the presecence of 
# Homoscesdacity in a model- Breusch-Pagan test is used.
# To test/conformance for Homoscesdacity in a given linear regression model-----

library(lmtest)

Breusch_Pagan_Test<-bptest(Gen_Motors_Model_1, ~.,  data=General_Motors, studentize = TRUE)
Breusch_Pagan_Test


# So if p_val < 0.05 (or your chosen alpha value); you reject the Null Hypothesis 
# and infer the presence of Heteroscedasticity.

# If p_val > 0.05 (or your chosen alpha value); you accept the Null Hypothesis and
# conclude the presence of Homoscedasticity.
# In this case, p value (0.1365) > 0.05, hence we accept Null Hypothesis and conclude 
# the presence of Homoscedasticity  and therefore the absence of any 
# Heterscedasticity within the given dataset, thereby in conformance with the 
# assumptions of Linear Regression.

# 2) Multicollinearity---

# Multicollinearity exists when two or more of the predictors in a regression model 
# are moderately or highly correlated. Unfortunately, when it exists, it can wreak 
# havoc on our analysis and thereby limit the research conclusions we can draw. 
# All the variables having VIF > 2.5 are faced with a problem of multicollinearity and
# hence it is better to discard such variables.

# To detect Multicollinarity in the given dataset, a function called as
# Variance Inflation Factors (VIF) is used. 
# As the name suggests, a # variance inflation factor (VIF) quantifies 
# how much the variance is inflated. 

require(car)
multicollinearity_test<-vif(General_Motors)
summary(multicollinearity_test)

##############################################################################3






