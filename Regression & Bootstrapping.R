## install and load the necessary packages
library(lubridate)
library(tree)
library(Matching)
library(boot)
library(randomForest)
library(ggplot2)
library(arm)
library(rgenoud)
# we need to set the seed of R's random number generator, in order to produce comparable results 
set.seed(32)

set.seed(11)
# independent variable1: geographical distance between cities currently located in
geo_dis <- runif(1000,0,20000)

# independent variable2: cultural proximity index (100 when coming from the same culture)
cul_prox <- runif(1000) * 100

# independent variable3: friends have in common
friend_num <- as.integer(runif(1000) * 200)

irreducible_error <- rnorm(1000,5,5)

friendship_idx <- -(1/100)*(geo_dis)  + 80 * cul_prox + 12 * (friend_num) + irreducible_error

df <- data.frame(geo_dis, cul_prox, friend_num, friendship_idx)
head(df)

incorrect = df
# generate an incorrect model of the systematic component
lm <- lm(friendship_idx ~ geo_dis + cul_prox + friend_num, data = df)
sim <- sim(lm,1)
coef <- sim@coef
# report the model
sprintf("The simulated model of the systematic component is: Friendship_index = %f + %f * geographical_distance + %f * cultural_proximity + %f * friend_number", coef[1], coef[2], coef[3], coef[4])

# generate the one prediction based on the coefficient simulated
for (i in 1:1000) {
  incorrect[i,4] <- coef[1] + coef[2] * incorrect[i,1] + coef[3] * incorrect[i,2] + coef[4] * incorrect[i,3]
}
names(incorrect)[4] = "simulated outcome"

# calculate RMSE
RMSE_incorrect <- sqrt(mean((df[,4] - incorrect[,4])**2))
sprintf("The RMSE for the incorrect modelis: %f.", RMSE_incorrect)

correct = df
correct[,4] = 0
# generate an correct model of the systematic component and stochastic components
sigma <- sim@sigma
# report the model
sprintf("The simulated model of the systematic component and stochastic components is: Friendship_index = %f + %f * geographical_distance + %f * cultural_proximity + %f * friend_number + %f", coef[1], coef[2], coef[3], coef[4], sigma)

# generate the one prediction based on the coefficient and error simulated
for (i in 1:1000) {
  correct[i,4] <- coef[1] + coef[2] * correct[i,1] + coef[3] * correct[i,2] + coef[4] * correct[i,3] + sigma
}
names(correct)[4] = "simulated outcome"

# calculate RMSE
RMSE_correct <- sqrt(mean((df[,4] - correct[,4])**2))
sprintf("The RMSE for the correct model is: %f.", RMSE_correct)

set.seed(22)
data = data.frame()
for (i in seq(1,50)) {
  data[i,1] = i
  data[i,2] = i * 20 + 50 + rnorm(1) * 200
}
names(data)[1] = "independent_variable"
names(data)[2] = "dependent_variable"
plot(data, main = "Regression line with a positive slop", xlab = "Independent variable", ylab = "Dependent variable")
lm2 <- lm(dependent_variable ~ independent_variable, data = data)
abline(lm2,lwd=3,col="red")

data = data.frame()
for (i in seq(1,50)) {
  data[i,1] = i
  data[i,2] = i * 20 + 50 + rnorm(1) * 200
}
names(data)[1] = "independent_variable"
names(data)[2] = "dependent_variable"
data[1,2] = 25000
plot(data, main = "Change of regression line slope when adding an outlier", xlab = "Independent variable", ylab = "Dependent variable")
lm3 <- lm(dependent_variable ~ independent_variable, data = data)
abline(lm3,lwd=3,col="red")

data(lalonde)
# fit a linear regression model
lm1 <- lm(re78~age+educ+re74+re75+hisp+black, data=lalonde)
summary(lm1)

# report coefficients and R-sqaured
print("The coefficients estimated from the model are:") 
print(coef(lm1))
print("The reported R-squared is: 0.039")

# predict using the fitted model
predicted_re78 <- predict.lm(lm1, lalonde)

# hand-calculation of R-squared
mean <- mean(lalonde$re78)
# SSR (regression sum of squares): how far the estimated sloped regression line is from the horizontal 'no relationship line', the sample mean
SSR <- sum((predicted_re78 - mean) ** 2)
# SSTO (total sum of squres): how much the data points, vary around their mean
SSTO <- sum((lalonde$re78 - mean) ** 2)
SSE <- sum((lalonde$re78 - predicted_re78) ** 2)
R_squared <- SSR/SSTO
sprintf("The calculated R-squared by hand is: %s, the answer is the same with the R-squared from the summary statistics above.",round(R_squared,3))

set.seed(254)
# setting all predictors at their mean
age = mean(lalonde$age)
re74 = mean(lalonde$re74)
re75 = mean(lalonde$re75)
hisp = mean(lalonde$hisp)
black = mean(lalonde$black)

# create a function to calculate the dependent variable given coefficients and independent variables
calc_turnout <- function(coef, person) {
  turnout <- coef[1] + 
    coef[2] * person[1] + 
    coef[3] * person[2] + 
    coef[4] * person[3] + 
    coef[5] * person[4] + 
    coef[6] * person[5] + 
    coef[7] * person[6]
  return(turnout)
}


# use three nested loops to get the expected values of 're78'
storage.matrix_exp <- matrix(NA, nrow = 1000, ncol = 14)

for (j in c(1:1000)) {
  sim_exp <- sim(lm1, 1000)
  sim_coef_exp <- sim_exp@coef 
  for (educ in c(3:16)) {
    person <- c(age, educ, re74, re75, hisp, black)
    store <- rep(0,1000)
    for (i in c(1:1000)) {
      store[i] <- calc_turnout(sim_coef_exp[i,],person) 
    }
    storage.matrix_exp[j,educ-2] <- mean(store)
  }
}

# 95% confidence interval of the expected values
conf.intervals_exp <- apply(storage.matrix_exp, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(3,16), ylim = c(1,10000), 
     main = "Expected Earnings in 1978 by Years of Schooling (95% CI)", xlab = "Years of Schooling", 
     ylab = "Real Earnings in 1978")

for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = storage.matrix_exp[1, educ - 2],
    x1 = educ,
    y1 = storage.matrix_exp[2, educ - 2],
    lwd = 2)
}


set.seed(25)
# simulate 1000 set of coefficients 
sim_pred <- sim(lm1, 1000)
sim_coef_pred <- sim_pred@coef

# store 1000 predicted values for each education value
storage.matrix_pred <- matrix(NA, nrow = 1000, ncol = 14)
for (educ in c(3:16)) {
  for (i in c(1:1000)) {
    person <- c(age, educ, re74, re75, hisp, black)
    storage.matrix_pred[i,educ-2] <- calc_turnout(sim_coef_pred[i,],person)
  }
}
conf.intervals_pred <- apply(storage.matrix_pred, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(3,16), ylim = c(1,10000), 
     main = "Predicted Earnings in 1978 by Years of Schooling (95% CI)", xlab = "Years of Schooling", 
     ylab = "Real Earnings in 1978")

for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = storage.matrix_pred[1, educ - 2],
    x1 = educ,
    y1 = storage.matrix_pred[2, educ - 2],
    lwd = 2)
}

glm.treat <- glm(treat~age+educ+hisp+re74+re75,data=lalonde,family=binomial)
print("The regression coefficients are:")
print(coef(glm.treat))
conf_int <- confint(glm.treat)
sprintf("The 95%% confidence interval for 'age' is [%f,%f].", conf_int[2,1], conf_int[2,2])
sprintf("The 95%% confidence interval for 'education' is [%f,%f].", conf_int[3,1], conf_int[3,2])

set.seed(42)
age.fn <- function(data,index) {
  d <- data[index,]
  return(coef(glm(treat~age+educ+hisp+re74+re75,data=d,family=binomial))[2])
}

boot.age <- boot(lalonde, age.fn, 1000)
age_conf <- quantile(boot.age$t, probs = c(0.025, 0.975))
```
```{r}
educ.fn <- function(data,index) {
  d <- data[index,]
  return(coef(glm(treat~age+educ+hisp+re74+re75,data=d,family=binomial))[3])
}

boot.educ <- boot(lalonde, educ.fn, 1000)
educ_conf <- quantile(boot.educ$t, probs = c(0.025, 0.975))

sprintf("The bootstrapped confidence interval for 'age' is [%f, %f].", age_conf[1], age_conf[2])
sprintf("The bootstrapped confidence interval for 'education' is [%f, %f].", educ_conf[1], educ_conf[2])

set.seed(43)
glm.treat <- glm(treat~age+educ+hisp+re74+re75,data=lalonde,family=binomial)

# create a function to calculate the dependent variable given coefficients and independent variables
calc_turnout_2 <- function(coef, person) {
  turnout <- coef[1] + 
    coef[2] * person[1] + 
    coef[3] * person[2] + 
    coef[4] * person[3] + 
    coef[5] * person[4] + 
    coef[6] * person[5]
  return(exp(turnout) / (1 + exp(turnout)))
}

# store 1000 predicted values for each education value
storage.matrix_exp_2 <- matrix(NA, nrow = 1000, ncol = 14)
for (j in c(1:1000)) {
  sim_exp <- sim(glm.treat, 1000)
  sim_coef_exp <- sim_exp@coef 
  for (educ in c(3:16)) {
    person <- c(age, educ, hisp, re74, re75)
    store <- rep(0,1000)
    for (i in c(1:1000)) {
      store[i] <- calc_turnout_2(sim_coef_exp[i,],person) 
    }
    storage.matrix_exp_2[j,educ-2] <- mean(store)
  }
}

conf.intervals_exp_2 <- apply(storage.matrix_exp_2, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(3,16), ylim = c(0,1), 
     main = "Expected probability of receiving treatment by years of schooling(95%CI)", xlab = "Years of Schooling", 
     ylab = "Probability of Receiving Treatment")

for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = storage.matrix_exp_2[1, educ - 2],
    x1 = educ,
    y1 = storage.matrix_exp_2[2, educ - 2],
    lwd = 2)
}


set.seed(44)
# simulate 1000 set of coefficients 
sim_pred_2 <- sim(glm.treat, 1000)
sim_coef_pred_2 <- sim_pred_2@coef

# create a function to calculate the dependent variable given coefficients and independent variables
calc_turnout_2 <- function(coef, person) {
  turnout <- coef[1] + 
    coef[2] * person[1] + 
    coef[3] * person[2] + 
    coef[4] * person[3] + 
    coef[5] * person[4] + 
    coef[6] * person[5]
  return(exp(turnout) / (1 + exp(turnout)))
}

# store 1000 predicted values for each education value
storage.matrix_pred_2 <- matrix(NA, nrow = 1000, ncol = 14)
for (educ in c(3:16)) {
  for (i in c(1:1000)) {
    person <- c(age, educ, hisp, re74, re75)
    storage.matrix_pred_2[i,educ-2] <- calc_turnout_2(sim_coef_pred_2[i,],person)
  }
}

conf.intervals_pred_2 <- apply(storage.matrix_pred_2, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(3,16), ylim = c(0,1), 
     main = "Predicted probability of receiving treatment by years of schooling(95%CI)", xlab = "Years of Schooling", 
     ylab = "Probability of Receiving Treatment")

for (educ in 3:16) {
  segments(
    x0 = educ,
    y0 = storage.matrix_pred_2[1, educ - 2],
    x1 = educ,
    y1 = storage.matrix_pred_2[2, educ - 2],
    lwd = 2)
}


# Note that if you knit this document, this part of the code won't 
# show up in the final pdf which is OK. We don't need to see the code
# we wrote.

# How effective is a therapy method against stress

# Participants in the study record their stress level for a month.
# Every day, participants assign a value from 1 to 10 for their stress level. 
# At the end of the month, we average the results for each participant.

#adds the confidence interval (first row of the matrix is lower 
# bound, second row is the upper bound)
trt1 = matrix(NA,nrow=2,ncol=7)
ctrl = matrix(NA,nrow=2,ncol=7) 

trt1[,1]=c(3.7, 6.5) #18  
ctrl[,1]=c(5, 8)

trt1[,2]=c(5, 8.5) #22
ctrl[,2]=c(7.5, 9)

trt1[,3]=c(6, 9) #26
ctrl[,3]=c(8.5, 10)

trt1[,4]=c(5, 7) #30
ctrl[,4]=c(6, 8)

trt1[,5]=c(3.5, 5) #34
ctrl[,5]=c(4.5, 7)

trt1[,6]=c(2, 3.5) #38
ctrl[,6]=c(3.5, 6)

trt1[,7]=c(0.5, 2) #42
ctrl[,7]=c(2.5, 5)

# colors to each group
c1 = rgb(red = 0.3, green = 0, blue = 1, alpha = 0.7) #trt1
c2 = rgb(red = 1, green = 0.6, blue = 0, alpha = 1) #trt2
c3 = rgb(red = 0, green = 0.5, blue = 0, alpha = 0.7) #ctrl

# creates the background of the graph
plot(x = c(1:100), y = c(1:100), 
     type = "n", 
     xlim = c(17,43), 
     ylim = c(0,11), 
     cex.lab=1,
     main = "Stress Level - 95% Prediction Intervals", 
     xlab = "Age", 
     ylab = "Average Stress Level per Month", 
     xaxt = "n")

axis(1, at=seq(18,42,by=4), seq(18, 42, by=4))

grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted",
     lwd=par("lwd"), equilogs = TRUE)

# adds the legend
legend('topright',legend=c('Treatment','Control'),fill=c(c1,c2))

# iterates to add stuff to plot
for (age in seq(from=18,to=42,by=4)) { 
  #treatment
  segments(x0=age-0.2, y0=trt1[1, (age-18)/4+1],
           x1=age-0.2, y1=trt1[2, (age-18)/4+1], lwd=4, col=c1)
  
  #control
  segments(x0=age+0.2, y0=ctrl[1, (age-18)/4+1],
           x1=age+0.2, y1=ctrl[2, (age-18)/4+1], lwd=4, col=c2)
}


# import the dataset
ks_df <- read.csv("ks-projects-201801.csv")

ks_df[ks_df==""] <- NA   # set all the blanks to NAs
ks_df <- na.omit(ks_df)    # omit the NA values

# see the head
head(ks_df)

# A project is successful if the real pledged value is larger than or equal to the goal value
# create a column of dummy variables to indicate the status
ks_df$success <- ifelse(ks_df$usd_pledged_real - ks_df$usd_goal_real >= 0, 1, 0)
head(ks_df)

# extract the date using lubricate package
ks_df$deadline = date(ks_df$deadline)
ks_df$launched = date(ks_df$launched)
# create a new column to store the length of the project
ks_df$length <- ks_df$deadline - ks_df$launched
# remove any project length that is higher than 60
ks_df <- ks_df[!(ks_df$length > 60),]

# randomly select 80% of the data 
tr_idx <- sample(nrow(ks_df),as.integer(nrow(ks_df) * 0.80))
ks_tr <- ks_df[tr_idx,]  # training set
ks_te <- ks_df[-tr_idx,]  # rest to testing set

ks_df$main_category <- as.factor(ks_df$main_category)

# fit a logistic regression model to the dataset
glm_ks <- glm(success ~ backers + length + main_category + usd_goal_real, data = ks_tr, family=binomial)
summary(glm_ks)

# predict the response of probability of the test set using the fitted model
ks_p <- predict(glm_ks, ks_te, type = "response")
# convert into class labels
ks.pred=rep("No",nrow(ks_te))
ks.pred[ks_p > .5] = "Yes"
ks_te$ks.pred = ks.pred
# produce a confusion matrix
acc_table <- table(ks.pred,ks_te$success)
acc_table

# use the same process to predict for the training set
ks_p_tr <- predict(glm_ks, ks_tr, type = "response")
ks.pred_tr <- rep("No",nrow(ks_tr))
ks.pred_tr[ks_p_tr>.5] = "Yes"
ks_tr$ks.pred_tr = ks.pred_tr
acc_table_tr <- table(ks.pred_tr,ks_tr$success)
acc_table_tr

# report misclassification rates
misclass_tr <- (acc_table_tr[1,2]+acc_table_tr[2,1])/sum(acc_table_tr)
sprintf("The misclassification rate of the predictions for the training sets is: %f",misclass_tr)
misclass_te <- (acc_table[1,2]+acc_table[2,1])/sum(acc_table)
sprintf("The misclassification rate of the predictions for the test sets is: %f",misclass_te)

# sample 5% of the data set to apply LOOCV in order to reduce running time
ks_df_sam <- ks_df[sample(nrow(ks_df),as.integer(nrow(ks_df)*0.05)),]

# randomly select 80% of the data 
sam_tr_idx <- sample(nrow(ks_df_sam),as.integer(nrow(ks_df_sam) * 0.80))
ks_sam_tr <- ks_df_sam[sam_tr_idx,]  # training set from 5% sample
ks_sam_te <- ks_df_sam[-sam_tr_idx,]  # rest to testing set from 5% sample

# fit a logistic model on the training data set
glm_ks_sam <- glm(success ~ backers + length + main_category + usd_goal_real, data = ks_sam_tr, family=binomial)

# apply LOOCV method to the training set
cv.err=cv.glm(ks_sam_tr,glm_ks_sam)

# apply LOOCV method to the test set
cv.err.test =cv.glm(ks_sam_te,glm_ks_sam)

sprintf("The misclassification rate of the training set is: raw: %f, adjusted: %f.", cv.err$delta[1], cv.err$delta[2])
sprintf("The misclassification rate of the test set is: raw: %f, adjusted: %f.", cv.err.test$delta[1], cv.err.test$delta[2])


