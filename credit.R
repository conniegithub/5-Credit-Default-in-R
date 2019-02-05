#install.packages("readxl")
library(readxl)

getwd()
setwd("C:/Users/../multivariate/project")
data <- read_excel("default of credit card clients.xls")

#reassign values
data$X2[data$X2==2] <- 0 #changed female=2 to female=0
data$X3[data$X3==1] <- 2 #changed grad=1 to grad=2
data$X3[data$X3==2] <- 1 #changed undergrad=2 to grad=1
data$X3[data$X3==3] <- 0 #changed hs=3 to hs=0
data$X3[data$X3==4] <- 0 #changed below hs=4 to 0
data$X4[data$X4==0] <- 3 #changed marriage 0 value to 3(others)

data$X6 <- data$X6+2 #add 2 to each
data$X7 <- data$X7+2
data$X8 <- data$X8+2
data$X9 <- data$X9+2
data$X10 <- data$X10+2
data$X11 <- data$X11+2


#dataframe form
#split into training and validating data sets
training <- data[1:15000,]
validating <- data[15001:30000,]
#rename
train <- data.frame(ID = training$ID, Loan_Amount = training$X1, Gender = training$X2, Education = training$X3, 
                    Marital_Status = factor(training$X4), Age = training$X5, 
                    Pay_Delay_Sep = training$X6, Pay_Delay_Aug = training$X7, 
                    Pay_Delay_Jul = training$X8, Pay_Delay_Jun = training$X9, 
                    Pay_Delay_May = training$X10, Pay_Delay_Apr = training$X11, 
                    Bill_Sep = training$X12, Bill_Aug = training$X13, 
                    Bill_Jul = training$X14, Bill_Jun = training$X15, 
                    Bill_May = training$X16, Bill_Apr = training$X17, 
                    Paid_Sep = training$X18, Paid_Aug = training$X19, 
                    Paid_Jul = training$X20, Paid_Jun = training$X21, 
                    Paid_May = training$X22, Paid_Apr = training$X23, Default = training$Y)
valid <- data.frame(ID = validating$ID, Loan_Amount = validating$X1, Gender = validating$X2, Education = validating$X3, 
                    Marital_Status = factor(validating$X4), Age = validating$X5, 
                    Pay_Delay_Sep = validating$X6, Pay_Delay_Aug = validating$X7, 
                    Pay_Delay_Jul = validating$X8, Pay_Delay_Jun = validating$X9, 
                    Pay_Delay_May = validating$X10, Pay_Delay_Apr = validating$X11, 
                    Bill_Sep = validating$X12, Bill_Aug = validating$X13, 
                    Bill_Jul = validating$X14, Bill_Jun = validating$X15, 
                    Bill_May = validating$X16, Bill_Apr = validating$X17, 
                    Paid_Sep = validating$X18, Paid_Aug = validating$X19, 
                    Paid_Jul = validating$X20, Paid_Jun = validating$X21, 
                    Paid_May = validating$X22, Paid_Apr = validating$X23, Default = validating$Y)
#split into response and predictor
train_y <- train$Default
train_rest <- train[,2:24]
valid_y <- valid$Default
valid_rest <- valid[,2:24]


#fitting training data to logistic
fit.glm1 =  glm(train_y ~ ., data = train_rest, family=binomial())
summary(fit.glm1)

#install.packages("car")
library(car)
vif(fit.glm1)

#taking out insignificant terms
fit.glm2 =  glm(train_y ~ ., data = train_rest[c(2:4,6,8,10,12:13,18:20)], family=binomial())
summary(fit.glm2)

#stepwise selection of model
back = step(fit.glm1, direction="back")
forward = step(fit.glm1, direction="forward")
both = step(fit.glm1, direction="both")
formula(back)
formula(forward)
formula(both)
summary(both)

#assign final model
fit.glm <- both
vif(both)

# Estimate posterior probabilities
post.prob = fit.glm$fitted

# Predict groups
Group.hat = ifelse(post.prob>0.5, 1, 0)

cbind(round(post.prob,3), Group.hat)
summary(Group.hat != train_y)
mean(Group.hat != train_y)


#-----------------------------------------------------------------
#validation data
pred = predict(fit.glm, newdata = valid_rest, type="response")

Group.val = ifelse(pred>0.5, 1, 0)
summary(Group.val != valid_y)
mean(Group.val != valid_y)

#-----------------------------------------------------------------
#-----------------------------------------------------------------
#restructure training data to stacked for gee
temp1 <- training[,c(1,2:6,7:12,25)] #pay delay and others
temp2 <- training[,c(1,13:18)] #bill amount
temp3 <- training[,c(1,19:24)] #pay amount

#install.packages("tidyr")
#install.packages("dplyr")
library(tidyr)
library(dplyr)
gather(temp1, key=names, value=pay_delay, X6,X7,X8,X9,X10,X11) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long1
long1$time <- long1$time-5

gather(temp2, key=names, value=bill_amt, X12,X13,X14,X15,X16,X17) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long2
long2$time <- long2$time-5

gather(temp3, key=names, value=pay_amt, X18,X19,X20,X21,X22,X23) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long3
long3$time <- long3$time-5

training_stack <- cbind(long1[,c(1:6,9:10)],long2[,3],long3[,3],long1[,7])
train_st_y <- training_stack$Y
train_st_rest <- data.frame(ID = training_stack$ID, Loan_Amount = training_stack$X1, 
                            Gender = training_stack$X2, Education = training_stack$X3, 
                            Marital_Status = factor(training_stack$X4), Age = training_stack$X5, 
                            Pay_Delay = training_stack$pay_delay, Bill_Amount = training_stack$bill_amt,
                            Pay_Amount = training_stack$pay_amt, Time = training_stack$time)
#note that Time 1,2,3,4,5,6 corresponds to Sep/Aug/Jul/Jun/May/Apr

#restructure validation data to stacked for gee
temp4 <- validating[,c(1,2:6,7:12,25)] #pay delay and others
temp5 <- validating[,c(1,13:18)] #bill amount
temp6 <- validating[,c(1,19:24)] #pay amount

gather(temp4, key=names, value=pay_delay, X6,X7,X8,X9,X10,X11) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long4
long4$time <- long4$time-5

gather(temp5, key=names, value=bill_amt, X12,X13,X14,X15,X16,X17) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long5
long5$time <- long5$time-5

gather(temp6, key=names, value=pay_amt, X18,X19,X20,X21,X22,X23) %>%
  mutate(time = as.numeric(substr(names,2,3))) %>%
  arrange(ID) -> long6
long6$time <- long6$time-5

validating_stack <- cbind(long4[,c(1:6,9:10)],long5[,3],long6[,3],long4[,7])
valid_st_y <- validating_stack$Y
valid_st_rest <- data.frame(ID = validating_stack$ID, Loan_Amount = validating_stack$X1, 
                            Gender = validating_stack$X2, Education = validating_stack$X3, 
                            Marital_Status = factor(validating_stack$X4), Age = validating_stack$X5, 
                            Pay_Delay = validating_stack$pay_delay, Bill_Amount = validating_stack$bill_amt,
                            Pay_Amount = validating_stack$pay_amt, Time = validating_stack$time)

#fitting data to gee
library(geepack)
model <- train_st_y ~ Loan_Amount + Gender + Education + Marital_Status + Age + Pay_Delay:Time + Bill_Amount:Time + Pay_Amount:Time + Time

fit.gee.cs = geeglm(model, data = train_st_rest, id=ID, family="binomial", 
                    corstr = "exchangeable", std.err = "san.se")
fit.gee.in <- update(fit.gee.cs, corstr = "independence")
fit.gee.ar <- update(fit.gee.cs, corstr = "ar1")
fit.gee.un <- update(fit.gee.cs, corstr = "unstructured")
summary(fit.gee.cs)
summary(fit.gee.in)
summary(fit.gee.ar)
summary(fit.gee.un)

#install.packages("MuMIn")
library(MuMIn)
sel <- model.sel(fit.gee.cs, fit.gee.in, fit.gee.ar, fit.gee.un, rank = QIC)

#assign final model
fit.gee <- fit.gee.ar #ar/un/in/cs

# Estimate posterior probabilities
post.prob.st = fit.gee$fitted

# Predict groups
Group.hat.st = ifelse(post.prob.st>0.5, 1, 0)

cbind(round(post.prob.st,3), Group.hat.st)
summary(Group.hat.st != train_st_y)
mean(Group.hat.st != train_st_y)


#-----------------------------------------------------------------
#validation data
pred.st = predict(fit.gee, newdata = valid_st_rest, type="response")

Group.val.st = ifelse(pred.st>0.5, 1, 0)
summary(Group.val.st != valid_st_y)
mean(Group.val.st != valid_st_y)

#=================================================================
#LDA
#install.packages("klaR")
library(klaR)
library(MASS)

Groups_t = as.factor(train_y) 

#graph pairwise significant factors
partimat(train[, c(6,12)], grouping=Groups_t, method = "lda")

#### LDA regions
lda.fit = lda(training[,1:23], grouping = Groups_t)  ## perform LDA
lda.pred_t = predict(lda.fit, training[,1:23])$class 

APER.lda_t = mean(lda.pred_t != Groups_t) ## Apparent error rate for LDA
APER.lda_t

#-----------------------------------------------------------------
Groups_v = as.factor(valid_y) 

#### LDA regions
lda.pred_v = predict(lda.fit, validating[,1:23])$class 

APER.lda_v = mean(lda.pred_v != Groups_v) ## Apparent error rate for LDA
APER.lda_v

#=================================================================
#KNN
library(class)

## k=17
out.kt = knn(train_rest, train_rest, train_y, k=17)
table(out.kt, train_y)
print(mean(out.kt!=train_y))

out.kv = knn(valid_rest, valid_rest, valid_y, k=17)
table(out.kv, valid_y)
print(mean(out.kv!=valid_y))

#out.kv = knn(train_rest, valid_rest, train_y, k=17)
#table(out.kv, train_y)
#print(mean(out.kv!=train_y))
#out.kv = knn(train_rest, valid_rest, valid_y, k=17)
#table(out.kv, valid_y)
#print(mean(out.kv!=valid_y))


#finding best k
# misclassification rate for training and testing samles
n = 10
misclass.train = rep(NA, n)
misclass.test = rep(NA, n)

# Do CV for k = 1,2,...,50
for(k in 1:n){
  out = knn(train_rest, train_rest, train_y, k=k)
  out.test = knn(train_rest, valid_rest, train_y, k=k)
  
  misclass.train[k] = mean(out!=train_y)    
  misclass.test[k] = mean(out.test!=valid_y)
}

plot(1:n, misclass.train, type="b", lwd=2, ylim=c(0,.3), cex=0.2, xlab="k (number of nearest neighbors)", ylab="misclassification error")
lines(1:n, misclass.test, type="b", lwd=2, lty=2, col="red", cex=0.2)
legend("topright", lwd = c(2,2), lty=c(2,1), legend=c("Validating", "Training"), col=c("red", "black"))



#=================================================================
#PCA
pca <-(prcomp(training[,1:23],scale=TRUE))
summary(pca)


dat=pca$x[,1:2]
Xdat=dat
name=c("PC1","PC2")
Groups=as.factor(train_y)

partimat(x=Xdat,grouping=Groups,method="lda",name=name)

lda.fit.pca=lda(Xdat,grouping=Groups)

score_valid= scale(validating[,1:23], pca$center, pca$scale) %*% pca$rotation
Xdat2=score_valid[,1:2]

lda.pred=predict(lda.fit.pca,Xdat2)$class
Aper.pca=mean(lda.pred !=valid_y)
Aper.pca

