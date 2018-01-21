library(class)

## Descriptive Analysis
##forestfires <- read.csv("kl.csv",header = TRUE, fill = TRUE)
forestfires<-read.csv("new_diag.txt",sep = ",")
#print(forestfires$Age)



#forestfires<-forestfires[which(forestfires$area != 0),]
n<-nrow(forestfires);

test <- sample(1:n, round(n)/10)
forestfires.train <- forestfires[-test, ]
forestfires.test <- forestfires[test, ]
print(n)


for (i in 1:n)
  if (forestfires[i,10]=="N")
    forestfires[i,10]=1.0
  if(forestfires[i,10]=="O")
    forestfires[i,10]=0
print(forestfires)

x=table(forestfires$season,forestfires$output)
print(chisq.test(x));

print(chisq.test(forestfires$age,forestfires$output));

print(chisq.test(forestfires$diseases,forestfires$output));

print(chisq.test(forestfires$accident,forestfires$output));

print(chisq.test(forestfires$surgical,forestfires$output));

print(chisq.test(forestfires$fever,forestfires$output));

print(chisq.test(forestfires$freq,forestfires$output));

print(chisq.test(forestfires$smoke,forestfires$output));

print(chisq.test(forestfires$hours,forestfires$output));
 
##regression lines of various attributes with output

plot(forestfires$season~forestfires$output)
abline(lm(forestfires$season~forestfires$output))

plot(forestfires$age~forestfires$output)
abline(lm(forestfires$age~forestfires$output))

plot(forestfires$diseases~forestfires$output)
abline(lm(forestfires$diseases~forestfires$output))

plot(forestfires$accident~forestfires$output)
abline(lm(forestfires$accident~forestfires$output))

plot(forestfires$surgical~forestfires$output)
abline(lm(forestfires$surgical~forestfires$output))

plot(forestfires$fever~forestfires$output)
abline(lm(forestfires$fever~forestfires$output))

plot(forestfires$freq~forestfires$output)
abline(lm(forestfires$freq~forestfires$output))

plot(forestfires$smoke~forestfires$output)
abline(lm(forestfires$smoke~forestfires$output))

plot(forestfires$hours~forestfires$output)
abline(lm(forestfires$hours~forestfires$output))

##correlation of output with various attributes

cor(forestfires$season,forestfires$output)

cor(forestfires$age,forestfires$output)

cor(forestfires$diseases,forestfires$output)

cor(forestfires$accident,forestfires$output)

cor(forestfires$surgical,forestfires$output)

cor(forestfires$fever,forestfires$output)

cor(forestfires$freq,forestfires$output)

cor(forestfires$smoke,forestfires$output)

cor(forestfires$hours,forestfires$output)

###  root mean square error

rmserror <-function(error)
{
  sqrt(mean(error^2))
}

linear1=lm(forestfires$season~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$age~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$diseases~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$accident~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$surgical~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$fever~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$freq~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$smoke~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(forestfires$hours~forestfires$output)
x<-rmserror(linear1$residuals)
print(x)



null <- lm((output + 1) ~ 1, forestfires[,c(-3, -4)])
full <- lm((output + 1)~., forestfires[,c(-3, -4)])
summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

print(forestfires)
FFMC2 <- (forestfires.train$)^2
FFMC3 <- (forestfires.train$FFMC)^3

DMC2 <- (forestfires.train$DMC)^2
DMC3 <- (forestfires.train$DMC)^3

DC2 <- (forestfires.train$DC)^2
DC3 <- (forestfires.train$DC)^3

ISI2 <- (forestfires.train$ISI)^2
ISI3 <- (forestfires.train$ISI)^3

temp2 <- (forestfires.train$temp)^2
temp3 <- (forestfires.train$temp)^3

RH2 <- (forestfires.train$RH)^2
RH3 <- (forestfires.train$RH)^3

wind2 <- (forestfires.train$wind)^2
wind3 <- (forestfires.train$wind)^3

rain2 <- (forestfires.train$rain)^2
rain3 <- (forestfires.train$rain)^3

lenearmodel <- lm( y ~ forestfires.train$FFMC + I(FFMC2) + I(FFMC3) +
                     forestfires.train$DMC + I(DMC2) + I(DMC3) +
                     forestfires.train$DC + I(DC2) + I(DC3) +
                     forestfires.train$ISI + (ISI2) + (ISI3) +
                     forestfires.train$temp + I(temp2) + I(temp3) +
                     forestfires.train$RH + I(RH2) + I(RH3) +
                     forestfires.train$wind + I(wind2) + I(wind3) +
                     forestfires.train$rain + I(rain2) + I(rain3) )


#set.seed(100)
#x<-read.csv("abc.txt")
#print(x)
#dim(x)
#ind<-sample(2,nrow(x),replace=TRUE,prob=c(0.7,0.3))
#train<-x[ind==1,]
#test<-x[ind==2,]
#print(train)
# knn
#library(class)
#train_input<-as.matrix(train[,-7])
#train_output<-as.vector(train[,7])
#test_input<-as.matrix(test[,-7])
#prediction<-knn(train_input[-7],test_input[-7],train_output[7],k=5)
##
##s<-sample(250,125)
#train<-x[s,]
#test<-x[-s,]
#dim(test)
#dim(train)
#print(test)
#print(train)
#cl<-factor(c(rep("a",25), rep("b",25)))
#cl
#knn(train, test, cl, k = 2, prob=TRUE)

