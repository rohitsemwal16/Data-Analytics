library(class)

## Descriptive Analysis
##fertility <- read.csv("kl.csv",header = TRUE, fill = TRUE)
fertility<-read.csv("new_diag.txt",sep = ",")
#print(fertility$Age)



#fertility<-fertility[which(fertility$area != 0),]
n<-nrow(fertility);

test <- sample(1:n, round(n)/10)
fertility.train <- fertility[-test, ]
fertility.test <- fertility[test, ]
print(n)

## converting nominal to ordinal data
for (i in 1:n)
  if (fertility[i,10]=="N")
    fertility[i,10]=1.0
if(fertility[i,10]=="O")
  fertility[i,10]=0
print(fertility)

##chisq test for dependence of variables
## value less than .05 will depict interdependence between x and y

x=table(fertility$season,fertility$output)
print(chisq.test(x));

print(chisq.test(fertility$age,fertility$output));

print(chisq.test(fertility$diseases,fertility$output));

print(chisq.test(fertility$accident,fertility$output));

print(chisq.test(fertility$surgical,fertility$output));

print(chisq.test(fertility$fever,fertility$output));

print(chisq.test(fertility$freq,fertility$output));

print(chisq.test(fertility$smoke,fertility$output));

print(chisq.test(fertility$hours,fertility$output));

##regression lines of various attributes with output

plot(fertility$season~fertility$output)
abline(lm(fertility$season~fertility$output))

plot(fertility$age~fertility$output)
abline(lm(fertility$age~fertility$output))

plot(fertility$diseases~fertility$output)
abline(lm(fertility$diseases~fertility$output))

plot(fertility$accident~fertility$output)
abline(lm(fertility$accident~fertility$output))

plot(fertility$surgical~fertility$output)
abline(lm(fertility$surgical~fertility$output))

plot(fertility$fever~fertility$output)
abline(lm(fertility$fever~fertility$output))

plot(fertility$freq~fertility$output)
abline(lm(fertility$freq~fertility$output))

plot(fertility$smoke~fertility$output)
abline(lm(fertility$smoke~fertility$output))

plot(fertility$hours~fertility$output)
abline(lm(fertility$hours~fertility$output))

##correlation of output with various attributes

cor(fertility$season,fertility$output)

cor(fertility$age,fertility$output)

cor(fertility$diseases,fertility$output)

cor(fertility$accident,fertility$output)

cor(fertility$surgical,fertility$output)

cor(fertility$fever,fertility$output)

cor(fertility$freq,fertility$output)

cor(fertility$smoke,fertility$output)

cor(fertility$hours,fertility$output)

###  root mean square error

rmserror <-function(error)
{
  sqrt(mean(error^2))
}

linear1=lm(fertility$season~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$age~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$diseases~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$accident~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$surgical~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$fever~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$freq~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$smoke~fertility$output)
x<-rmserror(linear1$residuals)
print(x)

linear1=lm(fertility$hours~fertility$output)
x<-rmserror(linear1$residuals)
print(x)



null <- lm((output + 1) ~ 1, fertility[,c(-3, -4)])
full <- lm((output + 1)~., fertility[,c(-3, -4)])
summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

print(fertility)

season2 <- (fertility.train$season)^2
season3 <- (fertility.train$season)^3

age2 <- (fertility.train$age)^2
age3 <- (fertility.train$age)^3

diseases2 <- (fertility.train$diseases)^2
diseases3 <- (fertility.train$diseases)^3

accident2 <- (fertility.train$accident)^2
accident3 <- (fertility.train$accident)^3

surgical2 <- (fertility.train$surgical)^2
surgical3 <- (fertility.train$surgical)^3

fever2 <- (fertility.train$fever)^2
fever3 <- (fertility.train$fever)^3

freq2 <- (fertility.train$freq)^2
freq3 <- (fertility.train$freq)^3

smoke2 <- (fertility.train$smoke)^2
smoke3 <- (fertility.train$smoke)^3

hours2 <- (fertility.train$hours)^2
hours3 <- (fertility.train$hours)^3

accident2
y <- fertility.train$output

## checking if it is linear or power2 or power3 dependent
lenearmodel <- lm( y ~ fertility.train$season + I(season2) + I(season3) +
                     fertility.train$age + I(age2) + I(age3) +
                     fertility.train$diseases + I(diseases2) + I(diseases3) +
                     fertility.train$accident + (accident2) + (accident3) +
                     fertility.train$surgical + I(surgical2) + I(surgical3) +
                     fertility.train$fever + I(fever2) + I(fever3) +
                     fertility.train$freq + I(freq2) + I(freq3) +
                     fertility.train$smoke + I(smoke2) + I(smoke3) +
                     fertility.train$hours + I(hours2) + I(hours3))

print(lenearmodel)


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

