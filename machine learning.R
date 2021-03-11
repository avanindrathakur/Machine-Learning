install.packages("caret")
install.packages("kernlab")
library(caret); library(kernlab); data(spam)
InTrain <- createDataPartition(y=spam$type, p=0.75, list= FALSE)
training <- spam[InTrain, ]
testing <- spam[-InTrain, ]
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8, arr.ind=T)
names(spam[c(34,32)])
plot(spam[,34],spam[,32])
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation
typeColor <- ((spam$type=="spam")*1 +1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1", ylab="PC2")
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)
data("faithful");set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain, ]; testFaith <- faithful[-inTrain, ]
head(trainFaith)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19,col="blue", xlab="Waiting", ylab="DUration")
lm1 <- lm(eruptions ~ waiting, data= trainFaith)
summary(lm1)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19,col="blue", xlab="Waiting", ylab="DUration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)
coef(lm1)[1]+coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

# training data to test data
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions,pch=19,col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting,predict(lm1,lwd=3))
plot(testFaith$waiting, testFaith$eruptions,pch=19,col="blue", xlab="Waiting", ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)
#calculating RMSE
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
#RMSE on test data
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
#prediction intervals
pred1 <- predict(lm1,newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions,pch=19, col="blue")
matlines(testFaith$waiting[ord],pred1[ord, ],type="l",, col=c(1,2,2), lty=c(1,1,1),lwd=3)

#in caret package

modFit <- train(eruptions ~ waiting, data=trainFaith,method="lm")
summary(modFit$finalModel)

library(ISLR)
install.packages("ISLR")
library(ISLR); library(ggplot2);library(caret);data(Wage)
head(Wage)
Wage <- subset(Wage,select=c(logwage))
summary(Wage)
data("Wage")
summary(Wage)
Wage <- subset(Wage, select = -c(logwage))
head(Wage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]; testing <- Wage[-inTrain, ]
dim(training); dim(testing)
featurePlot(x=training[, c("age","education","jobclass")],y=training$wage,plot="pairs")
head(Wage)
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)
modFit <- train(wage~age+jobclass+education, method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)
plot(finMod,1,pch=19,cex=0,col="#00000010")
qplot(finMod$fitted,finMod$residuals, colour=race,data=training)
par(mfrow=c(1,1))
plot(finMod$residuals,pch=19)
pred <- predict(modFit,testing)
qplot(wage,pred,colour=year,data=testing)
modFitAll <- train(wage~., data=training, method="lm")
pred <- predict(modFitAll,testing)
qplot(wage,pred,data=testing)

# decision tree

#week 3 quiz
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
# creating test and train data sets
inTrain = createDataPartition(y=segmentationOriginal$Case, p = 3/4, list = FALSE)
training = segmentationOriginal[inTrain,]
testing  = segmentationOriginal[-inTrain,]
set.seed(125)
library(rpart)
modFit <- train(Class ~ ., method = "rpart", data = training)
install.packages("e1071")
print(modFit$finalModel)
install.packages("rattle")
suppressMessages(library(rattle))
install.packages("rpart.plot")
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
library(caret)
modFit <- train(Area ~ ., method = "rpart", data = olive)
print(modFit$finalModel)
rattle::fancyRpartPlot(modFit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
# predicting on newdata values
predict(modFit, newdata = newdata)

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)
mod_rf <- train(diagnosis ~ ., training, method = "rf")
mod_gbm <- train(diagnosis ~ ., training, method = "gbm", verbose = FALSE)
mod_lda <- train(diagnosis ~ ., training, method = "lda")

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]


