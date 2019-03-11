setwd("C:\\Users\\vishnu\\Documents\\Project1")

mydata = read.csv("ASHOKLEYALLN.csv")

myData1 = mydata[,4:10]
dim(myData1)


t = floor(dim(myData1)[1]*70/100)
train = myData1[1:t,]
test = myData1[(t+1):dim(myData1)[1],c(1:5,7)]


x = (t+1):(dim(myData1)[1])
actClosing = myData1[(t+1):(dim(myData1)[1]),6]

##Prediction with SVM
library(e1071)

svmFit = svm(Close.Price ~ ., data = train)

summary(svmFit)

predictedVal = predict(svmFit,test)


library(ggplot2)
plot(x, predictedVal,type = "o",col = "red", xlab = "Days", ylab = "Closing Price", main = "Ashok Ln Prediction")
lines(x,actClosing, type = "o", col = "blue")


##Prediction with linear regression
lmFit = lm(Close.Price ~ ., data = train)
predictedValLM = predict(lmFit,test)
plot(x, predictedValLM,type = "o",col = "red", xlab = "Days", ylab = "Closing Price", main = "Ashok Ln Prediction")
lines(x,actClosing, type = "o", col = "blue")

##Prediction with Naive Bayes
NBFit = naiveBayes(Close.Price ~ ., data = train)
predictedValNB = predict(NBFit,test)
plot(x, predictedValNB,type = "o",col = "red", xlab = "Days", ylab = "Closing Price", main = "Ashok Ln Prediction")
lines(x,actClosing, type = "o", col = "blue")

### Reandom Forest
library(randomForest)
RFFit = randomForest(Close.Price ~ ., train, ntree = 5000)
predictedValRF = predict(RFFit,test)
plot(x, predictedValRF,type = "o",col = "red", xlab = "Days", ylab = "Closing Price", main = "Ashok Ln Prediction")
lines(x,actClosing, type = "o", col = "blue")

