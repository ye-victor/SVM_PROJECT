



library(DMwR)
set.seed(12345)
  #IMPORTER CSV DIRECT
creditcard <- read.csv("/Users/Victor/Desktop/SVM Shiny/SVMS/creditcard.csv")
creditcard$Class <- factor(creditcard$Class, levels = c("1", "0"))                                  
creditcard <- creditcard[sample(nrow(creditcard)),]
  #REACTIVE SMOTE PERC.UNDER
smote_data <- SMOTE(Class ~ ., data  = creditcard, perc.over = 300, perc.under = 500, k=5)
new.data <- sample(2, nrow(smote_data), replace = TRUE, prob = c(0.7, 0.3))
trainSplit <-smote_data[new.data==1,]
testSplit <-smote_data[new.data==2,]


### SVM MODEL
library(e1071)
library(pROC)
#tune.svm <- tune(svm,train.x=trainSplit[,-31],train.y=trainSplit[,31],kernel='radial',ranges=list(cost=c(5,6,7,8,9,10), gamma=.3))
#summary(tune.svm)
  #REACTIVE KERNEL, COST, GAMMA
dim(trainSplit)
svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=5, gamma=0.3)
svm.predict <- predict(svm.model,testSplit)
roc(svm.predict,order(testSplit$Class),plot=T,main="SVM",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=10,print.auc.x=20)


#LOGISTIC REGRESSION
set.seed(123)
log.model <- glm(Class~.,binomial,trainSplit)
log.predict <- predict(log.model,testSplit,type="response")
fitted.results <- ifelse(log.predict > 0.5,1,0)

#KNN
library(class)
#predicted.purchase = NULL
#error.rate = NULL
#for(i in 1:20)
#{
#  set.seed(101)
#  predicted.purchase = knn(trainSplit[,-31],testSplit[,-31],trainSplit[,31],k=i)
#  error.rate[i] = mean(testSplit$Class != predicted.purchase)
#}
#print(error.rate)
#k.values <- 1:20
#error.df <- data.frame(error.rate,k.values)
knn.model = knn(trainSplit[,-31],testSplit[,-31],trainSplit[,31],k=7)

#RANDOM FOREST
library(randomForest)
model.rf <- randomForest(Class ~ ., trainSplit , ntree = 1000, importance = TRUE)
plot(model.rf)
cv.tree.pred <- predict(model.rf, testSplit)
plot(cv.tree.pred)

#Comparaison des mod?les
roc(svm.predict,order(testSplit$Class),plot=T,main="SVM",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=50)
plot.roc(fitted.results,order(testSplit$Class),percent=T,col="#4daf4a",lwd=4,print.auc=TRUE,add=TRUE,print.auc.y=40)
plot.roc(knn.model,order(testSplit$Class),percent=T,col="#850606",lwd=4,print.auc=TRUE,add=TRUE,print.auc.y=30)
legend("bottomright",legend=c("SVM","Logistic Regression","KNN"),col=c("#377eb8","#4daf4a","#850606"),lwd=4)


