#Answers
#a
#reading the dataset
#using bankloan dataset
bankloan1 <- read.csv("D:/acadgild/bankloan1.csv")
View(bankloan1)

str(bankloan1)

# tree
library(caTools)
library(tree)
set.seed(1)
sam<- sample(x=1: nrow(bankloan1), size = 0.80*nrow(bankloan1))
train1<- bankloan1[sam, ]
table(bankloan1$default)
table(train1$default)

test1<- bankloan1[-sam, ]
table(test1$default)

model_tree1<- tree(default~., data = train1)
summary(model_tree1)

plot(model_tree1); text(model_tree1, pretty = 0, cex = 0.75)

pred_tree1<- predict(model_tree1, newdata = test1,
                     type = 'class')

conf_tree1<- table(test1$default, pred_tree1)
conf_tree1

OAA_tree1<- (conf_tree1[1,1]+conf_tree1[2,2])/sum(conf_tree1)
OAA_tree1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~RF

#random forest
library(randomForest)
model_rf1<- randomForest(default~., data = train1)
model_rf1

summary(model_rf1)

#b),c),d),e)
#Answers
#validation 
#interpretation, Accuracy and model goodness  of our model
#verify model goodness of fit
library(rpart)
library(caret)        
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
#method rf
model<- train(default~., data=train1, trControl=train_control, method="rf")
model

# make predictions
predictions<- predict(model,test1)

# append predictions
pred<- cbind(test1,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$default)
confusionMatrix

# define training control
train_control<- trainControl(method="cv", number=10)


# method boosted tree
model<- train(default~., data=train1, trControl=train_control, method="bstTree")
model

# make predictions
predictions<- predict(model,test1)

# append predictions
pred<- cbind(test1,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$churn)
confusionMatrix

#how do we create a cross validation scheme
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3)
seed <-7
metric <- 'Accuracy'
set.seed(seed)
mtry <- sqrt(ncol(train1))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(default~., 
                    data = train1,
                    method = 'rf',
                    metric = metric,
                    tuneGrid = tunegrid,
                    trControl = control)
print(rf_default)

#prediction of model_rf1
pred_rf1<- predict(model_rf1, test1, type = 'class')
head(pred_rf1, 15)

#interpretation, Accuracy and model goodness  of our model
#verify model goodness of fit
#summary
summary(model_rf1)

#confusion matrix of model_rf1
conf_rf1<- table(test1$default, pred_rf1)
conf_rf1

#accuracy of model_rf1
OAA_rf1<- (conf_rf1[1,1]+conf_rf1[2,2])/sum(conf_rf1)
OAA_rf1

#plotting imp of variance 
library(caret)

importance(model_rf1)
varImp(model_rf1)
varImpPlot(model_rf1, col = 'red')