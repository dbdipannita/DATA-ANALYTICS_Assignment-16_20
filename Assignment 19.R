#Answers a),b),c)
#using dataset cs2m
#reading the dataset
cs2m <- read.csv("D:/acadgild/cs2m.csv")
View(cs2m)

names(cs2m)
nrow(cs2m)
ncol(cs2m)
str(cs2m)


#classification 
library(caTools)
library(tree)
#splitting
set.seed(1)
split<- sample.split(cs2m$DrugR,SplitRatio = 0.70)
cs2mTrain <- subset(cs2m,split == TRUE)
cs2mTest<- subset(cs2m, split == FALSE)

table(cs2m$DrugR)

table(cs2mTrain$DrugR)

table(cs2mTest$DrugR)

prop.table(table(cs2mTest$DrugR))

table(cs2mTest$DrugR)

prop.table(table(cs2mTrain$DrugR))

modelClassTree<- tree(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH,data = cs2mTrain)
plot(modelClassTree)

text(modelClassTree,pretty = 0 ,cex=0.75)

pred<- predict(modelClassTree,newdata= cs2mTest)
head(pred,3)
cs2m$predict <- predict
cs2m$predictROUND<- round(predict,digits = 0)

#confusion matrix
table(cs2m$DrugR,predict>= 0.5)

sum<- sum(table(cs2m$DrugR,predict>= 0.5))

#interpretation, Accuracy and model goodness  of our model
#accuracy of our model
accuracy<- (13+12)/(30)
accuracy
#0.8333333333

#model goodness
library(verification)
predictTrain<- predict(model,cs2m,type="response")
table(cs2m$DrugR,predictTrain >=0.5)
head(predictTrain,3)
auc(cs2m$DrugR,predictTrain)