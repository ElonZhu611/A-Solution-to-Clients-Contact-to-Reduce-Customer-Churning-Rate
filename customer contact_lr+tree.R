#==================================LOGISTIC REGRESSION============================================#
library(data.table)
set.seed(2020)
trainset <- fread("balanced_train.csv",stringsAsFactors = TRUE)
trainset$age1 <- (trainset$age - min(trainset$age))/(max(trainset$age)-min(trainset$age))
trainset <- trainset[,-1]
trainset$job = relevel(trainset$job, "unemployed")
#View(trainset)
testset <- fread("final_test.csv",,stringsAsFactors = TRUE)
testset$age1 <- (testset$age - min(testset$age))/(max(testset$age)-min(testset$age))
testset <- testset[,-1]
trainset$job = relevel(trainset$job, "unemployed")
#View(trainset)
#------------------------------------------------------------------------------------------------------
#logistics
#=====model1=====#
m1 <- glm(y ~ . , family = binomial, data = trainset)
summary(m1)
#variables not sig: age,education,housing,loan,day_of_week
#=====model2=====#
m2 <- glm(y ~ .-age1-housing-loan-day_of_week, family = binomial, data = trainset)
summary(m2)
#=====accuracy for model1=====#
library(data.table)
library(caTools)
threshold1 <- 0.4
# Confusion Matrix on Trainset
prob.train1 <- predict(m1, type = 'response')
predict.y.train1 <- ifelse(prob.train1 > threshold1, "yes", "no")
table1.1 <- table(trainset$y, predict.y.train1)
table1.1
prop.table(table1.1)
# Overall Accuracy
mean(predict.y.train1 == trainset$y)
# Confusion Matrix on Testset
prob.test1 <- predict(m1, newdata = testset, type = 'response')
predict.y.test1 <- ifelse(prob.test1 > threshold1, "yes", "no")
table1.2 <- table(testset$y, predict.y.test1)
table1.2
prop.table(table1.2)
# Overall Accuracy
mean(testset$y==predict.y.test1)

#=====accuracy for model2=====#
library(data.table)
library(caTools)
threshold1 <- 0.6
# Confusion Matrix on Trainset
prob.train <- predict(m2, type = 'response')
predict.y.train <- ifelse(prob.train > threshold1, "yes", "no")
table2.1 <- table(trainset$y, predict.y.train)
table2.1
prop.table(table2.1)
# Overall Accuracy
mean(predict.y.train == trainset$y)
# Confusion Matrix on Testset
prob.test <- predict(m2, newdata = testset, type = 'response')
predict.y.test <- ifelse(prob.test > threshold1, "yes", "no")
table2.2 <- table(testset$y, predict.y.test)
table2.2
prop.table(table2.2)
# Overall Accuracy
mean(testset$y==predict.y.test)

#==================================TREE============================================#
library(data.table)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Matrix)
library(xgboost)

dt = fread("contact_short.csv",stringsAsFactors = TRUE)
dt$y = relevel(dt$y,"yes")

#preprocessing
#train-test split

set.seed(2020)
train <- sample.split(Y = dt$y, SplitRatio = 0.8)
trainset <- subset(dt, train == T)
testset <- subset(dt, train == F)

#Decision Tree

set.seed(2020)   # for CV
# Grow the full tree m1
m1 <- rpart(y ~ . , data = trainset, method = 'class', cp = 0)
# prp(m1, type=2, extra=101, nn.box.col = 'light blue')
printcp(m1, digits = 3) 
# However, Root node error is relatively low, which means the data are faced with some imbalanced problems.
summary(trainset$y)
# no: 27,190, yes: 3,406, P(yes) = 0.111.

# Random sample from majority class "no" and combine into new trainset
train.yes <- trainset[y == "yes"]
train.no <- trainset[y == "no"]
set.seed(2020)
chosen <- sample(seq(1:nrow(train.no)), size = nrow(train.yes))
train.no.chosen <- train.no[chosen]

# Combine two data tables by appending the rows
trainset2 <- rbind(train.yes, train.no.chosen)
summary(trainset2$y)
# no: 3,406, yes: 3,406, P(yes) = 0.5

set.seed(2020)   # for CV
# Grow the full tree m2, with balanced data
m2 <- rpart(y ~ . , data = trainset2, method = 'class', cp = 0)
summary(m2)
printcp(m2, digits = 3)
plotcp(m2)
prp(m2, type=2, extra=101, nn.box.col = 'light blue')

# Prune the tree
# Use 1SE rule in the full tree m2 
CVerror.cap <- m2$cptable[which.min(m2$cptable[,"xerror"]), "xerror"] + m2$cptable[which.min(m2$cptable[,"xerror"]), "xstd"]
# Find the optimal cp whose CV error is just below CVerror.cap in m2.
i = 1
j = 4
while (m2$cptable[i,j] > CVerror.cap) {
  i = i + 1
}

# Get geometric mean of the two identified optimal cp if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(m2$cptable[i,1] * m2$cptable[i-1,1]), 1)
# Get best tree based on 10 fold CV with 1 SE
m2.best <- prune(m2, cp = cp.opt)
summary(m2.best)
printcp(m2.best, digits = 3)
plotcp(m2.best)
prp(m2.best, type=2, extra=101, nn.box.col = 'light blue')

library(rattle)
fancyRpartPlot(m2.best,main="CART for client contact",sub="",cex=0.7)

m2.best.rules <- rpart.rules(m2.best, nn = T, cover = T)
View(m2.best.rules)  ## View just the decision rules.
print(m2.best) ## View the tree on the console.

# Compare importance of different variables
m2.best$variable.importance
# Scale the importance so that they are more comparable
m2.best.scaledVarImpt <- round(100*m2.best$variable.importance/sum(m2.best$variable.importance))
m2.best.scaledVarImpt

# Here, we illustrate CART model predictions on testset.
predicted.test <- predict(m2.best, newdata = testset, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
cm.table.test = table(testset$y, predicted.test)
cm.table.test
acc.test = (cm.table.test[1,1]+cm.table.test[2,2])/nrow(testset); acc.test
FNR.test = cm.table.test[1,2]/(cm.table.test[1,1]+cm.table.test[1,2]); FNR.test 
FPR.test = cm.table.test[2,1]/(cm.table.test[2,1]+cm.table.test[2,2]); FPR.test
# While the accuracy is quite decent, false negative rate is extra high, because we are using imbalanced data.

# Random Forest
n = length(trainset2)

# Choose a suitable mtry
set.seed(2020)
for(i in 1:(n-1)){
  mtry_fit <- randomForest(y~.,data=trainset2,mtry=i)
  err <- mean(mtry_fit$err.rate)
  print(err)
}
# When i=2, we can get the lowest err. 
# But when i=3, the false negative rate will go down.

# Choose a suitable ntree
set.seed(2020)
ntree_fit <- randomForest(y~.,data=trainset2,mtry=3,ntree=1000)
plot(ntree_fit)
# When ntree=200, error begins to be stable.
# And after ntree reaches 800, error almost stops changing.

set.seed(2020)
rf <- randomForest(y~.,data=trainset2,mtry=3,ntree=1000,importance=T)
rf
rf.impo = randomForest::importance(rf)
rf.impo
varImpPlot(rf,main="Variables Importance Based on Random Forest Model")

# Here, we illustrate random forest model predictions on testset.
predicted.rf <- predict(rf, newdata = testset, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
cm.table.rf = table(testset$y, predicted.rf)
cm.table.rf
acc.rf = (cm.table.rf[1,1]+cm.table.rf[2,2])/nrow(testset); acc.rf
FNR.rf = cm.table.rf[1,2]/(cm.table.rf[1,1]+cm.table.rf[1,2]); FNR.rf 
FPR.rf = cm.table.rf[2,1]/(cm.table.rf[2,1]+cm.table.rf[2,2]); FPR.rf

# XGBoost
# Convert dataset into matrix
trainset2$adjy = ifelse(trainset2$y=="yes",1,0)
trainmat1 <- data.matrix(trainset2[,c(1:10)])
trainmat2 <- Matrix(trainmat1,sparse=T)
trainmat3 <- trainset2$adjy
trainmat <- list(data=trainmat2,label=trainmat3)
dtrain <- xgb.DMatrix(data=trainmat$data, label=trainmat$label)

testset$adjy = ifelse(testset$y=="yes",1,0)
testmat1 <- data.matrix(testset[,c(1:10)])
testmat2 <- Matrix(testmat1,sparse=T)
testmat3 <- testset$adjy
testmat <- list(data=testmat2,label=testmat3)
dtest <- xgb.DMatrix(data=testmat$data, label=testmat$label)

set.seed(2020)
xgb <- xgboost::xgboost(data=dtrain,max_depth=5,eta=0.1,objective='binary:logistic',nrounds=500)

predicted.xgb = ifelse(predict(xgb,newdata = dtest)>0.5,1,0)
cm.table.xgb = table(testset$adjy,predicted.xgb)
cm.table.xgb
acc.xgb = (cm.table.xgb[1,1]+cm.table.xgb[2,2])/nrow(testset); acc.xgb
FPR.xgb = cm.table.xgb[1,2]/(cm.table.xgb[1,1]+cm.table.xgb[1,2]); FPR.xgb 
FNR.xgb = cm.table.xgb[2,1]/(cm.table.xgb[2,1]+cm.table.xgb[2,2]); FNR.xgb

xgb.impo <- xgb.importance(model = xgb)
xgb.impo
xgb.plot.importance(xgb.impo[1:10,])

xgb.roc <- roc(testset$adjy,as.numeric(predicted.xgb))
plot(xgb.roc, print.auc=T,col="blue",auc.polygon=T,print.thres=T,main="ROC curve for XGBoost")








