# Author: Jasper

import pandas as pd
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout
from sklearn import preprocessing
from sklearn.metrics import classification_report, confusion_matrix
import tensorflow

#import data
train = pd.read_csv("balanced_train.csv")
test = pd.read_csv("final_test.csv")

#data wrangling
train["y"] = train["y"].replace("yes",1).replace("no",0)
test["y"] = test["y"].replace("yes",1).replace("no",0)

X_train = train.iloc[:,1:11]
X_test = test.iloc[:,1:11]
Y_train = train.iloc[:,11]
Y_test = test.iloc[:,11]

#set dummy variables
#X_train
job_train = pd.get_dummies(X_train["job"],prefix = "job")
marital_train = pd.get_dummies(X_train["marital"],prefix = "marital")
education_train = pd.get_dummies(X_train["education"],prefix = "education")
month_train = pd.get_dummies(X_train["month"],prefix = "month")
day_train = pd.get_dummies(X_train["day_of_week"],prefix = "day")
poutcome_train = pd.get_dummies(X_train["poutcome"],prefix = "poutcome")

job_train = job_train.drop("job_unemployed",1)
marital_train = marital_train.drop("marital_single",1)
education_train = education_train.drop("education_basic.4y",1)
month_train = month_train.drop("month_mar",1)
day_train = day_train.drop("day_mon",1)
poutcome_train = poutcome_train.drop("poutcome_failure",1)

X_train = X_train.drop(['job','marital', 'education','month','day_of_week','poutcome'],1)

for i in [job_train,marital_train,education_train,month_train,day_train,poutcome_train]:
    X_train = pd.concat([X_train,i],axis = 1)

X_train["housing"] = X_train["housing"].replace("yes",1).replace("no",0)
X_train["loan"] = X_train["loan"].replace("yes",1).replace("no",0)

#X_test
job_test = pd.get_dummies(X_test["job"],prefix = "job")
marital_test = pd.get_dummies(X_test["marital"],prefix = "marital")
education_test = pd.get_dummies(X_test["education"],prefix = "education")
month_test = pd.get_dummies(X_test["month"],prefix = "month")
day_test = pd.get_dummies(X_test["day_of_week"],prefix = "day")
poutcome_test = pd.get_dummies(X_test["poutcome"],prefix = "poutcome")

job_test = job_test.drop("job_unemployed",1)
marital_test = marital_test.drop("marital_single",1)
education_test = education_test.drop("education_basic.4y",1)
month_test = month_test.drop("month_mar",1)
day_test = day_test.drop("day_mon",1)
poutcome_test = poutcome_test.drop("poutcome_failure",1)

X_test = X_test.drop(['job','marital', 'education','month','day_of_week','poutcome'],1)

for i in [job_test,marital_test,education_test,month_test,day_test,poutcome_test]:
    X_test = pd.concat([X_test,i],axis = 1)

X_test["housing"] = X_test["housing"].replace("yes",1).replace("no",0)
X_test["loan"] = X_test["loan"].replace("yes",1).replace("no",0)

#Scaling
scaler = preprocessing.MinMaxScaler().fit(X_train)
scaled_X_train = scaler.transform(X_train)
scaled_X_test = scaler.transform(X_test)

# First structure: 2 hidden layers, 25 nodes
# 1.epochs=100, batch_size=30
model = Sequential()
model.add(Dense(25, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(25, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
#set seed
tensorflow.random.set_seed(2020)
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions1 = model.predict(scaled_X_test)
Y_pred1 = (predictions1 > 0.5)
Y_pred1 = Y_pred1*1 #convert to 0,1 instead of True False
cm1 = confusion_matrix(Y_test, Y_pred1)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm1)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc1 = (cm1[0,0] + cm1[1,1]) / sum(sum(cm1))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc1*100))
fnr1 = cm1[1,0] / (cm1[1,0]+cm1[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr1*100))


# 2.epochs=100, batch_size=50
model = Sequential()
model.add(Dense(25, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(25, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=50,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions2 = model.predict(scaled_X_test)
Y_pred2 = (predictions2 > 0.5)
Y_pred2 = Y_pred2*1 #convert to 0,1 instead of True False
cm2 = confusion_matrix(Y_test, Y_pred2)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm2)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc2 = (cm2[0,0] + cm2[1,1]) / sum(sum(cm2))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc2*100))
fnr2 = cm2[1,0] / (cm2[1,0]+cm2[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr2*100))

# 3.epochs=300, batch_size=30
model = Sequential()
model.add(Dense(25, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(25, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=300, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions3 = model.predict(scaled_X_test)
Y_pred3 = (predictions3 > 0.5)
Y_pred3 = Y_pred3*1 #convert to 0,1 instead of True False
cm3 = confusion_matrix(Y_test, Y_pred3)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm3)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc3 = (cm3[0,0] + cm3[1,1]) / sum(sum(cm3))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc3*100))
fnr3 = cm3[1,0] / (cm3[1,0]+cm3[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr3*100))

# 4.epochs=100, batch_size=10
model = Sequential()
model.add(Dense(25, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(25, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=10,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions4 = model.predict(scaled_X_test)
Y_pred4 = (predictions4 > 0.5)
Y_pred4 = Y_pred4*1 #convert to 0,1 instead of True False
cm4 = confusion_matrix(Y_test, Y_pred4)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm4)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc4 = (cm4[0,0] + cm4[1,1]) / sum(sum(cm4))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc4*100))
fnr4 = cm4[1,0] / (cm4[1,0]+cm4[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr4*100))

# Second Structure: 3 hidden layers, 30 nodes
# epochs=100, batch_size=30
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
tensorflow.random.set_seed(2020)
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions1 = model.predict(scaled_X_test)
Y_pred1 = (predictions1 > 0.5)
Y_pred1 = Y_pred1*1 #convert to 0,1 instead of True False
cm1 = confusion_matrix(Y_test, Y_pred1)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm1)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc1 = (cm1[0,0] + cm1[1,1]) / sum(sum(cm1))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc1*100))
fnr1 = cm1[1,0] / (cm1[1,0]+cm1[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr1*100))

#2. epochs=100, batch_size=50
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=50,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions2 = model.predict(scaled_X_test)
Y_pred2 = (predictions2 > 0.5)
Y_pred2 = Y_pred2*1 #convert to 0,1 instead of True False
cm2 = confusion_matrix(Y_test, Y_pred2)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm2)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc2 = (cm2[0,0] + cm2[1,1]) / sum(sum(cm2))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc2*100))
fnr2 = cm2[1,0] / (cm2[1,0]+cm2[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr2*100))

#3. epochs=300, batch_size=30
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=300, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions3 = model.predict(scaled_X_test)
Y_pred3 = (predictions3 > 0.5)
Y_pred3 = Y_pred3*1 #convert to 0,1 instead of True False
cm3 = confusion_matrix(Y_test, Y_pred3)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm3)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc3 = (cm3[0,0] + cm3[1,1]) / sum(sum(cm3))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc3*100))
fnr3 = cm3[1,0] / (cm3[1,0]+cm3[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr3*100))

#4.epochs=100, batch_size=10
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(30, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=10,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
Y_pred4 = (predictions4 > 0.5)
Y_pred4 = Y_pred4*1 #convert to 0,1 instead of True False
cm4 = confusion_matrix(Y_test, Y_pred4)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm4)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc4 = (cm4[0,0] + cm4[1,1]) / sum(sum(cm4))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc4*100))
fnr4 = cm4[1,0] / (cm4[1,0]+cm4[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr4*100))



# Third Structure: 3 layers, 30,20,10 nodes
#1.epochs=100, batch_size=30
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(20, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
tensorflow.random.set_seed(2020)
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions1 = model.predict(scaled_X_test)
Y_pred1 = (predictions1 > 0.5)
Y_pred1 = Y_pred1*1 #convert to 0,1 instead of True False
cm1 = confusion_matrix(Y_test, Y_pred1)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm1)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc1 = (cm1[0,0] + cm1[1,1]) / sum(sum(cm1))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc1*100))
fnr1 = cm1[1,0] / (cm1[1,0]+cm1[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr1*100))

# 2.epochs=100, batch_size=10
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(20, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=100, batch_size=10,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions2 = model.predict(scaled_X_test)
Y_pred2 = (predictions2 > 0.5)
Y_pred2 = Y_pred2*1 #convert to 0,1 instead of True False
cm2 = confusion_matrix(Y_test, Y_pred2)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm2)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc2 = (cm2[0,0] + cm2[1,1]) / sum(sum(cm2))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc2*100))
fnr2 = cm2[1,0] / (cm2[1,0]+cm2[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr2*100))

# 3.epochs=300, batch_size=30
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(20, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=300, batch_size=30,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions3 = model.predict(scaled_X_test)
Y_pred3 = (predictions3 > 0.5)
Y_pred3 = Y_pred3*1 #convert to 0,1 instead of True False
cm3 = confusion_matrix(Y_test, Y_pred3)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm3)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc3 = (cm3[0,0] + cm3[1,1]) / sum(sum(cm3))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc3*100))
fnr3 = cm3[1,0] / (cm3[1,0]+cm3[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr3*100))

# 4.epochs=500, batch_size=50
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(20, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
model.fit(scaled_X_train, Y_train, epochs=500, batch_size=50,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions4 = model.predict(scaled_X_test)
Y_pred4 = (predictions4 > 0.5)
Y_pred4 = Y_pred4*1 #convert to 0,1 instead of True False
cm4 = confusion_matrix(Y_test, Y_pred4)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm4)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc4 = (cm4[0,0] + cm4[1,1]) / sum(sum(cm4))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc4*100))
fnr4 = cm4[1,0] / (cm4[1,0]+cm4[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr4*100))


#%%

# model diagnosis

# rebuilt the optimal model with more epochs
model = Sequential()
model.add(Dense(30, input_dim = 37 , activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(20, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='sigmoid'))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
tensorflow.random.set_seed(2020)
model.compile(loss='binary_crossentropy', optimizer="adamax", metrics=['accuracy'])
history = model.fit(scaled_X_train, Y_train, epochs=300, batch_size=10,validation_split=0.1)
# evaluate the model
scores = model.evaluate(scaled_X_train, Y_train)
#print(scores)
print("Neural Network Trainset: \n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
predictions2 = model.predict(scaled_X_test)
Y_pred2 = (predictions2 > 0.5)
Y_pred2 = Y_pred2*1 #convert to 0,1 instead of True False
cm2 = confusion_matrix(Y_test, Y_pred2)
print("==================================")
print("==================================")
print("Neural Network on testset confusion matrix")
print(cm2)
## Get accurary from Confusion matrix
## Position 0,0 and 1,1 are the correct predictions
acc2 = (cm2[0,0] + cm2[1,1]) / sum(sum(cm2))
print("Neural Network on TestSet: Accuracy %.2f%%" % (acc2*100))
fnr2 = cm2[1,0] / (cm2[1,0]+cm2[1,1])
print("Neural Network on TestSet: fnr: %.2f%%" % (fnr2*100))


import matplotlib.pyplot as plt
# plot trainset and validation set accuracy
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.title('Model accuracy')
plt.ylabel('Accuracy')
plt.xlabel('Epoch')
plt.legend(['Train', 'Validation'], loc='upper left')
plt.show()
# plot trainset and validation set loss
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Model loss')
plt.ylabel('Loss')
plt.xlabel('Epoch')
plt.legend(['Train', 'Validation'], loc='upper left')
plt.show()
