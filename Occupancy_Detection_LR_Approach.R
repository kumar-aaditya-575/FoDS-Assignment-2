#---------------------------------------------------------------
#LOGISTIC REGRESSION APPROACH TO SOLVE THE TIME SERIES PROBLEM
#---------------------------------------------------------------


setwd("C:/Users/ACER/Desktop/Assignment#2")
library(psych)
roomtrain = read.table('datatraining.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest1= read.table('datatest.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest2= read.table('datatest2.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
room = rbind(roomtrain,roomtest1,roomtest2)

#converting occupancy status into a label 
room = room[-1]
a<-room$Occupancy
room$Occupancy = factor(room$Occupancy)

#Feature Scaling
room_n = scale(room[1:5])#excluding the lable column

room_n = cbind(room_n,room[6])

cor(room_n$Light,a)
#Randomply sample the data into two parts with replacement and probability
library(caTools)
set.seed(123)
split = sample.split(room_n, SplitRatio = 0.8)
train = subset(room_n, split == TRUE)
test = subset(room_n, split == FALSE)


#Model using all vars
model =  glm(Occupancy~.,train,family= binomial(link = "logit"))
#


#plotting graph of correlation of room dataset with pairwise
pairs.panels(room)
#Light CO2 and Temperatures have corr coeffts >=.5 so we'll attenmpt to build a model with theses
model2<-glm(Occupancy~Light+Temperature+CO2,train,family= binomial(link = "logit")) 

#Light and Occupancy have highest corrleation. So modelling the problem through only cadence levels will make our smodel simpler
model3<-glm(Occupancy~Light,train,family= binomial(link = "logit"))            


#pred = predict(model,test[,-6])
#pred = predict(model2,test[,-6])
#pred = predict(model3,test[,-6])

#All values with probability >= than 0.5 are considered occupied. Threshold value is arbitrarily picked to be 0.5 
pred[pred>=0.5] = 1
pred[pred<0.5] = 0
pred = factor(pred)

   
accuracy<-sum(pred==test$Occupancy)/length(pred)
accuracy*100

#forming confusion matrix
tt<-sum((pred=="1")*(test$Occupancy=="1"))
tt
tf<-sum((pred=="1") * (test$Occupancy=="0"))
tf
ft<-sum((pred=="0") * (test$Occupancy=="1"))
ft
ff<-sum((pred=="0") * (test$Occupancy=="0"))
ff

#Output:Apprrox 98.92% accurate--model1
#Output:Apprrox 98.89% accurate--model2
#Output: Apprrox 98.89% accurate--model3

#CONFUSION MATRIX
#tt-true +ve 
#tf-false +ve
#ft-false -ve
#ff-true -ve

#MODEL1
# > tt<-sum((pred=="1")*(test$Occupancy=="1"))
# > tt
# [1] 1573
# > tf<-sum((pred=="1") * (test$Occupancy=="0"))
# > tf
# [1] 67
# > ft<-sum((pred=="0") * (test$Occupancy=="1"))
# > ft
# [1] 7
# > ff<-sum((pred=="0") * (test$Occupancy=="0"))
# > ff
# [1] 5206

#MODEL 2
# tt<-sum((pred=="1")*(test$Occupancy=="1"))
# > tt
# [1] 1568
# > tf<-sum((pred=="1") * (test$Occupancy=="0"))
# > tf
# [1] 64
# > ft<-sum((pred=="0") * (test$Occupancy=="1"))
# > ft
# [1] 12
# > ff<-sum((pred=="0") * (test$Occupancy=="0"))
# > ff
# [1] 5209

#MODEL 3

# > tt<-sum((pred=="1")*(test$Occupancy=="1"))
# > tt
# [1] 1573
# > tf<-sum((pred=="1") * (test$Occupancy=="0"))
# > tf
# [1] 69
# > ft<-sum((pred=="0") * (test$Occupancy=="1"))
# > ft
# [1] 7
# > ff<-sum((pred=="0") * (test$Occupancy=="0"))
# > ff
# [1] 5204
# > 


