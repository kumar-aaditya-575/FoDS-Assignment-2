
#----------------------------------------------
#NEURAL NETWORK APPROACH TO SOLVE THE TIME SERIES PROBLEM
#----------------------------------------------
setwd("C:/Users/ACER/Desktop/Assignment#2")

roomtrain = read.table('datatraining.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest1= read.table('datatest.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest2= read.table('datatest2.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
room = rbind(roomtrain,roomtest1,roomtest2)

room = room[-1]

room_n = scale(room[1:5])#excluding the lable column
room_n = cbind(room_n,room[6])

#Randomply sample the data into two parts with replacement and probability
library(caTools)
set.seed(123)
split = sample.split(room_n, SplitRatio = 0.8)
train = subset(room_n, split == TRUE)
test = subset(room_n, split == FALSE)



library(neuralnet)
#building a neural network with one hidden layer of two nodes
NN = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,data=train,linear.output = TRUE,hidden = 2)

plot(NN)
predict_testNN = compute(NN, test[,-6])
pred<-predict_testNN$net.result


#From the graph, for all predictions greater than 0.5 ???1 else 0
pred[pred>=0.5] = 1
pred[pred<0.5] = 0
pred = factor(pred)
plot(pred)

accuracy<-sum(pred==test$Occupancy)/length(pred)
accuracy*100


a<-attributes(pred)$Levels

#forming confusion matrix
tt<-sum((pred=="1")*(test$Occupancy=="1"))
tt
tf<-sum((pred=="1") * (test$Occupancy=="0"))
tf
ft<-sum((pred=="0") * (test$Occupancy=="1"))
ft
ff<-sum((pred=="0") * (test$Occupancy=="0"))
ff










