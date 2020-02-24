remove(list = ls())
setwd("H:/DATA SCIENCE TRAINING/Machine Learning")

TrLink<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TsLink<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training<- read.csv(TrLink)
test<- read.csv(TsLink)

library(caret)
library(tidyr)
library(dplyr)

set.seed(5673)

#get id number
training <- group_by(training,num_window) 
training<- mutate(training, winID = row_number())

#separate out data with details and without
train_nodeet<-training[training$new_window=="yes",-c(8:11,37:49,60:68,84:86,102,113:124,140,151:159,161)]
train_deet<-training[training$num_window %in% train_nodeet$num_window,c(1:2,7:11,37:49,60:68,84:86,102,113:124,140,151:161)]

#create wide version of data - but not useful for test code, so whatever
train_w<-train_nodeet
for(i in 4:55){
  temp<-pivot_wider(train_deet[,c(3,i,57)],names_from=winID,values_from = 2,names_prefix=names(train_deet[,i]))
  train_w<- cbind(train_w,temp)
}

#create training data that is useful
train_deet2<-training[,c(1:4,7:11,37:49,60:68,84:86,102,113:124,140,151:160)]
#add time since last measurement
train_deet2$time_per<- with(train_deet2,ifelse(user_name==shift(user_name) 
                          & num_window==shift(num_window) 
                          & !X==1,
                        shift(raw_timestamp_part_1)*1000000+shift(raw_timestamp_part_2)-(raw_timestamp_part_1*1000000+raw_timestamp_part_2),0))

kpart<-createTimeSlices(train_deet2$classe,1000,horizon = 1000)
kpart2<-createTimeSlices(train_deet2$classe,1000,horizon = 1000)

#partition for stacked analysis
instack <- createDataPartition(y=train_deet2$classe, p=.5, list=FALSE)
initial<- train_deet2[instack,]
vote<- train_deet2[-instack,]

#model random forest, linear descrimenent, gbm
mod_rf<-train(classe~.,data=initial[,-(1:5)],preProcess=c("center","scale"),method="rf")
mod_lda <-train(classe~.,data=initial[,-(1:5)],preProcess=c("center","scale"),method="lda")
mod_gbm <-train(classe~.,data=initial[,-(1:5)],preProcess=c("center","scale"),method="gbm")

#get accuracies of different models
pred_rf<-predict(mod_rf,vote)
pred_lda<-predict(mod_lda,vote)
pred_gbm<-predict(mod_lda,vote)

match_rf<-as.logical(pred_rf==vote$classe)
match_lda<-as.logical(pred_lda==vote$classe)
match_gbm<-as.logical(pred_gbm==vote$classe)

acc_rf<-sum(match_rf)/length(match_rf)*100
acc_lda<-sum(match_lda)/length(match_lda)*100
acc_gbm<<-sum(match_gbm)/length(match_gbm)*100

print(acc_rf)
print(acc_lda)
print(acc_gbm)

match_gbm_lda<-as.logical(pred_gbm==pred_lda)
acc_gbm_lda<<-sum(match_gbm_lda)/length(match_gbm_lda)*100

#create testing data that is useful
test_deet<-test[,c(1:4,7:11,37:49,60:68,84:86,102,113:124,140,151:160)]
#add time since last measurement
test_deet$time_per<- with(test_deet,ifelse(user_name==shift(user_name) 
                                               & num_window==shift(num_window) 
                                               & !X==1,
                                               shift(raw_timestamp_part_1)*1000000+shift(raw_timestamp_part_2)-(raw_timestamp_part_1*1000000+raw_timestamp_part_2),0))

final_rf_pred <-predict(mod_rf,test_deet[,-(1:5)]) 
