
ptm=proc.time()

library(caret)
library(tidyr)
library(plyr)
library(dplyr)
library(caTools)
library(reshape2)
library(gbm)
library(caTools)
library(randomForest)
library(ggplot2)
library(data.table)
require(bit64)

##scientific notation off for the session
options(scipen = 999)
rm(list = ls());gc()



 ## Read given files
 app_event=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\app_events.csv",data.table = FALSE,colClasses = c("character","character","character","character"))
 app_labels=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\app_labels.csv",data.table=FALSE,colClasses = c("character","integer"))
 
 events=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\events.csv",data.table=FALSE,header = TRUE,colClasses = c("character","character","character","numeric","numeric"))
 
 label_categories=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\label_categories.csv",data.table=FALSE,colClasses = c("integer","factor"),stringsAsFactors = TRUE)
 ph_bd_dev_model=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\phone_brand_device_model.csv",colClasses = c("character","factor","factor"))
  tdtrain=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\gender_age_train.csv",data.table=FALSE,colClasses = c("character","character","integer","character"))
 tdtest=fread("E:\\AbhinavB\\Kaggle\\TalkingData\\gender_age_test.csv",data.table=FALSE,colClasses = "character")
 gc();

 colnames(ph_bd_dev_model)[colSums(is.na(ph_bd_dev_model))>0]
 colnames(events)[colSums(is.na(events))>0]
 colnames(app_event)[colSums(is.na(app_event))>0]
 colnames(app_labels)[colSums(is.na(app_labels))>0]
 colnames(label_categories)[colSums(is.na(label_categories))>0]
 colnames(tdtrain)[colSums(is.na(tdtrain))>0]
 colnames(tdtest)[colSums(is.na(tdtest))>0]
 
 ##remove full row duplicates from ph brand model device data
 
 ph_bd_dev_model=ph_bd_dev_model[!duplicated(ph_bd_dev_model),]
 
 
 ## converting factor brand and model to numeric values and re converting to factor
 
 ph_bd_dev_model$numbrand=as.numeric(factor(ph_bd_dev_model$phone_brand,levels=levels(ph_bd_dev_model$phone_brand)))
 ph_bd_dev_model$nummodel=as.numeric(factor(ph_bd_dev_model$device_model,levels=levels(ph_bd_dev_model$device_model)))
 ph_bd_dev_model$numbrand=as.factor(ph_bd_dev_model$numbrand)
 ph_bd_dev_model$nummodel=as.factor(ph_bd_dev_model$nummodel)
 label_categories$numCategories=as.numeric(factor(label_categories$category,levels = levels(label_categories$category)))
 label_categories$numCategories=as.factor(label_categories$numCategories)
 str(ph_bd_dev_model)
 str(label_categories)
  dim(tdtrain)
 dim(ph_bd_dev_model)
 
 ##removing duplicates from testset
tdtest1=distinct(tdtest)
 

 ##Adding train + test set and preparing a full set
 tdtrain$ind="train"
 tdtest1$gender=NA
 tdtest1$age=NA
 tdtest1$group=NA
 tdtest1$ind="test"
 
 fullset=rbind(tdtrain,tdtest1)
 dim(fullset)
 head(fullset)
 dim(tdtest1)
 dim(tdtrain)
 
 
 ##Merging trainset+testset  with phonebrand and phoneModels
 
 TrainWithPh=left_join(fullset,ph_bd_dev_model,by="device_id")
 ##remove duplicates
 TrainWithPh=distinct(TrainWithPh)
 colnames(TrainWithPh)




 
 
 ##Merging with Events to get app details 
 TrainWithEvents=left_join(TrainWithPh,events,by="device_id")
 dim(TrainWithEvents)
 TrainWithEvents=distinct(TrainWithEvents)
 colnames(TrainWithEvents)
 ##removing objects to free up memory
 rm(TrainWithPh)
 rm(tdtrain,tdtest,tdtest1,ph_bd_dev_model,events);gc()
 
 #######################################################################################
##Merging with app event 

TrainWithAppevents=left_join(TrainWithEvents,app_event,by="event_id")
 colnames(TrainWithAppevents)
 
##removing original datasets to free up memory 
rm(TrainWithEvents,app_event);gc()

##remove duplicates
TrainWithAppevents_rel4=distinct(TrainWithAppevents)

##removing phnbrand model text cols alongwith timestamp,lat,long,is_active columns

temp2_rel=TrainWithAppevents_rel4[,-c(6,7,10,11,12,13,15)]
colnames(temp2_rel)
rm(TrainWithAppevents,TrainWithAppevents_rel4);gc()
##removing duplicates now without losing any device id
temp2_rel2=distinct(temp2_rel)
dim(temp2_rel2)
colnames(temp2_rel2)
rm(temp2_rel);gc()


##Now joining reduced set with labels
####MERGING WITH APP_LABELS TO GET CATEGORY OF APP USED
temp3=left_join(temp2_rel2,app_labels,by="app_id");
length(unique(temp3$device_id))
dim(temp3)
colnames(temp3)
rm(temp2_rel2);gc()

##find unique rows based on all columns using dplyr otherwise fails with duplicated
temp4=distinct(temp3)
length(unique(temp4$device_id))
rm(temp3);gc()
colnames(temp4)


## NOW joining to get label_categories (master table)  in numerical form 

 temp5=left_join(temp4,label_categories,by="label_id")
 length(unique(temp5$device_id))
 colnames(temp5)

 rm(app_event,app_labels,events,label_categories,ph_bd_dev_model,tdtest,tdtest1,tdtrain);gc()
 
 ## how to remove unnecc colums without losing any unique device id 
 ## remove label_id and category text field
 temp5_rel=temp5[,-c(10,11)]
 dim(temp5_rel)
 head(temp5_rel)
 length(unique(temp5_rel$device_id))
 colnames(temp5)
 
 

 
full_activeset=temp5_rel 
rm(temp4,temp5,temp5_rel);gc()
## add code to update NA where Is_active =0
   
full_activeset$numCategories <-ifelse(full_activeset$is_active==0,"NA",full_activeset$numCategories)

##remove duplicates without losing any device id 

full_activeset1=distinct(full_activeset)
colnames(full_activeset1)
colSums(is.na(full_activeset1))
full_activeset2=full_activeset1[,-c(8,9)]
full_activeset3=distinct(full_activeset2)

full_activeset3$numCategories=paste("AppCat",full_activeset3$numCategories,sep="_")
rm(fullset,full_activeset,full_activeset1,full_activeset2);gc()
colnames(full_activeset3)
colSums(is.na(full_activeset3))


### making categories wide so that there is one row per device id 
wide=full_activeset3 %>%mutate(i=1) 

full_wide=dcast(wide,device_id+gender+age+group+ind+numbrand+nummodel ~ numCategories,value.var = "i",fun.aggregate = sum)
dim(full_wide)

##preparing train and validate set

trainset=subset(full_wide,full_wide$ind=="train")
## remove duplicate based on just one field ie device id ,doesn't matter which row is removed 
trainset=trainset[!duplicated(trainset$device_id),]


##spltvar=sample.split(trainset$group,SplitRatio = 0.7)
##Actual_trainset=subset(trainset,spltvar==TRUE)
##Actual_validateset=subset(trainset,spltvar==FALSE)
##Actual_trainset=Actual_trainset[,-c(2,3,5)]
##levels(Actual_trainset$group)=make.names(levels(Actual_trainset$group))
##Actual_validateset=Actual_validateset[,-c(2,3,5)]
##levels(Actual_validateset$group)=make.names((levels(Actual_validateset$group)))

trainset=trainset[,-c(2,3,5)]

trainset$group=make.names(trainset$group)
trainset$group=as.factor(trainset$group)
spltvar=sample.split(trainset$group,SplitRatio = 0.1)
small_trainset=subset(trainset,spltvar==TRUE)
dim(small_trainset)
colnames(trainset)


##preparing test set
testset=subset(full_wide,full_wide$ind=="test")
testset=testset[,-c(2,3,4,5)]
## remove duplicates (from testset) based on just one field ie device id ,doesn't matter which row is removed 
testset=testset[!duplicated(testset$device_id),]
colnames(testset)
rm(full_activeset3,full_wide,wide)

##Preparing GBM model and running with caret/train

set.seed(1)
###gbmgrid=expand.grid(interaction.depth=c(1),shrinkage=seq(.01),n.minobsinnode=10,n.trees=(0:1)*20)
trctrlobj=trainControl(method="cv",verboseIter=FALSE,classProbs = TRUE,summaryFunction = mnLogLoss)
gbmmod=train(group ~.,
          data=small_trainset,
          method="gbm",
         distribution="multinomial",
         metric="logLoss",
        trControl=trctrlobj,
           verbose=FALSE
            )

##gbmmod1=gbm(group ~ .,data=trial_train,distribution = "multinomial")


proc.time()-ptm
##making predictions on unseen data

pred=predict(gbmmod,newdata=Actual_validateset,type='raw',na.action = NULL)

table(Actual_validateset$group,pred)


###predict on test data
predfinal=predict(gbmmod,newdata = testset,type='raw')
predfinal2=predict(gbmmod,newdata = testset,type="prob")

##Making submission file
pred_detail=data.frame(testset$device_id,predfinal2)
names(pred_detail)=gsub(pattern = '[.]',replacement = '-',names(pred_detail))
names(pred_detail)=gsub(pattern = "testset-",replacement = "",names(pred_detail))
names(pred_detail)=gsub(pattern = "F43-",replacement = "F43+",names(pred_detail))
names(pred_detail)=gsub(pattern = "M39-",replacement = "M39+",names(pred_detail))
##i=pred_detail[pred_detail$device_id=="1186608308763910000",]
##pred_detail=rbind(pred_detail,i)
write.csv(pred_detail,"E:\\AbhinavB\\Kaggle\\TalkingData\\submissions\\fread_With_GBM.csv")




