
starttime <-proc.time()

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
library(xgboost)
library(randomForest)
library(Matrix)
library(mlbench)
library(Boruta)
library(MLmetrics)
library(class)
library(sqldf)
library(scales)
library(TeachingDemos)
library(neuralnet)


## This stacked model has 2 levels -level 1 -xgboost and rf/boruta ,level 2-gbm/caret


#scientific notation off for the session
options(scipen = 999)
rm(list = ls());gc()

txtStart("capturecode.txt")

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
 label_categories$numCategories=as.numeric(factor(label_categories$category,levels = levels(label_categories$category)))
 
 ##Getting bins for all calls made from any device id 
 temp_bins=events
 temp_bins$timepart=substr(temp_bins$timestamp,11,nchar(temp_bins$timestamp))
 temp_bins$timepos=as.POSIXct(temp_bins$timepart,format="%H:%M:%S",tz="UTC")
 temp_bins$bins=cut(temp_bins$timepos,breaks = "6 hours",labels = FALSE)
 temp_bins$bins=paste0("Bin_",temp_bins$bins,sep="")
 temp_bins=temp_bins %>% mutate(i=1)
 temp_wide_bins=dcast(temp_bins,device_id ~ bins,value.var="i",fun.aggregate=sum)
 rm(temp_bins)
 
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
head(full_activeset3)
dim(full_activeset3)
colnames(full_activeset3)
colSums(is.na(full_activeset3))

##new::: merging with device id and their timebin counts
full_activeset4=left_join(full_activeset3,temp_wide_bins,by="device_id")


### making categories wide so that there is one row per device id 
wide=full_activeset4 %>%mutate(i=1) 

full_wide=dcast(wide,device_id+gender+age+group+ind+numbrand+nummodel+Bin_1+Bin_2+Bin_3+Bin_4 ~ numCategories,value.var = "i",fun.aggregate = sum)
dim(full_wide)

txtStop()
##trainset prep common for BOTH STACKING MODELS

##preparing train and validate set

trainset=subset(full_wide,full_wide$ind=="train")
## remove duplicate based on just one field ie device id ,doesn't matter which row is removed 
trainset=trainset[!duplicated(trainset$device_id),]

##removing cols including device id which is unique and not useful for modelling
trainsetfull=trainset[,-c(1,2,3,5)]
##all categorical variables are of character datatype so far 

##Breaking trainsetfull into a train and validation set so that stacking can be built
splitt=sample.split(trainsetfull$group,SplitRatio = 0.6)
trainset_splitt=subset(trainsetfull,splitt==TRUE)
validate_splitt=subset(trainsetfull,splitt==FALSE)

##train and validate set prep for boruta +rf model and xgboost 
trainset_rf=trainset_splitt
trainset_xg=trainset_splitt
validateset_rf=validate_splitt
validateset_xg=validate_splitt

#Converting dependent variable to corresponding factor numeric value and storing as numeric and then converting to factor again
trainset_rf$group=as.factor(make.names(trainset_rf$group))
str(trainset)
small_trainset=trainset_rf[1:5000,]
small_trainset[is.na(small_trainset)]<--999

##Testset prep common for BOTH STACKING models
testset=subset(full_wide,full_wide$ind=="test")
colnames(testset)
dim(testset)
#remove duplicates should be 112071 rows
testset=testset[!duplicated(testset$device_id),]
dim(testset)
test_dev_id=testset$device_id
length(test_dev_id)
head(test_dev_id)
##removing cols including device id which is unique and not useful for modelling
testset=testset[,-c(1,2,3,5)]
## remove duplicates (from testset) based on just one field ie device id ,doesn't matter which row is removed 

colnames(testset)
dim(testset)

##Test set prep For boruta+rf model and for xgboost
testset_rf=testset
testset_xg=testset

testset_rf$group=as.factor(testset_rf$group)


rm(full_activeset3,wide)

##Preparing Random Forest model and running with caret/train
set.seed(114)
##Adding variable importance part 


boruta_train=Boruta(group ~.,data=small_trainset,doTrace=2)
boruta_bank=TentativeRoughFix(boruta_train)
##keepimp=getSelectedAttributes(boruta_bank,withTentative = F)
##write.csv(as.data.frame(keepimp),"E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\boruta_Variables.csv")
keepimp=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\boruta_Variables.csv")
keepimp=keepimp[,-c(1)]
length(keepimp)
final_trainset_rf=trainset_rf[,names(trainset_rf) %in% keepimp]
dim(final_trainset_rf)



##paramter list for random F
trctrlobj=trainControl(method="cv",verboseIter=TRUE,classProbs = TRUE,summaryFunction = mnLogLoss)
#tgrid=expand.grid(.mtry=c(1:5))
rfmod=train(group ~.,
             data=final_trainset_rf,
             method="rf",
             distribution="multinomial",
             metric="logLoss",
             trControl=trctrlobj,
            allowParallel=T,
             verbose=TRUE
)

out1=capture.output(rfmod)
cat("SummaryOfRFModel",out1,file="E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\RfmodelDetails.csv",sep="/n",append = T) 

##making predictions on validate set (known data ) and checking accuracy mlogloss
dim(validateset_rf)
validateset_result=subset(validateset_rf,select=c("group"))

remove_col=c("group")
validateset_rf=validateset_rf[,!names(validateset_rf) %in% remove_col]

dim(validateset_rf)
pred_val_rf=predict(rfmod,newdata = validateset_rf,type = "prob")
#making training set for 2nd level of model stack
train_level_2=data.frame(pred_val_rf,validateset_result)
dim(train_level_2)
typeof(train_level_2)

##making predictions on unseen data

pred_rf=predict(rfmod,newdata=testset_rf,type="prob")

dim(pred_rf)
res_submit_rf=as.data.frame(pred_rf)
dim(res_submit_rf)
test_level_2=data.frame(res_submit_rf)

###############################################################

##prep for xgboost

#trainset for xgboost
trainset_xg$group=as.factor(trainset_xg$group)
trainset_xg$group=as.numeric(factor(trainset_xg$group),levels=levels(trainset_xg$group))-1
#trainset$group=as.factor(trainset$group)
str(trainset)
tr_labels=trainset_xg[,"group"]
length(tr_labels)
prev_action=options('na.action')
options(na.action = 'na.pass')

train_m=sparse.model.matrix(group ~ .-1,data=trainset_xg)
dim(train_m)
dtrain=xgb.DMatrix(data=as.matrix(train_m),label=tr_labels,missing = NA)


#validateset for xgboost

validateset_xg$group=as.factor(validateset_xg$group)
validateset_xg$group=as.numeric(factor(validateset_xg$group),levels=levels(validateset_xg$group))-1
str(validateset_xg)
vr_labels=validateset_xg[,"group"]
validate_m=sparse.model.matrix(group ~ .-1,data=validateset_xg)
dim(validate_m)
dvalidate=xgb.DMatrix(data=as.matrix(validate_m),label=vr_labels,missing = NA)
##write.csv(dvalidate,"E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\dvalidate.csv",quote = F)




##Testset prep for xgboost ::::

testset_xg$group=as.factor(testset_xg$group)
testset_xg$group=as.numeric(factor(testset_xg$group),levels=levels(testset_xg$group))-1
#testset$group=as.factor(testset$group)
ts_labels=testset_xg[,"group"]
test_m=testset_xg[,-c(1)]
dim(test_m)
dtest=xgb.DMatrix(data=as.matrix(test_m),label=ts_labels,missing = NA)
##write.csv(dtest,"E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\dtest.csv",quote = F)

options(na.action = prev_action$na.action)

rm(full_activeset3,wide)

##Preparing XGBoost model and running with caret/train
set.seed(114)
###gbmgrid=expand.grid(interaction.depth=c(1),shrinkage=seq(.01),n.minobsinnode=10,n.trees=(0:1)*20)
##trctrlobj=trainControl(method="cv",verboseIter=TRUE,classProbs = TRUE,savePredictions = TRUE)
##gbmmod1=gbm(group ~ .,data=trial_train,distribution = "multinomial")

##paramter list for xgboost 
nc=length(unique(tr_labels))

param=list(booster="gblinear",
           num_class=nc,
           objective="multi:softprob",
           eval_metric="mlogloss",
           eta=0.01,
           lambda=5,
           lambda_bias=0,
           alpha=2)

watch=list(train=dtrain)
#ntree=280
set.seed(114)


##xgboost model



xgb_mod1=xgb.train(params = param,
                   data=dtrain,
                   watchlist = watch,
                   nrounds = 280,
                   verbose = 1)


#making predictions on validate set 
pred_val_xg=predict(xgb_mod1,newdata = dvalidate)
length(pred_val_xg)
#making 2nd level of train
pred_val_xg_matrix=matrix(pred_val_xg,ncol =12)
pred_val_xg_df=as.data.frame(pred_val_xg_matrix)
train_level_3=cbind(train_level_2,pred_val_xg_df)
train_level_3$group=make.names(train_level_3$group)
dim(train_level_2)
typeof(train_level_2)
dim(train_level_3)


##making predictions on unseen data

pred_xg=predict(xgb_mod1,newdata=dtest)

length(pred_xg)
#making 2nd level of TEST

pred_xg_mat=matrix(pred_xg,ncol = 12)
pred_xg_df=as.data.frame(pred_xg_mat)
test_level_3=cbind(test_level_2,pred_xg_df)

dim(test_level_2)
typeof(test_level_2)
dim(test_level_3)
typeof(test_level_3)

# passing prev predictions and test set to 2nd level of gbm model 

trctrlobj=trainControl(method="cv",verboseIter=FALSE,classProbs = TRUE,summaryFunction = mnLogLoss)
gbmmod1=train(group ~.,
             data=train_level_3,
             method="gbm",
             distribution="multinomial",
             metric="logLoss",
             trControl=trctrlobj,
             verbose=FALSE
)


pred_final_gbm=predict(gbmmod1,newdata=test_level_3,type='prob')

res_submit=cbind(test_dev_id,as.data.frame(pred_final_gbm))

##colnames(res_submit)=c("device_id","F23-","F24-26","F27-28","F29-32","F33-42","F43+","M22-","M23-26","M27-28","M29-31","M32-38","M39+")
##write.csv(res_submit,"E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\Submit3.csv",row.names = F,quote = F)



##endtime=proc.time()
timetakens=starttime-endtime
cat(timetakens)

colnames(res_submit)=c("device_id","F23-","F24-26","F27-28","F29-32","F33-42","F43+","M22-","M23-26","M27-28","M29-31","M32-38","M39+")


write.csv(res_submit,"E:\\AbhinavB\\Kaggle\\TalkingData\\SubmissionFiles\\Submit10_TWOlevelStack_timedata_stacking.csv",row.names = F,quote = F)
