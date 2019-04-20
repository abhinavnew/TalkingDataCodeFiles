
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
##trainset preparation

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

dim(trainset_splitt)
dim(validate_splitt)
colnames(trainset_splitt)
colnames(validate_splitt)
##trainset_splitt=trainset_splitt[complete.cases(trainset_splitt),]
##validate_splitt=validate_splitt[complete.cases(validate_splitt),]

#drop time bins and Appcat_NA columns due to missing values
trainset_splitt=trainset_splitt[,-c(4,5,6,7,444)]
validate_splitt-validate_splitt[,-c(4,5,6,7,444)]

dim(trainset_splitt)
dim(validate_splitt)
colnames(trainset_splitt)
colnames(validate_splitt)





#min max scaling for all input variables 
sclfun=function(x){ 
  if (x==0 && min(x)==0 && max(x)==0 ){
 k= x
  }
  else
    { k=(x-min(x))/(max(x)-min(x))  }   
  
  return(k)
  }
trainset_scl=trainset_splitt
trainset_scl2=data.frame(apply(trainset_scl[-1],2,sclfun))
dim(trainset_scl2)
trainset_scl2=cbind(trainset_splitt[,1],trainset_scl2)
colnames(trainset_scl2)[colnames(trainset_scl2)=='trainset_splitt[, 1]']<-'group'
dim(trainset_scl2)
glimpse(trainset_scl2)



##Testset prep 
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
##dropping timeBin and App_catNA columns due to NA values 
testset_fin=testset[,-c(4,5,6,7,444)]
dim(testset_fin)
##min max scaling 
testset_scl2=data.frame(apply(testset_fin[-1],2,sclfun))

dim(testset_scl2)

glimpse(testset_scl2)







##training a neuralnet model and tuning
rm(m)
set.seed(123)

m=neuralnet(group ~ numbrand+nummodel+AppCat_100+AppCat_105+AppCat_106+AppCat_107+AppCat_109+AppCat_110+AppCat_112+AppCat_114+AppCat_118+AppCat_119+AppCat_121+AppCat_123+
              AppCat_124+AppCat_126+AppCat_129+AppCat_131+AppCat_134+AppCat_135+AppCat_136+AppCat_137+AppCat_140+AppCat_141+AppCat_142+AppCat_143+AppCat_145+AppCat_147+
              AppCat_148+AppCat_151+AppCat_152+AppCat_158+AppCat_159+AppCat_16+AppCat_160+AppCat_161+AppCat_165+AppCat_17+AppCat_170+AppCat_172+AppCat_175+AppCat_176
            +AppCat_177+AppCat_179+AppCat_18+AppCat_182+AppCat_185+AppCat_188+AppCat_19+AppCat_191+AppCat_192+AppCat_193+AppCat_194+AppCat_199+AppCat_2+AppCat_200+AppCat_202+
              AppCat_203+AppCat_205+AppCat_206+AppCat_21+AppCat_210+AppCat_211+AppCat_219+AppCat_22+AppCat_220+AppCat_227+AppCat_239+AppCat_24+AppCat_242+AppCat_245+AppCat_247
            +AppCat_249+AppCat_252+AppCat_254+AppCat_255+AppCat_260+AppCat_261+AppCat_262+AppCat_269+AppCat_270+AppCat_271+AppCat_272+AppCat_276+AppCat_277+AppCat_28+AppCat_280+
              AppCat_281+AppCat_284+AppCat_285+AppCat_287+AppCat_289+AppCat_29+AppCat_290+AppCat_291+AppCat_292+AppCat_294+AppCat_296+AppCat_297+AppCat_298+AppCat_299+AppCat_3+
              AppCat_302+AppCat_306+AppCat_309+AppCat_31+AppCat_310+AppCat_312+AppCat_313+AppCat_328+AppCat_329+AppCat_33+AppCat_330+AppCat_331+AppCat_335+AppCat_337+AppCat_339+
              AppCat_34+AppCat_341+AppCat_343+AppCat_344+AppCat_345+AppCat_346+AppCat_350+AppCat_351+AppCat_352+AppCat_355+AppCat_357+AppCat_358+AppCat_359+AppCat_36+AppCat_360+
              AppCat_365+AppCat_366+AppCat_368+AppCat_37+AppCat_370+AppCat_379+AppCat_38+AppCat_381+AppCat_383+AppCat_385+AppCat_388+AppCat_39+AppCat_392+AppCat_396+AppCat_398+
              AppCat_4+AppCat_400+AppCat_401+AppCat_402+AppCat_403+AppCat_406+AppCat_407+AppCat_408+AppCat_409+AppCat_41+AppCat_411+AppCat_412+AppCat_414+AppCat_415+AppCat_423+
              AppCat_424+AppCat_425+AppCat_427+AppCat_428+AppCat_43+AppCat_430+AppCat_431+AppCat_432+AppCat_433+AppCat_434+AppCat_437+AppCat_438+AppCat_439+AppCat_44+AppCat_440+
              AppCat_442+AppCat_443+AppCat_444+AppCat_445+AppCat_446+AppCat_450+AppCat_451+AppCat_452+AppCat_453+AppCat_454+AppCat_455+AppCat_458+AppCat_459+AppCat_461+AppCat_464+
              AppCat_465+AppCat_466+AppCat_467+AppCat_468+AppCat_469+AppCat_470+AppCat_471+AppCat_472+AppCat_474+AppCat_475+AppCat_477+AppCat_478+AppCat_479+AppCat_48+AppCat_480+
              AppCat_481+AppCat_485+AppCat_487+AppCat_488+AppCat_489+AppCat_49+AppCat_490+AppCat_495+AppCat_498+AppCat_499+AppCat_501+AppCat_504+AppCat_505+AppCat_514+AppCat_516+
              AppCat_519+AppCat_522+AppCat_524+AppCat_525+AppCat_527+AppCat_530+AppCat_531+AppCat_536+AppCat_537+AppCat_538+AppCat_540+AppCat_541+AppCat_542+AppCat_546+AppCat_547+
              AppCat_548+AppCat_549+AppCat_550+AppCat_551+AppCat_552+AppCat_553+AppCat_554+AppCat_555+AppCat_556+AppCat_560+AppCat_561+AppCat_562+AppCat_566+AppCat_569+AppCat_570+
              AppCat_571+AppCat_575+AppCat_579+AppCat_580+AppCat_582+AppCat_585+AppCat_586+AppCat_588+AppCat_59+AppCat_591+AppCat_592+AppCat_596+AppCat_597+AppCat_60+AppCat_604+
              AppCat_605+AppCat_606+AppCat_607+AppCat_608+AppCat_610+AppCat_611+AppCat_612+AppCat_613+AppCat_614+AppCat_616+AppCat_618+AppCat_620+AppCat_621+AppCat_624+AppCat_625+
              AppCat_627+AppCat_628+AppCat_63+AppCat_630+AppCat_631+AppCat_632+AppCat_633+AppCat_635+AppCat_636+AppCat_638+AppCat_639+AppCat_64+AppCat_640+AppCat_642+AppCat_644+
              AppCat_646+AppCat_647+AppCat_648+AppCat_649+AppCat_650+AppCat_651+AppCat_652+AppCat_653+AppCat_655+AppCat_656+AppCat_658+AppCat_659+AppCat_660+AppCat_661+AppCat_662+
              AppCat_663+AppCat_664+AppCat_665+AppCat_668+AppCat_670+AppCat_671+AppCat_672+AppCat_674+AppCat_676+AppCat_677+AppCat_678+AppCat_679+AppCat_68+AppCat_680+AppCat_681+
              AppCat_682+AppCat_683+AppCat_684+AppCat_685+AppCat_687+AppCat_688+AppCat_689+AppCat_690+AppCat_691+AppCat_692+AppCat_694+AppCat_697+AppCat_699+AppCat_7+AppCat_700+
              AppCat_701+AppCat_703+AppCat_705+AppCat_707+AppCat_708+AppCat_709+AppCat_710+AppCat_711+AppCat_712+AppCat_713+AppCat_714+AppCat_715+AppCat_717+AppCat_719+AppCat_722+
              AppCat_723+AppCat_725+AppCat_726+AppCat_727+AppCat_728+AppCat_729+AppCat_73+AppCat_730+AppCat_731+AppCat_732+AppCat_733+AppCat_734+AppCat_735+AppCat_737+AppCat_738+
              AppCat_739+AppCat_74+AppCat_744+AppCat_745+AppCat_760+AppCat_762+AppCat_765+AppCat_766+AppCat_767+AppCat_768+AppCat_769+AppCat_770+AppCat_771+AppCat_773+AppCat_775+
              AppCat_776+AppCat_777+AppCat_778+AppCat_780+AppCat_781+AppCat_784+AppCat_785+AppCat_786+AppCat_787+AppCat_789+AppCat_790+AppCat_791+AppCat_792+AppCat_793+AppCat_797+
              AppCat_799+AppCat_80+AppCat_800+AppCat_801+AppCat_804+AppCat_805+AppCat_806+AppCat_807+AppCat_808+AppCat_809+AppCat_81+AppCat_810+AppCat_812+AppCat_815+AppCat_817+
              AppCat_820+AppCat_821+AppCat_823+AppCat_824+AppCat_827+AppCat_828+AppCat_829+AppCat_83+AppCat_830+AppCat_832+AppCat_833+AppCat_836+AppCat_84+AppCat_85+AppCat_87+
              AppCat_90+AppCat_92+AppCat_95+AppCat_96+AppCat_97+AppCat_99,data=trainset_scl2,linear.output = FALSE,hidden = 2,threshold = 0.1)
plot(m)

##making predictions

p=compute(m,validate_splitt)
prob_validate=p$net.result
write.csv(prob_validate,"E:\\AbhinavB\\Kaggle\\prob_validate.csv")




















