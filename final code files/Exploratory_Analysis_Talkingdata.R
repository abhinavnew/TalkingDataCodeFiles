
##failing due to memory issues





train_fin=full_activeset3[full_activeset3$ind=="train",]

train_wide=train_fin %>% mutate(i=1) %>% spread(numCategories,i,fill=0)

library(tidyr)











  
   
   
   ########################################################

> table(full_activeset$is_active)

##0        1 
13233464  5527760 
> nrow(full_activeset)
[1] 18917680
> (13233464+5527760)-18917680
[1] -156456##/###


   
    
 ## finding rows where lattitude and longitude of the data call/event is zero 
 a=subset(TrainWithAppevents_rel2,TrainWithAppevents_rel2$longitude==0 & TrainWithAppevents_rel2$latitude==0)
 dim(a)
 ## 3031846      15
 
 b=subset(TrainWithAppevents_rel2,TrainWithAppevents_rel2$longitude!=0 & TrainWithAppevents_rel2$latitude!=0)
 write.csv(b,"E:\\AbhinavB\\Kaggle\\TalkingData\\b.csv")
 
 ##Extracting the timepart and plotting against DV to see if there is any relation
 
 a$timepart=format(as.POSIXct(strptime(a$timestamp,"%Y-%m-%d %H:%M:%S",tz="")),format="%H:%M:%S")
 a$hourpart=format(as.POSIXct(strptime(a$timestamp,"%Y-%m-%d %H:%M:%S",tz="")),format="%H")
 a$Geoloc=paste(a$latitude,a$longitude,sep="_")
 aFemale=subset(a,a$gender=="F")
 aMale=subset(a,a$gender=="M")
 
 ggplot(aes(x=hourpart,y=group),data=aFemale)+geom_point()
 ggplot(aes(x=hourpart,y=group),data=aMale)+geom_point(alpha=0.3)
 ggplot(aes(x=Geoloc,y=group),data=aMale)+geom_point(alpha=0.3)
 ggplot(aes(x=Geoloc,y=group),data=aFemale)+geom_point(alpha=0.3)
 
 ggplot(data=deputies_small,aes(x=receipt_description,y=receipt_value))+geom_boxplot()+coord_flip()
 
 colnames(deputies_small)