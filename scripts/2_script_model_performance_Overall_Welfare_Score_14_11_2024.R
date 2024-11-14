###########
###Libraries
library(h2o)
library(tidyr)
############

h2o.init()

##Loading the beast model for each individual welfare indicator
model.repro<-h2o.loadModel("best_RF_model_mean_repro/model_welfare_RF3_model_6")

model.mast<-h2o.loadModel("best_RF_model_mean_mast/model_welfare_RF3_model_19")

model.long<-h2o.loadModel("best_RF_model_mean_long/model_welfare_RF3_model_2")

model.keto<-h2o.loadModel(best_RF_model_mean_keto/model_welfare_RF3_model_17")

model.acid<-h2o.loadModel("best_RF_model_mean_acid/model_welfare_RF3_model_4")

##Loading the test dataset
test.db<-read.table("test_db_LessFeatures_Welfare_12_11_24.txt", h=T,sep="\t")

##Obtaining the predicted values for each individual welfare indicator and merging the results in a single data frame
prob.out.acid <- as.data.frame(h2o.predict(model.acid, test.db.h2o))
prob.out.keto <- as.data.frame(h2o.predict(model.keto, test.db.h2o))
prob.out.long <- as.data.frame(h2o.predict(model.long, test.db.h2o))
prob.out.mast <- as.data.frame(h2o.predict(model.mast, test.db.h2o))
prob.out.repro <- as.data.frame(h2o.predict(model.repro, test.db.h2o))

class.pred.db<-cbind(prob.out.acid,prob.out.keto,prob.out.long,prob.out.mast,prob.out.repro)

colnames(class.pred.db)<-c("mean_acid_pred","mean_keto_pred","mean_long_pred","mean_mast_pred","mean_repro_pred")

##Combining the results with the original test set
test.db<-cbind(test.db,class.pred.db)

##Creating the welfare classes (good, intermediary and risk) for each individual welfare indicator using the original values

test.db$class_repro<-ifelse(test.db$mean_repro<10,"G",ifelse(test.db$mean_repro>10 & test.db$mean_repro<20,"I","B"))

test.db$class_keto<-ifelse(test.db$mean_keto<10,"G",ifelse(test.db$mean_keto>10 & test.db$mean_keto<20,"I","B"))

test.db$class_mast<-ifelse(test.db$mean_mast<10,"G",ifelse(test.db$mean_mast>10 & test.db$mean_mast<20,"I","B"))

test.db$class_long<-ifelse(test.db$mean_long<10,"G",ifelse(test.db$mean_long>10 & test.db$mean_long<20,"I","B"))

test.db$class_acid<-ifelse(test.db$mean_acid<10,"G",ifelse(test.db$mean_acid>10 & test.db$mean_acid<20,"I","B"))


##Number of good records
good.wel<-rowSums(test.db[,c("mean_repro", "mean_long", "mean_mast", "mean_keto", "mean_acid")]<10)

##Number of bad records
bad.wel<-rowSums(test.db[,c("mean_repro", "mean_long", "mean_mast", "mean_keto", "mean_acid")]>20)

##Number of intermediary records
int.wel<-(5-(good.wel+bad.wel))

test.db$Welfare_value.pred<-as.numeric(paste0(bad.wel,int.wel,good.wel))

test.db$Welfare_class.2<-ifelse(test.db$Welfare_value.pred<10,"G",ifelse(test.db$Welfare_value.pred>10 & test.db$Welfare_value.pred<=50,"I","B"))

##Creating the welfare classes (good, intermediary and risk) for each individual welfare indicator using the predicted values


test.db$class_repro_pred<-ifelse(test.db$mean_repro_pred<10,"G",ifelse(test.db$mean_repro_pred>10 & test.db$mean_repro_pred<20,"I","B"))

test.db$class_keto_pred<-ifelse(test.db$mean_keto_pred<10,"G",ifelse(test.db$mean_keto_pred>10 & test.db$mean_keto_pred<20,"I","B"))

test.db$class_mast_pred<-ifelse(test.db$mean_mast_pred<10,"G",ifelse(test.db$mean_mast_pred>10 & test.db$mean_mast_pred<20,"I","B"))

test.db$class_long_pred<-ifelse(test.db$mean_long_pred<10,"G",ifelse(test.db$mean_long_pred>10 & test.db$mean_long_pred<20,"I","B"))

test.db$class_acid_pred<-ifelse(test.db$mean_acid_pred<10,"G",ifelse(test.db$mean_acid_pred>10 & test.db$mean_acid_pred<20,"I","B"))


##Number of good records
good.wel<-rowSums(test.db[,c("mean_repro_pred", "mean_long_pred", "mean_mast_pred", "mean_keto_pred", "mean_acid_pred")]<10)

##Number of bad records
bad.wel<-rowSums(test.db[,c("mean_repro_pred", "mean_long_pred", "mean_mast_pred", "mean_keto_pred", "mean_acid_pred")]>20)

##Number of intermediary records
int.wel<-(5-(good.wel+bad.wel))

test.db$Welfare_value.pred.2<-as.numeric(paste0(bad.wel,int.wel,good.wel))

test.db$Welfare_class.pred<-ifelse(test.db$Welfare_value.pred.2<10,"G",ifelse(test.db$Welfare_value.pred.2>10 & test.db$Welfare_value.pred.2<=50,"I","B"))

##Confusion matrix for the predicted and real overall welfare classes

table(test.db$Welfare_class.2,test.db$Welfare_class.pred)
