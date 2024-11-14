###########
###Libraries
library(h2o)
library(dplyr)
library(iml)
library(shapviz)
library(counterfactuals)
library(tidyr)
############

h2o.init()

##Loading the beast model for each individual welfare indicator
model.repro<-h2o.loadModel("best_RF_model_mean_repro/model_welfare_RF3_model_6")

model.mast<-h2o.loadModel("best_RF_model_mean_mast/model_welfare_RF3_model_19")

model.long<-h2o.loadModel("best_RF_model_mean_long/model_welfare_RF3_model_2")

model.keto<-h2o.loadModel(best_RF_model_mean_keto/model_welfare_RF3_model_17")

model.acid<-h2o.loadModel("best_RF_model_mean_acid/model_welfare_RF3_model_4")




###Loading shapley values

load("../best_models/shapley_objects.rda")



###Beeswarn plots

##Reproducton welfare indicator
shapviz::sv_importance(shp.repro, kind = "beeswarm") &
  theme_gray(base_size = 20) 

##Acidosis welfare indicator
shapviz::sv_importance(shp.acid, kind = "beeswarm")&
  theme_gray(base_size = 20) 

##Mastitis welfare indicator
shapviz::sv_importance(shp.mast, kind = "beeswarm")&
  theme_gray(base_size = 20) 

##Ketosis welfare indicator
shapviz::sv_importance(shp.keto, kind = "beeswarm")&
  theme_gray(base_size = 20) 
  
##Longevity welfare indicator
shapviz::sv_importance(shp.long, kind = "beeswarm")&
  theme_gray(base_size = 20) 


###Waterfall plots for good and bad overall welfare scores observed for the individual "-9223372036850594816". The numbers in the row_id argument corresponds to the rowname in the shapley object that stores the information for each observation


####Ketosis

###Good
keto.good<-shapviz::sv_waterfall(shp.keto, row_id = 12131)&
  theme_gray(base_size = 20)

##Bad
keto.bad<-shapviz::sv_waterfall(shp.keto, row_id = 17106)&
  theme_gray(base_size = 20)

keto.good / keto.bad


####Reproduction

###Good
repro.good<-shapviz::sv_waterfall(shp.repro, row_id = 12131)&
  theme_gray(base_size = 20)

##Bad
repro.bad<-shapviz::sv_waterfall(shp.repro, row_id = 17106)&
  theme_gray(base_size = 20)

repro.good / repro.bad


####Acidosis

###Good
acid.good<-shapviz::sv_waterfall(shp.acid, row_id = 12131)&
  theme_gray(base_size = 20)

##Bad
acid.bad<-shapviz::sv_waterfall(shp.acid, row_id = 17106)&
  theme_gray(base_size = 20)

acid.good / acid.bad


####Mastitis

###Good
mast.good<-shapviz::sv_waterfall(shp.mast, row_id = 12131)&
  theme_gray(base_size = 20)

##Bad
mast.bad<-shapviz::sv_waterfall(shp.mast, row_id = 17106)&
  theme_gray(base_size = 20)

mast.good / mast.bad


####Longevity

###Good
long.good<-shapviz::sv_waterfall(shp.long, row_id = 12131)&
  theme_gray(base_size = 20)

##Bad
long.bad<-shapviz::sv_waterfall(shp.long, row_id = 17106)&
  theme_gray(base_size = 20)

long.good / long.bad


##################
#######Counterfactual explanations
##################
test.db<-read.table(".test_db_LessFeatures_Welfare_12_11_24.txt", h=T,sep="\t")

#Building test sets as a h2o object
test.db.h2o<-as.h2o(test.db[,c("mean_acetona", "mean_fat", "mean_lactose", "mean_milk_cond","mean_protein", "mean_SCC", "mean_urea","mean_mast")])

#Veryfing the row numbers with the information for the observation of the animal ID "-9223372036850594816" corresponding to the good and bad overal welfare score.
test.db[c(16394,17019),]

#Creating a iml predictor witht he mastitis best model and the test set
predictor = iml::Predictor$new(model.mast, data = as.data.frame(test.db.h2o), y = "mean_mast")

#Obtaining the Nearest Instance Counterfactual Explanations (NICE) for the obtaine dpredictor
nice_regr = NICERegr$new(predictor, optimization = "proximity", 
                         margin_correct = 0.5, return_multiple = FALSE)


#Extracting the information for the bad overall welfare score 
x_interest<-as.data.frame(test.db.h2o[17019,])

x_interest$mean_SCC<-as.numeric(x_interest$mean_SCC)
x_interest$mean_milk_cond<-as.numeric(x_interest$mean_milk_cond)

#Identifying the changes in the variables (counterfactuals) that might result ina prediction of the mastitis individual welfare indicator ina  range between 0-5 (good welfare).
cfactuals = nice_regr$find_counterfactuals(x_interest = x_interest, 
                                           desired_outcome = c(0, 5))

cfactuals

#Creating a paralel plot showing the dynamics in the features values changes based on the counterfactual explanations
cfactuals$plot_parallel(feature_names = names(
  cfactuals$get_freq_of_feature_changes()), digits_min_max = 2L)

