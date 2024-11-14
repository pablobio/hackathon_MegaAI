#########
###Library
library(h2o)
library(dplyr)
#######
h2o.init(nthreads=50, max_mem_size = "100G")
h2o.removeAll()

set.seed(123)

#Loading preprocessed train and test sets
train.db<-read.table("train_db_LessFeatures_Welfare_12_11_24.txt", h=T,sep="\t")

test.db<-read.table("test_db_LessFeatures_Welfare_12_11_24.txt", h=T,sep="\t")

#################
#####The following code must the excuted for each individual welfare indicator
#####Here, mean_keto was used as example
###################



#Building train and test set in h2o format (excluding the overall welfare score and the individual welfare indicators that will not be predicted)
train.db.h2o<-as.h2o(train.db[,-c(1,9:12)])
test.db.h2o<-as.h2o(test.db[,-c(1,9:12)])


# Set random grid search criteria: 
search_criteria_db <- list(strategy = "RandomDiscrete",
                           stopping_metric = "rmse",
                           stopping_tolerance = 0.005,
                           stopping_rounds = 100,
                           max_runtime_secs = 3000*60)


# Set hyperparameter grid: 
hyper_grid.h2o <- list(ntrees = seq(10, 100, by = 10),
                       mtries = -1,
                       sample_rate = c(0.8, 1))

# Turn parameters for RF: 
st.train.rf<-system.time(random_grid <- h2o.grid(algorithm = "randomForest",
                                                 grid_id = "model_welfare_RF3",
                                                 y = "mean_keto", 
                                                 seed = 29, 
                                                 nfolds = 10,
                                                
                                                 training_frame = train.db.h2o,
                                                 hyper_params = hyper_grid.h2o,
                                                 search_criteria = search_criteria_db))



# Collect the results and sort by our models: 
grid_perf2 <- h2o.getGrid(grid_id = "model_welfare_RF3", 
                          sort_by = "rmse", 
                          decreasing = FALSE)

# Best RF: 
system.time(best_model2 <- h2o.getModel(grid_perf2@model_ids[[1]]))

#Prediciton
st.test.rf<-system.time(prob.out <- h2o.predict(best_model2, test.db.h2o[,-c(1,ncol(test.db))]))

prob.out<-as.data.frame(prob.out)

#Model evaluation
prob.out$real<-test.db$mean_keto

cor(prob.out$predict,prob.out$real)

rmse <- sqrt(mean((prob.out$predict - prob.out$real) ^ 2))

rmse
