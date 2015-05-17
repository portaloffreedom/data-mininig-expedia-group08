source("learning.R")
source("load.R")

train_fn=function(data, target){
  model <- mlp(   x=data,                    #input data for training
                  y=target,                  #output data (targets) for training
                  size=2,                    #number of neurons in the hidden layer
                  learnFunc="Std_Backpropagation", #type of learning
#                   learnFuncParams=c(0.1),  #paramenters of the learning function (eta)
                  maxit=100)                 #maximum number of iterations
  
  return(model)
}


fwd_fn=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}

calculate_order <- function(srch_id, prop_id, model, test_x) {
    
    registerDoParallel(cores=3)    
    
    foreach(id = uinque_id_iter(unique(srch_id)), .combine='rbind') %dopar% {
        
        result <- fwd_fn(model, test_x[srch_id == id,])
        
        real_prob_order <- order(result, decreasing=T)
        prop_id_sort <- prop_id[real_prob_order]
        
        cbind(id, prop_id_sort)
    }
}

interesing_columns <- c(
#     'visitor_location_country_id',
    'prop_starrating',
#     'prop_location_score1',
    'prop_location_score2',
    'prop_review_score',
    'price_usd',
    'promotion_flag'
)


start.time <- Sys.time()

print("A")
## creating train and test dataset
train <- read.csv("train_full_expanded_v4.csv")[1:40007,]
print("A1")
test <- read.csv("test_set_VU_DM_2014.csv", na.strings = "NULL")

print("A2")
## "test" set normalization
# test <- create_competitors_columns(test)
# test$season <- create_seasons(test)
# 
# test$srch_saturday_night_bool <- parse_bool_col(dataset$srch_saturday_night_bool)
# test$random_bool <- parse_bool_col(dataset$random_bool)
# test$click_bool <- parse_bool_col(dataset$click_bool)
# test$booking_bool <- parse_bool_col(dataset$booking_bool)
# test$promotion_flag <- parse_bool_col(dataset$promotion_flag)
# test$prop_brand_bool <- parse_bool_col(dataset$prop_brand_bool)
test <- keep_only_needed(test)
test <- fill_missing_values(test)
test <- refactor_some_columns(test)
test$price_usd <- normalize_price(test$srch_id, test$price_usd) 

print("B")
## creating targets
target_train <- train$booked_prob

print("B1")
train_x <- train[,interesing_columns]

print("B2")
test_x <- test[,interesing_columns]


# test if the sets are empty
if (nrow(train) == 0) stop("train set is empty")
if (nrow(test) == 0) stop("test set is empty")
    
# print(paste("nrow",nrow(train)))
# print(paste("is.na",sum(is.na(train))))
if (sum(is.na(train)) != 0) stop("train set has NA")
if (sum(is.na(test)) != 0) stop("test set has NA")

if (sum(is.na(target_train)) != 0) stop("target_train set has NA")

print("C")
## train
model <- train_fn(train_x, target_train)

print("D")
## results
# result_train <- fwd_fn(model, train)
# result_test <- fwd_fn(model, test_x)

# result_test <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% { 
#     fwd_fn(model, test_x[srch_id == id])
# }


print("E")
## calculate positions
final_order <- calculate_order(test$srch_id, test$prop_id, model, test_x)

print("F")
## writing file
write.csv(final_order, "final_order.csv", row.names=F)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("tame taken:",time.taken))
