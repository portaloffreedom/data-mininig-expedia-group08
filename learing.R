N_CORES <- 2


ndcg <- function(x) {
    # x is a vector of relevance scores
    ideal_x <- sort(x, decreasing=T)
    DCG <- function(y) y[1] + sum(y[-1]/log(2:length(y), base=2))
    
    DCG(x)/DCG(ideal_x)
}
source("load.R")

summed_squared_difference <- function(result,target) {
  difference <- result-target
  return(mean(difference*difference))
  #return(mean(abs(difference)))
}

calculate_error_for_id <- function(srch_id, booked_prob) {
    
    registerDoParallel(cores=N_CORES)
    errors <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% {
        print(paste("now at",id))
        
        prob_order <- order(booked_prob[srch_id == id], decreasing=F)
        return(ndcg(prob_order));
    }
    
    return(errors)
}

analyze <- function(data, train, fwd) {
  
  cross_sections <- 10
  unique_ids <- unique(data$srch_id)
  crossSelection <- sample(1:cross_sections, size=length(unique_ids), replace=TRUE)
  
  error_train <- numeric(cross_sections)
  error_test <- numeric(cross_sections)
  
  interesing_columns <- c('site_id', 'prop_starrating', 'prop_location_score1', 'prop_review_score', 'prop_brand_bool')
    
  for (i in 1:cross_sections) {
    ## creating selection for ids
    selection <- crossSelection == i
    train_ids <- unique_ids[!selection]
    #test_ids <- unique_ids[selection]
    
    ## creating selection for data lines
    train_selection <- c()
    for (id in data$srch_id) {
        train_selection <- c(train_selection, id %in% train_ids)
        #test_selection <- c(test_selection, id %in% test_ids)
    }
    
    ## creating train and test dataset
    train <- data[train_selection,interesing_columns]
    test <- data[!train_selection,interesing_columns]
    
    ## creating targets
    target_train <- train$booked_prob
    #target_test <- test$booked_prob
    
    ## train
    model <- train(train, target_train)
    
    ## results
    result_train <- fwd(model, train)
    result_test <- fwd(model, test)
    
    ## calculate positions
    errors_train <- calculate_error_for_id(train$srch_id, result_train)
    errors_test <- calculate_error_for_id(test$srch_id, result_test)
    
    ## calculate errors for id
    
    ## error calculation
    error_train[i] <- mean(errors_train)
    error_test[i] <- mean(errors_test)
  }
  
  error = c(mean(error_train), mean(error_test))
  print(error)
  
  return(error)
}
