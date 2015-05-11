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

analyze <- function(data, train_fn, fwd_fn) {
  
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
    
    print("A")
    ## creating train and test dataset
    train <- data[train_selection,interesing_columns]
    test <- data[!train_selection,interesing_columns]
    
    print("B")
    ## creating targets
    target_train <- train$booked_prob
    #target_test <- test$booked_prob
    
    print("C")
    ## train
    model <- train_fn(train, target_train)
    
    print("D")
    ## results
    result_train <- fwd_fn(model, train)
    result_test <- fwd_fn(model, test)
    
    print("E")
    ## calculate positions
    errors_train <- calculate_error_for_id(train$srch_id, result_train)
    errors_test <- calculate_error_for_id(test$srch_id, result_test)
    
    print("F")
    ## error calculation
    error_train[i] <- mean(errors_train)
    error_test[i] <- mean(errors_test)
  }
  
  error = c(mean(error_train), mean(error_test))
  print(error)
  
  return(error)
}

mlp_analyze <- function(data) {
    library("RSNNS")
    
    errors<-c()
    
    forward <- function(model, data){
        print("C1")
        predict_data <- predict(model,data)
        return(predict_data)
    }

    i_values <- 2:3
    for (i in i_values) {
        train <- function(train_data, target){
            print(paste("C2",i))
            print(train_data)
            model <- mlp(x=train_data,                    #input data for training
                         y=target,                        #output data (targets) for training
                         size=i,                          #number of neurons in the hidden layer
                         learnFunc="Std_Backpropagation", #type of learning
                         learnFuncParams=c(0.01),         #paramenters of the learning function (eta)
                         maxit=50000)                     #maximum number of iterations
                                                        
            return(model)
        }
        
        error <- analyze(data, train, forward)
        errors <- rbind(errors, error)
    }
    
    plot(c(min(i_values),max(i_values)), c(min(errors), max(errors)) ,type="n")
    lines(i_values, errors[,1], col="black")
    lines(i_values, errors[,2], col="red")
}
