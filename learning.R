N_CORES <- 3
library(foreach)
library(doParallel)


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

calculate_error_for_id <- function(srch_id, booked_prob, real_booked_prob) {
    
    registerDoParallel(cores=N_CORES)
    
    errors <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% {
#         print(paste("now at",id))
        
        real_prob_order <- order(real_booked_prob[srch_id == id], decreasing=T)
        prob_order <- order(order(booked_prob[srch_id == id], decreasing=F))
        return((1-ndcg(prob_order[real_prob_order]))^2);
    }
    
    return(errors)
}

calculate_error_random <- function(srch_id, real_booked_prob) {
    
    registerDoParallel(cores=N_CORES)
    
    errors <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% {
#         print(paste("now at",id))

        random_values <- runif(sum(srch_id == id), 0, 1)
        
        real_prob_order <- order(real_booked_prob[srch_id == id], decreasing=T)
        prob_order <- order(order(random_values, decreasing=F))
        return((1-ndcg(prob_order[real_prob_order]))^2);
    }
    
    return(errors)
}

create_selection_ids <- function(srch_ids, unique_ids, crossSelection) {
  id_index <- 1
  unique_id_index <- 1

  rows <- length(srch_ids)
  selection_ids <- integer(rows)

  for (selected_id in sort(unique_ids)) {
    #find id
    while (srch_ids[id_index] != selected_id) {
        id_index <- id_index + 1
        if (id_index > rows)
            return(selection_ids)
    }
    
    #find finish_id
    finish_id <- id_index
    while (srch_ids[finish_id] == selected_id) {
        finish_id <- finish_id + 1
        if (finish_id > rows)
            break;
    }
    finish_id <- finish_id - 1
    
    # add the number
    selection_ids[id_index:finish_id] <- crossSelection[unique_id_index]
    
#     print(paste("line!",unique_id_index,":",selected_id,selection_ids[id_index],selection_ids[finish_id]))
#     print(paste("line!",unique_id_index,":",selected_id,id_index,finish_id))
    
    unique_id_index <- unique_id_index +1
  }

  return(selection_ids)
}

analyze <- function(data, train_fn, fwd_fn) {
  
  cross_sections <- 10
  unique_ids <- unique(data$srch_id)
  crossSelectionIds <- sample(1:cross_sections, size=length(unique_ids), replace=TRUE)
  
  error_train <- numeric(cross_sections)
  error_test <- numeric(cross_sections)
  error_random <- numeric(cross_sections)
  
  interesing_columns <- c(
#     'visitor_location_country_id',
    'prop_starrating',
    'prop_location_score1',
    'prop_location_score2',
    'prop_review_score'
  )
  
  print("A0")  
  cross_selection <- create_selection_ids(data$srch_id, unique_ids, crossSelectionIds)
    
  for (i in 1:cross_sections) {
      print(paste("cross_section:", i))
#     print("A0")
#     ## creating selection for ids
#     selection <- crossSelectionIds == i
#     train_ids <- unique_ids[!selection]
#     #test_ids <- unique_ids[selection]
# 
#     print("A1")
#     ## creating selection for data lines
#     registerDoParallel(cores=8)
#     train_selection <- foreach(id = uinque_id_iter(train_ids), .combine='c') %dopar% {
#         id %in% train_ids
#         data$srch_id == id
#         
#     }
#     
#     for (id in data$srch_id) {
#         train_selection <- c(train_selection, id %in% train_ids)
#         #test_selection <- c(test_selection, id %in% test_ids)
#     }
    print("A1")
    train_selection <- !(cross_selection == i)
    
    # test if the selection sets are empty
    if (sum(train_selection) == 0) stop("train set selection is empty")
    if (sum(!train_selection) == 0) stop("test set selection is empty")
    
    print("A2")
    ## creating train and test dataset
    train <- data[train_selection,interesing_columns]
    test <- data[!train_selection,interesing_columns]
    
    # test if the sets are empty
    if (nrow(train) == 0) stop("train set is empty")
    if (nrow(test) == 0) stop("test set is empty")
    
#     print(paste("nrow",nrow(train)))
#     print(paste("is.na",sum(is.na(train))))
    if (sum(is.na(train)) != 0) stop("train set has NA")
    if (sum(is.na(test)) != 0) stop("test set has NA")
    
    print("B")
    ## creating targets
    target_train <- data$booked_prob[train_selection]
    #target_test <- data$booked_prob[!train_selection]
    
    if (sum(is.na(target_train)) != 0) stop("target_train set has NA")
    
    print("C")
    ## train
    model <- train_fn(train, target_train)
    
    print("D")
    ## results
    result_train <- fwd_fn(model, train)
    result_test <- fwd_fn(model, test)
    
    print("E")
    ## calculate positions errors
    errors_train <- calculate_error_for_id(data$srch_id[train_selection], result_train, data$booked_prob[train_selection])
    errors_test <- calculate_error_for_id(data$srch_id[!train_selection], result_test, data$booked_prob[!train_selection])
    errors_random <- calculate_error_random(data$srch_id[!train_selection], data$booked_prob[!train_selection])
    
    print("F")
    ## error calculation
    error_train[i] <- mean(errors_train, na.rm=T)
    error_test[i] <- mean(errors_test, na.rm=T)
    error_random[i] <- mean(errors_random, na.rm=T)
    print(c(error_train[i], error_test[i], error_random[i]))
  }
  
  error = c(mean(error_train, na.rm=T), mean(error_test, na.rm=T),  mean(error_random, na.rm=T))
  print(error)
  
  return(error)
}

mlp_analyze <- function(data) {
    library("RSNNS")
    
    errors<-c()
    
    forward <- function(model, data){
        print("D1")
        predict_data <- predict(model,data)
        return(predict_data)
    }

    i_values <- 2:4
    for (i in i_values) {
        train <- function(train_data, target){
            print(paste("C1",i))
            model <- mlp(x=train_data,                    #input data for training
                         y=target,                        #output data (targets) for training
                         size=i,                          #number of neurons in the hidden layer
                         learnFunc="Std_Backpropagation", #type of learning
#                        learnFuncParams=c(0.01),         #paramenters of the learning function (eta)
                         maxit=100                        #maximum number of iterations
                         )
                               
            print(paste("C2",i))                         
            return(model)
        }
        
        error <- analyze(data, train, forward)
        errors <- rbind(errors, error)
    }
    
    plot(c(min(i_values),max(i_values)), c(min(errors), max(errors)) ,type="n")
    lines(i_values, errors[,1], col="black")
    lines(i_values, errors[,2], col="red")
    lines(i_values, errors[,3], col="green")
    
    return(errors)
}
