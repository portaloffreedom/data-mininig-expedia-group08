N_CORES <- 1
library(foreach)
library(doParallel)
registerDoParallel(cores=N_CORES)

source("load.R")
# library("RSNNS")
library(nnet)

DCG <- function(y) y[1] + sum(y[-1]/log(2:length(y), base=2))
ndcg <- function(x) {
    # x is a vector of relevance scores
    ideal_x <- sort(x, decreasing=T)
    #print(x)
    #print(ideal_x)
    #print(paste("results:",DCG(x),DCG(ideal_x),DCG(x)/DCG(ideal_x)))
    
    DCG(x)/DCG(ideal_x)
}

summed_squared_difference <- function(result,target) {
  difference <- result-target
  return(mean(difference*difference))
  #return(mean(abs(difference)))
}

calculate_error_for_id <- function(srch_id, booked_prob, real_booked_prob, ndcg_target) {
    
    registerDoParallel(cores=N_CORES)
    
    errors <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% {
#         print(paste("now at",id))
        selection <- srch_id == id
        
        #real_prob_order <- order(real_booked_prob[selection], decreasing=T)
        prob_order <- ndcg_target[selection][order(booked_prob[selection], decreasing=F)]
        return((ndcg(prob_order)));
    }
    
    return(errors)
}

calculate_error_random <- function(srch_id, real_booked_prob, ndcg_target) {
    
    registerDoParallel(cores=N_CORES)
    
    errors <- foreach(id = uinque_id_iter(unique(srch_id)), .combine='c') %dopar% {
#         print(paste("now at",id))
        selection <- srch_id == id

        random_values <- runif(sum(selection), 0, 1)
        
        #real_prob_order <- order(real_booked_prob[selection], decreasing=T)
        prob_order <- ndcg_target[selection][order(random_values, decreasing=F)]
        return((ndcg(prob_order)));
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

normalize_price <- function(srch_ids, price) {
  id_index <- 1
  unique_id_index <- 1

  rows <- length(srch_ids)
  price_norm <- numeric(rows)

  for (selected_id in sort(unique(srch_ids))) {
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
    
    # normalize the block
    price_block <- price[id_index:finish_id] 
    price_block <- price_block - min(price_block)
    price_block[is.na(price_block)] <- median(price_block, na.rm=T)
    max_price <- max(price_block, na.rm=T)
    
    if (max_price != 0) {
        price_block <- price_block / max(price_block, na.rm=T)
    } else {
        price_block <- .3
    }
    
    price_norm[id_index:finish_id] <- price_block
    
    unique_id_index <- unique_id_index +1
  }

  return(price_norm)
}

analyze <- function(data, train_fn, fwd_fn) {
  
  cross_sections <- 10
  unique_ids <- unique(data$srch_id)
  crossSelectionIds <- sample(1:cross_sections, size=length(unique_ids), replace=TRUE)
  
  error_train <- numeric(cross_sections)
  error_test <- numeric(cross_sections)
  error_random <- numeric(cross_sections)
  
  interesing_columns <- c(
    'visitor_location_country_id',
    'prop_starrating',
    'prop_location_score1',
    'prop_location_score2',
    'prop_review_score',
    'price_usd',
    'n_comp_cheaper',
    'n_comp_expensive',
#    'season',
    's1',
    's2',
    's3',
    's4',
    's5',
    's6',
    's11',
    's12',
    'srch_adults_count',
    'srch_children_count',
    'promotion_flag',
    'prop_brand_bool',
    'booked_prob'
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
    
    print(paste("size of train:",nrow(train),ncol(train)))
    
#     train_5 <- data[data$ndcg_target == 5 & train_selection, interesing_columns]
#     train_1 <- data[data$ndcg_target == 1 & train_selection, interesing_columns]
#     train_0 <- data[data$ndcg_target == 0 & train_selection, interesing_columns][nrow(train_5),]
    
#     train <- rbind(train_5,train_1,train_0)
    
    print(paste("size of train:",nrow(train),ncol(train)))
    
    
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
    
    
#     target_train_5 <- data$booked_prob[data$ndcg_target == 5 & train_selection]
#     target_train_1 <- data$booked_prob[data$ndcg_target == 1 & train_selection]
#     target_train_0 <- data$booked_prob[data$ndcg_target == 0 & train_selection][nrow(train_1)]
    
#     target_train <- c(target_train_5,target_train_1,target_train_0)
    
    if (sum(is.na(target_train)) != 0) stop("target_train set has NA")
    
    print("C")
    ## train
    model <- train_fn(train, target_train)
    
    print("D")
    ## results
    result_train <- fwd_fn(model, train)
    result_test <- fwd_fn(model, test)
    
    #print(cbind(result_train,target_train))
    #print(paste("difference:", sum(target_train - train$booked_prob)))
    
    print("E")
    ## calculate positions errors
    errors_train <- calculate_error_for_id(data$srch_id[train_selection], result_train, data$booked_prob[train_selection], data$ndcg_target[train_selection])
    errors_test <- calculate_error_for_id(data$srch_id[!train_selection], result_test, data$booked_prob[!train_selection], data$ndcg_target[!train_selection])
    errors_random <- calculate_error_random(data$srch_id[!train_selection], data$booked_prob[!train_selection], data$ndcg_target[!train_selection])
    
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

add_season_vector <- function(d) {
 d$s1 <- ifelse(d$season == 1, T, F)
 d$s2 <- ifelse(d$season == 2, T, F)
 d$s3 <- ifelse(d$season == 3, T, F)
 d$s4 <- ifelse(d$season == 4, T, F)
 d$s5 <- ifelse(d$season == 5, T, F)
 d$s6 <- ifelse(d$season == 6, T, F)
 # 7 to 10 missing
 d$s11 <- ifelse(d$season == 11, T, F)
 d$s12 <- ifelse(d$season == 12, T, F)
 
 return(d)
}


load_mini_test_data <- function() {
    nlines <- 40007 #4005

    train <- read.csv("train_full_expanded_v4.csv")[1:nlines,]
    train$ndcg_target <- read.table("ndcg_target.txt")[1:nlines,]
    
    train <- add_season_vector(train)
    train
}

mlp_analyze <- function(data) {
    
    errors<-c()
    registerDoParallel(cores=N_CORES)
    
    forward <- function(model, data){
        print("D1")
        #for (country_id in unique(data$visitor_location_country_id)) {
        r <- foreach(country_id = uinque_id_iter(unique(data$visitor_location_country_id)), .combine='rbind') %dopar% {
            blockSize <- sum(data$visitor_location_country_id == country_id)
            if (is.null(model[country_id][[1]])) {
                print(paste("missing country",country_id))
                return(cbind(rep(0,blockSize),rep(country_id,blockSize)));
            }
            cbind(
                predict(model[[country_id]],data[data$visitor_location_country_id == country_id, (colnames(data) != "visitor_location_country_id")]),
                country_id
            )
        }
        results <- rep(0,nrow(data))
        for (country_id in unique(data$visitor_location_country_id)) {
            if (length(r[r[,2] == country_id,1]) <=0) next;
            results[data$visitor_location_country_id == country_id] <- r[r[,2] == country_id,1]
        }
        #print(cbind(results, r[,1]))
        results
    }

    i_values <- c(2)#,3,5,10,20,50)
    for (i in i_values) {
    registerDoParallel(cores=N_CORES)
        train <- function(train_data, target){
            print(paste("C1",i))
            model <- list()
            #for (country_id in unique(train_data$visitor_location_country_id)) {
            models <- foreach(country_id = uinque_id_iter(unique(data$visitor_location_country_id))) %dopar% {
                input <- train_data[train_data$visitor_location_country_id == country_id,(colnames(train_data) != "visitor_location_country_id")]
                if (sum(train_data$visitor_location_country_id == country_id) == 0) {
                    model[[country_id]] <- NULL
                    return()
                }
                nnet(input, target[train_data$visitor_location_country_id == country_id], 
                        size = i, rang = 0.1, decay = 5e-4, maxit = 100, trace=T)
#                 mlp(x=input,                   #input data for training
#                     y=target[train_data$visitor_location_country_id == country_id],                        #output data (targets) for training
# #                     y=runif(length(target[train_data$visitor_location_country_id == country_id])),                        #output data (targets) for training
#                     size=i,                    #number of neurons in the hidden layer
#                     learnFunc="BackpropBatch", #type of learning
# #                   learnFuncParams=c(0.01),   #paramenters of the learning function (eta)
#                     maxit=150                 #maximum number of iterations
#                     )
                                           
            }
            j <- 0
            for (country_id in unique(data$visitor_location_country_id)) {
                j <- j+1
                model[[country_id]] <- models[[j]]
            }
            print(paste("C2",i))
            return(model)
        }
        
        error <- analyze(data, train, forward)
        errors <- rbind(errors, error)
    }
    
    plot(c(min(i_values),max(i_values)), c(min(errors), max(errors[,1:2])) ,type="n")
    lines(i_values, errors[,1], col="black")
    lines(i_values, errors[,2], col="red")
#     lines(i_values, errors[,3], col="green")
    
    return(errors)
}
