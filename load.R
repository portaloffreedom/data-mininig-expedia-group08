
DATE_FORMAT <- "%Y-%m-%d %H:%M:%S"

parseDateFromFactor <- function(date_factor) {
    date <- strptime(as.character(date_factor), format = DATE_FORMAT)
    return(date)
}

parseDateFromString <- function(date_string) {
    date <- strptime(date_string, format = DATE_FORMAT)
    return(date)
}

##
## Create the column of the probability of beeing booked.
## The columns is calculated this way:
##  - if the booking flag is present, then the probability is 1
##  - else, if the clicking flag is present, then the probabilty is
##     P(booked/clicked) [= 0.6237183]
##  - else the probability is P(booked/position)
##
create_prob_col <- function(d) {
    d$booked_prob <- 0;
    
    ##############
    ## POSITION ##
    ##############
    # this line has problems with the minified dataset
    prob_known_position <- table(d$position[d$booking_bool]) / table(d$position)
    
    # cheating
    prob_known_position[35:length(prob_known_position)] <- prob_known_position[35]
    
    # giving a probability for the position
    for (i in 1:length(prob_known_position)){
        p <- prob_known_position[i]
        position <- as.numeric(names(p))
        d$booked_prob[d$position == position] <- p
    }
    
    
    ##########################
    ## POSITION AND CLICKED ##
    ##########################
    # this line has problems with the minified dataset (for clicked)
    prob_known_position <- table(d$position[d$booking_bool]) / table(d$position[d$click_bool])
    
    # cheating again
    prob_known_position[35:length(prob_known_position)] <- prob_known_position[35]
    
    # giving a probability for the position for clicked
    for (i in 1:length(prob_known_position)){
        p <- prob_known_position[i]
        position <- as.numeric(names(p))
        d$booked_prob[d$click_bool & (d$position == position)] <- p
    }
    
    ############
    ## BOOKED ##
    ############
    # overwriting who has the booking flag
    d$booked_prob[d$booking_bool] <- 1
    
    return(d)
}

uinque_id_iter <- function(unique_ids) {
    count <- length(unique_ids)
    i <- 0L
    nextEl <- function() {
        if (i < count) 
            unique_ids[i <<- i + 1L]
        else stop("StopIteration", call. = FALSE)
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it

}

test_parallel <- function() { 

    cicle <- 1:1e5
    a <- foreach(i = uinque_id_iter(1:1e5), .combine='c') %dopar% {
        b <- rnorm(1e6) + i
        b <- summary(b)[3]
        b
    }
    return(a)
}


create_prob_pos_col <- function(d) {
    library(foreach)
    library(doParallel)
    
    #cl <- makeCluster(8)
    #registerDoParallel(cl)
    registerDoParallel(cores=8)
    
    booked_prob_pos <- c()
    
    booked_prob_pos <- foreach(id = uinque_id_iter(unique(d$srch_id)), .combine='c') %dopar% {
        print(paste("now at",id))
        
        return(order(d$booked_prob[d$srch_id == id], decreasing=T))
    }
    
    #stopCluster(cl)
    
    d$booked_prob_pos <- booked_prob_pos
    
    return(d)
}

create_competitors_columns <- function(dataset) {
    
    comp_rate_columns <- cbind(
            dataset$comp1_rate,
            dataset$comp2_rate,
            dataset$comp3_rate,
            dataset$comp4_rate,
            dataset$comp5_rate,
            dataset$comp6_rate,
            dataset$comp7_rate,
            dataset$comp8_rate
        )
    
    # number of competitors
    dataset$n_comp <- rowSums   (
        !is.na(comp_rate_columns)
    )
    
    # number of cheaper competitors
    dataset$n_comp_cheaper <- rowSums(
        (comp_rate_columns == -1), na.rm = TRUE
    )
    
    # number of more expensive competitors
    dataset$n_comp_expensive <- rowSums(
        (comp_rate_columns == 1), na.rm = TRUE
    )
    
    return(dataset)
}

loadData <- function(filename = "train_min.csv", minified = TRUE) {

  if (minified)
    rownames = 1
  else
    rownames = NULL
  
  dataset <- read.csv(filename, row.names = rownames, na.strings = "NULL")
  
  # parse dates
  #dataset$date_time <- sapply(dataset$date_time, parseDate)
  dataset$date_time <- sapply(dataset$date_time, as.character)

  ## BOOLEAN PARSING
  # parse srch_saturday_night_bool
  dataset$srch_saturday_night_bool <- ifelse(dataset$srch_saturday_night_bool == 1, TRUE, FALSE)
  # parse random_bool
  dataset$random_bool <- ifelse(dataset$random_bool == 1, TRUE, FALSE)
  # parse click_bool
  dataset$click_bool <- ifelse(dataset$click_bool == 1, TRUE, FALSE)
  # parse booking_bool
  dataset$booking_bool <- ifelse(dataset$booking_bool == 1, TRUE, FALSE)
  # parse promotion_flag
  dataset$promotion_flag <- ifelse(dataset$promotion_flag == 1, TRUE, FALSE)
  # parse prop_brand_bool
  dataset$prop_brand_bool <- ifelse(dataset$prop_brand_bool == 1, TRUE, FALSE)
  
  ## FACTORS
  # parse prop_review_score
  dataset$prop_review_score <- as.factor(dataset$prop_review_score)

  
  return(dataset)
}

parse_bool_col <- function(col) {
    ifelse(col == 1, TRUE, FALSE)
}

create_seasons <- function(train){
  return(
    as.numeric(gsub('....-(..)-..*',"\\1",train$date_time))
#     levels = 1:12,
#     labels = c("Jan","Feb","Mar","Apr","May","June","July","August","September","October","November","December"))
  )
}

keep_only_needed <- function(data) {
    data <- data[, c(
        "srch_id",
        "date_time",
#         "site_id",
        "visitor_location_country_id",
#         "visitor_hist_starrating",
#         "visitor_hist_adr_usd",
        "prop_country_id",
        "prop_id",
        "prop_starrating",
        "prop_review_score",
        "prop_brand_bool",
        "prop_location_score1",
        "prop_location_score2",
        "prop_log_historical_price",
#         "position",
        "price_usd",
        "promotion_flag",
        "srch_destination_id",
        "srch_length_of_stay",
        "srch_booking_window",
        "srch_adults_count",
        "srch_children_count",
        "srch_room_count",
        "srch_saturday_night_bool",
#         "srch_query_affinity_score",
        "orig_destination_distance",
#         "random_bool",
#         "click_bool",
#         "gross_bookings_usd",
#         "booking_bool",
        "booked_prob",
        "n_comp",
        "n_comp_cheaper",
        "n_comp_expensive",
        "booked_prob_pos",
        "season"
    )]
    
    normalize <- function(column) column/max(column, na.rm=T)
    
    data$prop_starrating <- normalize(data$prop_starrating)
    data$prop_review_score <- normalize(data$prop_review_score)
    data$prop_location_score1 <- normalize(data$prop_location_score1)
    data$prop_location_score2 <- normalize(data$prop_location_score2)
    
    data$srch_length_of_stay <- normalize(data$srch_length_of_stay)
    data$srch_booking_window <- normalize(data$srch_booking_window)
    data$srch_adults_count <- normalize(data$srch_adults_count)
    data$srch_children_count <- normalize(data$srch_children_count)
    data$srch_room_count <- normalize(data$srch_room_count)
    
    data$orig_destination_distance <- normalize(data$orig_destination_distance)
    
    data$n_comp <- normalize(data$n_comp)
    data$n_comp_cheaper <- normalize(data$n_comp_cheaper)
    data$n_comp_expensive <- normalize(data$n_comp_expensive)
    
    return(data)
}

fill_missing_values <- function(d) {
    #orig_destination_distance
    fill_NA_with_median <- function(column) {
        column[is.na(column)] <- median(column, na.rm=T)
        return(column)
    }
    
    d$orig_destination_distance <- fill_NA_with_median(d$orig_destination_distance)
    d$prop_review_score <- fill_NA_with_median(d$prop_review_score)
    d$prop_location_score2 <- fill_NA_with_median(d$prop_location_score2)
    
    return(d)
}

refactor_some_columns <- function(d) {
    
    r_exp <- function(column) {
        c_exp <- 2^column
        c_exp/max(c_exp,na.rm=T)
    }
    
    d$prop_review_score <- r_exp(d$prop_review_score)
    
    historicalPrice = d$prop_log_historical_price
    historicalPrice[historicalPrice == 0] = median(historicalPrice[historicalPrice != 0])
    historicalPrice = historicalPrice -  mean(historicalPrice)
    historicalPrice = historicalPrice /  sd(historicalPrice)
    d$prop_log_historical_price = historicalPrice
    
    return(d)
}

check_sorting_in_srch_id <- function(d) {
    tot <- 0;
    print(paste("max: ",max(d$srch_id)))
    
    for (id in unique(d$srch_id)) {
        print(paste("now at",id, "(tot = ",tot,")"))
        #selection <- d[d$srch_id == id,]
        #print("done counting")
        
        count <- sum(d$random_bool[d$srch_id == id])
        #print("done sum")
        if (count != 0 && count != sum(d$srch_id == id)) {
            tot <- tot+1
        }
    }
    
    return(tot)
}