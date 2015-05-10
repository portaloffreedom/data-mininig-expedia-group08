
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
create_prob_col <- function(dataset) {
    dataset$booked_prob <- 0;
    
    # this line has problems with the minified dataset
    prob_known_position <- table(d$position[d$booking_bool]) / table(d$position)
    
    # cheating
    prob_known_position[35:40] <- prob_known_position[35]
    
    # giving a probability for the position
    for (p in prob_known_position) {
        position <- as.numeric(names(p))
        dataset$booked_prob[dataset$position == position] <- p
    }
    
    # overwriting who has the click flag
    p <- sum(d$click_bool[d$booking_bool]) / sum(d$click_bool)
    dataset$booked_prob[dataset$click_bool] <- p
    
    # overwriting who has the booking flag
    dataset$booked_prob[dataset$booking_bool] <- 1
    
    return(dataset)
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