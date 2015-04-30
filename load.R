
DATE_FORMAT <- "%Y-%m-%d %H:%M:%S"

parseDateFromFactor <- function(date_factor) {
    date <- strptime(as.character(date_factor), format = DATE_FORMAT)
    return(date)
}

parseDateFromString <- function(date_string) {
    date <- strptime(date_string, format = DATE_FORMAT)
    return(date)
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
  

  
  return(dataset)
}