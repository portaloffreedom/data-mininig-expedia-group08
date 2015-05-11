pie_competitor_rate <- function(rate) {
    pie_values <- c(
        sum(rate == 0, na.rm = TRUE),
        sum(rate == 1, na.rm = TRUE),
        sum(rate == -1, na.rm = TRUE)
    )

    pie(pie_values,labels=c("equal", "expedia", "competitor"))
}

competitor_rate_ration <- function(dataset) {
    print("make sure you already called create_competitors_columns")
    
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
    totals <- colSums(
        !is.na(comp_rate_columns)
    )
    
    # number of times competitor is cheaper
    cheaper <- colSums(
        (comp_rate_columns == -1), na.rm = TRUE
    )
    
    # number of times competitor have the same price
    same <- colSums(
        (comp_rate_columns == 0), na.rm = TRUE
    )
    
    # number of times competitor is more expensive
    expensive <- colSums(
        (comp_rate_columns == 1), na.rm = TRUE
    )
    
    return(data.frame(totals,cheaper,same, expensive))
}

plot_competitor_rate_ration <- function(column, total, title) {
    values <- column / total
    
    barplot(values, main=title, ylim=c(0,1), names.arg=as.character(seq(1,length(column))))
    return(values)
}

pie_ids <- function(site_ids) {
    pie_values <- rep(0,max(site_ids))
    
    for (id in site_ids) {
        pie_values[id] <- pie_values[id] +1
    }
    
    pie(pie_values)
}

site_location_cor <- function(d) {
    site_ids <- sort(unique(d$site_id))
    checks <- cbind(site_ids, rep(0,length(site_ids)), rep(0,length(site_ids)))
    
    for (site_id in site_ids) {
        
        site_id_set <- d[d$site_id == site_id,]
        
        most_frequent <- names(sort(table(site_id_set$visitor_location_country_id), decreasing=TRUE))[1]
        
        tot <- sum(site_id_set$visitor_location_country_id != most_frequent)
    
        checks[checks[,1] == site_id,2:4] <- c(tot, as.numeric(most_frequent),0)
    }
    
    return(checks)
}

error_in_sort <- function(selection) {
    return(sum(abs(
        selection$position - seq(1,nrow(selection))
    )))
}

check_price_sorting_error <- function(d) {
    
    tot <- 0;
    tot_rnd <- 0;
    
    print(paste("max: ",max(d$srch_id)))
    
    cn <- colnames(d)
    needed_columns <- (cn == "position") |
                      (cn == "price_usd") 
    
    for (id in unique(d$srch_id[!d$random_bool])) {
        print(paste("now at",id, "(tot=",tot,"tot_rnd=",tot_rnd,")"))
        
        selection <- d[d$srch_id == id,needed_columns]
        print("done selecting")
        
        selection <- selection[order(selection$price_usd),]
        selection_random <- selection[sample(nrow(selection)),]
        
        tot <- tot + error_in_sort(selection)
        tot_rnd <- tot_rnd + error_in_sort(selection_random)
    }
    
    return(c(tot,tot_rnd))
}