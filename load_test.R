# loading test data section
test <- read.csv("test_set_VU_DM_2014.csv", na.strings = "NULL")
test <- refactor_some_columns(test)
test <- fill_missing_values(test)
test$price_usd <- normalize_price(test$srch_id, test$price_usd)
test$season <- create_seasons(test)
test <- create_competitors_columns(test)
test <- test[,c(
    "srch_id",
    "date_time",
#     "site_id",
    "visitor_location_country_id",
#     "visitor_hist_starrating",
#     "visitor_hist_adr_usd",
    "prop_country_id",
    "prop_id",
    "prop_starrating",
    "prop_review_score",
    "prop_brand_bool",
    "prop_location_score1",
    "prop_location_score2",
    "prop_log_historical_price",
    "price_usd",
    "promotion_flag",
    "srch_destination_id",
    "srch_length_of_stay",
    "srch_booking_window",
    "srch_adults_count",
    "srch_children_count",
    "srch_room_count",
    "srch_saturday_night_bool",
#     "srch_query_affinity_score",
    "orig_destination_distance",
    "random_bool",
#     "comp1_rate",
#     "comp1_inv",
#     "comp1_rate_percent_diff",
#     "comp2_rate",
#     "comp2_inv",
#     "comp2_rate_percent_diff",
#     "comp3_rate",
#     "comp3_inv",
#     "comp3_rate_percent_diff",
#     "comp4_rate",
#     "comp4_inv",
#     "comp4_rate_percent_diff",
#     "comp5_rate",
#     "comp5_inv",
#     "comp5_rate_percent_diff",
#     "comp6_rate",
#     "comp6_inv",
#     "comp6_rate_percent_diff",
#     "comp7_rate",
#     "comp7_inv",
#     "comp7_rate_percent_diff",
#     "comp8_rate",
#     "comp8_inv",
#     "comp8_rate_percent_diff",

    # created ones
    "season",
    "n_comp",
    "n_comp_cheaper",
    "n_comp_expensive"
)]

test <- add_season_vector(test)
