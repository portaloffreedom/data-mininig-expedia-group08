pie_competitor_rate <- function(rate) {
    pie_values = c(
        sum(rate == 0, na.rm = TRUE),
        sum(rate == 1, na.rm = TRUE),
        sum(rate == -1, na.rm = TRUE)
    )

    pie(pie_values,labels=c("equal", "expedia", "competitor"))
}