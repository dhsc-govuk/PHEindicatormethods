# -------------------------------------------------------------------------------------------------
#' Calculate Means using phe_mean
#'
#' Calculates means with confidence limits using Student's t-distribution method.
#'
#' @param data a data.frame containing the data to calculate means for, pre-grouped if multiple means required; unquoted string; no default
#' @param x field name from data containing the values to calculate the means for; unquoted string; no default
#'
#' @inheritParams calculate_dsr
#'
#' @import dplyr
#' @importFrom rlang sym quo_name
#' @importFrom stats qt sd
#' @export
#'
#' @return When type = "full", returns a data.frame of value_sum, value_count, stdev, value, lowercl, uppercl, confidence, statistic and method
#'         for each grouping set
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(values = c(30,40,50,60))
#'
#' ## default execution
#' phe_mean(df, values)
#'
#' ## calculate 95% and 99.8% CIs in single execution
#' phe_mean(df, values, confidence = c(0.95, 0.998))
#'
#' ## calculate multiple means in a single execution
#'
#' df2 <- data.frame(area = rep(c("Area1", "Area2"),each=3),
#'                   values = c(20,30,40,200,300,400)) %>%
#'                   group_by(area)
#' phe_mean(df2,values)
#' phe_mean(df2,values,type="standard", confidence=0.998)
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_mean function using Student's t-distribution method
phe_mean <- function(data, x, type = "full", confidence=0.95) {

    # check required arguments present
    if (missing(data)|missing(x)) {
        stop("function phe_mean requires at least 2 arguments: data, x")
    }


    # validate arguments - copied from proportion need editing
    if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
        stop("type must be one of value, lower, upper, standard or full")
    } else if (length(confidence) >2) {
        stop("a maximum of two confidence levels can be provided")
    } else if (length(confidence) == 2) {
        if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
            stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
        }
    } else if ((confidence < 0.9)|(confidence > 1 & confidence < 90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }


    # if two confidence levels are specified
    if (length(confidence) == 2) {

        # if two confidence levels requested
        p1 <- (1 - confidence[1]) / 2
        p2 <- (1 - confidence[2]) / 2

        # calculate mean and CIs
        phe_mean <- data %>%
            summarise(value_sum   = sum({{ x }}),
                      value_count = length({{ x }}),
                      stdev   = sd({{ x }}),
                      .groups = "keep") %>%
            mutate(value = .data$value_sum / .data$value_count,
                   lower95_0cl = .data$value - abs(qt(p1, .data$value_count - 1)) *
                     .data$stdev / sqrt(.data$value_count),
                   upper95_0cl = .data$value + abs(qt(p1, .data$value_count - 1)) *
                     .data$stdev / sqrt(.data$value_count),
                   lower99_8cl = .data$value - abs(qt(p2, .data$value_count - 1)) *
                     .data$stdev / sqrt(.data$value_count),
                   upper99_8cl = .data$value + abs(qt(p2, .data$value_count - 1)) *
                     .data$stdev / sqrt(.data$value_count),
                   confidence = paste(confidence[1]*100,"%, ",confidence[2]*100,"%", sep=""),
                   statistic = "mean",
                   method  = "Student's t-distribution")

        # drop fields not required based on type argument
        if (type == "lower") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "value", "upper95_0cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "value", "lower95_0cl", "lower99_8cl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "lower95_0cl", "lower99_8cl", "upper95_0cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_mean <- phe_mean %>%
                select(!c("confidence", "statistic", "method"))
        }

    } else {

        # scale confidence level
        if (confidence[1] >= 90) {
            confidence <- confidence/100
        }


        # calculate mean with a single CI
        p <- (1 - confidence) / 2

        phe_mean <- data %>%
            summarise(value_sum   = sum({{ x }}),
                      value_count = length({{ x }}),
                      stdev   = sd({{ x }}),
                      .groups = "keep") %>%
            mutate(value = .data$value_sum / .data$value_count,
                   lowercl = .data$value - abs(qt(p, .data$value_count - 1)) * .data$stdev / sqrt(.data$value_count),
                   uppercl = .data$value + abs(qt(p, .data$value_count - 1)) * .data$stdev / sqrt(.data$value_count),
                   confidence = paste(confidence*100,"%", sep=""),
                   statistic = "mean",
                   method  = "Student's t-distribution")

        # drop fields not required based on type argument
        if (type == "lower") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "value", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "value", "lowercl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_mean <- phe_mean %>%
                select(!c("value_sum", "value_count", "stdev", "lowercl", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_mean <- phe_mean %>%
                select(!c("confidence", "statistic", "method"))
        }
    }

    return(phe_mean)
}
