#' Assign Trend markers using assign_trend_markers
#'
#' @param data a data.frame containing the data to calculate the recent trends for; unquoted string; no default
#' @param value field name within data that contains the indicator value. value field is not needed for "proportion"; unquoted string; no default
#' @param denominator field name from data containing the population(s) in the sample; unquoted string; no default
#' @param numerator  field name from data containing the observed numbers of cases in the sample meeting the required condition; unquoted string; no default
#' @param t field name within the data that contains time period values; unquoted string; numeric
#' @param value_type indicates the indicator type (proportion, other, dsr); quoted string
#' @param lower_ci field name within data that contains 95 percent lower confidence limit of indicator value (to calculate standard error of indicator value).
#' lower_ci is not needed for "proportions". unquoted string; no default
#' @param upper_ci field name within data that contains 95 percent upper confidence limit of indicator value (to calculate standard error of indicator value).
#' upper_ci is not needed for "proportions". unquoted string; no default
#' @param denominator_transform option to transform denominator if denominator is missing. For directly standardised rates, the populations may be present, but the denominators
#' should still be transformed and the derived denominators used; logical; default FALSE
#' @param numerator_transform option to transform numerator if numerator is missing; logical; default FALSE
#' @param multiplier the multiplier that the rate is normalised with (ie, per 100,000). Multiplier is not
#' needed for "proportion"; numeric; default = 1
#'
#' @return a tibble with the 5 most recent data points with the trend direction and reasond for the
#' trend for each subgroup of the inputted data.frame.
#'
#'
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang quo_is_null is_quosure
#' @importFrom rlang := .data
#' @export
#'
#'
#'
#' @examples
#'
#' set.seed(5)
#'
#' df <- data.frame(time = rep(2021:2025, each = 2),
#'   Sex = rep(rep(c("Male", "Female")), 5),
#'   numerator = sample(50000:99900, 10, replace = TRUE),
#'   denominator = sample(100000:200000, 10, replace = TRUE ))
#'
#' df %>%
#'   group_by(Sex) %>%
#'   assign_trend_markers(
#'   denominator = denominator,
#'   numerator = numerator,
#'   t = time,
#'   value_type = 'proportion'
#'   )
#'
#' df <- data.frame(time = rep(2021:2025),
#'   numerator = sample(10000:99900, 5, replace = TRUE),
#'   denominator = sample(100000:200000, 5, replace = TRUE ))
#'
#' assign_trend_markers(
#'   df = df,
#'   denominator = denominator,
#'   numerator = numerator,
#'   t = time,
#'   value_type = 'proportion')
#'
#'
#'
#'
#'
#' @section Notes: Tests must only be applied when the following conditions apply:
#' At least 5 points in the time series must be available: the most recent 5 points will be used, and these must be equally
#' spaced, eg the last five 1-year figures with no missing values.
#'
#' Time series points must be non-overlapping, ie single years, or non-overlapping 3-year periods.  The tests are not valid if
#' applied to rolling averages, eg 2021-23, 2022-24,… as these points are not independent of each other.
#'
#' For proportions all points must have counts and denominators.
#'
#' For all indicators other than proportions all points must have upper and lower 95% confidence limits.
#'
#' If there are only indicator values and numerators are present the denominator needs to be calculated
#' For directly standardised rates, the populations may be present, but the denominators
#' should still be calculated as above and these derived denominators used.
#'
#'
#'
#' @family PHEindicatormethods package functions
#'

assign_trend_markers <- function(data,
                                 year_col,
                                 value,
                                 denominator,
                                 numerator,
                                 value_type = c("proportion", "other"),
                                 lower_cl,
                                 upper_cl,
                                 multiplier = 1) {

  if (missing(data)) {
    stop("function assign_trend_markers requires the arguments: data")
  }

  if (missing(year_col)) {
    stop("function assign_trend_markers requires the year_col field")
  }

  if (!(value_type %in% c("proportion", "other"))) {
    stop('value_type should be either "proportion", "other"')
  }



  if (value_type =='other'){
    if ( missing(lower_cl) | missing(upper_cl)) {
       stop(paste('function assign_trend_markers requires "lower_cl" and "upper_cl" when value_type is other'))
    }

    if (missing(value)){

     stop(paste('function assign_trend_markers requires value when value_type is "other"'))

    }
  }


  year_col <- deparse(substitute(year_col))


  value <- if (!missing(value)) deparse(substitute(value)) else NULL

  denominator <- if (!missing(denominator)) deparse(substitute(denominator)) else NULL

  numerator <- if (!missing(numerator)) deparse(substitute(numerator)) else NULL


  lower_cl <- if (!missing(lower_cl)) deparse(substitute(lower_cl)) else NULL

  upper_cl <- if (!missing(upper_cl)) deparse(substitute(upper_cl)) else NULL

  if (value_type == 'proportion'){


    if (is.null(numerator) && is.null(denominator)){

      stop(paste('function assign_trend_markers requires at least a numerator or denominator when value_type is "proportion"'))


    }

    if (is.null(numerator)){

      data <- derive_proportion_element(data = data, denominator = denominator, value = value)

      numerator <- 'numerator'

    } else if (is.null(denominator)){

      data <- derive_proportion_element(data = data, numerator = numerator, value = value)

      denominator <- 'denominator'

    }


  }



  df_list <- if (is_grouped_df(data)) {
    group_split(data)
  } else {
      list(data)}



    df_results <- map(df_list, function(df){

      df <- df %>%
        arrange(.data[[year_col]])

      n_rows <- nrow(df)

      df$RecentTrend <- NA_character_
      df$RecentTrendReason <- NA_character_

      if (n_rows < 5) {

        df$RecentTrend[n_rows] <- "Cannot be calculated"
        df$RecentTrendReason[n_rows] <- "Not enough data points"

        return(df)
      }

      if (value_type == 'other'){


        for (i in 5:n_rows){

          df_sub <- df[(i - 4):i,]

          values <- df_sub[[value]] / multiplier
          denominator_value <- df_sub[[denominator]]
          numerator_value <- df_sub[[numerator]]
          time_value <- df_sub[[year_col]]
          lower_cl_value <- df_sub[[lower_cl]]
          upper_cl_value <- df_sub[[upper_cl]]


          if (anyNA(values)) {

            df$RecentTrend[i] <- "Cannot be calculated"
            df$RecentTrendReason[i] <- "Missing values"

            next

          }



          if(anyNA(lower_cl_value) | anyNA(upper_cl_value)){

            df$RecentTrend[i] <- "Cannot be calculated"
            df$RecentTrendReason[i] <- "missing at least one confidence interval value"

            return(df)
          }


          beta <- calculate_trend_weighted_regression(value = values, year_col= time_value,
                                                      lower_cl = lower_cl_value,
                                                      upper_cl = upper_cl_value,
                                                      trend_direction_only  = TRUE)



          test_statistic <- calculate_trend_weighted_regression(value = values, year_col= time_value,
                                                                lower_cl = lower_cl_value,
                                                                upper_cl = upper_cl_value,
                                                                trend_direction_only = FALSE)



          if (test_statistic > 9.5495) {

            opposite_found <- FALSE

            for (j in 1:length(values)) {

              beta_sub <- calculate_trend_weighted_regression(value = values[-j], year_col= time_value[-j],
                                                              lower_cl = lower_cl_value[-j],
                                                              upper_cl = upper_cl_value[-j],
                                                              trend_direction_only  = TRUE)

              if (sign(beta_sub) != sign(beta)) {

                opposite_found <- TRUE

                break
              }
            }

            if (opposite_found) {

              df$RecentTrend[i] <- "No significant change"
              df$RecentTrendReason[i] <- paste0("Outlier found")

            } else {

              df$RecentTrend[i] <- ifelse(beta < 0, "decreasing", "increasing")
              df$RecentTrendReason[i] <- "No outliers and chi-square is bigger than 9.5495"
            }

          } else {

            df$RecentTrend[i] <- "No significant change"
            df$RecentTrendReason[i]  <- "chi-square is less than 9.5495"


          }
        }

        return(df)

      } else if (value_type == 'proportion'){



        for (i in 5:n_rows){

          df_sub <- df[(i - 4):i,]

          denominator_value <- df_sub[[denominator]]
          numerator_value <- df_sub[[numerator]]
          time_value <- df_sub[[year_col]]



          if (anyNA(denominator_value) | anyNA(numerator_value)) {

            df$RecentTrend[i] <- "Cannot be calculated"
            df$RecentTrendReason[i] <- "Missing values"

            next

          }


          beta <-  calculate_trend_logistic_regression(denominator = denominator_value,
                                                       numerator = numerator_value,
                                                       year_col = time_value,
                                                       trend_direction_only = TRUE)

          test_statistic <- calculate_trend_logistic_regression(denominator = denominator_value,
                                                                numerator = numerator_value,
                                                                year_col = time_value,
                                                                trend_direction_only = FALSE)

          if (test_statistic > 9.5495) {

            opposite_found <- FALSE

            for (j in 1:length(time_value)) {

              beta_sub <- calculate_trend_logistic_regression(denominator = denominator_value[-j],
                                                              numerator = numerator_value[-j],
                                                              year_col = time_value[-j],
                                                              trend_direction_only = TRUE)

              if (sign(beta_sub) != sign(beta)) {

                opposite_found <- TRUE
                break
              }
            }

            if (opposite_found) {

              df$RecentTrend[i] <- "No significant change"
              df$RecentTrendReason[i] <- paste0("Outlier found")
            } else {
              df$RecentTrend[i] <- ifelse(beta < 0, "decreasing", "increasing")
              df$RecentTrendReason[i] <- "No outliers and chi-square is bigger than 9.5495"
            }

          } else {
            df$RecentTrend[i] <- "No significant change"
            df$RecentTrendReason[i] <- "chi-square is less than 9.5495"
          }
        }

        return(df)

      }

  })

  results <- bind_rows(df_results)


  return(results)
}
