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
                                 t,
                                 value = NULL,
                                 denominator = NULL,
                                 numerator = NULL,
                                 value_type = c("proportion", "other", 'dsr'),
                                 lower_cl= NULL,
                                 upper_cl = NULL,
                                 denominator_transform = FALSE,
                                 numerator_transform = FALSE,
                                 multiplier = 1) {

  if (missing(data)) {
    stop("function assign_trend_markers requires the arguments: data")
  }

  if (missing(t)) {
    stop("function assign_trend_markers requires the t field")
  }

  if (!(value_type %in% c("proportion", "other", 'dsr'))) {
    stop('value_type should be either "proportion", "other", "dsr"')
  }

  if (denominator_transform == TRUE && numerator_transform == TRUE) {

    stop('"denominator_transform" and "numerator_transform" cannot both be TRUE')

  }

  t <- as_label(rlang::enquo(t))


  value <- rlang::enquo(value)

  denominator <- rlang::enquo(denominator)

  numerator <- rlang::enquo(numerator)


  if (numerator_transform == TRUE){

    if ((is_quosure(value) & quo_is_null(value)) | (is_quosure(denominator) & quo_is_null(denominator))) {

      stop("function assign_trend_markers requires denominator and value when numerator_transform is true")

    }

    value <- as_label(value)
    denominator <- as_label(denominator)

    data <-transform_numerator(data = data, denominator = denominator , value= value, multiplier = multiplier)

    numerator <- 'numerator_transformed'


  } else if (is_quosure(numerator)){

    if (quo_is_null(numerator)) {

      stop("function assign_trend_markers requires a numerator if numerator_transform is FALSE")

    }


    numerator <- as_label(numerator)


  }

  if (denominator_transform == TRUE){

    if (is_quosure(value) && quo_is_null(value)) {

      stop("function assign_trend_markers requires numerator and value when denominator_transform is true")

    }

    value <- as_label(value)

    data <-transform_denominator(data, numerator , value, multiplier = multiplier )

    denominator <- 'denominator_transformed'

  } else if (is_quosure(denominator)){

    if (quo_is_null(denominator)) {

      stop("function assign_trend_markers requires a denominator if denominator_transform is FALSE")

    }


    denominator <- as_label(denominator)

  }


  if (is_grouped_df(data)) {


    df_list <- data %>%
      group_split()

    } else {

    df_list <- list(data)

  }

  if (value_type == 'proportion'){



    df_results <- map(df_list, function(df){

      if (nrow(df) < 5) {

        df <- df %>%
          mutate(
            RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
            TrendReason = case_when(row_number() == n() ~ "Not enough data points", TRUE ~ NA_character_)
          )

        return(df)
      }

      df %>%
        slice_max(order_by = .data[[t]], n = 5, with_ties = FALSE) %>%
        arrange(.data[[t]])

      values <- df[[numerator]] / df[[denominator]]
      denominator_value <- df[[denominator]]
      numerator_value <- df[[numerator]]
      time_value <- df[[t]]

      if (any(is.na(values))) {

        df <- df %>%
          mutate(
            RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
            TrendReason = case_when(row_number() == n() ~ "Missing values", TRUE ~ NA_character_)
          )

        return(df)

      }

      time_difference<- diff(time_value)

      if(!all(time_difference==time_difference[1])){

        df <- df %>%
          mutate(
            RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
                   TrendReason = case_when(row_number() == n() ~ "time values not equal", TRUE ~ NA_character_)
                 )
      }

      beta <- calculate_trend_direction(value = values,
                                        t = time_value,
                                        value_type = value_type)

      test_statistic <- calculate_trend_logistic_regression(denominator = denominator_value,
                                                            numerator = numerator_value,
                                                            t = time_value)

      if (test_statistic > 9.5495) {

        opposite_found <- FALSE

        for (i in 1:length(values)) {

          beta_sub <- calculate_trend_direction(value = values[-i], t = time_value[-i], value_type = value_type)

          if (sign(beta_sub) != sign(beta)) {

            opposite_found <- TRUE
            break
          }
        }

        if (opposite_found) {

          RecentTrend <- "No significant change"
          TrendReason <- paste0("Outlier found")
        } else {
          RecentTrend <- ifelse(beta < 0, "decreasing", "increasing")
          TrendReason <- "No outliers and x2 > 9.5495"
        }

      } else {
        RecentTrend <- "No significant change"
        TrendReason <- "x2 < 9.5495"
      }

      df %>%
        mutate(
          RecentTrend = case_when(row_number() == n() ~ RecentTrend, TRUE ~ NA_character_),
          TrendReason = case_when(row_number() == n() ~ TrendReason, TRUE ~ NA_character_)
        )
    })

    } else if (value_type %in% c('other','dsr')){



      if (missing(lower_cl) | missing(upper_cl) ) {
        stop(paste('function assign_trend_markers requires "lower_cl" and "upper_cl" when value_type is other or "dsr"'))
      }

      if (is_quosure(value)){

        if (quo_is_null(value)) {
          stop(paste('function assign_trend_markers requires value when value_type is "other" or "dsr"'))
        }

        value <- as_label(value)


      }


      if (value_type=='dsr' && denominator_transform == FALSE){

        stop("denominator needs to be transformed for dsr")
      }



      lower_cl <- as_label(enquo(lower_cl))
      upper_cl <- as_label(enquo(upper_cl))


        df_results <- map(df_list, function(df){

          if (nrow(df) < 5) {

              df <- df %>%
                mutate(
                  RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
                  TrendReason = case_when(row_number() == n() ~ "Not enough data points", TRUE ~ NA_character_)
                )

              return(df)
          }

          df %>%
            slice_max(order_by = .data[[t]], n = 5, with_ties = FALSE) %>%
            arrange(.data[[t]])

          values <- df[[value]] / multiplier
          denominator_value <- df[[denominator]]
          numerator_value <- df[[numerator]]
          time_value <- df[[t]]
          upper_cl_value <- df[[upper_cl]]
          lower_cl_value <- df[[lower_cl]]

          if (any(is.na(values))) {


            df <- df %>%
              mutate(
                RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
                TrendReason = case_when(row_number() == n() ~ "Missing values", TRUE ~ NA_character_)
              )

            return(df)
          }

          time_difference<- diff(time_value)

          if(!all(time_difference==time_difference[1])){

            df <- df %>%
              mutate(
                RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
                TrendReason = case_when(row_number() == n() ~ "time points not equally spaced", TRUE ~ NA_character_)
              )

            return(df)
          }

          if(any(is.na(lower_cl_value)) | any(is.na(upper_cl_value)) ){

            df <- df %>%
              mutate(
                RecentTrend = case_when(row_number() == n() ~ "Cannot be calculated", TRUE ~ NA_character_),
                TrendReason = case_when(row_number() == n() ~ "missing at least one confidence interval value", TRUE ~ NA_character_)
              )

            return(df)
          }


          beta <- calculate_trend_direction(value = values,
                                            t = time_value,
                                            value_type= value_type,
                                            lower_cl = lower_cl_value,
                                            upper_cl = upper_cl_value)



          test_statistic <- calculate_trend_weighted_regression(value = values, t= time_value,
                                                               lower_cl = lower_cl_value,
                                                               upper_cl = upper_cl_value)


          if (test_statistic > 9.5495) {

            opposite_found <- FALSE

            for (i in 1:length(values)) {

              beta_sub <- calculate_trend_direction(value = values[-i],
                                                    t = time_value[-i],
                                                    value_type = value_type,
                                                    lower_cl = lower_cl_value[-i],
                                                    upper_cl = upper_cl_value[-i])
              print(beta_sub)
              if (sign(beta_sub) != sign(beta)) {

                opposite_found <- TRUE

                break
              }
            }

            if (opposite_found) {

              RecentTrend <- "No significant change"
              TrendReason <- paste0("Outlier found")

            } else {

              RecentTrend <- ifelse(beta < 0, "decreasing", "increasing")
              TrendReason <- "No outliers and x2 > 9.5495"
            }

          } else {

            RecentTrend <- "No significant change"
            TrendReason <- "x2 < 9.5495"
          }

          df %>%
            mutate(
              RecentTrend = case_when(row_number() == n() ~ RecentTrend, TRUE ~ NA_character_),
              TrendReason = case_when(row_number() == n() ~ TrendReason, TRUE ~ NA_character_)
            )
        })

    }

    results <- bind_rows(df_results)


    return(results)
}
