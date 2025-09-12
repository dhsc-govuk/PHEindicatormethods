#' Assign Trend markers using assign_trend_markers
#'
#' @param data a data.frame containing the data to calculate the recent trends for; unquoted string; no default
#' @param value field name within data that contains the indicator value. value field is not needed for "proportion"; unquoted string; no default
#' @param denominator field name from data containing the population(s) in the sample; unquoted string; no default
#' @param numerator  field name from data containing the observed numbers of cases in the sample meeting the required condition; unquoted string; no default
#' @param year_col field name within the data that contains time period values; unquoted string; numeric
#' @param value_type indicates the indicator type (proportion, other, dsr); quoted string
#' @param lower_ci field name within data that contains 95 percent lower confidence limit of indicator value (to calculate standard error of indicator value).
#' lower_ci is not needed for "proportions". unquoted string; no default
#' @param upper_ci field name within data that contains 95 percent upper confidence limit of indicator value (to calculate standard error of indicator value).
#' upper_ci is not needed for "proportions". unquoted string; no default
#' @param confidence confidence level used to derive standard errors; numeric between 0.9 and 0.9999 or 90 and 99.99; default 0.95
#' @param multiplier the multiplier that the rate is normalised with (ie, per 100,000). Multiplier is not
#' needed for "proportion"; numeric; default = 1
#'
#' @return a tibble showing the recent trend based on the 5 most recent data points, with trend direction and the reason for the
#' trend for each subgroup of the inputted data.frame.
#'
#'
#' @import dplyr
#' @importFrom purrr map
#' @export
#'
#'
#'
#' @examples
#'
#' set.seed(5)
#'
#' df <- data.frame(
#'   time = rep(2021:2025, each = 2),
#'   Sex = rep(rep(c("Male", "Female")), 5),
#'   numerator = sample(50000:99900, 10, replace = TRUE),
#'   denominator = sample(100000:200000, 10, replace = TRUE)
#' )
#'
#' df %>%
#'   group_by(Sex) %>%
#'   assign_trend_markers(
#'     denominator = denominator,
#'     numerator = numerator,
#'     year_col = time,
#'     value_type = "proportion"
#'   )
#'
#' df <- data.frame(
#'   time = rep(2021:2025),
#'   numerator = sample(10000:99900, 5, replace = TRUE),
#'   denominator = sample(100000:200000, 5, replace = TRUE)
#' )
#'
#' assign_trend_markers(
#'   data = df,
#'   denominator = denominator,
#'   numerator = numerator,
#'   year_col = time,
#'   value_type = "proportion"
#' )
#'
#' df <- data.frame(Year= c(1,2,3,4,5),
#' Count = c(1367,1332, 1199, 1023, 994),
#' Denominator = c(44701, 44064, 42811, 41527,40299),
#' Value = c(30.6, 30.2, 28, 24.6, 24.67),
#' LL = c(28.98, 28.63,26.44,23.15, 23.16),
#' UL = c(32.25, 31.9,29.64, 26.19,26.25))
#'
#' assign_trend_markers(
#'   data=df,
#'   value = Value,
#'   denominator = Denominator,
#'   numerator = Count,
#'   year_col = Year,
#'   value_type = "other",
#'   multiplier = 1,
#'   confidence = 95,
#'   lower_cl = LL,
#'   upper_cl = UL)
#'
#' @section Notes: Tests must only be applied when the following conditions apply:
#'
#' There must be at least five points in the series
#' Time periods must be independent of one another: not overlapping, or rolling time periods
#' Data must be for years: monthly or quarterly data are not to be tested
#' Indicator type is one of the following: rate (crude, directly standardised or indirectly standardised), proportion (crude or indirectly standardised), ratio (crude, indirectly standardised or rate ratio), life expectancy, slope index of inequality, excess risk, mean, median, gap
#' There must be no breaks in the series (the method described here can be applied to time series with missing values or varying time intervals, but this is not currently implemented in Fingertips)
#' For indicator types other than proportions (crude or indirectly standardised) the indicator values must have confidence intervals.
#'
#' For proportions there must be a numerator and denominator present. If there are only indicator values and numerators are present the denominator will be calculated or
#' if there are only indicator values and denominators are present the numerator will be calculated
#'
#' @references (1)
#'   Office for Health Improvement and Disparities. Fingertips trend markers. Department of Health and Social Care; 2025
#'   [Fingertips Public Health Technical Guidance](https://fingertips.phe.org.uk/static-reports/public-health-technical-guidance/Presentation/Trend_markers.html)
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
                                 value_type,
                                 lower_cl,
                                 upper_cl,
                                 confidence = 0.95,
                                 multiplier = 1) {
  if (missing(data)) {
    stop("function assign_trend_markers requires the arguments: data")
  }

  if (missing(year_col)) {
    stop("function assign_trend_markers requires the year_col field")
  }

  if (missing(value_type) || !(value_type %in% c("proportion", "other"))) {
    stop('function assign_trend_markers requires the value_type to be either "proportion" or "other"')
  }

  if (value_type == "other") {
    if (missing(lower_cl) || missing(upper_cl)) {
      stop(paste('function assign_trend_markers requires "lower_cl" and "upper_cl" when value_type is other'))
    } else {
      if (deparse(substitute(lower_cl)) %in% colnames(data)) {
        lower_cl <- deparse(substitute(lower_cl))
        if (!is.numeric(data[[lower_cl]])) {
          stop("field lower_cl must be numeric")
        }
      } else {
        stop("lower_cl is not a field name from data")
      }

      if (deparse(substitute(upper_cl)) %in% colnames(data)) {
        upper_cl <- deparse(substitute(upper_cl))

        if (!is.numeric(data[[upper_cl]])) {
          stop("field upper_cl must be numeric")
        }
      } else {
        stop("upper_cl is not a field name from data")
      }
    }

    if (missing(value)) {
      stop(paste('function assign_trend_markers requires value when value_type is "other"'))
    } else {
      if (deparse(substitute(value)) %in% colnames(data)) {
        value <- deparse(substitute(value))

        if (!is.numeric(data[[value]])) {
          stop("field value must be numeric")
        }
      } else {
        stop("value is not a field name from data")
      }
    }



    if ((confidence < 0.9) || (confidence > 1 && confidence < 90) || (confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (confidence >= 90) {
      confidence <- confidence / 100
    }
  }

  if (!missing(denominator)) {
    if (deparse(substitute(denominator)) %in% colnames(data)) {
      denominator <- deparse(substitute(denominator))

      if (!is.numeric(data[[denominator]])) {
        stop("field denominator must be numeric")
      }
    } else {
      stop("denominator is not a field name from data")
    }
  } else {
    denominator <- NULL
  }


  if (!missing(numerator)) {
    if (deparse(substitute(numerator)) %in% colnames(data)) {
      numerator <- deparse(substitute(numerator))

      if (!is.numeric(data[[numerator]])) {
        stop("field numerator must be numeric")
      }
    } else {
      stop("numerator is not a field name from data")
    }
  } else {
    numerator <- NULL
  }

  if (value_type == "proportion") {
    if (is.null(numerator) && is.null(denominator)) {
      stop(paste('function assign_trend_markers requires at least a numerator or denominator when value_type is "proportion"'))
    } else if (is.null(numerator) || is.null(denominator)) {
      if (missing(value)) {
        stop(paste("function assign_trend_markers requires value when either numerator or denominator is missing"))
      } else {
        if (deparse(substitute(value)) %in% colnames(data)) {
          value <- deparse(substitute(value))

          if (!is.numeric(data[[value]])) {
            stop("field value must be numeric")
          }
        } else {
          stop("value is not a field name from data")
        }
      }
    }


    if (is.null(numerator)) {
      data <- derive_proportion_element(data = data, denominator = denominator, value = value)

      numerator <- "numerator"
    } else if (is.null(denominator)) {
      data <- derive_proportion_element(data = data, numerator = numerator, value = value)

      denominator <- "denominator"
    }
  }

  if (deparse(substitute(year_col)) %in% colnames(data)) {
    year_col <- deparse(substitute(year_col))
  } else {
    stop("year_col is not a field name from data")
  }

  if (!is.numeric(data[[year_col]])) {
    stop("field year_col must be numeric")
  }



  df_list <- if (is_grouped_df(data)) {
    group_split(data)
  } else {
    list(data)
  }

  df_count <- length(df_list)


  df_results <- map(df_list, function(df) {
    df <- df %>%
      arrange(.data[[year_col]])


    n_rows <- nrow(df)

    df$recent_trend <- NA_character_
    df$recent_trend_reason <- NA_character_


    if (any(duplicated(df[[year_col]]))) {
      if (df_count > 1) {
        df$recent_trend[n_rows] <- "Cannot be calculated"
        df$recent_trend_reason[n_rows] <- "Duplicate years in group"

        return(df)
      } else {
        stop(paste0(year_col, " must contain unique values"))
      }
    }

    df_sub <- df[(n_rows - 4):n_rows, ]


    if (n_rows < 5) {
      df$recent_trend[n_rows] <- "Cannot be calculated"
      df$recent_trend_reason[n_rows] <- "Not enough data points"

      return(df)
    }

    if (value_type == "other") {
      values <- df_sub[[value]] / multiplier
      denominator_value <- df_sub[[denominator]]
      numerator_value <- df_sub[[numerator]]
      time_value <- df_sub[[year_col]]
      lower_cl_value <- df_sub[[lower_cl]]
      upper_cl_value <- df_sub[[upper_cl]]


      if (anyNA(values)) {
        df$recent_trend[n_rows] <- "Cannot be calculated"
        df$recent_trend_reason[n_rows] <- "Missing values"

        return(df)
      }


      if (anyNA(lower_cl_value) | anyNA(upper_cl_value)) {
        df$recent_trend[n_rows] <- "Cannot be calculated"
        df$recent_trend_reason[n_rows] <- "Missing at least one confidence interval value"

        return(df)
      }


      beta <- calculate_trend_weighted_regression(
        value = values, year_col = time_value,
        lower_cl = lower_cl_value,
        upper_cl = upper_cl_value,
        confidence = confidence,
        trend_calculation = "trend_direction"
      )


      test_statistic <- calculate_trend_weighted_regression(
        value = values, year_col = time_value,
        lower_cl = lower_cl_value,
        upper_cl = upper_cl_value,
        confidence = confidence,
        trend_calculation = "significance"
      )


      if (test_statistic > 9.5495) {
        opposite_found <- FALSE

        for (j in seq_along(time_value)) {
          beta_sub <- calculate_trend_weighted_regression(
            value = values[-j], year_col = time_value[-j],
            lower_cl = lower_cl_value[-j],
            upper_cl = upper_cl_value[-j],
            confidence = confidence,
            trend_calculation = "trend_direction"
          )

          if (sign(beta_sub) != sign(beta)) {
            opposite_found <- TRUE

            break
          }
        }

        if (opposite_found) {
          df$recent_trend[n_rows] <- "No significant change"
          df$recent_trend_reason[n_rows] <- paste0("Outlier found")
        } else {
          df$recent_trend[n_rows] <- ifelse(beta < 0, "Decreasing", "Increasing")
          df$recent_trend_reason[n_rows] <- "No outliers and significant trend"
        }
      } else {
        df$recent_trend[n_rows] <- "No significant change"
        df$recent_trend_reason[n_rows] <- "No significant trend"
      }


      return(df)
    } else if (value_type == "proportion") {
      denominator_value <- df_sub[[denominator]]
      numerator_value <- df_sub[[numerator]]
      time_value <- df_sub[[year_col]]


      if (anyNA(denominator_value) | anyNA(numerator_value)) {
        df$recent_trend[n_rows] <- "Cannot be calculated"
        df$recent_trend_reason[n_rows] <- "Missing values"

        return(df)
      }

      beta <- calculate_trend_logistic_regression(
        denominator = denominator_value,
        numerator = numerator_value,
        year_col = time_value,
        trend_calculation = "trend_direction"
      )

      test_statistic <- calculate_trend_logistic_regression(
        denominator = denominator_value,
        numerator = numerator_value,
        year_col = time_value,
        trend_calculation = "significance"
      )

      if (test_statistic > 9.5495) {
        opposite_found <- FALSE

        for (j in seq_along(time_value)) {
          beta_sub <- calculate_trend_logistic_regression(
            denominator = denominator_value[-j],
            numerator = numerator_value[-j],
            year_col = time_value[-j],
            trend_calculation = "trend_direction"
          )

          if (sign(beta_sub) != sign(beta)) {
            opposite_found <- TRUE
            break
          }
        }

        if (opposite_found) {
          df$recent_trend[n_rows] <- "No significant change"
          df$recent_trend_reason[n_rows] <- paste0("Outlier found")
        } else {
          df$recent_trend[n_rows] <- ifelse(beta < 0, "Decreasing", "Increasing")
          df$recent_trend_reason[n_rows] <- "No outliers and significant trend"
        }
      } else {
        df$recent_trend[n_rows] <- "No significant change"
        df$recent_trend_reason[n_rows] <- "No significant trend"
      }


      return(df)
    }
  })

  results <- bind_rows(df_results)

  return(results)
}
