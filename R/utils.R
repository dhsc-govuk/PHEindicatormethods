# -------------------------------------------------------------------------------------------------
#' Convert NAs to zeros using na.zero
#'
#' converts NAs to zeros
#'
#' @param y input vector
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------
na.zero <- function (y) {
    y[is.na(y)] <- 0
    return(y)
}



# -------------------------------------------------------------------------------------------------
#' byars_lower
#'
#' Calculates the lower confidence limits for observed numbers of events using Byar's method (1).
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams calculate_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using Byar's method (1)
#'
#' @section Notes: This is an internal package function that is appropriately
#'   called by exported 'phe_' prefixed functions within the PHEindicatormethods
#'   package.  \cr \cr The internal byars_lower and byars_upper functions
#'   together return symmetric confidence intervals around counts, therefore for
#'   a specified confidence level, \eqn{\alpha}, the probability that, by
#'   chance, the lower limit returned will be above the true underlying value,
#'   is \eqn{\alpha}/2. If the confidence level is very close to 1 or the number
#'   of events is very small Byar's method is inaccurate and may return a
#'   negative number - in these cases an error is returned.
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's lower CI limit
byars_lower <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    # populate z
    z <- qnorm(confidence + (1-confidence)/2)

    # calculate
    byars_lower <- x*(1-1/(9*x)-z/(3*sqrt(x)))^3

    # set negative values to NA
    byars_lower[byars_lower < 0]  <- NA

    return(byars_lower)

}


# -------------------------------------------------------------------------------------------------
#' byars_upper
#'
#' Calculates the upper confidence limits for observed numbers of events using Byar's method (1).
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams calculate_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using Byar's method (1)
#'
#' @section Notes: This is an internal package function that is appropriately
#'   called by exported 'phe_' prefixed functions within the PHEindicatormethods
#'   package.  \cr \cr The internal byars_lower and byars_upper functions
#'   together return symmetric confidence intervals around counts, therefore for
#'   a specified confidence level, \eqn{\alpha}, the probability that, by
#'   chance, the upper limit returned will be below the true underlying value,
#'   is \eqn{\alpha}/2.
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    # populate z
    z <- qnorm(confidence + (1-confidence)/2)

    byars_upper <- (x+1)*(1-1/(9*(x+1))+z/(3*sqrt(x+1)))^3
    return(byars_upper)
}


# -------------------------------------------------------------------------------------------------
#' wilson_lower
#'
#' Calculates lower confidence limits for observed numbers of events using the Wilson Score method (1,2).
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#'
#' @inheritParams calculate_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using the Wilson Score method (1,2)
#'
#' @section Notes: This is an internal package function that is appropriately
#'   called by exported 'phe_' prefixed functions within the PHEindicatormethods
#'   package.  \cr \cr The internal wilson_lower and wilson_upper functions
#'   together return symmetric confidence intervals, therefore for a specified
#'   confidence level, \eqn{\alpha}, the probability that, by chance, the lower
#'   limit returned will be above the true underlying value, is
#'   \eqn{\alpha}/2.#'
#'
#' @references
#' (1) Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' (2) Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' @noRd
#'
# ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_lower <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    if (confidence == 1) {
        wilson_lower <- 0
    } else {
        # set z
        z <- qnorm(confidence+(1-confidence)/2)
        # calculate
        wilson_lower <- (2*x+z^2-z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)
    }

  return(wilson_lower)
}



# -------------------------------------------------------------------------------------------------
#' wilson_upper
#'
#' Calculates upper confidence limits for observed numbers of events using the Wilson Score method (1,2).
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#'
#' @inheritParams calculate_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using the Wilson Score method (1,2)
#'
#' @section Notes: This is an internal package function that is appropriately
#'   called by exported 'phe_' prefixed functions within the PHEindicatormethods
#'   package.  \cr \cr The internal wilson_lower and wilson_upper functions
#'   together return symmetric confidence intervals, therefore for a specified
#'   confidence level, \eqn{\alpha}, the probability that, by chance, the upper
#'   limit returned will be below the true underlying value, is
#'   \eqn{\alpha}/2.#'
#'
#' @references
#' (1) Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' (2) Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' @noRd
#'
# ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_upper <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    if (confidence == 1) {
        wilson_upper <- 1
    } else {
        # set z
        z <- qnorm(confidence+(1-confidence)/2)
        # calculate
        wilson_upper <- (2*x+z^2+z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)
    }

    return(wilson_upper)
}

# -------------------------------------------------------------------------------------------------
#' FindXValues
#'
#' Calculates mid-points of cumulative population for each quantile
#'
#' @param xvals field name in input dataset that contains the quantile
#'        populations; unquoted string; no default
#' @param no_quantiles number of quantiles supplied in dataset for SII;
#'        integer; no default
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

FindXValues <- function(xvals, no_quantiles){

  # Create blank table with same number of rows as quantiles
  df <- data.frame(prop = numeric(no_quantiles),
                   cumprop = numeric(no_quantiles),
                   output = numeric(no_quantiles))

  # Calculates each group's population as proportion of total population
  df$prop <-xvals/sum(xvals, na.rm=TRUE)

  # Calculate cumulative proportion for subsequent groups
  df$cumprop = cumsum(df$prop)

  # lag calculates difference between the two proportions
  # "output" is the calculated mid-point of each proportion segment
  df$output = ifelse(is.na(lag(df$cumprop,1)), #(the lag function will produce NA for first item)
                     df$prop/2,
                     df$prop/2 + lag(df$cumprop,1))

  # Return output field
  FindXValues <- df$output

}

# -------------------------------------------------------------------------------------------------
#' SimulationFunc
#'
#' Function to simulate SII and RII range through random sampling of the indicator value
#' for each quantile, based on the associated mean and standard error
#'
#' @return returns lower and upper SII and RII confidence limits according to user
#' specified confidence
#'
#' @param data data.frame containing the data to calculate SII and RII
#'   confidence limits from; unquoted string; no default
#' @param value field name within data that contains the indicator value; unquoted
#'        string; no default
#' @param value_type indicates the indicator type (1 = rate, 2 = proportion, 0 = other);
#'        integer; default 0
#' @param se field name within data that contains the standard error of the indicator
#'        value; unquoted string; no default
#' @param repeats number of random samples to perform to return confidence interval of SII;
#'        numeric; default 100,000
#' @param confidence confidence level used to calculate the lower and upper confidence limits of SII;
#'        numeric between 0.5 and 0.9999 or 50 and 99.99; default 0.95
#' @param multiplier factor to multiply the SII and SII confidence limits by (e.g. set to 100 to return
#'        prevalences on a percentage scale between 0 and 100). If the multiplier is negative, the
#'        inverse of the RII is taken to account for the change in polarity; numeric; default 1;
#' @param sqrt_a field name within dataset containing square root of a values;
#'        unquoted string; no default
#' @param b_sqrt_a field name within dataset containing square root of a values multiplied
#'        by b values;unquoted string; no default
#' @param rii option to return the Relative Index of Inequality (RII) with associated confidence limits
#'        as well as the SII; logical; default FALSE
#' @param reliability_stat option to carry out the SII confidence interval simulation 10 times instead
#'        of once and return the Mean Average Difference between the first and subsequent samples (as a
#'        measure of the amount of variation); logical; default FALSE
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

SimulationFunc <- function(data,
                           value,
                           value_type = 0,
                           se,
                           repeats = 100000,
                           confidence = 0.95,
                           multiplier = 1,
                           sqrt_a,
                           b_sqrt_a,
                           rii = FALSE,
                           transform = FALSE,
                           reliability_stat = FALSE) {

  # Use NSE on input fields - apply quotes
  value = enquo(value)
  se = enquo(se)
  sqrt_a = enquo(sqrt_a)
  b_sqrt_a = enquo(b_sqrt_a)

  # find critical upper value at given confidence
  confidence_value <- confidence + ((1 - confidence) / 2)

  # Set 10x no. of reps if reliability stats requested
  no_reps <- if (reliability_stat == TRUE) {
    10 * repeats
  } else {
    repeats
  }

  # Take random samples from a normal distribution with mean as the indicator value
  # sd. as the associated standard error. Store results in a
  # (no. of quantiles)x(no. of repeats) dimensional matrix.
  yvals <- matrix(stats::rnorm(no_reps*length(pull(data, !!value)), pull(data, !!value), pull(data, !!se)), ncol = no_reps)

  # Retransform y values for rates (1) and proportions (2)
  if (value_type == 1 & transform == FALSE) {
    yvals_transformed <- exp(yvals)
  } else if (value_type == 2 & transform == FALSE) {
    yvals_transformed <- exp(yvals)/(1 + exp(yvals))
  } else {
    yvals_transformed <- yvals
  }

  # Combine explanatory variables sqrta and bsqrta into a matrix
  # (This is the transpose of matrix X in the regression formula)
  m <- as.matrix(rbind(pull(data, !!sqrt_a), pull(data, !!b_sqrt_a)))

  # Calculate inverse of (m*transpose (m))*m to use in calculation
  # of least squares estimate of regression parameters
  # Ref: https://onlinecourses.science.psu.edu/stat501/node/38
  invm_m <- solve((m) %*% t(m)) %*% m

  # Multiply transformed yvals matrix element-wise by sqrta - this weights the sampled
  # yvals by a measure of population
  final_yvals <- yvals_transformed*pull(data, !!sqrt_a)

  # Define function to matrix multiply invm_m by a vector
  matrix_mult <- function(x) {invm_m %*% x}

  # Carry out iterations - matrix multiply invm_m by each column of
  # population-weighted yvals in turn
  temp <- apply(final_yvals, 2, matrix_mult)

  # Extract the b_sqrt_a and sqrt_a parameters
  params_bsqrta <- temp[2,]
  params_sqrta <- temp[1,]

  # RII only calculations
  if (rii == TRUE) {

    # Calculate the RII
    RII_results <- (params_bsqrta + params_sqrta)/params_sqrta

    # Split simulated SII/RIIs into 10 samples if reliability stats requested
    if (reliability_stat == TRUE) {
      SII_results <- matrix(params_bsqrta, ncol = 10)
      RII_results <- matrix(RII_results, ncol = 10)
    } else {
      SII_results <- matrix(params_bsqrta, ncol = 1)
      RII_results <- matrix(RII_results, ncol = 1)
    }

    # Apply multiplicative factor to RII if transform = FALSE
     if (multiplier < 0 & transform == FALSE) {
       RII_results <- 1/RII_results
     } else {
       RII_results <- RII_results
     }

    # Order simulated RIIs from lowest to highest
    sortresults_RII <- apply(RII_results, 2, sort, decreasing = FALSE)

  } else {

    # Split simulated SII/RIIs into 10 samples if reliability stats requested
    if (reliability_stat == TRUE) {
      SII_results <- matrix(params_bsqrta, ncol = 10)
    } else {
      SII_results <- matrix(params_bsqrta, ncol = 1)
    }
  }

    # Apply multiplicative factor to SII if transform = FALSE
     if (transform == FALSE) {
       SII_results <- SII_results * multiplier
     } else {
       SII_results <- SII_results
     }

  # Order simulated SIIs from lowest to highest
  sortresults_SII <- apply(SII_results, 2, sort, decreasing = FALSE)

  # Extract specified percentiles (2.5 and 97.5 for 95% confidence) and return
  # as confidence limits

  # position of lower percentile
  pos_lower <- round(repeats * (1 - confidence_value), digits = 0)
  # position of upper percentile
  pos_upper <- round(repeats * confidence_value, digits = 0)

  # Combine position indexes for SII CLs
  pos <- rbind(pos_lower, pos_upper)
  colnames(pos) <- formatC(confidence * 100, format = "f", digits = 1)

  results_SII <- data.frame(sortresults_SII[pos, , drop = FALSE])
  names(results_SII) <- paste0("Rep_", seq_along(results_SII))
  rownames(results_SII) <- paste0(
    rep(c("sii_lower", "sii_upper"), length(confidence)),
    rep(paste0(gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)), "cl"), each = 2)
  )

  if (isFALSE(rii)) {
    results <- data.frame(t(results_SII))
  }

  if (isTRUE(rii)) {
    results_RII <- data.frame(sortresults_RII[pos, , drop = FALSE])
    names(results_RII) <- paste0("Rep_", seq_along(results_RII))
    rownames(results_RII) <- paste0(
      rep(c("rii_lower", "rii_upper"), length(confidence)),
      rep(paste0(gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)), "cl"), each = 2)
    )

  results <- data.frame(t(bind_rows(results_SII, results_RII)))
  }

  results

}


# ------------------------------------------------------------------------------
#' SII reliability stats
#' @param CI_data A nested dataframe containing the SII and RII CIs for each rep
#' @param confidence Confidence level used to calculate the lower and upper confidence limits of SII;
#'        numeric between 0.5 and 0.9999 or 50 and 99.99; default 0.95
#' @param rii Option to return the Relative Index of Inequality (RII) with
#'   associated confidence limits
#'
#' @return a data frame
#'
#' @noRd
# ------------------------------------------------------------------------------

calc_reliability <- function(CI_data,
                             confidence,
                             rii) {

  groups <- group_vars(CI_data)

  reliabity_stats <- CI_data %>%
    mutate(
      reliabity_stats_data = purrr::map(.data$CI_calcs, function(x){
        # Calculate mean average difference in SII and RII from first rep
        diffs_sample_original <- x |>
          mutate(across(everything(), function(y) {abs(y - y[1])})) |>
          slice(-1)

        map(confidence, function(conf) {
          conf_formatted <-
            gsub("\\.", "_", formatC(conf * 100, format = "f", digits = 1))

          if (isTRUE(rii)) {
            diffs_sample_original |>
              select(contains(conf_formatted)) |>
              summarise(
                "sii_mad{conf_formatted}" := mean(c_across(contains("sii"))),
                "rii_mad{conf_formatted}" := mean(c_across(contains("rii")))
              )
          } else {
            diffs_sample_original |>
              select(contains(conf_formatted)) |>
              summarise(
                "sii_mad{conf_formatted}" := mean(c_across(contains("sii")))
              )
          }

        }) |>
          bind_cols()

      }
      )
    ) |>
    select(all_of(groups), "reliabity_stats_data") |>
    unnest("reliabity_stats_data")

}

# ------------------------------------------------------------------------------
#' Poisson Function for funnel plots for ratios and rates
#'
#' @noRd
#'
# ------------------------------------------------------------------------------


poisson_cis <- function(z, x_a, x_b) {
  # none of the following can occur based on Funnels.R
  # if (any(z < 0, x_a < 0, x_b < 0, x_b < x_a, x_a %% 1 > 0, x_b %% 1 > 0))
  #   return(NA)

  q <- 1
  tot <- 0
  s <- 0
  k <- 0

  while (any(k <= z, q > tot * 1e-10)) {
    tot <- tot + q
    if (k >= x_a & k <= x_b) s <- s + q
    if (tot > 1e30) {
      s <- s / 1e30
      tot <- tot / 1e30
      q <- q / 1e30
    }

    k <- k + 1
    q <- q * z / k

  }
  return(s / tot)
}


# ------------------------------------------------------------------------------
#' Function for funnel plots for ratios and rates
#'
#' @noRd
#'
# ------------------------------------------------------------------------------

poisson_funnel <- function(obs, p, side) {
  # None of the following can occur based on Funnels.R
  # if (any(obs < 0, p < 0, p > 1, obs %% 1 != 0)) return(NA)

  v <- 0.5
  dv <- 0.5

  if (side == "low") {

    # this is in the Excel macro code, but obs can't be 0 based on Funnels.R
    # if (obs == 0) return(0)

    while (dv > 1e-7) {
      dv <- dv / 2

      if (poisson_cis((1 + obs) * v / (1 - v),
                      obs,
                      10000000000) > p) {
        v <- v - dv
      } else {
        v <- v + dv
      }
    }
  } else if (side == "high") {

    while (dv > 1e-7) {
      dv <- dv / 2
      if (poisson_cis((1 + obs) * v / (1 - v),
                      0,
                      obs) < p) {
        v <- v - dv
      } else {
        v <- v + dv
      }
    }

  }
  p <- (1 + obs) * v / (1 - v)
  return(p)
}

# ------------------------------------------------------------------------------
#' Function for funnel plots for rates and ratios
#'
#' @noRd
#' @importFrom stats qchisq
#'
# ------------------------------------------------------------------------------


funnel_ratio_significance <- function(obs, expected, p, side) {
  if (obs == 0 & side == "low") {
    test_statistic <- 0
  } else if (obs < 10) {
    if (side == "low") {
      degree_freedom <- 2 * obs
      lower_tail_setting <- FALSE
    } else if (side == "high") {
      degree_freedom <- 2 * obs + 2
      lower_tail_setting <- TRUE
    }

    test_statistic <- qchisq(p = 0.5 + p / 2,
                             df = degree_freedom,
                             lower.tail = lower_tail_setting) / 2
  } else {
    if (side == "low") {
      obs_adjusted <- obs
      test_statistic <- obs_adjusted * (1 - 1 / (9 * obs_adjusted) -
                                          qnorm(0.5 + p / 2) / 3 / sqrt(obs_adjusted))^3
    } else if (side == "high") {
      obs_adjusted <- obs + 1
      test_statistic <- obs_adjusted * (1 - 1 / (9 * obs_adjusted) +
                                          qnorm(0.5 + p / 2) / 3 / sqrt(obs_adjusted))^3
    }
  }

  test_statistic <- test_statistic / expected
  return(test_statistic)
}

#' Calculate the proportion funnel point value for a specific population based
#' on a population average value
#'
#' Returns a value equivalent to the higher/lower funnel plot point based on the
#' input population and probability
#'
#' @param p numeric (between 0 and 1); probability to calculate funnel plot
#'   point (will normally be either 0.975 or 0.999)
#' @param population numeric; the population for the area
#' @param average_proportion numeric; the average proportion for all the areas
#'   included in the funnel plot (the sum of the numerators divided by the sum
#'   of the denominators)
#' @param side string; "low" or "high" to determine which funnel to calculate
#' @param multiplier  numeric; the multiplier used to express the final values
#'   (eg 100 = percentage); default 100
#' @return returns a value equivalent to the specified funnel plot point for the
#'   input population
#'
#' @noRd
#'
#' @author Sebastian Fox, \email{sebastian.fox@@phe.gov.uk}
#'

sigma_adjustment <- function(p, population, average_proportion, side, multiplier) {
  first_part <- average_proportion * (population /
                                        qnorm(p)^2 + 1)

  adj <- sqrt((-8 * average_proportion * (population /
                                            qnorm(p)^2 + 1))^2 - 64 *
                (1 / qnorm(p)^2 + 1 / population) *
                average_proportion * (population *
                                        (average_proportion * (population /
                                                                 qnorm(p)^2 + 2) - 1) +
                                        qnorm(p)^2 *
                                        (average_proportion - 1)))

  last_part <- (1 / qnorm(p)^2 + 1 /
                  population)

  if (side == "low") {
    adj_return <- (first_part - adj / 8) / last_part
  } else if (side == "high") {
    adj_return <- (first_part + adj / 8) / last_part
  }
  adj_return <- (adj_return /
                   population) * multiplier
  return(adj_return)
}

# ------------------------------------------------------------------------------
#' Function to transform the denominator or numerator for trend markers
#'
#' If there are only indicator values and numerators are present the denominator needs to be calculated
#' If there are only indicator values and denominators are present the numerator needs to be calculated
#'
#' @param data a data.frame containing the data to transform the denominator; unquoted string; no default
#' @param numerator field name from data containing the observed numbers of cases in the sample meeting the required condition; unquoted string; no default
#' @param denominator field name from data containing the population(s) in the sample; unquoted string; no default
#' @param value field name within data that contains the indicator value; unquoted string; no default
#'
#' @noRd
#'
# ------------------------------------------------------------------------------



derive_proportion_element <- function(data, numerator, denominator, value){

  if (missing(denominator)){



  df <- data %>%
    mutate(denominator = .data[[numerator]] / .data[[value]])

  } else if (missing(numerator)) {

  df <- data %>%
    mutate(numerator = .data[[denominator]] * .data[[value]])

  }

  return(df)

}



# ------------------------------------------------------------------------------
#' function to test for trend significance and direction for proportions
#'
#' @param denominator field name from data containing the population(s) in the sample; unquoted string; no default
#' @param numerator field name from data containing the observed numbers of cases in the sample meeting the required condition; unquoted string; no default
#' @param year_col field name within the data that contains time period values; unquoted string; numeric
#' @param trend_direction_only option to calculate the trend direction or the significance. When TRUE, the trend direction will
#' be calculated; when FALSE the significance is calculated; logical; default FALSE
#'
#'
#' @noRd
#'
# ------------------------------------------------------------------------------


calculate_trend_logistic_regression <- function(denominator, numerator,
                                                year_col,
                                                trend_direction_only =FALSE){


  if (trend_direction_only == TRUE){

    value <- numerator / denominator


    N <- length(value)


    log_x <- log(value / (1 - value))
    sum_log_x <- sum(log_x)
    sum_log_x_t <- sum(log_x * year_col)
    sum_t <- sum(year_col)
    sum_log_x_sum_t <- sum_log_x * sum_t
    sum_t2 <- sum(year_col^2)
    t2_summed <- sum(year_col)^2


    beta_numerator <- (N * sum_log_x_t) - sum_log_x_sum_t
    beta_denominator <- (N * sum_t2) - t2_summed

    beta <- beta_numerator / beta_denominator

    return(beta)


  } else {

    denominator <-as.numeric(denominator)
    numerator <- as.numeric(numerator)
    year_col <- as.numeric(year_col)


    sum_d <- sum(denominator)
    sum_ct <- sum(numerator * year_col)
    sum_c <- sum(numerator)
    sum_dt <- sum(denominator * year_col)
    t_2 <- year_col^2
    sum_dt_2 <- sum(denominator * t_2)
    dt2 <- sum_dt^2

    x2_numerator <- sum_d * ((sum_d * sum_ct) - (sum_c * sum_dt))^2
    x2_denominator <- sum_c * (sum_d - sum_c)  * (sum_d * sum_dt_2 - dt2)

    x2 <- x2_numerator / x2_denominator

    return(x2)


  }

}


# ------------------------------------------------------------------------------
#' function to test for trend significance and trend direction for non-proportions
#'
#' @param value field name within data that contains the indicator value; unquoted string; no default
#' @param year_col field name within the data that contains time period values; unquoted string; numeric
#' @param lower_ci field name within data that contains  lower confidence limit of indicator value (to calculate standard error of indicator value).
#' lower_ci is not needed for "proportions". unquoted string; no default
#' @param upper_ci field name within data that contains  upper confidence limit of indicator value (to calculate standard error of indicator value).
#' upper_ci is not needed for "proportions". unquoted string; no default
#' @param confidence confidence level used to derive standard errors; numeric between 0.9 and 0.9999 or 90 and 99.99; default 0.95
#' @param trend_direction_only option to calculate the trend direction or the significance. When TRUE, the trend direction will
#' be calculated; when FALSE the significance is calculated; logical; default FALSE
#'
#' @importFrom stats qnorm
#'
#' @noRd
#'
# ------------------------------------------------------------------------------

calculate_trend_weighted_regression<- function(value,
                                               year_col,
                                               lower_cl= NULL,
                                               upper_cl = NULL,
                                               confidence = 0.95,
                                               trend_direction_only = FALSE){

  upper_cl <- as.numeric(upper_cl)
  lower_cl <- as.numeric(lower_cl)

  z_score_2 <- qnorm(1 - (1 - confidence) / 2 ) * 2


  sigma_2 <- ((upper_cl - lower_cl) / z_score_2)^2
  t_2 <- year_col^2
  sum_1_sigma_2 <- sum(1 / sigma_2)
  sum_valuet_sigma_2 <- sum((value * year_col) / sigma_2)
  sum_t_sigma_2 <- sum(year_col / sigma_2)
  sum_value_sigma_2 <- sum(value / sigma_2)
  sum_t2_sigma_2 <- sum(t_2 / sigma_2)

  beta_numerator <- (sum_1_sigma_2 * sum_valuet_sigma_2) - (sum_t_sigma_2 * sum_value_sigma_2)
  beta_denominator <- (sum_1_sigma_2 * sum_t2_sigma_2) - (sum_t_sigma_2)^2


  beta <- beta_numerator / beta_denominator

  if (trend_direction_only == TRUE){

    return(beta)

  } else{


  variance_beta <-sum_1_sigma_2 / ((sum_1_sigma_2 * sum_t2_sigma_2 - sum_t_sigma_2^2))


  x2<- (beta^2) / variance_beta

  return(x2)

  }


}

