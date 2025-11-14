# -------------------------------------------------------------------------------------------------
#' dsr_inner
#'
#' Generates dsrs. All arguments are passed from the calculate_dsr wrapper
#' function with the exception of those defined below
#'
#' @param rtn_nonindependent_vardsr bool, keep default (FALSE) for independent
#'   events. For non-independent events, set to TRUE on the first iteration
#'   through the function to output the variance to pass in to the second
#'   iteration.
#' @param use_nonindependent_vardsr bool, keep default (FALSE) for independent
#'   events. For non-independent events, pass TRUE to use the custom_vardsr
#'   field for CI calculation in second run of function.
#'
#' @inheritParams calculate_dsr
#'
#' @return When rtn_nonindependent_vardsr = FALSE, returns a data frame of ID
#'   variables plus the dsr value, confidence limit & metadata fields as
#'   specified by the type argument. For non-independent events first iteration
#'   with rtn_nonindependent_vardsr = TRUE, returns just the ID variables and
#'   the dsr variance to be passed into the second iteration.
#'
#' @section Notes: This is an internal package function that is called by
#'   calculate_dsr.  It will run through once when events are independent. When
#'   events are non-independent, it will run through twice, calculating the
#'   variance for event frequencies on the first iteration and passing this
#'   value in to the function on the second iteration.
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

dsr_inner <- function(data,
                      x,
                      n,
                      stdpop,
                      type,
                      confidence,
                      multiplier,
                      rtn_nonindependent_vardsr = FALSE,
                      use_nonindependent_vardsr = FALSE) {

  # validate arguments specific to dsr_inner function
  if (isTRUE(rtn_nonindependent_vardsr) &&
      ("custom_vardsr" %in% names(data) || isTRUE(use_nonindependent_vardsr))) {
    stop("cannot get nonindependent vardsr and use nonindependent vardsr in the same execution")
  }

  # scale and extract confidence values
  confidence[confidence >= 90] <- confidence[confidence >= 90] / 100
  conf1 <- confidence[1]
  conf2 <- confidence[2]

  # create dummy custom_vardsr column when not in use to prevent errors evaluating vardsr code
  if (!use_nonindependent_vardsr) {
    method = "Dobson"
    data <- data %>%
      mutate(custom_vardsr = NA_real_)
  } else {
    method = "Dobson, with confidence adjusted for non-independent events"
  }

  # calculate DSR and vardsr
  dsrs <- data %>%
    mutate(
      wt_rate = na.zero(.data$x) *  .data$stdpop / .data$n,
      sq_rate = na.zero(.data$x) * (.data$stdpop / (.data$n))^2,
    ) %>%
    summarise(
      total_count = sum(.data$x, na.rm = TRUE),
      total_pop = sum(.data$n),
      value = sum(.data$wt_rate) / sum(.data$stdpop) * multiplier,
      vardsr = case_when(
        isTRUE(use_nonindependent_vardsr) ~ unique(.data$custom_vardsr),
        .default = 1 / sum(.data$stdpop)^2 * sum(.data$sq_rate)
      ),
      .groups = "keep"
    )


  if (!rtn_nonindependent_vardsr) {
    # Calculate CIs
    dsrs <- dsrs |>
      ungroup() |>
      mutate(
        lowercl = .data$value + sqrt(.data$vardsr / .data$total_count) *
          (byars_lower(.data$total_count, conf1) - .data$total_count) *
          multiplier,
        uppercl = .data$value + sqrt(.data$vardsr / .data$total_count) *
          (byars_upper(.data$total_count, conf1) - .data$total_count) *
          multiplier,
        lower99_8cl = .data$value + sqrt(.data$vardsr / .data$total_count) *
          (byars_lower(.data$total_count, 0.998) - .data$total_count) *
          multiplier,
        upper99_8cl =  .data$value + sqrt(.data$vardsr / .data$total_count) *
          (byars_upper(.data$total_count, 0.998) - .data$total_count) *
          multiplier
      ) %>%
      mutate(
        confidence = paste0(confidence * 100, "%", collapse = ", "),
        statistic = paste("dsr per",format(multiplier, scientific = FALSE)),
        method = method
      )


    # rename or drop confidence limits depending whether 1 or 2 CIs requested
    if (!is.na(conf2)) {
      names(dsrs)[names(dsrs) == "lowercl"] <- "lower95_0cl"
      names(dsrs)[names(dsrs) == "uppercl"] <- "upper95_0cl"
    } else {
      dsrs <- dsrs %>%
        select(!c("lower99_8cl", "upper99_8cl"))
    }


    # remove DSR calculation for total counts < 10
    dsrs <- dsrs %>%
      mutate(
        across(c("value", starts_with("upper"), starts_with("lower")),
               function(x) if_else(.data$total_count < 10, NA_real_, x)),
        statistic = if_else(
          .data$total_count < 10,
          "dsr NA for total count < 10",
          .data$statistic
        )
      )
  }

  # drop fields not required based on values of nonindependent_breakdowns and type arguments
  if (rtn_nonindependent_vardsr) {
    dsrs <- dsrs %>%
      select(group_cols(), "vardsr")
  } else if (type == "lower") {
    dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", "value", starts_with("upper"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "upper") {
    dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", "value", starts_with("lower"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "value") {
    dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", starts_with("lower"), starts_with("upper"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "standard") {
    dsrs <- dsrs %>%
      select(!c("vardsr", "confidence", "statistic", "method"))
  } else if (type == "full") {
    dsrs <- dsrs %>%
      select(!c("vardsr"))
  }

  return(dsrs)

}

# -------------------------------------------------------------------------------------------------
#' Calculate Directly Standardised Rates using calculate_dsr
#'
#' Calculates directly standardised rates with confidence limits using Byar's
#' method (1) with Dobson method adjustment (2) including option to further
#' adjust confidence limits for non-independent events (3).
#'
#' @param data data frame containing the data to be standardised, pre-grouped if
#'   multiple DSRs required; unquoted string; no default
#' @param x field name from data containing the observed number of events for
#'   each standardisation category (eg ageband) within each grouping set (eg
#'   area); unquoted string; no default
#' @param n field name from data containing the populations for each
#'   standardisation category (eg ageband) within each grouping set (eg area);
#'   unquoted string; no default
#' @param stdpop field name from data containing the standard populations for
#'   each age band; unquoted string; no default
#' @param type defines the data and metadata columns to be included in output;
#'   can be "value", "lower", "upper", "standard" (for all data) or "full" (for
#'   all data and metadata); quoted string; default = "full"
#' @param confidence the required level of confidence expressed as a number
#'   between 0.9 and 1 or a number between 90 and 100 or can be a vector of 0.95
#'   and 0.998, for example, to output both 95 percent and 99.8 percent percent
#'   CIs; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000
#'   = rate per 100,000); numeric; default 100,000
#' @param independent_events whether events are independent. Set to TRUE for
#'   independent events. When set to FALSE an adjustment is made to the
#'   confidence intervals - to do this, the dataset provided must include event
#'   frequency breakdowns and column x is redefined as the number of unique
#'   individuals who experienced each frequency of event, rather than the total
#'   number of events.
#' @param eventfreq field name from data containing the event frequencies. Only
#'   required when independent_events = FALSE; unquoted string; default NULL
#' @param ageband field name from data containing the age bands for
#'   standardisation. Only required when independent_events = FALSE; unquoted
#'   string; default NULL
#'
#' @return When type = "full", returns a tibble of total counts, total
#'   populations, directly standardised rates, lower confidence limits, upper
#'   confidence limits, confidence level, statistic and method for each grouping
#'   set. Use the type argument to limit the columns output.
#'
#' @importFrom rlang sym quo_name
#' @import dplyr
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(
#'   indicatorid = rep(c(1234, 5678, 91011, 121314), each = 19 * 2 * 5),
#'   year = rep(2006:2010, each = 19 * 2),
#'   sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'   ageband = rep(c(0,5,10,15,20,25,30,35,40,45,
#'                   50,55,60,65,70,75,80,85,90), times = 10),
#'   obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'   pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE),
#'   esp2013 = rep(esp2013, 40)
#' )
#'
#' ## Example 1 - Default execution
#' df %>%
#'   group_by(indicatorid, year, sex) %>%
#'   calculate_dsr(obs, pop, stdpop = esp2013)
#'
#' ## Example 2 - Calculate both 95% and 99.8% CIs in single execution
#' df %>%
#'   group_by(indicatorid, year, sex) %>%
#'   calculate_dsr(obs, pop, stdpop = esp2013, confidence = c(0.95, 0.998))
#'
#' ## Example 3 - Drop metadata columns from the output
#' df %>%
#'   group_by(indicatorid, year, sex) %>%
#'   calculate_dsr(obs, pop, stdpop = esp2013, type = "standard")
#'
#' ## Example 4 - Calculate DSRs for non-independent events
#'
#' library(tidyr)
#'
#' # For non-independent events the input data frame must breakdown events into
#' # counts of unique individuals by event frequency. The code chunk below
#' # creates a dummy data frame in this required format. Note that assignment of
#' # 10%, 20% and 70% of events to each event frequency is purely to create a
#' # data frame in the required format whilst retaining the same total event and
#' # population distributions by group and age band as example 1 to allow
#' # comparison of the outputs.
#'
#' df_freq <- df %>%
#'   mutate(
#'     f3 = floor((obs * 0.1)/3),         # 10 % of events in individuals with 3 events
#'     f2 = floor((obs * 0.2)/2),         # 20 % of events in individuals with 2 events
#'     f1 = (obs - (3 * f3) - (2 * f2))   # 70% of events in individuals with 1 event
#'   ) %>%
#'   select(!"obs") %>%
#'   pivot_longer(
#'     cols = c("f1", "f2", "f3"),
#'     names_to = "eventfrequency",
#'     values_to = "uniqueindividuals",
#'     names_prefix = "f"
#'   ) %>%
#'   mutate(eventfrequency = as.integer(eventfrequency))
#'
#' # Calculate the dsrs - notice that output DSR values match those in
#' # example 1 but the confidence intervals are wider
#'
#' df_freq %>%
#'   group_by(indicatorid, year, sex) %>%
#'   calculate_dsr(
#'     x = uniqueindividuals,
#'     n = pop,
#'     stdpop = esp2013,
#'     independent_events = FALSE,
#'     eventfreq = eventfrequency,
#'     ageband = ageband
#'   )
#'
#'
#' @section Notes: For total counts >= 10 Byar's method (1) is applied using
#'   the internal byars_lower and byars_upper functions. When the total count is
#'   < 10 DSRs are not reliable and will therefore be suppressed in the output.
#'
#' @references (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'   volume II: The design and analysis of cohort studies. Lyon: International
#'   Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr (2)
#'   Dobson A et al. Confidence intervals for weighted sums of Poisson
#'   parameters. Stat Med 1991;10:457-62. \cr \cr (3) See the DSR chapter of the
#'   [Fingertips Public Health Technical Guidance](https://fingertips.phe.org.uk/profile/guidance/supporting-information/PH-methods/)
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

calculate_dsr <- function(data,
                          x,
                          n,
                          stdpop = NULL,
                          type = "full",
                          confidence = 0.95,
                          multiplier = 100000,
                          independent_events = TRUE,
                          eventfreq = NULL,
                          ageband = NULL) {


  # Validate arguments ---------------------------------------------------------

   # check required arguments present
  if (missing(data) | missing(x) | missing(n) | missing(stdpop)) {
    stop("function calculate_dsr requires at least 4 arguments: data, x, n, stdpop")
  }

  # check columns exist in data frame

  if (!is.data.frame(data)) {
    stop("data must be a data frame object")
  }

  if (!deparse(substitute(x)) %in% colnames(data)) {
    stop("x is not a field name from data")
  }

  if (!deparse(substitute(n)) %in% colnames(data)) {
    stop("n is not a field name from data")
  }

  if (!deparse(substitute(stdpop)) %in% colnames(data)) {
    stop("stdpop is not a field name from data")
  }


  # hard-code field names
  data <- data %>%
    rename(
      x = {{ x }},
      n = {{ n }},
      stdpop = {{ stdpop }}
    )

  if (!is.numeric(data$x)) {
    stop("field x must be numeric")
  } else if (!is.numeric(data$n)) {
    stop("field n must be numeric")
  } else if (!is.numeric(data$stdpop)) {
    stop("field stdpop must be numeric")
  } else if (anyNA(data$n)) {
    stop("field n cannot have missing values")
 } else if (anyNA(data$stdpop)) {
    stop("field stdpop cannot have missing values")
  }  else if (any(pull(data, x) < 0, na.rm = TRUE)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, n) <= 0)) {
    stop("denominators must all be greater than zero")
  } else if (any(pull(data, stdpop) < 0)) {
    stop("stdpop must all be greater than or equal to zero")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
    stop("type must be one of value, lower, upper, standard or full")
  } else if (!is.numeric(confidence)) {
    stop("confidence must be numeric")
  } else if (length(confidence) > 2) {
    stop("a maximum of two confidence levels can be provided")
  } else if (length(confidence) == 2) {
    if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
      stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
    }
  } else if ((confidence < 0.9) | (confidence > 1 & confidence < 90) |
             (confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!is.numeric(multiplier)) {
    stop("multiplier must be numeric")
  }  else if (multiplier <= 0) {
    stop("multiplier must be greater than 0")
  } else if (!rlang::is_bool(independent_events)) {
    stop("independent_events must be TRUE or FALSE")
  }

  if (!independent_events) {
    # check that eventfrequency & ageband columns are specified & exist
    if (missing(eventfreq)) {
      stop(paste0("function calculate_dsr requires an eventfreq column ",
                  "to be specified when independent_events is FALSE"))
    } else if (!deparse(substitute(eventfreq)) %in% colnames(data)) {
      stop("eventfreq is not a field name from data")
    } else if (!is.numeric(data[[deparse(substitute(eventfreq))]])) {
      stop("eventfreq field must be numeric")
    } else if (anyNA(data[[deparse(substitute(eventfreq))]])) {
      stop("eventfreq field must not have any missing values")
    }

    if (missing(ageband)) {
      stop(paste0("function calculate_dsr requires an ageband column ",
                  "to be specified when independent_events is FALSE"))
    } else if (!deparse(substitute(ageband)) %in% colnames(data)) {
      stop("ageband is not a field name from data")
    } else if (anyNA(data[[deparse(substitute(ageband))]])) {
      stop("ageband field must not have any missing values")
    }
  }


  # Calculate DSR and CIs ------------------------------------------------------

  if (independent_events) {
    ## Perform dsr using CI calculation for independent events ----

    dsrs <- dsr_inner(
      data       = data,
      x          = x,
      n          = n,
      stdpop     = stdpop,
      type       = type,
      confidence = confidence,
      multiplier = multiplier
    )

  } else {
    ## Perform dsr using CI calculation for non independent events ----

    # hard code eventfreq and ageband column names,
    # and make sure data grouped by eventfreq
    data <- data %>%
      rename(
        eventfreq = {{ eventfreq }},
        ageband = {{ ageband }}
      ) %>%
      group_by(eventfreq, .add = TRUE)


    # check grouping variables and remove eventfrequency for use later
    grps <- group_vars(data)[!group_vars(data) %in% "eventfreq"]

    check_groups <- data |>
      group_by(pick(all_of(c(grps, "ageband")))) |>
      summarise(
        num_n = n_distinct(.data$n),
        num_stdpop = n_distinct(.data$stdpop),
        .groups = "drop"
      ) |>
      filter(.data$num_n > 1 | .data$num_stdpop > 1)

    if (nrow(check_groups) > 0) {
      stop(
        paste0("There are rows with the same grouping variables and ageband",
               " but with different populations (n) or standard populations",
               "(stdpop)")
      )
    }


    # get vardsrs for each event frequency and sum up
    freq_var <- data %>%
      dsr_inner(
        x          = x,
        n          = n,
        stdpop     = stdpop,
        type       = type,
        confidence = confidence,
        multiplier = multiplier,
        rtn_nonindependent_vardsr = TRUE
      ) %>%
      mutate(freqvars = .data$vardsr * .data$eventfreq^2) %>%
      group_by(pick(all_of(grps))) %>%
      summarise(
        custom_vardsr = sum(.data$freqvars),
        .groups = "drop"
      )

    # summarise total events
    event_data <- data %>%
      mutate(events = .data$eventfreq * .data$x) %>%
      group_by(pick(all_of(c(grps, "ageband", "n", "stdpop")))) %>%
      summarise(
        x = sum(.data$events, na.rm = TRUE),
        .groups = "drop"
      )

    # calculate overall DSR passing in nonindependent variance
    dsrs <- event_data %>%
      left_join(freq_var, by = grps) %>%
      group_by(pick(all_of(grps))) %>%
      dsr_inner(
        x          = x,
        n          = n,
        stdpop     = stdpop,
        type       = type,
        confidence = confidence,
        multiplier = multiplier,
        use_nonindependent_vardsr = TRUE
      )
  }

  return(dsrs)

}
