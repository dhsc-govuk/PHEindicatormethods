# -------------------------------------------------------------------------------------------------
#' Calculate Indirectly standardised ratios using calculate_ISRatio
#'
#' Calculates standard mortality ratios (or indirectly standardised ratios) with
#' confidence limits using Byar's (1) or exact (2) CI method.
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if
#'   multiple ISRs required; unquoted string; no default
#' @param x field name from data containing the observed number of events for
#'   each standardisation category (eg ageband) within each grouping set (eg
#'   area). Alternatively, if not providing age breakdowns for observed events,
#'   field name from observed_totals containing the observed number of events
#'   within each grouping set ; unquoted string; no default
#' @param x_ref the observed number of events in the reference population for
#'   each standardisation category (eg age band); unquoted numeric vector or
#'   field name from data depending on value of refpoptype; no default
#' @param n_ref the reference population for each standardisation category (eg
#'   age band); unquoted numeric vector or field name from data depending on
#'   value of refpoptype; no default
#' @param refpoptype whether x_ref and n_ref have been specified as vectors or a
#'   field name from data; quoted string "field" or "vector"; default = "vector"
#' @param refvalue   the standardised reference ratio, numeric, default = 1
#' @param observed_totals data.frame containing total observed events for each
#'   group, if not provided with age-breakdowns in data. Must only contain the
#'   count field (x) plus grouping columns required to join to data using the
#'   same grouping column names; default = NULL
#'
#' @inheritParams calculate_dsr
#'
#' @import dplyr
#' @importFrom stats qchisq
#' @export
#'
#' @return When type = "full", returns a tibble of observed events, expected
#'   events, standardised mortality ratios, lower confidence limits, upper
#'   confidence limits, confidence level, statistic and method for each grouping
#'   set
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314), each = 19 * 2 * 5),
#'                  year = rep(2006:2010, each = 19 * 2),
#'                  sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'                  ageband = rep(c(0,5,10,15,20,25,30,35,40,45,
#'                                  50,55,60,65,70,75,80,85,90), times = 10),
#'                  obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#'
#' refdf <- data.frame(refcount = sample(200, 19, replace = TRUE),
#'                     refpop = sample(10000:20000, 19, replace = TRUE))
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     calculate_ISRatio(obs, pop, refdf$refcount, refdf$refpop, type="standard")
#'
#' ## OR
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     calculate_ISRatio(obs, pop, refdf$refcount, refdf$refpop, confidence=99.8, refvalue=100)
#'
#' ## Calculate ISR when observed totals aren't available with age-breakdowns
#' observed_totals <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314), each = 10),
#'                        year = rep(rep(2006:2010, each = 2),4),
#'                        sex = rep(rep(c("Male", "Female"), each = 1),20),
#'                        observed = sample(1500:2500, 40))
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     calculate_ISRatio(observed, pop, refdf$refcount, refdf$refpop,
#'     observed_totals = observed_totals)
#'
#' @section Notes: User MUST ensure that x, n, x_ref and n_ref vectors are all
#'   ordered by the same standardisation category values as records will be
#'   matched by position. \cr \cr For numerators >= 10 Byar's method (1) is
#'   applied using the internal byars_lower and byars_upper functions.  For
#'   small numerators Byar's method is less accurate and so an exact method (2)
#'   based on the Poisson distribution is used. \cr  \cr This function directly
#'   replaced phe_smr which was fully deprecated in package version 2.0.0 due to
#'   ambiguous naming
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr
#' (2) Armitage P, Berry G. Statistical methods in medical research (4th edn).
#'   Oxford: Blackwell; 2002.
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

calculate_ISRatio <- function(data, x, n, x_ref, n_ref, refpoptype = "vector",
                              type = "full", confidence = 0.95, refvalue = 1,
                              observed_totals = NULL) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)|missing(x_ref)|missing(n_ref)) {
    stop("function calculate_ISRatio requires at least 5 arguments: data, x, n, x_ref and n_ref")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }

  # check x is in data/observed_totals
  if (!is.null(observed_totals)) {
    if (!(deparse(substitute(x)) %in% colnames(observed_totals))) {
      stop("observed_totals is provided but x is not a field name in it")
    }
  } else {
    if (!(deparse(substitute(x)) %in% colnames(data))) {
      stop("x is not in data")
    }
  }

  # check ref pops are valid and append to data
  if (!(refpoptype %in% c("vector","field"))) {
    stop("valid values for refpoptype are vector and field")

  } else if (refpoptype == "vector") {
    if (pull(slice(select(ungroup(count(data)),"n"),1)) != length(x_ref)) {
      stop("x_ref length must equal number of rows in each group within data")
    } else if (pull(slice(select(ungroup(count(data)),"n"),1)) != length(n_ref)) {
      stop("n_ref length must equal number of rows in each group within data")
    }
    data <- mutate(data,xrefpop_calc = x_ref,
                   nrefpop_calc = n_ref)

  } else if (refpoptype == "field") {
    if (deparse(substitute(x_ref)) %in% colnames(data)) {
      if(deparse(substitute(n_ref)) %in% colnames(data)) {
        data <- mutate(data,xrefpop_calc = {{ x_ref }},
                       nrefpop_calc = {{ n_ref }})
      } else stop("n_ref is not a field name from data")
    } else stop("x_ref is not a field name from data")
  }


  # validate arguments
  if (is.null(observed_totals)) {
    if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
      stop("numerators must all be greater than or equal to zero")
    }
  } else {
    if (any(pull(observed_totals, {{ x }}) < 0, na.rm=TRUE)) {
      stop("numerators must all be greater than or equal to zero")
    }
  }

  if (any(pull(data, {{ n }}) < 0, na.rm = TRUE)) {
    stop("denominators must all be greater than or equal to zero")
  } else if (any(pull(data, {{ n }}) < 0, na.rm = TRUE)) {
    stop("denominators must all be greater than or equal to zero")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
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

  # Identify join columns if observed events provided as totals
  if (!is.null(observed_totals)) {
    observed_total_join_cols <- base::intersect(colnames(data),
                                                colnames(observed_totals))
  }


  # calculate isr and cis and populate metadata fields
  if (length(confidence) == 2) {

    # if two confidence levels requested
    conf1 <- confidence[1]
    conf2 <- confidence[2]

    # calculate isr and CIs

    if (!is.null(observed_totals)) {
      ISRatio <- data %>%
        mutate(exp_x = na.zero(.data$xrefpop_calc) / .data$nrefpop_calc * na.zero({{ n }})) %>%
        summarise(expected = sum(.data$exp_x),
                  .groups  = "keep") %>%
        left_join(observed_totals, by = observed_total_join_cols) %>%
        rename("observed" = {{ x }}) %>%
        select("observed", "expected")
    } else {
      ISRatio <- data %>%
        mutate(exp_x = na.zero(.data$xrefpop_calc) / .data$nrefpop_calc * na.zero({{ n }})) %>%
        summarise(observed  = sum({{ x }}, na.rm = TRUE),
                  expected  = sum(.data$exp_x),
                  .groups   = "keep")
    }

    ISRatio <- ISRatio %>%
      mutate(value     = .data$observed / .data$expected * refvalue,
             lower95_0cl = if_else(.data$observed < 10, qchisq((1-conf1)/2,2*.data$observed)/2/.data$expected * refvalue,
                                   byars_lower(.data$observed,conf1)/.data$expected * refvalue),
             upper95_0cl = if_else(.data$observed < 10, qchisq(conf1+(1-conf1)/2,2*.data$observed+2)/2/.data$expected * refvalue,
                                   byars_upper(.data$observed,conf1)/.data$expected * refvalue),
             lower99_8cl = if_else(.data$observed < 10, qchisq((1-conf2)/2,2*.data$observed)/2/.data$expected * refvalue,
                                   byars_lower(.data$observed,conf2)/.data$expected * refvalue),
             upper99_8cl = if_else(.data$observed < 10, qchisq(conf2+(1-conf2)/2,2*.data$observed+2)/2/.data$expected * refvalue,
                                   byars_upper(.data$observed,conf2)/.data$expected * refvalue),
             confidence  = paste(conf1 * 100, "%, ", conf2 * 100, "%", sep=""),
             statistic   = paste("indirectly standardised ratio x ",format(refvalue, scientific=F), sep=""),
             method      = if_else(.data$observed < 10, "Exact", "Byars"))

    # drop fields not required based on type argument
    if (type == "lower") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "value", "upper95_0cl", "upper99_8cl", "confidence", "statistic", "method"))
    } else if (type == "upper") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "value", "lower95_0cl", "lower99_8cl", "confidence", "statistic", "method"))
    } else if (type == "value") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "lower95_0cl", "upper95_0cl", "lower99_8cl", "upper99_8cl", "confidence", "statistic", "method"))
    } else if (type == "standard") {
      ISRatio <- ISRatio %>%
        select(!c("confidence", "statistic", "method"))
    }

  } else {

    # scale confidence level
    if (confidence[1] >= 90) {
      confidence <- confidence/100
    }

    # calculate ISR and a single CI
    if (!is.null(observed_totals)) {
      ISRatio <- data %>%
        mutate(exp_x = na.zero(.data$xrefpop_calc) / .data$nrefpop_calc * na.zero({{ n }})) %>%
        summarise(expected = sum(.data$exp_x),
                  .groups  = "keep") %>%
        left_join(observed_totals, by = observed_total_join_cols) %>%
        rename("observed" = {{ x }}) %>%
        select("observed", "expected")
    } else {
      ISRatio <- data %>%
        mutate(exp_x = na.zero(.data$xrefpop_calc) / .data$nrefpop_calc * na.zero({{ n }})) %>%
        summarise(observed = sum({{ x }}, na.rm = TRUE),
                  expected = sum(.data$exp_x),
                  .groups  = "keep")
    }

    ISRatio <- ISRatio %>%
      mutate(value      = .data$observed / .data$expected * refvalue,
             lowercl    = if_else(.data$observed < 10, qchisq((1-confidence)/2,2*.data$observed)/2/.data$expected * refvalue,
                                  byars_lower(.data$observed,confidence)/.data$expected * refvalue),
             uppercl    = if_else(.data$observed < 10, qchisq(confidence+(1-confidence)/2,2*.data$observed+2)/2/.data$expected * refvalue,
                                  byars_upper(.data$observed,confidence)/.data$expected * refvalue),
             confidence = paste(confidence * 100, "%", sep=""),
             statistic  = paste("indirectly standardised ratio x ",format(refvalue, scientific=F), sep=""),
             method     = if_else(.data$observed < 10, "Exact", "Byars"))

    # drop fields not required based on type argument
    if (type == "lower") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "value", "uppercl", "confidence", "statistic", "method"))
    } else if (type == "upper") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "value", "lowercl", "confidence", "statistic", "method"))
    } else if (type == "value") {
      ISRatio <- ISRatio %>%
        select(!c("observed", "expected", "lowercl", "uppercl", "confidence", "statistic", "method"))
    } else if (type == "standard") {
      ISRatio <- ISRatio %>%
        select(!c("confidence", "statistic", "method"))
    }

  }

  return(ISRatio)
}
