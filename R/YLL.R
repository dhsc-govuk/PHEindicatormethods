# -------------------------------------------------------------------------------------------------
#' Calculate years of life lost using calculate_yll
#'
#' Calculates years of life lost rates with confidence limits using Dobson & Byar's (YLL variation) method.
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if
#'   multiple YLL required; unquoted string; no default
#' @param x field name from data containing the observed number of events for
#'   each standardisation category (eg ageband) within each grouping set (eg
#'   area); unquoted string; no default
#' @param n field name from data containing the populations for each
#'   standardisation category (eg ageband) within each grouping set (eg area);
#'   unquoted string; no default
#'  @param yll field name from data containing  the years of life lost for
#'  each standardisation category (eg ageband) within each grouping set (eg area)
#'  unquoted string; no default
#' @param stdpop field name from data containing the standard populations for
#'   each age band; unquoted string; no default
#' @param type defines the data and metadata columns to be included in output;
#'   can be "value", "lower", "upper", "standard" (for all data) or "full" (for
#'   all data and metadata); quoted string; default = "full"
#' @param confidence the required level of confidence expressed as a number
#'   between 0.9 and 1 or a number between 90 and 100 or can be a vector of 0.95
#'   and 0.998, for example, to output both 95 percent and 99.8 percent percent CIs; numeric;
#'   default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000
#'   = rate per 100,000); numeric; default 100,000
#'
#' @return When type = "full", returns a tibble of total counts, total
#'   populations, directly standardised rates, lower confidence limits, upper
#'   confidence limits, confidence level, statistic and method for each grouping
#'   set
#'
#' @importFrom rlang sym quo_name
#' @import dplyr
#' @export
#'
#' @examples
#'
#' @section Notes: For total counts >= 10 Byar's method (1) is applied using
#'   the \code{\link{byars_lower}} and \code{\link{byars_upper}} functions.
#'   When the total count is < 10 YLL are not reliable and will therefore not
#'   be calculated.
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr
#' (2) Dobson A et al. Confidence intervals for weighted sums of Poisson parameters. Stat Med 1991;10:457-62.
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# define the YLL function using Dobson method
calculate_yll <- function(data,
                          x,
                          n,
                          yll = NULL,
                          stdpop = NULL,
                          type = "full",
                          confidence = 0.95,
                          multiplier = 100000) {

# Validate arguments ---------------------------------------------------------

  # check required arguments present
  if (missing(data) | missing(x) | missing(n) | missing(yll) | missing(stdpop)) {
    stop("function calculate_yll requires at least 5 arguments: data, x, n, yll, stdpop")
  }

  # check columns exist in data.frame

  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  if (!deparse(substitute(x)) %in% colnames(data)) {
    stop("x is not a field name from data")
  }

  if (!deparse(substitute(n)) %in% colnames(data)) {
    stop("n is not a field name from data")
  }

  if (!deparse(substitute(yll)) %in% colnames(data)) {
    stop("yll is not a field name from data")
  }

  if (!deparse(substitute(stdpop)) %in% colnames(data)) {
    stop("stdpop is not a field name from data")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }

  # hard-code field names
  data <- data %>%
    rename(
      x = {{ x }},
      n = {{ n }},
      yll = {{ yll }},
      stdpop = {{ stdpop }}
    )

  # validate arguments
  if (any(pull(data, x ) < 0, na.rm=TRUE)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, n ) <= 0)) {
    stop("denominators must all be greater than zero")
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


  # scale and extract confidence values
  confidence[confidence >= 90] <- confidence[confidence >= 90] / 100
  conf1 <- confidence[1]
  conf2 <- confidence[2]

  # calculate YLL and CIs
    ylls <- data %>%
      mutate(yll=(yll * x *stdpop / sum(stdpop) / n),
      #mutate(yll=(yll * x *(stdpop/n)),
             #numerator=(yll * x ),
      err_frac = (yll/x)^2 * x) |>
             #err_frac =((stdpop/n)^2) * x * ((yll)^2)) %>%
      summarise(total_count = sum(x),
      #summarise(total_count = sum(numerator),
                total_pop = sum(n),
      value = sum(yll) * multiplier,
                #value = sum(yll),
                err_frac = sum(err_frac),
                lowercl = value + (sqrt((err_frac/sum(x, na.rm=TRUE))) *
        (byars_lower(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)) * multiplier),
                  #(byars_lower(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)),
                uppercl = value + (sqrt((err_frac/sum(x, na.rm=TRUE))) *
      (byars_upper(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)) * multiplier),
                  #(byars_upper(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)),
                lower99_8cl = case_when(
                  is.na(conf2) ~ NA_real_,
                  .default = value + (sqrt((err_frac/sum(x, na.rm=TRUE))) *
                    (byars_lower(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE)) * multiplier)
                  #(byars_lower(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE))
                  ),
                upper99_8cl = case_when(
                  is.na(conf2) ~ NA_real_,
                  .default = value + (sqrt((err_frac/sum(x, na.rm=TRUE)))*
                    (byars_upper(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE)) * multiplier)
                     #(byars_upper(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE))
                  ),
                .groups = "keep") %>%
      select(-err_frac) %>%
      mutate(confidence = paste0(confidence * 100, "%", collapse = ", "),
             statistic = paste("dsr per",format(multiplier,scientific=F)),
             method = " variation")

  # rename or drop confidence limits depending whether 1 or 2 CIs requested
    if (!is.na(conf2)) {
      names(ylls)[names(dsrs) == "lowercl"] <- "lower95_0cl"
      names(ylls)[names(dsrs) == "uppercl"] <- "upper95_0cl"
    } else {
      ylls <- ylls %>%
        select(!c("lower99_8cl", "upper99_8cl"))
    }

    ylls <- ylls %>%
      mutate(across(c("value", starts_with("upper"), starts_with("lower")),
                    function(x) if_else(.data$total_count < 10, NA_real_, x)),
             statistic = if_else(.data$total_count < 10,
                                 "yll NA for total count < 10",
                                 .data$statistic))


  # drop fields not required based on value of type argument
    if (type == "lower") {
      ylls <- ylls %>%
        select(!c("total_count", "total_pop", "value", starts_with("upper"),
               "confidence", "statistic", "method"))
    } else if (type == "upper") {
      ylls <- ylls %>%
        select(!c("total_count", "total_pop", "value", starts_with("lower"),
                  "confidence", "statistic", "method"))
    } else if (type == "value") {
      ylls <- ylls %>%
        select(!c("total_count", "total_pop", "value", starts_with("lower"),starts_with("upper"),
                  "confidence", "statistic", "method"))
    } else if (type == "standard") {
      ylls<- ylls %>%
        select(!c("confidence", "statistic", "method"))
    }


  return(ylls)

}

