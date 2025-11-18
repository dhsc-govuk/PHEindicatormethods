# -------------------------------------------------------------------------------------------------
#' Calculate Proportions using phe_proportion
#'
#' Calculates proportions with confidence limits using Wilson Score method (1,2).
#'
#' @param data a data.frame containing the data to calculate proportions for, pre-grouped if proportions required for
#'             group aggregates; unquoted string; no default
#' @param x field name from data containing the observed numbers of cases in the sample meeting the required condition
#'          (the numerator for the proportion); unquoted string; no default
#' @param n field name from data containing the number of cases in the sample (the denominator for the proportion);
#'          unquoted string; no default
#' @param confidence the required level of confidence expressed as a number
#'   between 0.9 and 1 or a number between 90 and 100. The vector c(0.95, 0.998)
#'   can also be passed to output both 95 percent and 99.8 percent CIs, or an NA
#'   value can be passed if no confidence intervals are required.; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 1
#'
#' @inheritParams calculate_dsr
#'
#' @return When type = "full", returns the original data.frame with the following appended:
#'         proportion, lower confidence limit, upper confidence limit, confidence level, statistic and method
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#' @export
#'
#' @section Notes: Wilson Score method (2) is applied using the internal wilson_lower
#'          and wilson_upper functions. \cr \cr
#'          The percentage argument was deprecated in v1_1_0, please use multiplier argument instead
#'
#' @examples
#'
#' # ungrouped data frame
#' df <- data.frame(area = rep(c("Area1","Area2","Area3","Area4"), each=3),
#'                  numerator = c(NA,82,9,48, 6500,8200,10000,10000,8,7,750,900),
#'                  denominator = rep(c(100,10000,10000,10000), each=3))
#'
#' phe_proportion(df, numerator, denominator)
#' phe_proportion(df, numerator, denominator, confidence=99.8)
#' phe_proportion(df, numerator, denominator, type="standard")
#' phe_proportion(df, numerator, denominator, confidence = c(0.95, 0.998))
#'
#' # grouped data frame
#' library(dplyr)
#' dfg <- df |> group_by(area)
#' phe_proportion(dfg, numerator, denominator, multiplier=100)
#'
#'
#' @references
#' (1) Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' (2) Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_proportion function using Wilson's method
phe_proportion <- function(data, x, n, type="full", confidence=0.95, multiplier=1) {


    # check required arguments present
    if (missing(data)|missing(x)|missing(n)) {
        stop("function phe_proportion requires at least 3 arguments: data, x, n")
    }


    # validate arguments
    if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(pull(data, {{ n }}) <= 0, na.rm=TRUE)) {
        stop("denominators must be greater than zero")
    } else if (any(pull(data, {{ x }}) > pull(data, {{ n }}), na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a proportion statistic")
    } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
        stop("type must be one of value, lower, upper, standard or full")
    } else if (length(confidence) >2) {
        stop("a maximum of two confidence levels can be provided")
    } else if (length(confidence) == 2) {
        if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
            stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
        }
    } else if (!is.na(confidence)) {
      if (confidence < 0.9 | (confidence > 1 & confidence < 90) | confidence > 100) {
        stop("confidence level, if specified, must be between 90 and 100 or between 0.9 and 1")
      }
    }

    # if data is grouped then summarise
    if(!n_groups(data) == 1) {
        data <- data |>
        summarise({{ x }} := sum({{ x }}),
                  {{ n }} := sum({{ n }}),
                  .groups = "keep")
    }


    # if two confidence levels are specified
    if (length(confidence) == 2) {
        # if two confidence levels requested
        conf1 <- confidence[1]
        conf2 <- confidence[2]

        # calculate proportion and CIs
        phe_proportion <- data |>
            mutate(value = ({{ x }})/({{ n }}) * multiplier,
                   lower95_0cl = wilson_lower(({{ x }}),({{ n }}),conf1) * multiplier,
                   upper95_0cl = wilson_upper(({{ x }}),({{ n }}),conf1) * multiplier,
                   lower99_8cl = wilson_lower(({{ x }}),({{ n }}),conf2) * multiplier,
                   upper99_8cl = wilson_upper(({{ x }}),({{ n }}),conf2) * multiplier,
                   confidence = paste(conf1*100,"%, ", conf2*100,"%",sep=""),
                   statistic = if_else(multiplier == 100,"percentage",paste0("proportion of ",multiplier)),
                   method = "Wilson")


        # generate output in required format
        if (type == "lower") {
            phe_proportion <- phe_proportion |>
                select(!c("value", "upper95_0cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_proportion <- phe_proportion |>
                select(!c("value", "lower95_0cl", "lower99_8cl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_proportion<- phe_proportion |>
                select(!c("lower95_0cl", "upper95_0cl", "lower99_8cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_proportion <- phe_proportion |>
                select(!c("confidence", "statistic", "method"))
        }


    } else {


        # scale confidence level
        if (!is.na(confidence)) {
          if (confidence[1] >= 90) {
            confidence <- confidence/100
          }
        }

        # calculate proportion and single CI or no CIs
        phe_proportion <- data |>
            mutate(value = ({{ x }})/({{ n }}) * multiplier,
                   statistic = if_else(multiplier == 100,"percentage",
                                       paste0("proportion of ",multiplier)),
            )

        if (!is.na(confidence)) {
          phe_proportion <- phe_proportion |>
          mutate(lowercl = wilson_lower(({{ x }}),({{ n }}),confidence) * multiplier,
                 uppercl = wilson_upper(({{ x }}),({{ n }}),confidence) * multiplier,
                 confidence = paste(confidence*100,"%",sep=""),
                 method = "Wilson") |>
          relocate("statistic", .after = "confidence")
        }


        # generate output in required format
        if (type == "lower") {
            phe_proportion <- phe_proportion |>
                select(!c("value", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_proportion <- phe_proportion |>
                select(!c("value", "lowercl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_proportion<- phe_proportion |>
                select(!c("lowercl", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_proportion <- phe_proportion |>
                select(!c("confidence", "statistic", "method"))
        }

    }

    return(phe_proportion)
}
