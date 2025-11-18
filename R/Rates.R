# -------------------------------------------------------------------------------------------------
#' Calculate Rates using phe_rate
#'
#' Calculates rates with confidence limits using Byar's (1) or exact (2) CI method.
#'
#' @param data the data.frame containing the data to calculate rates for,
#'   pre-grouped if proportions required for group aggregates; unquoted string;
#'   no default
#' @param x field name from data containing the rate numerators (eg observed
#'   number of events); unquoted string; no default
#' @param n field name from data containing the rate denominators (eg
#'   populations); unquoted string; no default
#'
#' @inheritParams calculate_dsr
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#' @importFrom stats qchisq
#' @export
#'
#' @return When type = "full", returns the original data.frame with the
#'   following appended: rate, lower confidence limit, upper confidence limit,
#'   confidence level, statistic and method
#'
#' @examples
#' # ungrouped data frame
#' df <- data.frame(area = rep(c("Area1","Area2","Area3","Area4"), each=3),
#'                  obs = c(NA,82,9,48, 6500,8200,10000,10000,8,7,750,900),
#'                  pop = rep(c(100,10000,10000,10000), each=3))
#'
#' phe_rate(df, obs, pop)
#' phe_rate(df, obs, pop, type="standard")
#' phe_rate(df, obs, pop, confidence=99.8, multiplier=100)
#'
#' # grouped data frame
#' library(dplyr)
#' dfg <- df %>% group_by(area)
#' phe_rate(dfg, obs, pop)
#'
#' @section Notes: For numerators >= 10 Byar's method (1) is applied using the
#'   internal byars_lower and byars_upper functions.  For
#'   small numerators Byar's method is less accurate and so an exact method (2)
#'   based on the Poisson distribution is used.
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr
#' (2) Armitage P, Berry G. Statistical methods in medical research (4th edn).
#'   Oxford: Blackwell; 2002.
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate rate and CIs using Byar's method
phe_rate <- function(data,x, n, type = "full", confidence = 0.95, multiplier = 100000) {


    # check required arguments present
    if (missing(data)|missing(x)|missing(n)) {
        stop("function phe_rate requires at least 3 arguments: data, x, n")
    }


    # validate arguments
    if (any(pull(data, {{ x }}) < 0, na.rm = TRUE)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(pull(data, {{ n }}) <= 0, na.rm = TRUE)) {
        stop("denominators must be greater than zero")
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


    # if data is grouped then summarise
    if(!n_groups(data) == 1) {
        data <- data %>%
                summarise({{ x }} := sum({{ x }}),
                          {{ n }} := sum({{ n }}),
                          .groups = "keep")
    }


    #calculate rate and CIs
    if (length(confidence) == 2) {

        # if two confidence levels requested
        conf1 <- confidence[1]
        conf2 <- confidence[2]

        # calculate rate and CIs
        phe_rate <- data %>%
            mutate(value = ({{ x }})/({{ n }})*multiplier,
                   lower95_0cl = if_else(({{ x }}) < 10, qchisq((1-conf1)/2,2*({{ x }}))/2/({{ n }})*multiplier,
                                         byars_lower(({{ x }}),conf1)/({{ n }})*multiplier),
                   upper95_0cl = if_else(({{ x }}) < 10, qchisq(conf1+(1-conf1)/2,2*({{ x }})+2)/2/({{ n }})*multiplier,
                                         byars_upper(({{ x }}),conf1)/({{ n }})*multiplier),
                   lower99_8cl = if_else(({{ x }}) < 10, qchisq((1-conf2)/2,2*({{ x }}))/2/({{ n }})*multiplier,
                                         byars_lower(({{ x }}),conf2)/({{ n }})*multiplier),
                   upper99_8cl = if_else(({{ x }}) < 10, qchisq(conf2+(1-conf2)/2,2*({{ x }})+2)/2/({{ n }})*multiplier,
                                         byars_upper(({{ x }}),conf2)/({{ n }})*multiplier),
                   confidence = paste(conf1*100,"%, ",conf2*100,"%",sep=""),
                   statistic  = paste("rate per",as.character(format(multiplier, scientific=F))),
                   method     = if_else(({{ x }}) < 10, "Exact","Byars"))


        # generate output in required format
        if (type == "lower") {
            phe_rate <- phe_rate %>%
                select(!c("value", "upper95_0cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_rate <- phe_rate %>%
                select(!c("value", "lower95_0cl", "lower99_8cl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_rate<- phe_rate %>%
                select(!c("lower95_0cl", "upper95_0cl", "lower99_8cl", "upper99_8cl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_rate <- phe_rate %>%
                select(!c("confidence", "statistic", "method"))
        }

    } else {

        # scale confidence level
        if (confidence[1] >= 90) {
            confidence <- confidence/100
        }


        # calculate rate and a single CI
        phe_rate <- data %>%
            mutate(value = ({{ x }})/({{ n }})*multiplier,
                   lowercl    = if_else(({{ x }}) < 10, qchisq((1-confidence)/2,2*({{ x }}))/2/({{ n }})*multiplier,
                                        byars_lower(({{ x }}),confidence)/({{ n }})*multiplier),
                   uppercl    = if_else(({{ x }}) < 10, qchisq(confidence+(1-confidence)/2,2*({{ x }})+2)/2/({{ n }})*multiplier,
                                        byars_upper(({{ x }}),confidence)/({{ n }})*multiplier),
                   confidence = paste(confidence*100,"%",sep=""),
                   statistic  = paste("rate per",as.character(format(multiplier, scientific=F))),
                   method     = if_else(({{ x }}) < 10, "Exact","Byars"))


        # generate output in required format
        if (type == "lower") {
            phe_rate <- phe_rate %>%
                select(!c("value", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "upper") {
            phe_rate <- phe_rate %>%
                select(!c("value", "lowercl", "confidence", "statistic", "method"))
        } else if (type == "value") {
            phe_rate<- phe_rate %>%
                select(!c("lowercl", "uppercl", "confidence", "statistic", "method"))
        } else if (type == "standard") {
            phe_rate <- phe_rate %>%
                select(!c("confidence", "statistic", "method"))
        }

    }

    return(phe_rate)
}
