#' Calculate Life Expectancy using phe_life_expectancy
#'
#' Compute life expectancy for a given age, and its standard error
#'
#' @param data data.frame or tbl containing the deaths and population data
#' @param deaths field name from data containing the number of deaths within age
#'   band; unquoted string; no default
#' @param population field name from data containing the population within age
#'   band; unquoted string; no default
#' @param startage field name from data containing the age band; no default
#' @param age_contents vector; describes the contents of startage in the
#'   ascending order. This vector is used to check whether each group in data
#'   contains the complete set of age bands for the calculation to occur. It is also
#'   used to reorder the data based on the startage field
#' @param le_age the age band to return the life expectancy for. The default is
#'   "all", where the function returns the life expectancy values for all ages
#'   appended onto the input table. Any other value (or vector of values) must be age bands
#'   described by the age_contents input
#' @param type type of output; can be "standard" or "full" (full contains
#'   added details on the calculation within the dataframe); quoted
#'   string; default full
#' @inheritParams calculate_dsr
#' @return returns a data frame containing the life expectancies and confidence intervals
#'         for each le_age requested.  When type = 'full' additionally returns the cumulative
#'         populations and deaths used in each LE calculation and metadata indicating parameters passed.
#' @details This function aligns with the methodology in Public Health England's
#'   Life Expectancy Calculator available on the
#'   [Fingertips Technical Guidance](https://fingertips.phe.org.uk/profile/guidance/supporting-information/PH-methods)
#'   web page.
#'
#'   The function is for an abridged life table using 5 year age intervals with
#'   a final age interval of 90+. The table has been completed using the methods
#'   described by Chiang.(1, 2)  This age structure and methodology is used by
#'   The Office for National Statistics to produce life expectancy at national
#'   and local authority level.(3)
#'
#'   This function includes an adjustment to the method for calculating the
#'   variance of the life expectancy estimate to include a term for the variance
#'   associated with the final age interval. In the Chiang method the variance of
#'   the life expectancy is the weighted sum of the variance of the probability
#'   of survival across all the age intervals.  For the final age interval the
#'   probability of survival is, Chiang argues, zero and has zero variance.
#'   However, Silcocks et al argue(4) that in the case of the final age interval
#'   the life expectancy is dependent not on the probability of survival but on
#'   the mean length of survival \eqn{(1/M<sub>omega</sub>)}{(1/M\omega)}.
#'   Therefore the variance associated with the final age interval depends on the
#'   age-specific mortality rate \eqn{M<sub>omega</sub>}{M\omega}.
#'
#'   Life expectancy cannot be calculated if the person-years in any given age
#'   interval is zero. It will also not be calculated if the total person-years
#'   is less than 5,000 as this is considered to be the minimum size for robust
#'   calculation of life expectancy.(5)  Zero death counts are not a problem,
#'   except for the final age interval - there must be at least one death in the
#'   90+ interval for the calculations to be possible.
#'
#'   Individual Life Expectancy values will be suppressed (although confidence
#'   intervals will be shown) when the 95% confidence interval is greater
#'   than 20 years.
#'
#'   The methodology used in this function, along with discussion of alternative
#'   options for life expectancy calculation for small areas, were described Eayres
#'   and Williams.(6)
#'
#' @references
#' (1) Chiang CL. The Life Table and its Construction. In: Introduction to
#' Stochastic Processes in Biostatistics. New York, John Wiley & Sons, 1968:189-214. \cr \cr
#' (2) Newell C. Methods and Models in Demography. Chichester, John Wiley & Sons, 1994:63-81 \cr \cr
#' (3) Office for National Statistics Report. Life expectancy at birth by
#' health and local authorities in the United Kingdom, 1998 to 2000 (3-year
#' aggregate figures.) Health Statistics Quarterly 2002;13:83-90 \cr \cr
#' (4) Silcocks PBS, Jenner DA, Reza R.  Life expectancy as a summary of mortality
#' in a population: statistical considerations and suitability for use by health
#' authorities. J Epidemiol Community Health 2001;55:38-43 \cr \cr
#' (5) Toson B, Baker A. Life expectancy at birth: methodological options for
#' small populations. National Statistics  Methodological Series No 33. HMSO 2003. \cr \cr
#' (6) Eayres DP, Williams ES. Evaluation of methodologies for small area
#' life expectancy estimation. J Epidemiol Community Health 2004;58:243-249 \cr \cr
#'
#' @import dplyr
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @examples
#' library(dplyr)
#'
#' ## A simple example
#' df <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
#'                               60L, 65L, 70L, 75L, 80L, 85L, 90L),
#'                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
#'                           37039L, 33288L, 23306L, 11936L, 11936L),
#'                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
#' phe_life_expectancy(df, deaths, pops, startage)
#'
#' ## or with multiple confidence limits
#' phe_life_expectancy(df, deaths, pops, startage, confidence = c(95, 99.8))
#'
#' ## OR
#'
#' phe_life_expectancy(df, deaths, pops, startage, le_age = c(5, 25), type = "standard")
#'
#' ## Unordered age bands example
#' df <- data.frame(startage = c("0", "1-4", "5-9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
#'                               "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54",
#'                               "55 - 59", "60 - 64", "65 - 69", "75 - 79", "80 - 84",
#'                               "85 - 89", "90 +", "70 - 74"),
#'                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37039L,
#'                           23306L, 11936L, 11936L, 37978L, 33288L),
#'                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                             263L, 304L, 872L, 1605L, 1936L, 1937L, 536L, 1390L))
#' phe_life_expectancy(df, deaths, pops, startage,
#'                     age_contents = c("0", "1-4", "5-9",
#'                                      "10 - 14", "15 - 19",
#'                                      "20 - 24", "25 - 29",
#'                                      "30 - 34", "35 - 39",
#'                                      "40 - 44", "45 - 49",
#'                                      "50 - 54", "55 - 59",
#'                                      "60 - 64", "65 - 69",
#'                                      "70 - 74", "75 - 79",
#'                                      "80 - 84", "85 - 89",
#'                                      "90 +"))
#'
# ## A grouped data example
#' df <- data.frame(area = c(rep("Area 1", 20), rep("Area 2", 20)),
#'                  startage = rep(c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
#'                                   60L, 65L, 70L, 75L, 80L, 85L, 90L), 2),
#'                  pops = rep(c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                               61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
#'                               37039L, 33288L, 23306L, 11936L, 11936L), 2),
#'                  deaths = rep(c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                                 263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L), 2))
#' df %>%
#'        group_by(area) %>%
#'        phe_life_expectancy(deaths, pops, startage)
#'
#' @author Sebastian Fox, \email{sebastian.fox@@phe.gov.uk}
#' @export
#'
#' @family PHEindicatormethods package functions

phe_life_expectancy <- function(data, deaths, population, startage,
                                age_contents = c(0L, 1L, 5L, 10L, 15L,
                                                 20L, 25L, 30L, 35L, 40L,
                                                 45L, 50L, 55L, 60L, 65L,
                                                 70L, 75L, 80L, 85L, 90L),
                                le_age = "all", type = "full", confidence = 0.95) {

  # check required arguments present
  if (missing(data) | missing(deaths) | missing(population) | missing(startage)) {
    stop("function life_expectancy requires at least 4 arguments: data, deaths, population, startage")
  }

  # check that min age  is 0
  stripped_age_contents <- as.integer(sub("\\D*(\\d+).*", "\\1", age_contents))
  if (stripped_age_contents[1] != 0) stop("first age band in age_contents must be 0")

  # check age_contents is ascending
  if (!identical(stripped_age_contents, sort(stripped_age_contents)))
    stop(paste("age_contents doesn't appear to be in ascending order; the following age bands appear out of position:",
               paste(age_contents[stripped_age_contents != sort(stripped_age_contents)],
                     collapse = ", ")))

  # check on confidence limit requirements
  if (any(confidence < 0.9) | (any(confidence > 1) & any(confidence < 90)) | any(confidence > 100)) {
    stop("all confidence levels must be between 90 and 100 or between 0.9 and 1")
  }

  # compare startage field with age_contents
  age_bands <- data %>%
          pull({{ startage }}) %>%
          unique() %>%
          sort()

  if (!identical(as.character(age_bands), as.character(sort(age_contents))))
    stop("the contents in the startage field do not match the contents of the age_contents vector")

  #calculate start age for each age band
  data <- data %>%
          mutate(startage_2b_removed = as.integer(sub("\\D*(\\d+).*", "\\1", {{ startage }})))

  # order the data by (group variables) and start age
  if (length(group_vars(data)) > 0) {
          data <- data %>%
                  arrange(.data$startage_2b_removed,
                          .by_group = TRUE)

          # preparation for later checks to prevent warnings around factors
          groupings <- group_vars(data)

          factor_vars <- lapply(data, is.factor) %>%
            unlist()

          # identify which of the grouping variables are factors
          grouping_factors <- intersect(groupings, names(factor_vars[factor_vars]))

  } else {
          data <- data %>%
                  arrange(.data$startage_2b_removed)
  }

  # check for negative deaths
  negative_deaths <- data
  if (length(group_vars(data)) > 0) {
    negative_deaths <- negative_deaths %>%
      ungroup() %>%
      mutate(across(all_of(grouping_factors), as.character)) %>% #stops warning in cases where filters result in 0 records
      group_by(across(all_of(group_vars(data))))
  }

  negative_deaths <- as_tibble(negative_deaths) %>%
          group_by(across(all_of(group_vars(data)))) %>%
          filter({{ deaths }} < 0) %>%
          count() %>%
          filter(n != 0) %>%
          select(!c("n"))

  if (nrow(negative_deaths) > 0) {
          warning("some age bands have negative deaths; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  negative_deaths <- negative_deaths %>%
                          left_join(data, by = group_vars(data))
                  # remove areas with deaths < 0 in any age band
                  data <- data %>%
                          anti_join(negative_deaths, by = group_vars(data))
          } else {
                  data <- data %>%
                    mutate(value = NA,
                           lowercl = NA,
                           uppercl = NA) %>%
                    select(!c("startage_2b_removed"))
                  return(data)
          }
  }

  # check for less than or equal to zero pops
  negative_pops <- data
  if (length(group_vars(data)) > 0) {
    negative_pops <- negative_pops %>%
      ungroup() %>%
      mutate(across(all_of(grouping_factors), as.character)) %>% #stops warning in cases where filters result in 0 records
      group_by(across(all_of(group_vars(data))))
  }

  negative_pops <- as_tibble(negative_pops) %>%
          group_by(across(all_of(group_vars(data)))) %>%
          filter({{ population }} <= 0) %>%
          count() %>%
          filter(n != 0) %>%
          select(-n)

  if (nrow(negative_pops) > 0) {
          warning("some age bands have a zero or less population; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  negative_pops <- negative_pops %>%
                          left_join(data, by = group_vars(data))
                  # remove areas with pops <= 0 in any age band
                  data <- data %>%
                          anti_join(negative_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                    mutate(value = NA,
                           lowercl = NA,
                           uppercl = NA) %>%
                    select(!c("startage_2b_removed"))
                  return(data)
          }
  }

  # check for all rows per group
  number_age_bands <- 20 #length(age_contents)
  incomplete_areas <- as_tibble(data) %>%
    group_by(across(all_of(group_vars(data)))) %>%
    count()
  if (length(group_vars(data)) > 0) {
    incomplete_areas <- incomplete_areas %>%
      ungroup() %>%
      mutate(across(all_of(grouping_factors), as.character)) %>% #stops warning in cases where filters result in 0 records
      group_by(across(all_of(group_vars(data))))
  }
  incomplete_areas <- incomplete_areas %>%
    filter(n != number_age_bands) %>%
    select(!c("n"))
  if (nrow(incomplete_areas) > 0) {
          warning("some groups contain a different number of age bands than 20; life expectancy cannot be calculated for these. These groups will contain NAs.")

          # Insert NAs into output fields to be row bound to the final output at end
          if (length(group_vars(data)) > 0) {
                  incomplete_areas <- incomplete_areas %>%
                          left_join(data, by = group_vars(data))

                  # remove areas with incomplete number of age bands
                  data <- data %>%
                          anti_join(incomplete_areas, by = group_vars(data))
          } else {
                  data <- data %>%
                    mutate(value = NA,
                           lowercl = NA,
                           uppercl = NA) %>%
                    select(!c("startage_2b_removed"))
                  return(data)
          }

  }

  # check for deaths > pops
  deaths_more_than_pops <- data
  if (length(group_vars(data)) > 0) {
    deaths_more_than_pops <- deaths_more_than_pops %>%
      ungroup() %>%
      mutate(across(all_of(grouping_factors), as.character)) %>% #stops warning in cases where filters result in 0 records
      group_by(across(all_of(group_vars(data))))
  }
  deaths_more_than_pops <- as_tibble(deaths_more_than_pops) %>%
          group_by(across(all_of(group_vars(data)))) %>%
          filter({{ deaths }} >  {{ population }}) %>%
          count() %>%
          filter(n != 0) %>%
          select(!c("n"))

  if (nrow(deaths_more_than_pops) > 0) {
          warning("some age bands have more deaths than population; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  deaths_more_than_pops <- deaths_more_than_pops %>%
                          left_join(data, by = group_vars(data))

                  # remove areas with deaths > pops in any age band
                  data <- data %>%
                          anti_join(deaths_more_than_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                    mutate(value = NA,
                           lowercl = NA,
                           uppercl = NA) %>%
                    select(!c("startage_2b_removed"))
                  return(data)
          }



  }

  # check for pops <= 5000
  total_pops <- data %>%
          summarise(total_pop = sum({{ population }}))
  if (length(group_vars(data)) > 0) {
    total_pops <- total_pops %>%
     mutate(across(all_of(grouping_factors), as.character)) #stops warning in cases where filters result in 0 records
  }
  total_pops <- total_pops %>%
          filter(.data$total_pop <= 5000) %>%
          select(!c("total_pop"))
  if (nrow(total_pops) > 0) {
          warning("some groups have a total population of less than 5,000; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  total_pops <- total_pops %>%
                          left_join(data, by = group_vars(data))

                  # remove areas with pops <= 5000
                  data <- data %>%
                          anti_join(total_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA) %>%
                    select(!c("startage_2b_removed"))
                  return(data)
          }

  }
  suppressed_data <- bind_rows(negative_deaths,
                               negative_pops,
                               incomplete_areas,
                               deaths_more_than_pops,
                               total_pops) %>%
          unique()
  if (nrow(suppressed_data) > 0) {
    suppressed_data <- suppressed_data %>%
     mutate(across(all_of(grouping_factors), as.factor))

  }

  # scale confidence level
  confidence[confidence >= 90] <- confidence[confidence >= 90] / 100

  z <- qnorm(confidence + (1 - confidence)/2)
  data$group_id_2b_removed <- data %>%
    group_indices()
  data <- data %>%
    mutate(id_2b_removed = row_number(),
           ni_2b_removed = as.numeric(lead(.data$startage_2b_removed) - .data$startage_2b_removed),
           ai_2b_removed = case_when(
             .data$startage_2b_removed == 0 ~ 0.1,
             TRUE ~ 0.5),
           M_2b_removed = {{ deaths }} / {{ population }},
           ni_2b_removed = case_when(
             is.na(.data$ni_2b_removed) ~ 2 / .data$M_2b_removed,
             TRUE ~ .data$ni_2b_removed),
           qi_2b_removed = case_when(
             {{ deaths }} <= {{ population }} / .data$ni_2b_removed / .data$ai_2b_removed ~
               .data$M_2b_removed * .data$ni_2b_removed / (1 + .data$M_2b_removed * .data$ni_2b_removed * (1 - .data$ai_2b_removed)),
             TRUE ~ 1),
           p_2b_removed = 1 - .data$qi_2b_removed,
           l_2b_removed = case_when(
             .data$id_2b_removed == 1 ~ 1e5,
             TRUE ~ 1),
           p1_2b_removed = lag(.data$p_2b_removed,
                               default = 1),
           l_2b_removed = case_when(
             id_2b_removed != 1 ~ cumprod(.data$l_2b_removed * .data$p1_2b_removed),
             TRUE ~ .data$l_2b_removed),
           di_2b_removed = .data$l_2b_removed - lead(.data$l_2b_removed, default = 0),
           Li_2b_removed = case_when(
             .data$id_2b_removed < number_age_bands ~ .data$ni_2b_removed *
               (lead(.data$l_2b_removed) + (.data$ai_2b_removed * .data$di_2b_removed)),
             TRUE ~ l_2b_removed / M_2b_removed),
           Ti_2b_removed = rev(cumsum(rev(.data$Li_2b_removed))),
           ei = case_when(
             .data$l_2b_removed == 0 ~ 0,
             TRUE ~ .data$Ti_2b_removed / .data$l_2b_removed),
           spi_2b_removed = case_when(
             .data$di_2b_removed == 0 ~ 0,
             TRUE ~ (.data$qi_2b_removed ^ 2) * (1 - .data$qi_2b_removed) / {{ deaths }}),
           spi_2b_removed = case_when(
             .data$id_2b_removed == number_age_bands ~ 4 / ({{ deaths }} * (.data$M_2b_removed ^ 2)),
             TRUE ~ .data$spi_2b_removed),
           W_spi_2b_removed = case_when(
             .data$id_2b_removed < number_age_bands ~ .data$spi_2b_removed *
               (.data$l_2b_removed ^ 2) * (((1 - .data$ai_2b_removed) * .data$ni_2b_removed + lead(ei)) ^ 2),
             TRUE ~ ((.data$l_2b_removed / 2) ^ 2) * .data$spi_2b_removed),
           STi_2b_removed = rev(cumsum(rev(.data$W_spi_2b_removed))),
           SeSE_2b_removed = sqrt(.data$STi_2b_removed / (.data$l_2b_removed ^ 2)),
           ciover20_2b_removed = case_when(
             qnorm(0.975) * .data$SeSE_2b_removed > 10 ~ TRUE, TRUE ~ FALSE))

  lower_cls <- z %>%
    lapply(function(z, x, y) x - z * y, x = data$ei, y = data$SeSE_2b_removed)
  upper_cls <- z %>%
    lapply(function(z, x, y) x + z * y, x = data$ei, y = data$SeSE_2b_removed)

  if (any(data$ciover20_2b_removed == TRUE)) {
      warning(paste0("some life expectancy values have a 95% confidence interval ",
                     "> 20 years; these values have been suppressed to NAs"))
  }

  if (length(lower_cls) > 1) {
    names(lower_cls) <- paste0("lower",
                               gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                               "cl")
    names(upper_cls) <- paste0("upper",
                               gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                               "cl")
  } else {
    names(lower_cls) <- "lowercl"
    names(upper_cls) <- "uppercl"
  }

  cls <- bind_cols(lower_cls, upper_cls)

  data <- data %>%
    bind_cols(cls) %>%
    rename(value = "ei")

  data$value[data$value == Inf] <- NA

  # suppress LE values when 95% CI is wider than 20 years
  data <- data %>%
    mutate(value = case_when(
      .data$ciover20_2b_removed == TRUE ~ NA_real_,
      TRUE ~ .data$value))

  if (nrow(suppressed_data) > 0) data <- bind_rows(data, suppressed_data)

  # calculate cumulative pops and deaths used in each calc (sum for all startages >= startage)
  cumdata <- data %>%
    arrange(desc(.data$startage_2b_removed)) %>%
    select("startage_2b_removed", {{ population }}, {{ deaths }}) %>%
    mutate(pops_used = cumsum({{ population }}),
           dths_used = cumsum({{ deaths }})) %>%
    select(!c({{ population }}, {{ deaths }})) %>%
    arrange(.data$startage_2b_removed)

  # join cumulative deaths and pops to data frame and drop original deaths and pops
  join_vars <- c(group_vars(data), "startage_2b_removed")
  data <- data %>%
    left_join(cumdata, by = join_vars) %>%
    select(!c({{ population }}, {{ deaths }}))

  data <- data %>%
    select(!ends_with("_2b_removed"))
  if (length(le_age) == 1) {
          if (le_age != "all") {
                  if (sum(age_contents %in% le_age) == 0) {
                          warning("le_age not in the vector described by age_contents; all life expectancies will be returned")
                  } else {
                          data <- data %>%
                                  filter({{ startage }} %in% le_age)
                  }
          }
  } else {
          if (sum(age_contents %in% le_age) == 0) {
                  warning("le_age not in the vector described by age_contents; all life expectancies will be returned")
          } else {
                  data <- data %>%
                          filter({{ startage }} %in% le_age)
          }
  }

  if (type == "full") {
          data <- data %>%
                  mutate(confidence = paste0(confidence * 100, "%", collapse = ", "),
                         statistic = paste("life expectancy at", {{ startage }}),
                         method = "Chiang, using Silcocks et al for confidence limits")
  } else {
          data <- data %>%
            select(!c("pops_used", "dths_used"))
  }

  # ensure output is a data frame with original group attributes
  grp_vars <- group_vars(data)
  data <- as.data.frame(data)
  if(length(grp_vars) > 0) {
    data <- data %>%
      group_by(across(all_of(grp_vars)))
  }

  return(data)

}
