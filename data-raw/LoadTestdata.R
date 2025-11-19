# This file loads all testdata used by the testthat scripts into R/sysdata
# there is also code to load all required libraries

library(dplyr)
library(tidyr)
library(devtools)
library(readxl)
library(usethis)


########################################
######### CREATE TEST DATA #############
########################################

##############
## EXTERNAL ##
##############

# esp2013
esp2013 <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)


##############
## INTERNAL ##
##############

# quantile lookup
qnames <- data.frame(quantiles = c(2L,3L,4L,5L,6L,7L,8L,10L,12L,16L,20L),
                     qname     = c("Half","Tertile","Quartile","Quintile","Sextile","Septile",
                                  "Octile","Decile","Duo-decile","Hexadecile","Ventile"),
                     stringsAsFactors = FALSE)


# quantile test data
test_quantiles <- read_excel("tests/testthat/testdata_Quantiles.xlsx",
                             sheet="testdata_Quantiles",   col_names=TRUE)  %>%
  select(-Rank, -RevValue, -Sex, -RowsInGrp)

test_quantiles$Polarity[test_quantiles$Polarity == "RAG - High is good"] <- FALSE
test_quantiles$Polarity[test_quantiles$Polarity == "RAG - Low is good"] <- TRUE
test_quantiles$Value <- as.numeric(test_quantiles$Value, digits = 10)

test_quantiles_ug <- test_quantiles %>%
  filter(substr(Test,1,4) == "Good")

test_quantiles_g <-test_quantiles_ug %>%
  group_by(IndSexRef)

test_quantiles_fail <- test_quantiles %>%
  filter(Test == "BadPolarity")



# Byars Wilson test data
test_BW <- read_excel("tests/testthat/testdata_Byars_Wilson.xlsx", sheet="testdata_B_W",   col_names=TRUE)

# Funnel test data

test_funnel_inputs <- read.csv("tests/testthat/testdata_funnel_prop_inputs.csv",
                                col.names = c("Area", "numerator", "denominator", "significance"),
                                colClasses = c("character", "numeric", "integer", "factor"))

test_funnel_outputs <- read.csv("tests/testthat/testdata_funnel_prop_outputs.csv",
                                col.names = c("Population",
                                              "lower_2s_limit", "upper_2s_limit",
                                              "lower_3s_limit", "upper_3s_limit",
                                              "baseline"),
                                colClasses = "numeric")

test_funnel_outputs_axis_variation <- read.csv("tests/testthat/testdata_funnel_prop_outputs_with_axis_variation.csv",
                                               col.names = c("Population",
                                                             "lower_2s_limit", "upper_2s_limit",
                                                             "lower_3s_limit", "upper_3s_limit",
                                                             "baseline"),
                                               colClasses = "numeric")
test_funnel_outputs_axis_variation$statistic <- "proportion"
test_funnel_outputs_axis_variation$method <- "Wilson"

test_funnel_ratio_inputs <- read.csv("tests/testthat/testdata_funnel_ratio_inputs.csv",
                                     col.names = c("obs", "expected", "significance"),
                                     colClasses = c("numeric", "numeric", "factor"))

test_funnel_ratio_outputs <- read.csv("tests/testthat/testdata_funnel_ratio_outputs.csv")
names(test_funnel_ratio_outputs)[1] <- "Observed_events"

test_funnel_rate_inputs <- read.csv("tests/testthat/testdata_funnel_rate_dsr_inputs.csv",
                                    colClasses = c("integer",
                                                   rep("numeric", 4),
                                                   rep("factor", 8))) %>%
  mutate(across(starts_with(c("dsr", "crude")),
                function(x) factor(x,
                                   levels = c("High (0.001)",
                                              "High (0.025)",
                                              "Low (0.001)",
                                              "Low (0.025)",
                                              "Not significant",
                                              "Not applicable for events less than 10 for dsrs")))) %>%
  as_tibble()


test_funnel_rate_funnels_input <- read.csv("tests/testthat/testdata_funnel_rate_input_funnels.csv")
test_funnel_rate_funnels <- read.csv("tests/testthat/testdata_funnel_rate_outputs.csv") %>%
  as_tibble()

test_funnel_rate_funnels_2 <- read.csv("tests/testthat/testdata_funnel_rate_outputs_2.csv") %>%
  as_tibble()

test_funnel_rate_funnels_3 <- read.csv("tests/testthat/testdata_funnel_rate_outputs_3.csv") %>%
  as_tibble()

# Proportions test data
test_Prop   <- read_excel("tests/testthat/testdata_Proportion.xlsx", sheet="testdata_Prop",   col_names=TRUE)

test_Prop_g <- test_Prop %>%
  group_by(Area)

test_Prop_g_results   <- read_excel("tests/testthat/testdata_Proportion.xlsx", sheet="testdata_Prop_g",   col_names=TRUE)

test_Prop_g_results_no_CI <- test_Prop_g_results |>
  mutate(lowercl = NA_real_,
         uppercl = NA_real_,
         confidence = "not requested",
         method = NA_character_)


#Rates test data
test_Rate <- read_excel("tests/testthat/testdata_Rate.xlsx", sheet="testdata_Rate", col_names=TRUE)

test_Rate_g <- test_Rate %>%
  group_by(Area)

test_Rate_g_results   <- read_excel("tests/testthat/testdata_Rate.xlsx", sheet="testdata_Rate_g",   col_names=TRUE)




#Means test data
test_Mean         <- read_excel("tests/testthat/testdata_Mean.xlsx", sheet="testdata_Mean",         col_names=TRUE)
test_Mean_results <- read_excel("tests/testthat/testdata_Mean.xlsx", sheet="testdata_Mean_results", col_names=TRUE)

test_Mean_Grp <- group_by(test_Mean,area)





# DSRs, ISRs test data
test_multiarea   <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_multiarea", col_names=TRUE) %>%
  group_by(area)

test_multiarea_esp <- test_multiarea %>%
  mutate(esp2013 = esp2013)

test_DSR_1976    <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_1976",   col_names=TRUE)

test_err1        <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_err1",   col_names=TRUE)
test_err2        <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_err2",   col_names=TRUE) %>%
  group_by(area)

test_err2_esp <- test_err2 %>%
  mutate(esp2013 = esp2013)

test_err3        <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_err3",   col_names=TRUE)
test_err4        <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_err4",   col_names = TRUE)


test_DSR_results <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testresults_DSR", col_names=TRUE)


test_ISR_results <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testresults_ISR", col_names=TRUE)
test_ISR_refdata <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="refdata",         col_names=TRUE)
test_ISR_ownref  <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_multiarea_isr", col_names=TRUE) %>%
                      group_by(area)
test_ISR_lookup  <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet="testdata_multiarea_lookup", col_names=TRUE)

# DSR test data for non-independent events needs pivoting longer for frequencies:
test_DSR_nonind <- read_excel("tests/testthat/testdata_DSR_ISR.xlsx", sheet = "testdata_nonindepe", col_names = TRUE) %>%
  select(!"count") %>%
  pivot_longer(cols = starts_with("f"),
               names_to = "freq",
               values_to = "persons",
               names_prefix = "f") %>%
  filter(!(area %in% c("testdata_nonind_dummy",
                       "testdata_nonind_small",
                       "testdata_nonind_tiny") & freq == 4)) %>%
  mutate(freq = as.integer(freq)) %>%
  arrange(freq, ageband) %>%
  group_by(area, freq) %>%
  mutate(esp2013 = esp2013) %>%
  group_by(area)

test_DSR_nonind_err1 <- test_DSR_nonind
test_DSR_nonind_err1$pop[1] <- 1

test_DSR_nonind_err2 <- test_DSR_nonind
test_DSR_nonind_err2$esp2013[1] <- 1


# SII
SII_test_data <- read_excel("tests/testthat/testdata_SII.xlsx")

# Grouped SII test data
SII_test_grouped <- SII_test_data %>%
     group_by(Area, Grouping1, Grouping2)

# assign_trend_markers data

test_prop_trends   <- read_excel("tests/testthat/testdata_trend_markers.xlsx", sheet="testdata_prop_trends",   col_names=TRUE)

test_prop_grouped   <- read_excel("tests/testthat/testdata_trend_markers.xlsx", sheet="testdata_prop_grouped",   col_names=TRUE)

test_other_trends   <- read_excel("tests/testthat/testdata_trend_markers.xlsx", sheet="testdata_other_trends",   col_names=TRUE)


test_other_6_trends   <- read_excel("tests/testthat/testdata_trend_markers.xlsx", sheet="testdata_other_6",   col_names=TRUE)

test_other_grouped   <- read_excel("tests/testthat/testdata_trend_markers.xlsx", sheet="testdata_other_grouped",   col_names=TRUE)


########################################
###### SAVE TEST DATA TO PACKAGE #######
########################################

# SAVE EXTERNALLY AVAILABLE DATA IN data\XXXXXX.rda - data available to user
usethis::use_data(esp2013, LE_data, DSR_data, prevalence_data,
                  internal=FALSE, overwrite=TRUE)


# SAVE INTERNAL DATA IN R\Sysdata.rda - data available to functions and test scripts but not available to user:
usethis::use_data(qnames, test_BW,
                  test_Prop, test_Prop_g, test_Prop_g_results, test_Prop_g_results_no_CI,
                  test_quantiles_g, test_quantiles_ug, test_quantiles_fail,
                  test_Rate, test_Rate_g, test_Rate_g_results,
                  test_Mean, test_Mean_Grp, test_Mean_results,
                  test_multiarea, test_multiarea_esp, test_DSR_1976, test_DSR_nonind,
                  test_DSR_nonind_err1, test_DSR_nonind_err2,
                  test_err1, test_err2, test_err2_esp,
                  test_err3, test_err4, test_DSR_results,
                  test_ISR_refdata, test_ISR_results, test_ISR_ownref, test_ISR_lookup,
                  SII_test_data, SII_test_grouped,
                  test_funnel_inputs, test_funnel_outputs, test_funnel_outputs_axis_variation,
                  test_funnel_ratio_inputs, test_funnel_ratio_outputs,
                  test_funnel_rate_inputs, test_funnel_rate_funnels_input,
                  test_funnel_rate_funnels, test_funnel_rate_funnels_2,
                  test_funnel_rate_funnels_3,
                  internal = TRUE, overwrite = TRUE)
