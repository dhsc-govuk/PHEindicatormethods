# test calculations
test_that("dsrs and CIs calculate correctly",{

  expect_equal(select(calculate_dsr(
                        test_multiarea_esp, count, pop, stdpop = esp2013,
                      ), 1:6),
               test_DSR_results[5:7, 1:6],
               ignore_attr = TRUE, info = "test default")

  expect_equal(select(calculate_dsr(
                        test_multiarea_esp, count, pop,  stdpop = esp2013,
                        confidence = c(0.95, 0.998)
                      ), 1:8, 10:11),
               test_DSR_results[5:7,],
               ignore_attr = TRUE, info = "test full output with 2 CIs")

  expect_equal(calculate_dsr(test_DSR_1976, count, pop, stdpop = esp1976,
                             type = "standard"),
               select(slice(test_DSR_results, 8), 2:6),
               ignore_attr = TRUE, info = "test with user specified stdpop")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013,
                             type = "standard"),
               select(slice(test_DSR_results, 5:7), 1:6),
               ignore_attr = TRUE, info = "test standard")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             confidence = c(0.95, 0.998), type = "standard"),
               select(slice(test_DSR_results, 5:7), 1:8),
               ignore_attr = TRUE, info = "test standard 2CIs")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             type = "value"),
               select(slice(test_DSR_results, 5:7), 1, 4),
               ignore_attr = TRUE, info = "test value")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             confidence = c(0.95, 0.998), type = "value"),
               select(slice(test_DSR_results, 5:7), 1, 4),
               ignore_attr = TRUE, info = "test value 2CIs")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             type = "lower"),
               select(slice(test_DSR_results, 5:7), 1, 5),
               ignore_attr = TRUE, info = "test lower")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             confidence = c(0.95, 0.998), type = "lower"),
               select(slice(test_DSR_results, 5:7), 1, 5, 7),
               ignore_attr = TRUE, info = "test lower 2 CIs")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             type = "upper"),
               select(slice(test_DSR_results, 5:7), 1, 6),
               ignore_attr = TRUE, info = "test upper")

  expect_equal(calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                             confidence = c(0.95, 0.998), type = "upper"),
               select(slice(test_DSR_results, 5:7), 1, 6, 8),
               ignore_attr = TRUE, info = "test upper 2 CIs")

  expect_equal(
    select(calculate_dsr(test_multiarea_esp, count, pop,
                         stdpop = esp2013, confidence = 99.8), 1:6, 8:9),
    select(slice(test_DSR_results, 5:7), 1:4, 7:10),
    ignore_attr = TRUE, info = "test confidence"
  )

  expect_equal(
    calculate_dsr(test_multiarea_esp, count, pop,
                  stdpop = esp2013, multiplier = 10000, type = "standard"),
    select(slice(test_DSR_results, 1:3), 1:6),
    ignore_attr = TRUE, info = "test multiplier"
  )

  expect_equal(
    data.frame(select(ungroup(
      calculate_dsr(
        test_multiarea_esp, count, pop,
        stdpop = esp2013, confidence = c(0.95, 0.998))), 9)),
    data.frame(confidence = c("95%, 99.8%", "95%, 99.8%", "95%, 99.8%"),
               stringsAsFactors = FALSE),
    ignore_attr = TRUE, info = "test 2 CIs metadata"
  )

  expect_equal(
    unlist(select(ungroup(calculate_dsr(test_multiarea_esp, count, pop,
                                        stdpop = esp2013,)), 7)),
    c("95%", "95%", "95%"),
    ignore_attr = TRUE, info = "test 95% CI metadata"
  )

  expect_equal(
    unlist(select(ungroup(calculate_dsr(test_multiarea_esp, count, pop,
                                        stdpop = esp2013,confidence = 0.998)), 7)),
    c("99.8%", "99.8%", "99.8%"),
    ignore_attr = TRUE, info = "test 99.8% CI metadata"
  )

  expect_equal(
    select(ungroup(
      calculate_dsr(test_DSR_nonind, persons, pop,  stdpop = esp2013,
                    confidence = c(0.95, 0.998), independent_events = FALSE,
                    eventfreq = freq, ageband = ageband)
    ), 1:8, 10:11),
    test_DSR_results[9:12,],
    ignore_attr = TRUE, info = "test nonindependent events no frequendy grouping and 2 CIs"
  )

  expect_equal(
    select(ungroup(
      calculate_dsr(group_by(test_DSR_nonind, freq, .add = TRUE),
                    persons, pop,  stdpop = esp2013,
                    independent_events = FALSE,
                    eventfreq = freq, ageband = ageband)
    ), 1:6, 8:9),
    select(test_DSR_results[9:12,], 1:6, 9:10),
    ignore_attr = TRUE, info = "test nonindependent events with frequency grouping and 1 CI"
  )

  expect_equal(
    names(calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013))[5:6],
    c("lowercl", "uppercl"),
  ignore_attr = TRUE, info = "test ci column names for single CI")

  expect_equal(
    names(calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013,
                        confidence = c(0.95, 0.998)))[5:8],
    c("lower95_0cl", "upper95_0cl", "lower99_8cl", "upper99_8cl"),
    ignore_attr = TRUE, info = "test ci column names for two CIs")

})




# test error handling

test_that("dsrs - errors are generated when invalid arguments are used",{

  # data, x, n, stdpop ---------------------------------------------------------

  expect_error(calculate_dsr(test_multiarea_esp, count, pop),
                "function calculate_dsr requires at least 4 arguments: data, x, n, stdpop",
               info = "error invalid number of arguments")

  expect_error(calculate_dsr(test_multiarea_esp, count, stdpop = stdpop),
               "function calculate_dsr requires at least 4 arguments: data, x, n, stdpop",
               info = "error invalid number of arguments")

  expect_error(calculate_dsr(test_multiarea_esp, n = pop, stdpop = stdpop),
               "function calculate_dsr requires at least 4 arguments: data, x, n, stdpop",
               info = "error invalid number of arguments")

  expect_error(calculate_dsr("a", count, pop, stdpop = stdpop),
               "data must be a data frame object",
               info = "error invalid data")

  expect_error(calculate_dsr(test_multiarea_esp, countX, pop, stdpop = stdpop),
               "x is not a field name from data",
               info = "error invalid count field")

  expect_error(calculate_dsr(test_multiarea_esp, count, popX, stdpop = stdpop),
               "n is not a field name from data",
               info = "error invalid pop field")

  expect_error(
    calculate_dsr(test_DSR_1976, count, pop, stdpop = esp),
    "stdpop is not a field name from data",
    info = "error stdpop field doesn't exist"
  )

  expect_error(
    test_multiarea_esp |>
      mutate(count = "CHAR") |>
      calculate_dsr(count, pop, stdpop = esp2013),
    "field x must be numeric",
    info = "error invalid count field type"
  )

  expect_error(
    test_multiarea_esp |>
      mutate(pop = "CHAR") |>
      calculate_dsr(count, pop, stdpop = esp2013),
    "field n must be numeric",
    info = "error invalid pop field type"
  )

  expect_error(
    test_multiarea_esp |>
      mutate(esp2013 = "CHAR") |>
      calculate_dsr(count, pop, stdpop = esp2013),
    "field stdpop must be numeric",
    info = "error invalid stdpop field type"
  )

  expect_error(
    test_multiarea_esp |>
      mutate(pop = NA_real_) |>
      calculate_dsr(count, pop, stdpop = esp2013),
    "field n cannot have missing values",
    info = "error pop field missing values"
  )

  expect_error(
    test_multiarea_esp |>
      mutate(esp2013 = NA_real_) |>
      calculate_dsr(count, pop, stdpop = esp2013),
    "field stdpop cannot have missing values",
    info = "error stdpop field missing values"
  )

  expect_error(calculate_dsr(test_err1, count, pop, stdpop = stdpop),
               "numerators must all be greater than or equal to zero",
               info = "error numerators < 0")

  expect_error(calculate_dsr(test_err2_esp, count, pop, stdpop = esp2013),
               "denominators must all be greater than zero",
               info = "error denominator = 0")

  expect_error(calculate_dsr(test_err3, count, pop, stdpop = stdpop),
               "denominators must all be greater than zero",
               info = "error denominator < 0")

  expect_error(calculate_dsr(test_err4, count, pop, stdpop = stdpop),
               "stdpop must all be greater than or equal to zero",
               info = "error denominator < 0")


  # Confidence -----------------------------------------------------------------

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013, confidence = "0.95"),
    "confidence must be numeric",
    info = "error confidence not numeric"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013, confidence = 0.74),
    "confidence level must be between 90 and 100 or between 0.9 and 1",
    info = "error confidence < 0.9"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013, confidence = 3),
    "confidence level must be between 90 and 100 or between 0.9 and 1",
    info = "error confidence between 1 and 90"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013, confidence = 1000),
    "confidence level must be between 90 and 100 or between 0.9 and 1",
    info = "error confidence >100"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                  confidence = c(90.95, 0.998, 1.00)),
    "a maximum of two confidence levels can be provided",
    info = "error more than 2 CIs requested"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop,  stdpop = esp2013,
                  confidence = c(0.95, 1.00)),
    "two confidence levels can only be produced if they are specified as 0.95 and 0.998",
    info = "error 2 CIs aren't 0.95 and 0.998"
  )


  # type -----------------------------------------------------------------------

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013, type = "combined"),
    "type must be one of value, lower, upper, standard or full",
    info = "error invalid type"
  )


  # multiplier -----------------------------------------------------------------

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013,
                  confidence = 0.95, multiplier = "1000"),
    "multiplier must be numeric",
    info = "error multiplier not numeric"
  )

  expect_error(
    calculate_dsr(test_multiarea_esp, count, pop, stdpop = esp2013,
                  confidence = 0.95, multiplier = -1),
    "multiplier must be greater than 0",
    info = "error multiplier less than 0"
  )


  # independent_events, eventfreq, ageband -------------------------------------

  expect_error(
    calculate_dsr(test_DSR_nonind, persons, pop, stdpop = esp2013,
                  independent_events = "a", eventfreq = freq,
                  ageband = ageband),
    "independent_events must be TRUE or FALSE",
    info = "error independent_events not bool"
  )

  expect_error(
    calculate_dsr(test_DSR_nonind, persons, pop, stdpop = esp2013,
                  independent_events = FALSE, ageband = ageband),
    paste0("function calculate_dsr requires an eventfreq column ",
           "to be specified when independent_events is FALSE"),
    info = "error eventfreq not specified"
  )

  expect_error(
    calculate_dsr(test_DSR_nonind, persons, pop, stdpop = esp2013,
                  independent_events = FALSE, eventfreq = freq),
    paste0("function calculate_dsr requires an ageband column ",
           "to be specified when independent_events is FALSE"),
    info = "error ageband not specified"
  )

  expect_error(
    calculate_dsr(test_DSR_nonind, persons, pop, stdpop = esp2013,
                  independent_events = FALSE, eventfreq = efreq, ageband = ageband),
    "eventfreq is not a field name from data",
    info = "error eventfreq field doesn't exist"
  )

  expect_error(
    test_DSR_nonind |>
      mutate(freq = "CHAR") |>
    calculate_dsr(persons, pop, stdpop = esp2013, independent_events = FALSE,
                  eventfreq = freq, ageband = ageband),
    "eventfreq field must be numeric",
    info = "error eventfreq field wrong type"
  )

  expect_error(
    calculate_dsr(test_DSR_nonind, persons, pop, stdpop = esp2013,
                  independent_events = FALSE, eventfreq = freq, ageband = ab),
    "ageband is not a field name from data",
    info = "error ageband field doesn't exist"
  )

  expect_error(
    test_DSR_nonind |>
      mutate(freq = NA_real_) |>
      calculate_dsr(persons, pop, stdpop = esp2013, independent_events = FALSE,
                    eventfreq = freq, ageband = ageband),
    "eventfreq field must not have any missing values",
    info = "error eventfreq field missing values"
  )

  expect_error(
    test_DSR_nonind |>
      mutate(ageband = NA_real_) |>
      calculate_dsr(persons, pop, stdpop = esp2013, independent_events = FALSE,
                    eventfreq = freq, ageband = ageband),
    "ageband field must not have any missing values",
    info = "error ageband field missing values"
  )

  expect_error(
      calculate_dsr(test_DSR_nonind_err1, persons, pop, stdpop = esp2013,
                    independent_events = FALSE, eventfreq = freq,
                    ageband = ageband),
    "There are rows with the same grouping variables and ageband but with different",
    info = "error non-consistent pop"
  )

  expect_error(
    calculate_dsr(test_DSR_nonind_err2, persons, pop, stdpop = esp2013,
                  independent_events = FALSE, eventfreq = freq,
                  ageband = ageband),
    "There are rows with the same grouping variables and ageband but with different",
    info = "error non-consistent stdpop"
  )

})


