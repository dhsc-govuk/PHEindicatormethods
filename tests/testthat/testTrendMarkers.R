# Trend Markers test data

test_that("assigns trend markers correctly", {
  expect_equal(
    select(assign_trend_markers(
      data = test_prop_trends,
      year_col = Year,
      denominator = denominator,
      numerator = Count,
      value_type = "proportion"
    ), 1:6),
    test_prop_trends[1:6],
    ignore_attr = TRUE, info = "test proportions"
  )

  expect_equal(
    select(assign_trend_markers(
      data = test_other_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ), 1:6),
    test_other_trends[1:6],
    ignore_attr = TRUE, info = "test other calculation"
  )

  expect_equal(
    select(assign_trend_markers(
      data = test_other_6_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ), 1:6),
    test_other_6_trends[1:6],
    ignore_attr = TRUE, info = "test other calculation with 6 values"
  )


  expect_equal(
    select(
      test_other_grouped %>%
        group_by(Ethnicity) %>%
        assign_trend_markers(
          value = Value,
          denominator = Denominator,
          numerator = Count,
          year_col = Year,
          value_type = "other",
          multiplier = 1,
          lower_cl = LL,
          upper_cl = UL
        ), 1:7
    ),
    test_other_grouped[1:7],
    ignore_attr = TRUE, info = "test other grouped data"
  )


  expect_equal(
    select(
      test_prop_grouped %>%
        group_by(Ethnicity) %>%
        assign_trend_markers(
          denominator = denominator,
          numerator = Count,
          year_col = Year,
          value_type = "proportion"
        ), 1:7
    ),
    test_prop_grouped[1:7],
    ignore_attr = TRUE, info = "test proportion grouped data"
  )
})


test_that("calculate weighted regression, significance and derive proportion", {
  expect_equal(
    round(calculate_trend_weighted_regression(
      value = test_other_trends$Value, year_col = test_other_trends$Year,
      lower_cl = test_other_trends$LL,
      upper_cl = test_other_trends$UL,
      confidence = 0.95,
      trend_calculation = "significance"
    ), 4),
    test_other_trends[[9]][5],
    ignore_attr = TRUE, info = "test other significance calculation"
  )


  test_other_6_sig <- tail(test_other_6_trends, 5)


  expect_equal(
    round(calculate_trend_weighted_regression(
      value = test_other_6_sig$Value, year_col = test_other_6_sig$Year,
      lower_cl = test_other_6_sig$LL,
      upper_cl = test_other_6_sig$UL,
      confidence = 0.95,
      trend_calculation = "significance"
    ), 4),
    test_other_6_calcs[[9]][5],
    ignore_attr = TRUE, info = "test other significance calculation 6 values"
  )


  expect_equal(
    round(calculate_trend_weighted_regression(
      value = test_other_trends$Value, year_col = test_other_trends$Year,
      lower_cl = test_other_trends$LL,
      upper_cl = test_other_trends$UL,
      confidence = 0.95,
      trend_calculation = "trend_direction"
    ), 4),
    test_other_trends[[10]][5],
    ignore_attr = TRUE, info = "test other trend direction calculation"
  )




  expect_equal(
    round(calculate_trend_weighted_regression(
      value = test_other_6_sig$Value,
      year_col = test_other_6_sig$Year,
      lower_cl = test_other_6_sig$LL,
      upper_cl = test_other_6_sig$UL,
      confidence = 0.95,
      trend_calculation = "trend_direction"
    ), 4),
    test_other_6_calcs[[10]][5],
    ignore_attr = TRUE, info = "test other trend direction calculation 6 values"
  )
})


test_that("calculate logistic regression and significance", {
  expect_equal(
    round(calculate_trend_logistic_regression(
      year_col = test_prop_trends$Year,
      denominator = test_prop_trends$denominator,
      numerator = test_prop_trends$Count,
      trend_calculation = "significance"
    ), 4),
    test_prop_trends[[7]][5],
    ignore_attr = TRUE, info = "test proportion significance"
  )

  expect_equal(
    round(calculate_trend_logistic_regression(
      year_col = test_prop_trends$Year,
      denominator = test_prop_trends$denominator,
      numerator = test_prop_trends$Count,
      trend_calculation = "trend_direction"
    ), 4),
    test_prop_trends[[8]][5],
    ignore_attr = TRUE, info = "test proportion trend direction"
  )

  expect_equal(
    derive_proportion_element(
      data = test_prop_trends,
      denominator = "denominator",
      value = "Value"
    )[[9]],
    test_prop_trends[[2]],
    ignore_attr = TRUE, info = "test proportion - missing numerator"
  )

  expect_equal(
    derive_proportion_element(
      data = test_prop_trends,
      numerator = "Count",
      value = "Value"
    )[3],
    test_prop_trends[3],
    ignore_attr = TRUE, info = "test proportion - missing denominator"
  )
})



# test error handling

test_that("assign_trend_markers - errors are generated when invalid arguments are used", {
  expect_error(
    assign_trend_markers(
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ),
    "function assign_trend_markers requires the arguments: data",
    info = "error missing data"
  )

  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ),
    "function assign_trend_markers requires the year_col field",
    info = "error missing year_col"
  )


  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "dsr",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ),
    'function assign_trend_markers requires the value_type to be either "proportion" or "other"',
    info = "error invalid value_type"
  )

  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      upper_cl = UL
    ),
    'function assign_trend_markers requires "lower_cl" and "upper_cl" when value_type is other',
    info = "error missing lower_cl"
  )

  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      value = Value,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL
    ),
    'function assign_trend_markers requires "lower_cl" and "upper_cl" when value_type is other',
    info = "error missing upper_cl"
  )

  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      denominator = Denominator,
      numerator = Count,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL
    ),
    'function assign_trend_markers requires value when value_type is "other"',
    info = "error missing value"
  )

  expect_error(
    assign_trend_markers(
      data = test_other_trends,
      denominator = denominator,
      numerator = Count,
      value = Value,
      year_col = Year,
      value_type = "other",
      multiplier = 1,
      lower_cl = LL,
      upper_cl = UL,
      confidence = 1000
    ),
    "confidence level must be between 90 and 100 or between 0.9 and 1",
    info = "error CI value"
  )

  expect_error(
    assign_trend_markers(
      data = test_prop_trends,
      year_col = Year,
      value_type = "proportion",
      multiplier = 1
    ),
    'function assign_trend_markers requires at least a numerator or denominator when value_type is "proportion"',
    info = "error missing numerator and denominator"
  )


  expect_error(assign_trend_markers(
    data = test_prop_trends,
    year_col = Year,
    numerator = Count,
    value_type = "proportion",
    "function assign_trend_markers requires value when either numerator or denominator is missing",
    info = "error missing value for numerator"
  ))

  expect_error(assign_trend_markers(
    data = test_prop_trends,
    denominator = denominator,
    year_col = Year,
    value_type = "proportion",
    "function assign_trend_markers requires value when either numerator or denominator is missing",
    info = "error missing value for denominator"
  ))

  expect_error(
    assign_trend_markers(
      data = test_prop_trends,
      denominator = denominatorz,
      numerator = Count,
      year_col = Year,
      value_type = "proportion"
    ),
    "denominator is not a field name from data",
    info = "error missing value for denominator"
  )

  expect_error(
    assign_trend_markers(
      data = test_prop_trends,
      denominator = denominator,
      numerator = Countz,
      year_col = Year,
      value_type = "proportion"
    ),
    "numerator is not a field name from data",
    info = "error missing value for numerator"
  )

  expect_error(
    test_prop_trends %>%
      mutate(Count = "CHAR") %>%
      assign_trend_markers(
        denominator = denominator,
        numerator = Count,
        year_col = Year,
        value_type = "proportion"
      ),
    "field numerator must be numeric",
    info = "error invalid numerator field type"
  )

  expect_error(
    test_other_trends |>
      mutate(Value = "CHAR") |>
      assign_trend_markers(
        denominator = Denominator,
        numerator = Count,
        value = Value,
        year_col = Year,
        value_type = "other",
        multiplier = 1,
        lower_cl = LL,
        upper_cl = UL
      ),
    "field value must be numeric",
    info = "error invalid value field type"
  )

  expect_error(
    test_other_trends |>
      mutate(UL = "CHAR") |>
      assign_trend_markers(
        denominator = Denominator,
        numerator = Count,
        value = Value,
        year_col = Year,
        value_type = "other",
        multiplier = 1,
        lower_cl = LL,
        upper_cl = UL
      ),
    "field upper_cl must be numeric",
    info = "error invalid upper_cl field type"
  )

  expect_error(
    test_other_trends |>
      mutate(LL = "CHAR") |>
      assign_trend_markers(
        denominator = Denominator,
        numerator = Count,
        value = Value,
        year_col = Year,
        value_type = "other",
        multiplier = 1,
        lower_cl = LL,
        upper_cl = UL
      ),
    "field lower_cl must be numeric",
    info = "error invalid lower_cl field type"
  )

  expect_error(
    test_other_trends |>
      mutate(Denominator = "CHAR") |>
      assign_trend_markers(
        denominator = Denominator,
        numerator = Count,
        value = Value,
        year_col = Year,
        value_type = "other",
        multiplier = 1,
        lower_cl = LL,
        upper_cl = UL
      ),
    "field denominator must be numeric",
    info = "error invalid denominator field type"
  )



  test_trend_duplicates <- tail(test_prop_grouped, 5)

  expect_error(
    assign_trend_markers(
      data = test_trend_duplicates,
      value = Value,
      year_col = Year,
      numerator = Count,
      value_type = "proportion"
    ),
    "Year must contain unique values",
    info = "error duplicate year"
  )
})
