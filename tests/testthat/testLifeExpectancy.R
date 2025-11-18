# dps to test to
n <- 4

df1 <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                               60L, 65L, 70L, 75L, 80L, 85L, 90L),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                           37039L, 33288L, 23306L, 11936L, 11936L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))

df1_cum <- data.frame(pops_used = c(787954,	780894,	745835,	698861,	650372,	607153,	568592,
                                    522583,	465375,	403940,	348339,	298130,	241714,	195303,
                                    155483,	117505,	80466,	47178,	23872,	11936),
                      dths_used = c(9357,	9340,	9331,	9327,	9319,	9299,	9284,
                                    9260,	9227,	9177,	9106,	9006,	8843,	8580,
                                    8276,	7740,	6868,	5478,	3873,	1937))

df2 <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                               60L, 70L, 75L, 80L, 85L, 90L, 65L),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37039L,
                           33288L, 23306L, 11936L, 11936L, 37978L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 872L, 1390L, 1605L, 1936L, 1937L, 536L))

df3 <- data.frame(startage = c("0", "1-4", "5-9", "10 – 14", "15 – 19", "20 – 24", "25 – 29",
                               "30 – 34", "35 – 39", "40 – 44", "45 – 49", "50 – 54",
                               "55 – 59", "60 – 64", "65 – 69", "75 – 79", "80 – 84",
                               "85 – 89", "90 +", "70 – 74"),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                           33288L, 23306L, 11936L, 11936L, 37039L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 536L, 1390L, 1605L, 1936L, 1937L, 872L))
df_neg_deaths <- data.frame(age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                    60L, 65L, 70L, 75L, 80L, 85L, 90L),
                            pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                                    61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                                    37039L, 33288L, 23306L, 11936L, 11936L),
                            deaths = c(17L, 9L, 4L, -5L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                       263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
df_zero_pop <- data.frame(age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                  60L, 65L, 70L, 75L, 80L, 85L, 90L),
                          pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 0L, 46009L, 57208L,
                                  61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                                  37039L, 33288L, 23306L, 11936L, 11936L),
                          deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                     263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
df_deaths_greater_pops <- data.frame(age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                                  60L, 65L, 70L, 75L, 80L, 85L, 90L),
                                     pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                                              61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                                              37039L, 33288L, 23306L, 119L, 11936L),
                                     deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                                263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
df_missing_age <- data.frame(age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                     60L, 65L, 70L, 75L, 80L, 85L),
                             pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                                     61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                                     37039L, 33288L, 23306L, 11936L),
                             deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                        263L, 304L, 536L, 872L, 1390L, 1605L, 1936L))
df_low_pops <- data.frame(stringsAsFactors=FALSE,
                          age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                  60L, 65L, 70L, 75L, 80L, 85L, 90L),
                          pop = c(128L, 152L, 120L, 176L, 194L, 180L, 145L, 149L, 107L, 185L,
                                  165L, 109L, 100L, 122L, 133L, 189L, 123L, 121L, 147L,
                                  138L),
                          deaths = c(58L, 93L, 78L, 94L, 59L, 71L, 80L, 73L, 69L, 72L, 91L, 69L,
                                     78L, 71L, 54L, 91L, 82L, 53L, 50L, 84L))
df_widecis_plus <- data.frame(area = c(rep("Area 1", 20), rep("Area 2", 20)),
                 startage = rep(c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                  60L, 65L, 70L, 75L, 80L, 85L, 90L), 2),
                 pops = rep(c(270L, 235L, 246L, 248L, 243L, 238L, 246L, 257L,
                              261L, 355L, 350L, 356L, 346L, 339L, 337L,
                              337L, 333L, 323L, 311L, 311L), 2),
                 deaths = rep(c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 51L, 10L, 16L,
                                26L, 30L, 36L, 22L, 13L, 5L, 6L, 1L), 2)) %>%
  group_by(area)

df_widecis_plus$deaths[df_widecis_plus$area == "Area 1" & df_widecis_plus$startage == 90] <- 312


df_grouped_with_warnings <- data.frame(stringsAsFactors=FALSE,
                                       area = c(rep("Good data", 20),
                                                rep("Negative deaths", 20),
                                                rep("Negative pops", 20),
                                                rep("Deaths more than pops", 20),
                                                rep("Low pops", 20),
                                                rep("Missing age band", 19)),
                                       age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                                               60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L, 20L,
                                               25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L,
                                               90L, 0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L,
                                               55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L,
                                               20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L,
                                               85L, 90L, 0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L,
                                               55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L,
                                               20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L,
                                               85L),
                                       pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 43219L, 46009L, 57208L,
                                               61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                                               37039L, 33288L, 23306L, 11936L, 11936L, 7060L, 35059L, 46974L, 48489L,
                                               43219L, 43219L, 46009L, 57208L, 61435L, 55601L, 50209L, 56416L,
                                               46411L, 39820L, 37978L, 37039L, 33288L, 23306L, 11936L, 11936L,
                                               7060L, 35059L, 46974L, -10L, 43219L, 43219L, 46009L, 57208L,
                                               61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L, 37039L,
                                               33288L, 23306L, 11936L, 11936L, 7060L, 35059L, 46974L, 48489L,
                                               43219L, 43219L, 46009L, 57208L, 61435L, 55601L, 50209L, 56416L,
                                               46411L, 39820L, 37978L, 37039L, 33288L, 23306L, 11936L, 11936L,
                                               100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L,
                                               100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 7060L,
                                               35059L, 46974L, 48489L, 43219L, 43219L, 46009L, 57208L, 61435L,
                                               55601L, 50209L, 56416L, 46411L, 39820L, 37978L, 37039L, 33288L,
                                               23306L, 11936L),
                                       deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                                                  263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L, 17L, 9L,
                                                  -2L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L,
                                                  536L, 872L, 1390L, 1605L, 1936L, 1937L, 17L, 9L, 4L, 8L, 20L, 15L,
                                                  24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L, 1390L,
                                                  1605L, 1936L, 1937L, 17L, 9L, 4L, 8L, 20L, 50000L, 24L, 33L,
                                                  50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L, 1390L, 1605L, 1936L,
                                                  1937L, 17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 80L, 80L,
                                                  80L, 80L, 80L, 80L, 80L, 80L, 80L, 80L, 17L, 9L, 4L, 8L, 20L,
                                                  15L, 24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L,
                                                  1390L, 1605L, 1936L))

answer1 <- round(data.frame(value = c(80.16960813, 79.36245674, 75.44193645, 70.47299936,
                                      65.52909542, 60.67510088, 55.78835987, 50.92752727,
                                      46.06740425, 41.24505636, 36.49322717, 31.83343842, 27.26027992,
                                      22.9719144, 18.76856852, 14.95858336, 11.51684445, 8.618849241,
                                      6.163942934, 6.16210635),
                            lowercl = c(79.88268548, 79.0898816, 75.17407307, 70.20675732,
                                        65.26548673, 60.41876577, 55.53801579, 50.68279939,
                                        45.82666794, 41.00842831, 36.26223216, 31.60996318, 27.04337271,
                                        22.76653134, 18.57570352, 14.77985004, 11.34831778, 8.451134045,
                                        5.978358351, 5.887683386),
                            uppercl = c(80.45653078, 79.63503188, 75.70979983, 70.7392414,
                                        65.79270412, 60.931436, 56.03870395, 51.17225516, 46.30814055,
                                        41.48168441, 36.72422219, 32.05691365, 27.47718713,
                                        23.17729746, 18.96143352, 15.13731667, 11.68537112, 8.786564437,
                                        6.349527517, 6.436529314)),
                 n)




answer2 <- cbind(df1[c(3, 7),],
                 round(answer1[c(3, 7),], n),
                 df1_cum[c(3, 7),],
                 data.frame(stringsAsFactors = FALSE,
                            confidence  = rep("95%", 2),
                            statistic = paste("life expectancy at", c(5, 25)),
                            method = rep("Chiang, using Silcocks et al for confidence limits", 2))) %>%
                select(-pops, -deaths)

answer_widecis <- round(tibble(value = c(19.22914685, 19.44120610, 18.33467808, 14.67659995,
                                         11.81284556, 11.63815414, 10.05641628, 9.93152356,
                                         11.95846561,
                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                               lower95_0cl = c(16.43900644, 16.53437505, 15.62922550,
                                               12.08108630, 9.21823373, 8.68414661,
                                               6.54020385, 4.71570153, 2.37762283,
                                               -1.60695210, -10.33328815, -17.16409724,
                                               -26.91715103, -45.04820928, -76.27067271,
                                               -137.12203187, -195.39453664, -242.37480291,
                                               -266.74198466, -298.54880400,
                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                               upper95_0cl = c(22.01928726, 22.34803716, 21.04013066,
                                               17.27211361, 14.40745738, 14.59216167,
                                               13.57262872, 15.14734559, 21.53930839,
                                               50.29765396, 97.99598549, 107.54413263,
                                               128.91238524, 181.91668353, 278.06441642,
                                               472.32822039, 649.40455756, 783.50845564,
                                               841.04259816, 920.54880400,
                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
                        n)

test1 <- phe_life_expectancy(df1, deaths, pops, startage, type="standard")
test1.1 <- phe_life_expectancy(df1, deaths, pops, startage, confidence = 95)
test2 <- phe_life_expectancy(df2, deaths, pops, startage)
test3 <- df1 %>%
  mutate(area = "test") %>%
  group_by(area) %>%
  phe_life_expectancy(deaths, pops, startage)
test4 <- phe_life_expectancy(df3, deaths, pops, startage,
                             age_contents = c("0", "1-4", "5-9",
                                              "10 – 14", "15 – 19",
                                              "20 – 24", "25 – 29",
                                              "30 – 34", "35 – 39",
                                              "40 – 44", "45 – 49",
                                              "50 – 54", "55 – 59",
                                              "60 – 64", "65 – 69",
                                              "70 – 74", "75 – 79",
                                              "80 – 84", "85 – 89",
                                              "90 +"))
test5 <- phe_life_expectancy(df1, deaths, pops, startage, le_age = 5)
test6 <- phe_life_expectancy(df1, deaths, pops, startage, le_age = c(5, 25), type="full") %>%
  mutate_at(c("value", "lowercl", "uppercl"), round, digits = n)

test7 <- phe_life_expectancy(df1, deaths, pops, startage, confidence = 99.8)
test8 <- phe_life_expectancy(df1, deaths, pops, startage, confidence = c(95, 99.8))

negative_warning <- capture_warnings(
  test_neg <- phe_life_expectancy(df_neg_deaths, deaths, pop, age))
zero_warning <- capture_warnings(
  test_zero_pop <- phe_life_expectancy(df_zero_pop, deaths, pop, age))
deaths_pops_warning <- capture_warnings(
  test_greater_than_pops <- phe_life_expectancy(df_deaths_greater_pops, deaths, pop, age))
low_pops_warning <- capture_warnings(
  test_low_pops <- phe_life_expectancy(df_low_pops, deaths, pop, age))

age_contents_short <- c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L,
                        40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L)

missing_warning     <- capture_warnings(
  test_missing_ageband <- phe_life_expectancy(df_missing_age, deaths, pop, age,
                                              age_contents = age_contents_short))
wideci_warning <- capture_warnings(
  test_widecis <- phe_life_expectancy(df_widecis_plus, deaths, pops, startage, confidence = c(0.95, 0.998)))
multi_warnings      <- capture_warnings(
  test_grouped_with_warnings <- df_grouped_with_warnings %>%
                                    group_by(area) %>%
                                    phe_life_expectancy(deaths, pop, age)
  )

cols_to_test <- c("value", "lowercl", "uppercl")
expected_num_cols <- 9

#test calculations
test_that("LE and CIs calculate correctly",{
  expect_equal(round(test1[, cols_to_test], n), round(answer1, n),
               info = "test defaults but with type standard")
  expect_equal(round(test1.1[, cols_to_test], n), round(answer1, n),
               info = "test confidence = 95")
  expect_equal(round(test2[, cols_to_test], n), round(answer1, n),
               info = "incorrect ageband order")
  expect_equal(round(test3[, cols_to_test], n), as_tibble(round(answer1, n)),
               info = "single area grouping")
  expect_equal(round(test4[, cols_to_test], n), round(answer1, n),
               info = "custom age bands in wrong order")
  expect_equal(round(test5[, cols_to_test], n), round(answer1[3, ], n),
               ignore_attr = TRUE, #because the row names are different and we are only interested in values
               info = "return single age band")
  expect_equal(test6, answer2,
               ignore_attr = TRUE, #because the row names are different and we are only interested in values
               info = "type = 'full' with two filters")
  expect_equal(sum(!is.na(test_neg[, cols_to_test])), 0,
               info = "negative deaths produces only NAs")
  expect_equal(sum(!is.na(test_zero_pop[, cols_to_test])), 0,
               info = "zero in pop age band produces only NAs")
  expect_equal(sum(!is.na(test_greater_than_pops[, cols_to_test])), 0,
               info = "deaths in age band greater than pops produces only NAs")
  expect_equal(sum(!is.na(test_missing_ageband[, cols_to_test])), 0,
               info = "missing age band produces only NAs")
  expect_equal(nrow(test_grouped_with_warnings), nrow(df_grouped_with_warnings),
               info = "correct number of rows for grouped calcs")
  expect_equal(test7[, c("lowercl", "uppercl")],
               test8[, c("lower99_8cl", "upper99_8cl")],
               ignore_attr = TRUE)
  expect_equal(round(test_widecis[, c("value", "lower95_0cl", "upper95_0cl")], n),
               round(answer_widecis, n),
               info = "suppress wide CI > 20")

})

# test that correct columns are output
test_that("LE - correct column numbers are output",{
  expect_equal(ncol(test1), expected_num_cols - 5)
  expect_equal(ncol(test2), expected_num_cols)
  expect_equal(ncol(test8), expected_num_cols - 2 + (2 * 2))
  expect_equal(ncol(phe_life_expectancy(df1, deaths, pops, startage, confidence = 90:99)),
               expected_num_cols - 2 + (2 * length(90:99)))
})

# test that output is in correct format
test_that("LE - correct output format",{
  expect_true(is.data.frame(test1),   info = "test1 is dataframe format")
  expect_true(is.data.frame(test1.1), info = "test1.1 is dataframe format")
  expect_true(is.data.frame(test2),   info = "test2 is dataframe format")
  expect_true(is.data.frame(test3),   info = "test3 is dataframe format")
  expect_true(is.data.frame(test4),   info = "test4 is dataframe format")
  expect_true(is.data.frame(test5),   info = "test5 is dataframe format")
  expect_true(is.data.frame(test6),   info = "test6 is dataframe format")
  expect_true(is.data.frame(test7),   info = "test7 is dataframe format")
  expect_true(is.data.frame(test8),   info = "test8 is dataframe format")
  expect_true(is.data.frame(test_grouped_with_warnings),   info = "test_grouped_with_warnings is dataframe format")
  expect_equal(group_vars(test3), c("area"), info = "test3 output is grouped by area")
})

# test warnings
test_that("LE - warnings are generated when invalid arguments are used",{
  expect_warning(phe_life_expectancy(df1, deaths, pops, startage, le_age = 4),
                 "le_age not in the vector described by age_contents; all life expectancies will be returned")
  expect_warning(phe_life_expectancy(df1, deaths, pops, startage, le_age = c(4, 6)),
                 "le_age not in the vector described by age_contents; all life expectancies will be returned")
  expect_match(negative_warning,
               "some age bands have negative deaths; outputs have been suppressed to NAs")
  expect_match(zero_warning,
               "some age bands have a zero or less population; outputs have been suppressed to NAs")
  expect_match(deaths_pops_warning,
               "some age bands have more deaths than population; outputs have been suppressed to NAs")
  expect_match(low_pops_warning,
               "some groups have a total population of less than 5,000; outputs have been suppressed to NAs")
  expect_match(missing_warning,
               "some groups contain a different number of age bands than 20; life expectancy cannot be calculated for these\\. These groups will contain NAs\\.")
  expect_match(wideci_warning,
               "some age bands have more deaths than population; outputs have been suppressed to NAs",
               all = FALSE)
  expect_match(wideci_warning,
               "some life expectancy values have a 95% confidence interval > 20 years; these values have been suppressed to NAs",
               all = FALSE)
  expect_match(multi_warnings, "some age bands have negative deaths; outputs have been suppressed to NAs",
               all = FALSE)
  expect_match(multi_warnings, "some age bands have a zero or less population; outputs have been suppressed to NAs",
               all = FALSE)
  expect_match(multi_warnings, "some groups contain a different number of age bands than 20; life expectancy cannot be calculated for these\\. These groups will contain NAs\\.",
               all = FALSE)
  expect_match(multi_warnings, "some age bands have more deaths than population; outputs have been suppressed to NAs",
               all = FALSE)
  expect_match(multi_warnings, "some groups have a total population of less than 5,000; outputs have been suppressed to NAs",
               all = FALSE)
})


# test error handling

test_that("LE - errors are generated when invalid arguments are used",{
  expect_error(phe_life_expectancy(df3, deaths, pops, startage),
               "the contents in the startage field do not match the contents of the age_contents vector")
  expect_error(phe_life_expectancy(df3, deaths, pops, startage,
                                   age_contents = c("0", "1 4", "5 9",
                                                    "20 24", "25 29",
                                                    "10 14", "15 19",
                                                    "30 34", "35 39",
                                                    "40 44", "45 49",
                                                    "50 54", "55 59",
                                                    "60 64", "65 69",
                                                    "70 74", "75 79",
                                                    "80 84", "85 89",
                                                    "90+")),
               "age_contents doesn't appear to be in ascending order; the following age bands appear out of position: 20 24, 25 29, 10 14, 15 19")
  expect_error(phe_life_expectancy(),
               "function life_expectancy requires at least 4 arguments: data, deaths, population, startage")
  expect_error(phe_life_expectancy(df1, deaths, pop, age,
                                   age_contents = c(1L, 0L, seq(5, 90, by = 5))),
               "first age band in age_contents must be 0")
  expect_error(phe_life_expectancy(df1, deaths, pop, age,
                                   confidence = 0.8),
               "all confidence levels must be between 90 and 100 or between 0.9 and 1")

})
