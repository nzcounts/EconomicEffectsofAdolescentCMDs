testthat::test_that("mortality timing respects interview month and fallback endpoint", {
  c4 <- cfg
  c4$outcome$current_wave <- 4L
  mc <- resolve_mortality_spec(c4, 4L)
  classified <- classify_mortality_timing(
    raw_window = rep(1L, 5),
    death_year = c(2008L, 2008L, 2008L, 2009L, 2009L),
    death_month = c(5L, 6L, 7L, 1L, NA_integer_),
    interview_year = c(2008L, 2008L, 2008L, NA_integer_, NA_integer_),
    interview_month = c(6L, 6L, 6L, NA_integer_, NA_integer_),
    mc = mc,
    fieldwork_start_year = 2008L,
    fieldwork_start_month = 6L,
    fieldwork_end_year = 2009L,
    fieldwork_end_month = 1L)
  testthat::expect_equal(classified$death, c(1L, 0L, 0L, 1L, 0L))
  testthat::expect_equal(classified$status[2], "same_interview_month_unordered")
  testthat::expect_equal(
    classified$status[5],
    "fieldwork_end_year_death_month_unknown_no_interview")
})

testthat::test_that("mortality month 997 remains unknown rather than no death", {
  c4 <- cfg
  c4$outcome$current_wave <- 4L
  mortality <- data.frame(AID = 1:2,
                          NDIDD19Y = c(2008L, 99997L),
                          NDIDD19M = c(997L, 997L))
  derived <- derive_mortality_indicator_from_data(mortality, c4)
  testthat::expect_equal(derived$data$NDIY4, c(2008L, NA_integer_))
  testthat::expect_true(is.na(derived$data$NDIM4[1]))
  testthat::expect_equal(derived$data$D4Raw, c(1L, 0L))
  testthat::expect_equal(derived$audit$n_997_death_months, 1)
})
