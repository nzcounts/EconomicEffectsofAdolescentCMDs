testthat::test_that("production outcome scope is explicit", {
  testthat::expect_equal(supported_outcome_waves("EducationalAttainment"), c(3L, 4L))
  testthat::expect_equal(supported_outcome_waves("HealthStatus"), c(3L, 4L))
  testthat::expect_equal(supported_outcome_waves("Compensation"), 4L)
  testthat::expect_equal(supported_outcome_waves("LaborForceParticipation"), 4L)
  testthat::expect_equal(supported_outcome_waves("HoursWorked"), 4L)
  testthat::expect_length(supported_outcome_waves("UsualHours"), 0L)
  testthat::expect_length(supported_outcome_waves("MentalHealth"), 0L)
  testthat::expect_length(supported_outcome_waves("SubstanceUse"), 0L)
})

testthat::test_that("Wave IV labor-force participation follows configured routes", {
  d <- data.frame(
    H4LM6 = c(1, 0, 0, 0, 0, 0),
    H4LM11 = c(5, 1, 0, 0, 0, 5),
    H4LM14 = c(95, 95, 1, 4, 10, 95))
  out <- construct_outcome_labor_force_participation(
    d, 4L, cfg$outcome$families$LaborForceParticipation,
    cfg$outcome, NULL)
  testthat::expect_equal(out$Y, c(1L, 1L, 1L, 0L, NA_integer_, NA_integer_))
})

testthat::test_that("Wave IV hours use route-specific fields and cap at 120", {
  d <- data.frame(
    H4LM6 = c(0, 1, 0, 0, 0, 0),
    H4LM11 = c(0, 5, 1, 1, 1, 1),
    H4LM12 = c(95, 1, 2, 98, 2, 1),
    H4LM13 = c(995, 995, 55, 60, 995, 995),
    H4LM19 = c(995, 40, 40, 35, 40, 168))
  out <- construct_outcome_hours_worked(
    d, 4L, cfg$outcome$families$HoursWorked,
    cfg$outcome, NULL)
  testthat::expect_equal(out$Y, c(0, 40, 55, 60, NA_real_, 120))
  testthat::expect_equal(out$uncapped[6], 168)
})

testthat::test_that("unsupported outcome waves fail validation", {
  bad <- cfg
  bad$outcome$family <- "HoursWorked"
  bad$outcome$waves <- 3L
  bad$paths$mortality <- "synthetic-not-read.xpt"
  testthat::expect_error(validate_cfg(bad), "not supported")
})
