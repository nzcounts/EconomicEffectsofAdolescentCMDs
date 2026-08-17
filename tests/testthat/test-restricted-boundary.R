testthat::test_that("no restricted or derived data artifacts are versioned", {
  files <- list.files(project_root, recursive = TRUE, all.files = TRUE,
                      full.names = FALSE)
  files <- files[!grepl("(^|/)\\.git(/|$)", files)]
  forbidden <- "\\.(xpt|sas7bdat|dta|sav|rds|rda|RData)$"
  testthat::expect_false(any(grepl(forbidden, files, ignore.case = TRUE)))
  testthat::expect_false(file.exists(file.path(project_root, "config", "config.yml")))
})
