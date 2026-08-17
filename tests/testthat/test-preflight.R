testthat::test_that("full synthetic production preflight succeeds", {
  testthat::skip_if(Sys.getenv("RUN_FULL_PREFLIGHT") != "true",
                    "Set RUN_FULL_PREFLIGHT=true with the frozen learner stack.")
  load_required_packages(cfg)
  cfg$paths <- setNames(lapply(names(cfg$paths), function(name)
    file.path(tempdir(), paste0(name, ".synthetic-not-read.xpt"))), names(cfg$paths))
  cfg$global$pipeline_source_path <- file.path(project_root, "R")
  cfg$global$output_dir <- tempfile(pattern = "addhealth_test_preflight_")
  cfg$safety$require_publication_ready_marker <- FALSE
  testthat::expect_true(isTRUE(run_preflight_unit_test(cfg)))
})
