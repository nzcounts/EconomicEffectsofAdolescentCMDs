#!/usr/bin/env Rscript

if (!requireNamespace("testthat", quietly = TRUE))
  stop("Package 'testthat' is required.", call. = FALSE)
testthat::test_dir("tests/testthat", reporter = "summary")
