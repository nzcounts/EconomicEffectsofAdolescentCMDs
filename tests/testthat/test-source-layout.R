testthat::test_that("public defaults contain no local restricted-data paths", {
  testthat::expect_true(all(vapply(cfg$paths, function(path)
    length(path) == 1L && is.na(path), logical(1))))
  r_text <- unlist(lapply(list.files(file.path(project_root, "R"), full.names = TRUE),
                          readLines, warn = FALSE), use.names = FALSE)
  testthat::expect_false(any(grepl("M:/AddHealth", r_text, fixed = TRUE)))
  testthat::expect_false(isTRUE(cfg$global$autorun_pipeline))
})

testthat::test_that("the modular source tree has a stable directory fingerprint", {
  fingerprint <- file_fingerprint(file.path(project_root, "R"))
  testthat::expect_true(isTRUE(fingerprint$exists))
  testthat::expect_equal(fingerprint$n_files, 16L)
  testthat::expect_match(fingerprint$md5, "^[0-9a-f]{32}$")
})
