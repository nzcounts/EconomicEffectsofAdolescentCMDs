project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                              winslash = "/", mustWork = TRUE)
source(file.path(project_root, "scripts", "bootstrap.R"), local = .GlobalEnv)
source_addhealth_pipeline(project_root, envir = .GlobalEnv)
