#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[1L]) else NA_character_
script_dir <- if (!is.na(script_path) && file.exists(script_path))
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)) else getwd()
source(file.path(script_dir, "bootstrap.R"), local = .GlobalEnv)
project_root <- find_addhealth_project_root(script_dir)
source_addhealth_pipeline(project_root)

# The preflight uses synthetic respondents and never reads these paths, but the
# production configuration validator requires a nonblank mortality path.
cfg$paths <- setNames(lapply(names(cfg$paths), function(name)
  file.path(tempdir(), paste0(name, ".synthetic-not-read.xpt"))), names(cfg$paths))
cfg$global$pipeline_source_path <- file.path(project_root, "R")
cfg$global$autorun_pipeline <- FALSE
cfg$global$output_dir <- tempfile(pattern = "addhealth_preflight_")
cfg$safety$require_publication_ready_marker <- FALSE

validate_cfg(cfg)
load_required_packages(cfg)
run_preflight_unit_test(cfg)
