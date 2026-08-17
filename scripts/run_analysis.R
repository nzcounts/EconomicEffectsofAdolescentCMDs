#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[1L]) else NA_character_
script_dir <- if (!is.na(script_path) && file.exists(script_path))
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)) else getwd()
source(file.path(script_dir, "bootstrap.R"), local = .GlobalEnv)
project_root <- find_addhealth_project_root(script_dir)
source_addhealth_pipeline(project_root)

args <- commandArgs(trailingOnly = TRUE)
config_flag <- grep("^--config=", args, value = TRUE)
config_path <- if (length(config_flag)) sub("^--config=", "", config_flag[1L])
  else file.path(project_root, "config", "config.yml")
if (!grepl("^(/|[A-Za-z]:[/\\\\])", config_path))
  config_path <- file.path(project_root, config_path)

cfg <- read_project_config(cfg, config_path, project_root = project_root)
cfg$global$autorun_pipeline <- FALSE
assert_restricted_inputs_present(cfg)

if (isTRUE(cfg$stages$run_multiseed_att)) {
  results <- run_multiseed_att(cfg, seeds = cfg$global$multiseed_seeds)
} else {
  results <- run_addhealth_pipeline(cfg)
}

invisible(results)
