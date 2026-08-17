# Locate and source the modular analysis in a deterministic order.

find_addhealth_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    project_files <- list.files(current, pattern = "[.]Rproj$", full.names = TRUE)
    if (length(project_files) == 1L &&
        dir.exists(file.path(current, "R")) &&
        dir.exists(file.path(current, "scripts"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current))
      stop("Could not locate the project root from: ", start, call. = FALSE)
    current <- parent
  }
}

source_addhealth_pipeline <- function(project_root = find_addhealth_project_root(),
                                      envir = .GlobalEnv) {
  module_dir <- file.path(project_root, "R")
  modules <- sort(list.files(module_dir, pattern = "^[0-9]{2}_.+\\.[Rr]$",
                             full.names = TRUE))
  if (!length(modules))
    stop("No numbered R modules found in: ", module_dir, call. = FALSE)
  invisible(lapply(modules, sys.source, envir = envir, chdir = FALSE))
  modules
}
