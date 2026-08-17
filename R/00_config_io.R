# Repository-specific configuration helpers.
#
# The analysis defaults remain in 02_config.R. Machine-specific paths and any
# deliberate run overrides are supplied in an ignored YAML file and merged only
# after every production module has been sourced.

assert_known_config_overlay <- function(base, overlay, location = "cfg") {
  if (is.null(overlay)) return(invisible(TRUE))
  if (!is.list(overlay) || is.null(names(overlay)) || any(!nzchar(names(overlay))))
    stop("Configuration overlay at ", location,
         " must be a named YAML mapping.", call. = FALSE)

  unknown <- setdiff(names(overlay), names(base))
  if (length(unknown))
    stop("Unknown configuration key(s) at ", location, ": ",
         paste(unknown, collapse = ", "), call. = FALSE)

  for (key in names(overlay)) {
    if (is.list(overlay[[key]]) && !is.null(overlay[[key]]) &&
        is.list(base[[key]]) && !is.null(base[[key]]))
      assert_known_config_overlay(base[[key]], overlay[[key]],
                                  paste0(location, "$", key))
  }
  invisible(TRUE)
}

merge_config_overlay <- function(base, overlay) {
  if (is.null(overlay)) return(base)
  for (key in names(overlay)) {
    if (is.list(overlay[[key]]) && !is.null(overlay[[key]]) &&
        is.list(base[[key]]) && !is.null(base[[key]]))
      base[[key]] <- merge_config_overlay(base[[key]], overlay[[key]])
    else
      base[[key]] <- overlay[[key]]
  }
  base
}

normalize_config_path <- function(path, base_dir) {
  if (is.null(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(as.character(path)))) return(NA_character_)
  path <- path.expand(as.character(path))
  absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", path)
  candidate <- if (absolute) path else file.path(base_dir, path)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

read_project_config <- function(default_cfg, path, project_root = getwd(),
                                strict_keys = TRUE) {
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Package 'yaml' is required to read the project configuration.",
         call. = FALSE)
  if (!file.exists(path))
    stop("Configuration file does not exist: ", path,
         ". Copy config/config.example.yml to config/config.yml and edit it.",
         call. = FALSE)

  config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  overlay <- yaml::read_yaml(config_path)
  if (isTRUE(strict_keys)) assert_known_config_overlay(default_cfg, overlay)
  resolved <- merge_config_overlay(default_cfg, overlay)
  config_dir <- dirname(config_path)

  resolved$paths <- lapply(resolved$paths, normalize_config_path,
                           base_dir = config_dir)
  resolved$global$output_dir <- normalize_config_path(
    resolved$global$output_dir, base_dir = config_dir)
  # Hash the complete modular R source tree, not only the command-line wrapper.
  resolved$global$pipeline_source_path <- normalizePath(
    file.path(project_root, "R"), winslash = "/", mustWork = TRUE)
  resolved
}

required_input_keys <- function(cfg) {
  required <- c(
    "wave1_inhome", "birth_records", "neighborhood_w1", "inschool_w1",
    "contextual_w1", "health_w1", "spatial_w1", "stchr95_w1",
    "polcon_w1", "weights_w1", "school_admin_w1", "wave2_inhome")
  waves <- cfg$outcome$waves
  if (identical(waves, "all")) waves <- 3:5
  wave_keys <- paste0("wave", as.integer(waves), "_inhome")
  wave_keys <- intersect(wave_keys, names(cfg$paths))
  if (isTRUE(cfg$mortality_sensitivity$enabled)) required <- c(required, "mortality")
  unique(c(required, wave_keys))
}

assert_restricted_inputs_present <- function(cfg) {
  required <- required_input_keys(cfg)
  missing_value <- required[vapply(cfg$paths, function(path)
    is.null(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(as.character(path))), logical(1))]
  absent <- required[!vapply(cfg$paths, function(path)
    !is.null(path) && length(path) == 1L && !is.na(path) &&
      nzchar(trimws(as.character(path))) && file.exists(path), logical(1))]
  if (length(absent)) {
    detail <- paste(sprintf("%s=%s", absent,
      vapply(cfg$paths[absent], function(path)
        if (is.null(path) || !length(path) || is.na(path)) "<unset>"
        else as.character(path), character(1))), collapse = "; ")
    stop("Restricted Add Health inputs are not fully configured/present: ",
         detail, call. = FALSE)
  }
  invisible(list(required = required, missing_value = missing_value))
}
