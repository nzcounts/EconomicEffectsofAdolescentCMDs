# 3) BASE DATA CONSTRUCTION
# =============================================================================
# Plain-English role: read the Add Health .xpt files, join them on AID/PSUSCID,
# then build the exposure and selected outcome from their source variables.
# Everything downstream starts from the single main_df produced here.

canonical_join_variable_name <- function(x) {
  x <- trimws(as.character(x))
  sub("(?:\\.[xy])+$", "", x, perl = TRUE)
}

canonical_role_key <- function(x) {
  toupper(canonical_join_variable_name(x))
}

empty_variable_source_registry <- function() {
  data.frame(
    source = character(0),
    raw_variable = character(0),
    canonical_variable = character(0),
    derived_from = character(0),
    stringsAsFactors = FALSE)
}

validate_variable_source_registry <- function(registry, label = "variable source registry") {
  required <- names(empty_variable_source_registry())
  if (!is.data.frame(registry))
    stop(label, " must be a data frame.", call. = FALSE)
  missing_cols <- setdiff(required, names(registry))
  if (length(missing_cols))
    stop(label, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  registry <- registry[, required, drop = FALSE]
  if (!nrow(registry)) return(registry)

  for (nm in required) registry[[nm]] <- as.character(registry[[nm]])
  if (anyNA(registry$source) || any(!nzchar(trimws(registry$source))) ||
      anyNA(registry$raw_variable) || any(!nzchar(trimws(registry$raw_variable))) ||
      anyNA(registry$canonical_variable) ||
      any(!nzchar(trimws(registry$canonical_variable))))
    stop(label, " contains missing or blank required values.", call. = FALSE)

  expected_canonical <- canonical_join_variable_name(registry$raw_variable)
  bad_canonical <- which(toupper(registry$canonical_variable) != toupper(expected_canonical))
  if (length(bad_canonical)) {
    ii <- head(bad_canonical, 8L)
    stop(sprintf(
      "%s has canonical-name mismatches, for example: %s.",
      label, paste(sprintf("%s->%s (expected %s)",
        registry$raw_variable[ii], registry$canonical_variable[ii],
        expected_canonical[ii]), collapse = "; ")), call. = FALSE)
  }

  key <- paste(toupper(trimws(registry$source)),
               toupper(trimws(registry$raw_variable)), sep = "::")
  duplicate_keys <- unique(key[duplicated(key)])
  if (length(duplicate_keys)) {
    for (kk in duplicate_keys) {
      d <- registry[key == kk, , drop = FALSE]
      signatures <- paste(d$canonical_variable,
                          ifelse(is.na(d$derived_from), "", d$derived_from), sep = "|")
      if (length(unique(signatures)) > 1L)
        stop(label, " contains conflicting duplicate source/raw-variable records for ",
             kk, ".", call. = FALSE)
    }
    registry <- registry[!duplicated(key), , drop = FALSE]
    rownames(registry) <- NULL
  }
  registry
}

get_variable_source_registry <- function(df, cfg, required = TRUE) {
  registry <- attr(df, "variable_source_registry")
  if (is.null(registry)) registry <- cfg$preprocessing$variable_source_registry %||% NULL
  if (is.null(registry)) {
    if (isTRUE(required))
      stop("The descriptive variable-source registry is absent; rebuild the cache with this pipeline version.",
           call. = FALSE)
    return(empty_variable_source_registry())
  }
  validate_variable_source_registry(registry)
}

get_mandatory_W <- function(cfg) {
  additional <- trimws(as.character(
    cfg$causal_governance$additional_mandatory_W %||% character(0)))
  additional <- additional[!is.na(additional) & nzchar(additional)]
  unique(c(as.character(cfg$final_tmle$protected_W %||% character(0)), additional))
}

build_wave1_variable_source_registry <- function(source_tables, cfg) {
  rows <- lapply(names(source_tables), function(src) {
    data.frame(
      source = src,
      raw_variable = names(source_tables[[src]]),
      canonical_variable = canonical_join_variable_name(names(source_tables[[src]])),
      derived_from = NA_character_,
      stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else empty_variable_source_registry()
  rownames(out) <- NULL
  validate_variable_source_registry(out)
}

append_derived_variable_registry <- function(registry, derived_variable,
                                             derived_from, source) {
  registry <- validate_variable_source_registry(registry)
  source <- as.character(source)
  derived_variable <- as.character(derived_variable)
  derived_from <- as.character(derived_from)
  if (length(source) != 1L || is.na(source) || !nzchar(trimws(source)) ||
      length(derived_variable) != 1L || is.na(derived_variable) ||
      !nzchar(trimws(derived_variable)) || length(derived_from) != 1L ||
      is.na(derived_from) || !nzchar(trimws(derived_from)))
    stop("Derived-variable registry entries require one nonblank source, variable, and parent.",
         call. = FALSE)

  src_rows <- registry[toupper(registry$source) == toupper(source), , drop = FALSE]
  if (!nrow(src_rows))
    stop("Cannot register derived variable '", derived_variable,
         "': source '", source, "' is absent from the descriptive registry.",
         call. = FALSE)
  if (!canonical_role_key(derived_from) %in% canonical_role_key(src_rows$raw_variable))
    stop("Cannot register derived variable '", derived_variable,
         "': parent '", derived_from, "' is absent from source '", source,
         "' in the registry.", call. = FALSE)

  row <- data.frame(
    source = source,
    raw_variable = derived_variable,
    canonical_variable = canonical_join_variable_name(derived_variable),
    derived_from = derived_from,
    stringsAsFactors = FALSE)
  validate_variable_source_registry(rbind(registry, row))
}

collapse_registry_by_canonical <- function(registry) {
  registry <- validate_variable_source_registry(registry)
  if (!nrow(registry)) {
    return(data.frame(
      canonical_key = character(0), canonical_variable = character(0),
      source = character(0), derived_from = character(0),
      stringsAsFactors = FALSE))
  }
  split_rows <- split(registry, canonical_role_key(registry$canonical_variable))
  out <- lapply(split_rows, function(d) {
    derived <- unique(na.omit(as.character(d$derived_from)))
    data.frame(
      canonical_key = canonical_role_key(d$canonical_variable[1L]),
      canonical_variable = canonical_join_variable_name(d$canonical_variable[1L]),
      source = paste(sort(unique(as.character(d$source))), collapse = ";"),
      derived_from = paste(sort(derived), collapse = ";"),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

restore_custom_data_frame_attributes <- function(df, saved_attributes) {
  core <- c("names", "row.names", "class")
  for (nm in setdiff(names(saved_attributes), core))
    attr(df, nm) <- saved_attributes[[nm]]
  df
}

canonicalize_mandatory_W_columns <- function(df, cfg) {
  if (!is.data.frame(df)) stop("Mandatory-W canonicalization requires a data frame.", call. = FALSE)
  saved_attributes <- attributes(df)
  audit_rows <- list()
  for (target in get_mandatory_W(cfg)) {
    key <- canonical_role_key(target)
    hits <- names(df)[canonical_role_key(names(df)) == key]
    if (!length(hits)) next
    exact <- hits[toupper(hits) == toupper(target)]
    preferred <- if (length(exact)) exact[1L] else hits[1L]
    if (length(hits) == 1L && identical(preferred, target)) next

    normalized <- lapply(hits, function(nm) normalize_design_field_values(df[[nm]]))
    names(normalized) <- hits
    canonical_chr <- rep(NA_character_, nrow(df))
    conflicts <- rep(FALSE, nrow(df))
    for (nm in c(preferred, setdiff(hits, preferred))) {
      z <- normalized[[nm]]
      z_chr <- trimws(as.character(z))
      z_chr[is.na(z)] <- NA_character_
      present <- !is.na(z_chr) & nzchar(z_chr)
      overlap <- present & !is.na(canonical_chr)
      conflicts <- conflicts | (overlap & canonical_chr != z_chr)
      canonical_chr[present & is.na(canonical_chr)] <- z_chr[present & is.na(canonical_chr)]
    }
    if (any(conflicts)) {
      ii <- head(which(conflicts), 5L)
      stop(sprintf(
        "Mandatory W '%s' has conflicting aliases in columns %s (example rows %s).",
        target, paste(hits, collapse = ", "), paste(ii, collapse = ", ")),
        call. = FALSE)
    }
    parsed <- suppressWarnings(as.numeric(canonical_chr))
    nonmissing <- !is.na(canonical_chr)
    canonical <- if (any(nonmissing) && all(is.finite(parsed[nonmissing]))) {
      if (all(abs(parsed[nonmissing] - round(parsed[nonmissing])) <= 1e-8))
        as.integer(round(parsed)) else parsed
    } else canonical_chr
    df <- df[, !names(df) %in% hits, drop = FALSE]
    df[[target]] <- canonical
    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      mandatory_variable = target,
      source_columns = paste(hits, collapse = ";"),
      n_sources = length(hits),
      n_conflicts = 0L,
      n_missing = sum(is.na(canonical) | trimws(as.character(canonical)) == ""),
      stringsAsFactors = FALSE)
  }
  df <- restore_custom_data_frame_attributes(df, saved_attributes)
  prior <- attr(df, "mandatory_W_canonicalization_audit")
  new <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame()
  if (is.data.frame(prior) && nrow(prior)) new <- rbind(prior, new)
  if (nrow(new)) {
    new <- new[!duplicated(new[, c("mandatory_variable", "source_columns"), drop = FALSE]), , drop = FALSE]
    rownames(new) <- NULL
  }
  attr(df, "mandatory_W_canonicalization_audit") <- new
  df
}

get_mortality_role_vars <- function(cfg) {
  mort <- cfg$mortality_sensitivity %||% list()
  specs <- mort$wave_specs %||% list()
  wave_vars <- unlist(lapply(specs, function(z) c(
    z$interview_year_var %||% character(0),
    z$interview_month_var %||% character(0),
    z$derived_death_year_var %||% character(0),
    z$derived_death_month_var %||% character(0),
    z$death_in_window_var %||% character(0),
    z$death_before_outcome_var %||% character(0),
    z$timing_status_var %||% character(0))), use.names = FALSE)
  unique(c(mort$source_var %||% character(0),
           mort$source_month_var %||% character(0), wave_vars))
}

build_candidate_alias_audit <- function(df, cfg) {
  registry <- get_variable_source_registry(
    df, cfg, required = TRUE)
  reg <- collapse_registry_by_canonical(registry)
  reg_match <- match(canonical_role_key(names(df)), reg$canonical_key)
  exclusions <- get_common_exclusion_vars(cfg, include_analysis_outputs = TRUE)
  exclusion_keys <- canonical_role_key(exclusions)
  role_names <- unique(c(
    cfg$analysis$id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
    cfg$analysis$weight_var, cfg$analysis$exposure_var,
    cfg$analysis$outcome_var, cfg$analysis$outcome_observed_var,
    get_mortality_role_vars(cfg),
    cfg$exposure$drop_from_candidates %||% character(0),
    get_outcome_drop_vars(cfg)
  ))
  role_keys <- canonical_role_key(role_names)
  actual <- names(df)
  canonical <- canonical_join_variable_name(actual)
  canonical_key <- canonical_role_key(actual)
  excluded <- canonical_key %in% exclusion_keys
  role_alias <- canonical_key %in% role_keys
  registered <- !is.na(reg_match)
  source <- rep(NA_character_, length(actual))
  if (any(registered)) source[registered] <- reg$source[reg_match[registered]]
  eligible <- !excluded & !role_alias
  reason <- ifelse(role_alias, "analysis_role_or_source_alias",
            ifelse(excluded, "configured_exclusion_or_alias", "eligible_by_role_exclusions"))
  data.frame(
    actual_variable = actual,
    canonical_variable = canonical,
    has_join_suffix = grepl("(?:\\.[xy])+$", actual, perl = TRUE),
    has_recursive_join_suffix = grepl("(?:\\.[xy]){2,}$", actual, perl = TRUE),
    source = source,
    registered_in_source_audit = registered,
    configured_exclusion = excluded,
    protected_analysis_role = role_alias,
    mandatory_W = canonical_key %in% canonical_role_key(get_mandatory_W(cfg)),
    eligible_candidate_W = eligible,
    eligibility_reason = reason,
    stringsAsFactors = FALSE)
}

validate_candidate_governance <- function(df, cfg, audit = NULL) {
  if (is.null(audit)) audit <- build_candidate_alias_audit(df, cfg)
  candidates <- audit$actual_variable[audit$eligible_candidate_W]
  bad_role <- canonical_role_key(candidates) %in% canonical_role_key(c(
    cfg$analysis$id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
    cfg$analysis$weight_var, cfg$analysis$exposure_var,
    cfg$analysis$outcome_var, cfg$analysis$outcome_observed_var,
    get_mortality_role_vars(cfg),
    cfg$exposure$drop_from_candidates %||% character(0),
    get_outcome_drop_vars(cfg)
  ))
  if (any(bad_role) && isTRUE(cfg$safety$fail_on_role_alias_leakage %||% TRUE))
    stop("Analysis-role alias(es) survived candidate governance: ",
         paste(candidates[bad_role], collapse = ", "), call. = FALSE)
  mandatory <- get_mandatory_W(cfg)
  mandatory_keys <- canonical_role_key(mandatory)
  candidate_keys <- canonical_role_key(candidates)
  missing_mandatory <- mandatory[!mandatory_keys %in% candidate_keys]
  if (length(missing_mandatory) && isTRUE(cfg$safety$fail_on_missing_mandatory_W %||% TRUE))
    stop("Mandatory pre-exposure W variable(s) are absent or ineligible: ",
         paste(missing_mandatory, collapse = ", "), call. = FALSE)
  invisible(list(candidates = candidates, audit = audit,
                 missing_mandatory = missing_mandatory))
}

get_outcome_drop_vars <- function(cfg) {
  fam_drop <- character(0)
  if (!is.null(cfg$outcome$family)) {
    fam_cfg <- cfg$outcome$families[[cfg$outcome$family]]
    if (!is.null(fam_cfg)) {
      cur_wave <- cfg$outcome$current_wave
      if (!is.null(cur_wave) && !is.null(fam_cfg$drop_from_candidates_by_wave)) {
        per_wave <- fam_cfg$drop_from_candidates_by_wave[[as.character(cur_wave)]]
        fam_drop <- if (!is.null(per_wave)) as.character(per_wave) else
          fam_cfg$drop_from_candidates %||% character(0)
      } else {
        fam_drop <- fam_cfg$drop_from_candidates %||% character(0)
      }
      if (!is.null(fam_cfg$source_var))
        fam_drop <- unique(c(fam_drop, as.character(fam_cfg$source_var)))
    }
  }
  unique(c(cfg$outcome$drop_from_candidates %||% character(0), fam_drop))
}

get_common_exclusion_vars <- function(cfg, include_analysis_outputs = TRUE) {
  death_vars <- get_mortality_role_vars(cfg)
  base <- unique(c(
    cfg$analysis$id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
    cfg$analysis$weight_var,
    "H1GH50", cfg$analysis$extra_exclude_from_candidates %||% character(0),
    death_vars,
    cfg$exposure$drop_from_candidates %||% character(0),
    get_outcome_drop_vars(cfg)))
  if (isTRUE(include_analysis_outputs)) {
    base <- unique(c(base, cfg$analysis$exposure_var, cfg$analysis$outcome_var,
                     cfg$analysis$outcome_observed_var))
  }
  base
}

design_field_source_audit <- function(source_tables, field_name) {
  rows <- lapply(names(source_tables), function(src) {
    df <- source_tables[[src]]
    hits <- names(df)[tolower(names(df)) == tolower(field_name)]
    data.frame(
      source = src,
      field = field_name,
      matching_columns = paste(hits, collapse = ";"),
      n_matches = length(hits),
      n_rows = nrow(df),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

normalize_design_field_values <- function(x) {
  missing <- is.na(x)
  chr <- trimws(as.character(x))
  chr[missing | is.na(chr) | chr == ""] <- NA_character_
  parsed <- suppressWarnings(as.numeric(chr))
  nonmissing <- !is.na(chr)
  if (any(nonmissing) && all(is.finite(parsed[nonmissing]))) {
    out <- parsed
    if (all(abs(out[nonmissing] - round(out[nonmissing])) <= 1e-8))
      out <- as.integer(round(out))
    return(out)
  }
  chr
}

# Canonicalize one required join key from an exact key name or documented aliases.
# Multiple matching source columns are accepted only when they agree row by row;
# otherwise the merge stops before any row multiplication can occur.
canonicalize_join_key_alias <- function(df, key, aliases = character(0),
                                        label = deparse(substitute(df)),
                                        require_complete = FALSE) {
  if (!is.data.frame(df)) stop(label, " is not a data frame.", call. = FALSE)
  accepted <- unique(tolower(c(key, aliases)))
  hits <- names(df)[tolower(names(df)) %in% accepted]
  if (!length(hits))
    stop(label, " is missing required join key ", key,
         " and aliases: ", paste(aliases, collapse = ", "), call. = FALSE)
  values <- lapply(hits, function(nm) normalize_design_field_values(df[[nm]]))
  names(values) <- hits
  combined <- values[[1L]]
  if (length(values) > 1L) {
    for (j in 2:length(values)) {
      z <- values[[j]]
      c_chr <- trimws(as.character(combined)); z_chr <- trimws(as.character(z))
      c_present <- !is.na(combined) & nzchar(c_chr)
      z_present <- !is.na(z) & nzchar(z_chr)
      conflict <- c_present & z_present & c_chr != z_chr
      if (any(conflict)) {
        ii <- which(conflict)[seq_len(min(5L, sum(conflict)))]
        stop(sprintf(
          "%s has conflicting aliases for join key %s in columns %s (examples at rows %s).",
          label, key, paste(hits, collapse = ", "), paste(ii, collapse = ", ")),
          call. = FALSE)
      }
      fill <- !c_present & z_present
      combined[fill] <- z[fill]
    }
  }
  df <- df[, !names(df) %in% hits, drop = FALSE]
  df[[key]] <- combined
  key_chr <- trimws(as.character(df[[key]]))
  incomplete <- is.na(df[[key]]) | is.na(key_chr) | !nzchar(key_chr)
  if (isTRUE(require_complete) && any(incomplete))
    stop(label, " contains missing or blank values in canonical join key ", key, ".", call. = FALSE)
  df
}

canonicalize_merged_design_field <- function(df, field_name, source_audit = NULL) {
  prior_audit <- attr(df, "canonical_design_field_audit")
  name_lower <- tolower(names(df))
  field_lower <- tolower(field_name)
  suffix_pattern <- paste0("^", field_lower, "(?:\\.[xy])+$")
  candidates <- names(df)[name_lower == field_lower |
                            grepl(suffix_pattern, name_lower, perl = TRUE)]
  if (!length(candidates)) {
    src_txt <- if (!is.null(source_audit) && nrow(source_audit)) {
      paste(sprintf("%s:[%s]", source_audit$source,
                    ifelse(source_audit$n_matches > 0L,
                           source_audit$matching_columns, "none")), collapse = "; ")
    } else "source audit unavailable"
    stop(sprintf(
      "Required design field '%s' was not found after the Wave-I merges. Raw-source audit: %s.",
      field_name, src_txt), call. = FALSE)
  }

  normalized <- lapply(candidates, function(nm) normalize_design_field_values(df[[nm]]))
  names(normalized) <- candidates
  canonical_chr <- rep(NA_character_, nrow(df))
  conflict <- rep(FALSE, nrow(df))
  conflict_examples <- character(0)
  for (nm in candidates) {
    z <- normalized[[nm]]
    z_chr <- trimws(as.character(z))
    z_chr[is.na(z)] <- NA_character_
    present <- !is.na(z_chr) & nzchar(z_chr)
    overlap <- present & !is.na(canonical_chr)
    bad <- overlap & canonical_chr != z_chr
    if (any(bad)) {
      conflict <- conflict | bad
      ii <- which(bad)[seq_len(min(3L, sum(bad)))]
      conflict_examples <- c(conflict_examples,
        sprintf("row %d: existing=%s, %s=%s", ii, canonical_chr[ii], nm, z_chr[ii]))
    }
    fill <- present & is.na(canonical_chr)
    canonical_chr[fill] <- z_chr[fill]
  }
  if (any(conflict)) {
    stop(sprintf(
      "Conflicting values were found while canonicalizing design field '%s' from columns %s (%d conflicting rows; examples: %s).",
      field_name, paste(candidates, collapse = ", "), sum(conflict),
      paste(head(conflict_examples, 6L), collapse = " | ")), call. = FALSE)
  }

  parsed <- suppressWarnings(as.numeric(canonical_chr))
  nonmissing <- !is.na(canonical_chr)
  canonical <- if (any(nonmissing) && all(is.finite(parsed[nonmissing]))) {
    if (all(abs(parsed[nonmissing] - round(parsed[nonmissing])) <= 1e-8))
      as.integer(round(parsed)) else parsed
  } else canonical_chr

  df <- df[, !names(df) %in% candidates, drop = FALSE]
  df[[field_name]] <- canonical
  new_audit <- data.frame(
    field = field_name,
    source_columns = paste(candidates, collapse = ";"),
    n_rows = nrow(df),
    n_missing = sum(is.na(canonical) | trimws(as.character(canonical)) == ""),
    n_unique_nonmissing = length(unique(canonical[!is.na(canonical)])),
    stringsAsFactors = FALSE)
  if (is.data.frame(prior_audit) && nrow(prior_audit) > 0L) {
    missing_old <- setdiff(names(new_audit), names(prior_audit))
    for (nm in missing_old) prior_audit[[nm]] <- NA
    missing_new <- setdiff(names(prior_audit), names(new_audit))
    for (nm in missing_new) new_audit[[nm]] <- NA
    combined_audit <- rbind(prior_audit[, names(new_audit), drop = FALSE], new_audit)
  } else combined_audit <- new_audit
  combined_audit <- combined_audit[!duplicated(
    combined_audit[, c("field", "source_columns"), drop = FALSE]), , drop = FALSE]
  rownames(combined_audit) <- NULL
  attr(df, "canonical_design_field_audit") <- combined_audit
  df
}

build_full_survey_design_frame <- function(w1_all, cfg) {
  id_var <- cfg$analysis$id_var
  psu_var <- cfg$analysis$cluster_var
  strata_var <- cfg$analysis$strata_var
  weight_var <- cfg$analysis$weight_var
  assert_required_columns(w1_all, c(id_var, psu_var, strata_var, weight_var),
                          "Wave-I merged survey-design frame")
  ids <- w1_all[[id_var]]
  if (anyNA(ids) || anyDuplicated(as.character(ids)))
    stop("Wave-I survey-design frame requires complete unique respondent IDs.", call. = FALSE)
  w <- suppressWarnings(as.numeric(w1_all[[weight_var]]))
  keep <- is.finite(w) & w > 0
  if (!any(keep)) stop("Wave-I survey-design frame has no positive finite sampling weights.", call. = FALSE)
  out <- w1_all[keep, c(id_var, psu_var, strata_var, weight_var), drop = FALSE]
  out[[weight_var]] <- w[keep]
  psu <- trimws(as.character(out[[psu_var]]))
  stratum <- trimws(as.character(out[[strata_var]]))
  if (anyNA(out[[psu_var]]) || any(!nzchar(psu)))
    stop(sprintf("Valid-weight Wave-I records contain missing or blank %s.", psu_var), call. = FALSE)
  if (anyNA(out[[strata_var]]) || any(!nzchar(stratum)))
    stop(sprintf("Valid-weight Wave-I records contain missing or blank %s.", strata_var), call. = FALSE)
  psu_strata_n <- tapply(stratum, psu, function(z) length(unique(z)))
  if (any(psu_strata_n != 1L))
    stop(sprintf("At least one %s maps to multiple %s values in the valid-weight Wave-I frame.",
                 psu_var, strata_var), call. = FALSE)
  expected_h <- as.integer(cfg$analysis$expected_strata_n %||% 4L)
  observed_h <- length(unique(stratum))
  if (is.finite(expected_h) && expected_h > 0L && observed_h != expected_h)
    stop(sprintf("Expected %d nonmissing %s levels in the valid-weight Wave-I frame, found %d.",
                 expected_h, strata_var, observed_h), call. = FALSE)
  psu_per_stratum <- tapply(psu, stratum, function(z) length(unique(z)))
  if (any(psu_per_stratum < 2L))
    stop(sprintf("Every %s level must contain at least two sampled %s values.",
                 strata_var, psu_var), call. = FALSE)
  out$.analysis_domain <- FALSE
  rownames(out) <- NULL
  attr(out, "design_audit") <- list(
    n_valid_weight_records = nrow(out),
    n_psu = length(unique(psu)),
    n_strata = observed_h,
    psu_per_stratum = as.integer(psu_per_stratum),
    strata_labels = names(psu_per_stratum))
  out
}

read_wave1_merged <- function(cfg) {
  msg("Reading and merging Wave 1 source files...", cfg = cfg)
  inhome_w1  <- read_xpt_df(cfg$paths$wave1_inhome)
  birth      <- read_xpt_df(cfg$paths$birth_records)
  nhood      <- read_xpt_df(cfg$paths$neighborhood_w1)
  inschool   <- read_xpt_df(cfg$paths$inschool_w1)
  context_w1 <- read_xpt_df(cfg$paths$contextual_w1)
  health_w1  <- read_xpt_df(cfg$paths$health_w1)
  spatial_w1 <- read_xpt_df(cfg$paths$spatial_w1)
  stchr95_w1 <- read_xpt_df(cfg$paths$stchr95_w1)
  polcon_w1  <- read_xpt_df(cfg$paths$polcon_w1)
  weights_w1 <- read_xpt_df(cfg$paths$weights_w1)
  school_admin <- read_xpt_df(cfg$paths$school_admin_w1)
  school_admin <- canonicalize_join_key_alias(
    school_admin, cfg$analysis$cluster_var, aliases = "ASCHLCDE",
    label = "school_admin_w1", require_complete = FALSE)

  source_tables <- list(
    wave1_inhome = inhome_w1,
    birth_records = birth,
    neighborhood_w1 = nhood,
    inschool_w1 = inschool,
    contextual_w1 = context_w1,
    health_w1 = health_w1,
    spatial_w1 = spatial_w1,
    stchr95_w1 = stchr95_w1,
    polcon_w1 = polcon_w1,
    weights_w1 = weights_w1,
    school_admin_w1 = school_admin)
  variable_source_registry <- build_wave1_variable_source_registry(source_tables, cfg)
  strata_audit <- design_field_source_audit(source_tables, cfg$analysis$strata_var)
  if (any(strata_audit$n_matches > 1L)) {
    bad <- strata_audit$source[strata_audit$n_matches > 1L]
    stop(sprintf(
      "Multiple case-insensitive %s columns were found within source table(s): %s. Canonicalize the raw source before merging.",
      cfg$analysis$strata_var, paste(bad, collapse = ", ")), call. = FALSE)
  }
  if (!any(strata_audit$n_matches > 0L)) {
    stop(sprintf(
      paste0("Required Add Health design field '%s' was absent from every configured Wave-I source. ",
             "Verify the restricted-use file inventory and the column name in %s; no model fitting was attempted."),
      cfg$analysis$strata_var, cfg$paths$weights_w1), call. = FALSE)
  }
  msg(sprintf("  [design] %s found in raw source(s): %s.", cfg$analysis$strata_var,
              paste(strata_audit$source[strata_audit$n_matches > 0L], collapse = ", ")),
      cfg = cfg)

  id_var <- cfg$analysis$id_var
  cluster_var <- cfg$analysis$cluster_var
  w1_all <- merge_by_key(
    list(inhome_w1, birth, nhood, inschool, context_w1, health_w1,
         spatial_w1, stchr95_w1, polcon_w1, weights_w1), id_var,
    labels = c("wave1_inhome", "birth_records", "neighborhood_w1", "inschool_w1",
               "contextual_w1", "health_w1", "spatial_w1", "stchr95_w1",
               "polcon_w1", "weights_w1"))
  w1_all <- canonicalize_merged_design_field(w1_all, cluster_var)
  cluster_canonical_audit <- attr(w1_all, "canonical_design_field_audit")
  w1_all <- left_join_many_to_one(
    w1_all, school_admin, cluster_var,
    x_label = "wave1_person_merged", y_label = "school_admin_w1", row_id = id_var)

  collision_result <- resolve_join_suffix_collisions(w1_all)
  w1_all <- collision_result$data
  # REGION can appear in more than one source and the generic collision resolver
  # intentionally retains an .x name. Reconstruct one canonical field before
  # the dictionary is frozen or candidate variables are enumerated.
  w1_all <- canonicalize_merged_design_field(
    w1_all, cfg$analysis$strata_var, source_audit = strata_audit)
  region_canonical_audit <- attr(w1_all, "canonical_design_field_audit")
  canonical_audit <- rbind(cluster_canonical_audit, region_canonical_audit)
  canonical_audit <- canonical_audit[!duplicated(
    canonical_audit[, c("field", "source_columns"), drop = FALSE]), , drop = FALSE]
  rownames(canonical_audit) <- NULL
  w1_all <- canonicalize_mandatory_W_columns(w1_all, cfg)
  attr(w1_all, "join_suffix_collision_audit") <- collision_result$audit
  attr(w1_all, "design_strata_source_audit") <- strata_audit
  attr(w1_all, "canonical_design_field_audit") <- canonical_audit
  attr(w1_all, "variable_source_registry") <- variable_source_registry

  full_design_frame <- build_full_survey_design_frame(w1_all, cfg)
  attr(w1_all, "full_survey_design_frame") <- full_design_frame

  # Freeze semantic missing-code meanings using all Wave I rows, before Wave II
  # exposure measurement, outcome construction, fold creation, or screening.
  # REGION is a survey-design field, not a candidate confounder, and is excluded
  # by get_common_exclusion_vars along with AID, PSUSCID, and GSWGT1.
  dict_exclude <- get_common_exclusion_vars(cfg, include_analysis_outputs = FALSE)
  dict_exclude_keys <- canonical_role_key(dict_exclude)
  dict_vars <- names(w1_all)[!canonical_role_key(names(w1_all)) %in% dict_exclude_keys]
  if (cfg$analysis$strata_var %in% dict_vars)
    stop("Internal error: the survey stratum entered the missing-code dictionary.", call. = FALSE)
  dictionary <- build_global_missing_code_dictionary(
    w1_all, dict_vars, cfg$preprocessing)
  attr(w1_all, "global_missing_dictionary") <- dictionary
  msg(sprintf(
    "  [missing dictionary] Frozen exact-code rules for %d Wave I variables using all %d Wave I rows before exposure restriction.",
    length(dictionary), nrow(w1_all)), cfg = cfg)
  w1_all
}

# Column typing: a variable with few substantive unique values (after
# applying the conservative exact-code classifier) is treated as a factor.
# The factor declaration is frozen in the complete-Wave-I dictionary; fold-
# specific recipes only learn levels/pooling from training rows and apply them
# unchanged to validation rows.
classify_factors_by_uniques <- function(df, cfg_pre) {
  as_factor_preserve_values <- function(x) {
    if (is.factor(x)) droplevels(x) else factor(x)
  }
  factor_cols <- names(df)[vapply(names(df), function(nm) {
    rule <- get_missing_rule(df[[nm]], cfg_pre, variable_name = nm)
    isTRUE(rule$as_factor)
  }, logical(1))]
  if (length(factor_cols))
    df[factor_cols] <- lapply(df[factor_cols], as_factor_preserve_values)
  df
}

# Numeric covariates receive two indicators:
# *_missA = native/general missingness; *_miss97 = inferred structural skip.
# Factor missingness is represented once through explicit Missing/Skip levels.
# Exact special codes come from the frozen complete-Wave-I dictionary; no value
# is classified by numeric suffix.
add_dual_missingness_indicators <- function(data, factor_vars, numeric_vars, cfg_pre) {
  data <- as.data.frame(data)
  assert_no_missing_indicator_name_collisions(
    names(data), intersect(numeric_vars, names(data)),
    context = "add_dual_missingness_indicators")
  n_questionnaire <- 0L; n_continuous <- 0L; n_with_codes <- 0L
  n_factor_skip_retained <- 0L
  total_missA_numeric <- 0L; total_miss97_numeric <- 0L
  total_missA_factor  <- 0L; total_miss97_factor  <- 0L
  miss_label <- cfg_pre$factor_missing_label %||% "Missing"
  skip_label <- cfg_pre$factor_skip_label %||% "Skip"
  min_n_skip <- cfg_pre$factor_special_code_min_n %||% 30L
  min_pr_skip <- cfg_pre$factor_special_code_min_prop %||% 0.02

  # Numeric variables
  for (nm in intersect(numeric_vars, names(data))) {
    rule <- get_missing_rule(data[[nm]], cfg_pre, variable_name = nm)
    masks <- missing_masks_from_rule(data[[nm]], rule)
    x <- masks$numeric
    missA <- masks$general; miss97 <- masks$skip
    if (isTRUE(rule$questionnaire_like)) n_questionnaire <- n_questionnaire + 1L
    else n_continuous <- n_continuous + 1L
    if (length(rule$general_codes) || length(rule$skip_codes)) n_with_codes <- n_with_codes + 1L
    fill_value <- compute_simple_impute(x[!(missA | miss97)], cfg_pre$numeric_imputation)
    if (!isTRUE(identical(cfg_pre$numeric_missing_scheme, "dual_indicators"))) {
      missA <- missA | miss97
      miss97 <- rep(FALSE, length(x))
    }
    data[[paste0(nm, "_missA")]]  <- as.integer(missA)
    data[[paste0(nm, "_miss97")]] <- as.integer(miss97)
    total_missA_numeric  <- total_missA_numeric  + sum(missA)
    total_miss97_numeric <- total_miss97_numeric + sum(miss97)
    x[missA | miss97] <- fill_value
    data[[nm]] <- x
  }

  # Factor variables: use the same learned exact-code rule, then optionally
  # preserve an inferred skip as its own level when common enough.
  for (nm in intersect(factor_vars, names(data))) {
    rule <- get_missing_rule(data[[nm]], cfg_pre, variable_name = nm)
    masks <- missing_masks_from_rule(data[[nm]], rule)
    xc <- canonicalize_factor_text(data[[nm]])
    missA <- masks$general; miss97 <- masks$skip
    assert_reserved_factor_labels_safe(
      data[[nm]], missA, miss97,
      c(miss_label, skip_label, cfg_pre$factor_other_label %||% "_Other_"),
      nm, "add_dual_missingness_indicators")
    if (isTRUE(rule$questionnaire_like)) n_questionnaire <- n_questionnaire + 1L
    else n_continuous <- n_continuous + 1L
    if (length(rule$general_codes) || length(rule$skip_codes)) n_with_codes <- n_with_codes + 1L
    # Factor missingness is represented once through the explicit Missing/Skip
    # levels. Separate factor indicators would be deterministic duplicates.
    total_missA_factor  <- total_missA_factor  + sum(missA)
    total_miss97_factor <- total_miss97_factor + sum(miss97)

    n_skip <- sum(miss97); pr_skip <- n_skip / length(xc)
    retain_skip <- n_skip >= min_n_skip || pr_skip >= min_pr_skip
    if (retain_skip) {
      n_factor_skip_retained <- n_factor_skip_retained + 1L
      xc[missA] <- NA_character_
      xc[miss97] <- skip_label
    } else {
      xc[missA | miss97] <- NA_character_
    }
    f <- addNA(factor(xc))
    levels(f)[is.na(levels(f))] <- miss_label
    data[[nm]] <- f
  }

  message(sprintf(
    "  [add_dual_missingness_indicators] processed %d numeric and %d factor cols.",
    length(intersect(numeric_vars, names(data))),
    length(intersect(factor_vars, names(data)))))
  message(sprintf(
    "    Numeric: %d _missA flags, %d _miss97 flags (imputation = '%s').",
    total_missA_numeric, total_miss97_numeric, cfg_pre$numeric_imputation))
  message(sprintf(
    "    Factor: %d general-missing and %d structural-skip values represented through levels; skip retained for %d factors.",
    total_missA_factor, total_miss97_factor, n_factor_skip_retained))
  message(sprintf(
    "    Source-informed exact classifier: %d questionnaire-source/like, %d contextual/other; %d variables had an exact special-code family recognized.",
    n_questionnaire, n_continuous, n_with_codes))
  data
}

# =============================================================================
# OUTCOME FAMILY DISPATCHER
# =============================================================================
# Each family has a constructor that takes (main_df, wave, family_cfg, outcome_cfg)
# and returns a numeric or integer vector Y of length nrow(main_df), with NA
# for rows where the outcome is not observed at the requested wave.
# construct_outcome is the dispatcher called by build_main_dataset. It
# reads cfg$outcome$family and cfg$outcome$family_member to pick which
# constructor to call, writes the result to main_df[[cfg$analysis$outcome_var]],
# and sets the censoring indicator.

# ---- Read the configured in-home file for an outcome wave -------------------
read_wave_inhome <- function(wave, cfg) {
  path_name <- paste0("wave", wave, "_inhome")
  if (!path_name %in% names(cfg$paths)) {
    stop(sprintf("cfg$paths$%s is not configured.", path_name), call. = FALSE)
  }
  p <- cfg$paths[[path_name]]
  if (is.null(p) || !file.exists(p)) {
    stop(sprintf("Wave %d in-home file not found at: %s", wave, p %||% "<NULL>"), call. = FALSE)
  }
  df <- read_xpt_df(p)
  df
}

# ---- Educational Attainment (nested binary) --------------------------------
# Wave III is attainment-to-date. H3ED5 establishes college graduation;
# H3ED1 values 13-17 or college graduation establish some college; and either
# H3ED2/H3ED3=1 or any higher threshold establishes high-school completion.
# Wave IV uses the verified nested thresholds of H4ED2.
construct_outcome_educational_attainment <- function(main_df, wave, family_cfg,
                                                    outcome_cfg, member, pipeline_cfg = NULL) {
  wave <- as.integer(wave)
  message(sprintf("    [outcome] Educational Attainment, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src))
    stop(sprintf("EducationalAttainment is not configured for wave %d.", wave), call. = FALSE)
  allowed_members <- c("at_least_hs", "at_least_some_college",
                       "at_least_college_grad")
  if (!member %in% allowed_members)
    stop(sprintf("Unknown EducationalAttainment member: %s", member), call. = FALSE)
  main_df <- join_outcome_fields(
    main_df, wave, unname(src), pipeline_cfg,
    sprintf("Wave-%d education source", wave))

  if (identical(wave, 3L)) {
    required_roles <- c("highest_grade", "high_school_equivalency",
                        "high_school_diploma", "college_graduate")
    if (!is.character(src) || any(!required_roles %in% names(src)))
      stop("Wave-III education requires the four named H3ED1/H3ED2/H3ED3/H3ED5 sources.",
           call. = FALSE)
    h1_raw <- main_df[[unname(src["highest_grade"])]]
    h2_raw <- main_df[[unname(src["high_school_equivalency"])]]
    h3_raw <- main_df[[unname(src["high_school_diploma"])]]
    h5_raw <- main_df[[unname(src["college_graduate"])]]
    h1 <- to_numeric_codes(h1_raw); h2 <- to_numeric_codes(h2_raw)
    h3 <- to_numeric_codes(h3_raw); h5 <- to_numeric_codes(h5_raw)
    assert_only_known_codes(h1_raw, c(0:22, 96L, 98L, 99L), "H3ED1")
    assert_only_known_codes(h2_raw, c(0L, 1L, 6L, 8L, 9L), "H3ED2")
    assert_only_known_codes(h3_raw, c(0L, 1L, 6L, 8L, 9L), "H3ED3")
    assert_only_known_codes(h5_raw, c(0L, 1L, 6L, 8L, 9L), "H3ED5")

    h1_observed <- is.finite(h1) & !(h1 %in% c(96L, 98L, 99L))
    h2_observed <- is.finite(h2) & !(h2 %in% c(6L, 8L, 9L))
    h3_observed <- is.finite(h3) & !(h3 %in% c(6L, 8L, 9L))
    h5_observed <- is.finite(h5) & !(h5 %in% c(6L, 8L, 9L))
    grade_some_college <- h1_observed & h1 > 12 & h1 < 18
    grade_above_rule <- h1_observed & h1 >= 18
    education_conflict <- grade_above_rule & h5_observed & h5 == 0L

    college_grad <- rep(NA_integer_, nrow(main_df))
    college_grad[h5_observed & h5 == 1L] <- 1L
    college_grad[h5_observed & h5 == 0L & !education_conflict] <- 0L

    some_college <- rep(NA_integer_, nrow(main_df))
    some_college[grade_some_college | college_grad %in% 1L] <- 1L
    some_college[h5_observed & h5 == 0L & h1_observed & h1 <= 12L] <- 0L

    high_school <- rep(NA_integer_, nrow(main_df))
    high_school[(h2_observed & h2 == 1L) |
                (h3_observed & h3 == 1L) |
                some_college %in% 1L | college_grad %in% 1L] <- 1L
    high_school[h2_observed & h2 == 0L & h3_observed & h3 == 0L &
                some_college %in% 0L] <- 0L

    # These assignments make nesting explicit even if a lower-threshold source
    # is missing: a verified higher attainment necessarily establishes lower
    # attainment. No lower-threshold value is allowed to overturn it.
    some_college[college_grad %in% 1L] <- 1L
    high_school[some_college %in% 1L | college_grad %in% 1L] <- 1L
    Y <- switch(member,
      at_least_hs = high_school,
      at_least_some_college = some_college,
      at_least_college_grad = college_grad)
    nested_violations <- sum(
      (college_grad == 1L & some_college != 1L) |
      (some_college == 1L & high_school != 1L), na.rm = TRUE)
    audit <- data.frame(
      metric = c("definition", "member", "primary", "n_observed", "n_one",
                 "n_zero", "n_missing", "n_h3ed1_13_to_17",
                 "n_college_grad_establishes_some_college",
                 "n_higher_attainment_establishes_hs",
                 "n_h3ed1_18_to_22_h3ed5_zero_conflict",
                 "n_nested_violations"),
      value = c("Wave III attainment-to-date nested binary", member, "TRUE",
                sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE),
                sum(Y == 0L, na.rm = TRUE), sum(is.na(Y)),
                sum(grade_some_college), sum(college_grad == 1L, na.rm = TRUE),
                sum(some_college == 1L | college_grad == 1L, na.rm = TRUE),
                sum(education_conflict), nested_violations),
      stringsAsFactors = FALSE)
  } else if (identical(wave, 4L)) {
    if (!is.character(src) || length(src) != 1L || !identical(unname(src), "H4ED2"))
      stop("Wave-IV education requires H4ED2 as its sole source.", call. = FALSE)
    raw_source <- main_df[[unname(src)]]
    raw <- to_numeric_codes(raw_source)
    assert_only_known_codes(raw_source, c(0:13, 96L, 98L), "H4ED2")
    observed <- is.finite(raw) & raw %in% 0:13
    threshold <- switch(member,
      at_least_hs = 3L,
      at_least_some_college = 6L,
      at_least_college_grad = 7L)
    Y <- rep(NA_integer_, nrow(main_df))
    Y[observed] <- as.integer(raw[observed] >= threshold)
    audit <- data.frame(
      metric = c("definition", "member", "primary", "source", "threshold",
                 "n_observed", "n_one", "n_zero", "n_missing",
                 "n_nested_violations"),
      value = c("Wave IV H4ED2 nested binary", member, "TRUE", "H4ED2",
                threshold, sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE),
                sum(Y == 0L, na.rm = TRUE), sum(is.na(Y)), 0L),
      stringsAsFactors = FALSE)
  } else {
    stop("EducationalAttainment is production-configured only for waves III and IV.",
         call. = FALSE)
  }
  if (!any(!is.na(Y)))
    stop("EducationalAttainment constructor produced no observed outcomes.", call. = FALSE)
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  list(Y = Y, audit = audit,
       support = list(natural_lower = 0, natural_upper = 1,
                      lower_rule = "fixed_binary", upper_rule = "fixed_binary"))
}

# ---- Labor Force Participation (binary) -----------------------------------
join_outcome_fields <- function(main_df, wave, vars, pipeline_cfg, label) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  need <- setdiff(vars, names(main_df))
  if (!length(need)) return(main_df)
  if (is.null(pipeline_cfg))
    stop(label, " requires the full pipeline configuration to read its source file.", call. = FALSE)
  inhome <- read_wave_inhome(wave, pipeline_cfg)
  id_var <- pipeline_cfg$analysis$id_var
  assert_required_columns(inhome, c(id_var, need), sprintf("wave%d inhome", wave))
  left_join_unique(
    main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, need))), id_var,
    x_label = "main_df", y_label = label)
}

assert_only_known_codes <- function(x, known, variable, allow_native_missing = TRUE) {
  z <- to_numeric_codes(x)
  native <- character_native_missing_mask(x)
  bad <- is.finite(z) & !(z %in% known)
  if (any(bad))
    stop(sprintf("%s contains unmapped finite code(s): %s.", variable,
      paste(head(sort(unique(z[bad])), 20L), collapse = ", ")), call. = FALSE)
  if (!isTRUE(allow_native_missing) && any(native))
    stop(variable, " contains native missing values but none are permitted.", call. = FALSE)
  invisible(z)
}

construct_outcome_labor_force_participation <- function(main_df, wave, family_cfg,
                                                       outcome_cfg, member, pipeline_cfg = NULL) {
  wave <- as.integer(wave)
  if (!identical(wave, 4L))
    stop("LaborForceParticipation is production-configured only for Wave IV.",
         call. = FALSE)
  message(sprintf("    [outcome] Labor Force Participation, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) stop(sprintf("LaborForceParticipation is not configured for wave %d.", wave), call. = FALSE)
  main_df <- join_outcome_fields(main_df, wave, unname(src), pipeline_cfg,
                                 sprintf("Wave-%d LFP source", wave))
  Y <- rep(NA_integer_, nrow(main_df))

  if (wave == 3L) {
    cc <- family_cfg$wave3_codes
    h7 <- to_numeric_codes(main_df[[unname(src["current_work"])]])
    known <- unique(c(cc$work_yes, cc$work_no, cc$missing))
    assert_only_known_codes(main_df[[unname(src["current_work"])]], known, "H3LM7")
    Y[h7 == cc$work_yes] <- 1L
    Y[h7 == cc$work_no] <- 0L
    audit <- data.frame(
      metric = c("definition", "n_observed", "n_one", "n_zero", "n_missing",
                 "note"),
      value = c("employment_10plus_proxy", sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE),
                sum(Y == 0L, na.rm = TRUE), sum(is.na(Y)),
                "Wave III cannot distinguish unemployed job-seekers from other nonworkers with this verified item"),
      stringsAsFactors = FALSE)
  } else if (wave == 4L) {
    cc <- family_cfg$wave4_codes
    h6 <- to_numeric_codes(main_df[[unname(src["first_job_current"])]])
    h11 <- to_numeric_codes(main_df[[unname(src["current_work"])]])
    h14 <- to_numeric_codes(main_df[[unname(src["current_status"])]])
    assert_only_known_codes(main_df[[unname(src["first_job_current"])]],
      unique(c(0L, cc$first_job_yes, cc$lm6_missing)), "H4LM6")
    assert_only_known_codes(main_df[[unname(src["current_work"])]],
      unique(c(cc$current_work_no, cc$current_work_yes, cc$lm11_missing)), "H4LM11")
    assert_only_known_codes(main_df[[unname(src["current_status"])]],
      unique(c(1:10, cc$lm14_missing)), "H4LM14")
    # Current employment can be established upstream by H4LM6=1, which
    # legitimately skips H4LM11. Otherwise H4LM11=1 establishes employment.
    Y[h6 == cc$first_job_yes | h11 == cc$current_work_yes] <- 1L
    idx <- is.na(Y) & h11 == cc$current_work_no
    Y[idx & h14 %in% cc$in_labor_force_status] <- 1L
    Y[idx & h14 %in% cc$out_labor_force_status] <- 0L
    audit <- data.frame(
      metric = c("definition", "n_observed", "n_one", "n_zero", "n_missing",
                 "n_h4lm6_employed", "n_h4lm11_employed",
                 "n_status_in_lf", "n_status_out_lf", "n_status_other_unresolved"),
      value = c("route_aware_lfp", sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE),
                sum(Y == 0L, na.rm = TRUE), sum(is.na(Y)),
                sum(h6 == cc$first_job_yes, na.rm = TRUE),
                sum(h11 == cc$current_work_yes, na.rm = TRUE),
                sum(h11 == cc$current_work_no & h14 %in% cc$in_labor_force_status, na.rm = TRUE),
                sum(h11 == cc$current_work_no & h14 %in% cc$out_labor_force_status, na.rm = TRUE),
                sum(h11 == cc$current_work_no & h14 == cc$unresolved_status, na.rm = TRUE)),
      stringsAsFactors = FALSE)
  } else stop("LaborForceParticipation is production-configured only for Wave IV.", call. = FALSE)

  if (!any(!is.na(Y))) stop("LaborForceParticipation constructor produced no observed outcomes.", call. = FALSE)
  message(sprintf("    [outcome] LFP observed=%d, one=%d, zero=%d, missing=%d.",
    sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE), sum(Y == 0L, na.rm = TRUE), sum(is.na(Y))))
  list(Y = Y, audit = audit,
       support = list(natural_lower = 0, natural_upper = 1,
                      lower_rule = "fixed_binary", upper_rule = "fixed_binary"))
}

# ---- Hours Worked (continuous) ---------------------------------------------
construct_outcome_hours_worked <- function(main_df, wave, family_cfg, outcome_cfg,
                                           member, pipeline_cfg = NULL) {
  wave <- as.integer(wave)
  if (!identical(wave, 4L))
    stop("HoursWorked is production-configured only for Wave IV.", call. = FALSE)
  message(sprintf("    [outcome] Hours Worked, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) stop(sprintf("HoursWorked is not configured for wave %d.", wave), call. = FALSE)
  main_df <- join_outcome_fields(main_df, wave, unname(src), pipeline_cfg,
                                 sprintf("Wave-%d hours source", wave))
  cap <- as.numeric(family_cfg$cap_hours %||% 120)
  if (!is.finite(cap) || cap <= 0) stop("HoursWorked cap_hours must be positive and finite.", call. = FALSE)
  Y_uncapped <- rep(NA_real_, nrow(main_df))

  if (wave == 3L) {
    cc <- family_cfg$wave3_codes
    h7 <- to_numeric_codes(main_df[[unname(src["current_work"])]])
    h16 <- to_numeric_codes(main_df[[unname(src["main_job_hours"])]])
    assert_only_known_codes(main_df[[unname(src["current_work"])]],
      unique(c(cc$work_yes, cc$work_no, cc$work_missing)), "H3LM7")
    observed_min <- as.numeric(cc$hours_observed_min %||% cc$hours_valid_min)
    known_h <- c(seq(observed_min, cc$hours_valid_max), cc$hours_missing)
    assert_only_known_codes(main_df[[unname(src["main_job_hours"])]], known_h, "H3LM16")
    Y_uncapped[h7 == cc$work_no] <- 0
    worker <- h7 == cc$work_yes
    valid_hours <- is.finite(h16) & h16 >= cc$hours_valid_min & h16 <= cc$hours_valid_max
    inconsistent_low <- worker & is.finite(h16) &
      h16 >= observed_min & h16 < cc$hours_valid_min
    Y_uncapped[worker & valid_hours] <- h16[worker & valid_hours]
    route <- ifelse(h7 == cc$work_no, "not_working_zero",
                    ifelse(worker & valid_hours, "current_main_job_hours",
                    ifelse(inconsistent_low, "worker_hours_below_10_unresolved", "missing")))
    n_low <- sum(inconsistent_low, na.rm = TRUE)
  } else if (wave == 4L) {
    cc <- family_cfg$wave4_codes
    h6 <- to_numeric_codes(main_df[[unname(src["first_job_current"])]])
    h11 <- to_numeric_codes(main_df[[unname(src["current_work"])]])
    h12 <- to_numeric_codes(main_df[[unname(src["current_jobs"])]])
    h13 <- to_numeric_codes(main_df[[unname(src["total_hours"])]])
    h19 <- to_numeric_codes(main_df[[unname(src["primary_job_hours"])]])
    assert_only_known_codes(main_df[[unname(src["first_job_current"])]], c(0L,1L,6L,7L), "H4LM6")
    assert_only_known_codes(main_df[[unname(src["current_work"])]], c(0L,1L,5L,6L,7L), "H4LM11")
    assert_only_known_codes(main_df[[unname(src["current_jobs"])]],
      unique(c(cc$current_jobs_valid, cc$current_jobs_missing)), "H4LM12")
    assert_only_known_codes(main_df[[unname(src["total_hours"])]],
      unique(c(seq(cc$total_hours_valid_min, cc$total_hours_valid_max), cc$total_hours_missing)), "H4LM13")
    assert_only_known_codes(main_df[[unname(src["primary_job_hours"])]],
      unique(c(seq(cc$primary_hours_valid_min, cc$primary_hours_valid_max), cc$primary_hours_missing)), "H4LM19")
    working <- h6 == cc$first_job_yes | h11 == cc$current_work_yes
    nonworking <- !working & h11 == cc$current_work_no
    Y_uncapped[nonworking] <- 0
    total_route <- working & h12 %in% cc$current_jobs_total_hours_route
    one <- working & h12 == 1
    valid_total <- is.finite(h13) & h13 >= cc$total_hours_valid_min & h13 <= cc$total_hours_valid_max
    valid_primary <- is.finite(h19) & h19 >= cc$primary_hours_valid_min & h19 <= cc$primary_hours_valid_max
    Y_uncapped[total_route & valid_total] <- h13[total_route & valid_total]
    # Deliberately no H4LM19 fallback for respondents routed to H4LM13 with a
    # missing total: primary-job hours would understate total current weekly hours.
    # H4LM12=98 (unknown number of current jobs) is explicitly routed to H4LM13
    # by the Wave-IV questionnaire and therefore uses a valid total if reported.
    Y_uncapped[one & valid_primary] <- h19[one & valid_primary]
    route <- ifelse(nonworking, "not_working_zero",
             ifelse(total_route & valid_total, "multiple_or_unknown_jobs_total_h4lm13",
             ifelse(one & valid_primary, "one_job_h4lm19", "missing")))
    n_low <- 0L
  } else stop("HoursWorked is production-configured only for Wave IV.", call. = FALSE)

  Y <- Y_uncapped
  Y[is.finite(Y)] <- pmin(Y[is.finite(Y)], cap)
  n_capped <- sum(is.finite(Y_uncapped) & Y_uncapped > cap)
  if (any(is.finite(Y) & (Y < 0 | Y > cap)))
    stop("HoursWorked escaped the configured [0, cap] support.", call. = FALSE)
  audit <- data.frame(
    metric = c("definition", "cap_hours", "n_observed", "n_missing", "n_zero",
               "n_capped", "n_wave3_worker_hours_below_10",
               "n_route_not_working_zero", "n_route_primary_or_main_job",
               "n_route_multiple_jobs_total", "n_route_worker_hours_below_10_unresolved",
               "n_route_missing"),
    value = c("unconditional_current_weekly_hours", cap, sum(is.finite(Y)), sum(!is.finite(Y)),
              sum(Y == 0, na.rm = TRUE), n_capped, n_low,
              sum(route == "not_working_zero", na.rm = TRUE),
              sum(route %in% c("current_main_job_hours", "one_job_h4lm19"), na.rm = TRUE),
              sum(route == "multiple_or_unknown_jobs_total_h4lm13", na.rm = TRUE),
              sum(route == "worker_hours_below_10_unresolved", na.rm = TRUE),
              sum(route == "missing", na.rm = TRUE)),
    stringsAsFactors = FALSE)
  message(sprintf("    [outcome] Hours observed=%d, zero=%d, missing=%d, capped=%d; range=[%s,%s].",
    sum(is.finite(Y)), sum(Y == 0, na.rm = TRUE), sum(!is.finite(Y)), n_capped,
    ifelse(any(is.finite(Y)), format(min(Y, na.rm=TRUE)), "NA"),
    ifelse(any(is.finite(Y)), format(max(Y, na.rm=TRUE)), "NA")))
  list(Y = Y, audit = audit, uncapped = Y_uncapped,
       support = list(natural_lower = 0, natural_upper = cap,
                      lower_rule = "natural", upper_rule = "fixed"))
}

# Route the UsualHours function name to the HoursWorked constructor.
construct_outcome_usual_hours <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  stop("UsualHours is hard-blocked; use the verified Wave-IV HoursWorked outcome.",
       call. = FALSE)
}

# ---- Compensation (continuous; log-transformed when configured) -----------
construct_outcome_compensation <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  wave <- as.integer(wave)
  if (!identical(wave, 4L))
    stop("Compensation is production-configured only for Wave IV.", call. = FALSE)
  message(sprintf("    [outcome] Compensation, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("Compensation source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (is.list(src)) {
    exact_v <- src$exact_var; brack_v <- src$bracket_var
  } else {
    stop("Compensation source must be a list with exact_var and bracket_var.", call. = FALSE)
  }
  # Join in the two source variables if not already present
  need <- setdiff(c(exact_v, brack_v), names(main_df))
  if (length(need) > 0L) {
    if (is.null(pipeline_cfg)) stop("Compensation constructor requires the full pipeline configuration to read its source file.", call. = FALSE)
    inhome <- read_wave_inhome(wave, pipeline_cfg)
    id_var <- pipeline_cfg$analysis$id_var
    assert_required_columns(inhome, c(id_var, need), sprintf("wave%d inhome", wave))
    main_df <- left_join_unique(
      main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, need))), id_var,
      x_label = "main_df", y_label = sprintf("wave%d compensation source", wave))
  }
  local_cfg <- list(
    exact_valid_min = family_cfg$exact_valid_min,
    exact_valid_max = family_cfg$exact_valid_max,
    exact_missing_codes = family_cfg$exact_missing_codes,
    bracket_valid_codes = family_cfg$bracket_valid_codes,
    bracket_missing_codes = family_cfg$bracket_missing_codes,
    bracket_map = family_cfg$bracket_map
  )
  comp <- compute_earnings(main_df[[exact_v]], main_df[[brack_v]], local_cfg)
  exact_only <- isTRUE(outcome_cfg$compensation_exact_only %||% FALSE)
  if (exact_only) {
    earnings <- ifelse(comp$source == "exact", comp$earnings, NA_real_)
    source_used <- ifelse(comp$source == "exact", "exact", "missing_exact_only")
  } else {
    earnings <- comp$earnings
    source_used <- comp$source
  }
  transform <- tolower(outcome_cfg$compensation_transform %||% "identity")
  if (identical(transform, "identity")) {
    Y <- earnings
    transform_label <- "earnings"
    natural_lower <- family_cfg$natural_lower_bound %||% 0
  } else if (identical(transform, "log1p")) {
    Y <- log1p(earnings)
    transform_label <- "log1p(earnings)"
    natural_lower <- 0
  } else if (identical(transform, "asinh")) {
    scale_v <- as.numeric(outcome_cfg$compensation_asinh_scale %||% 1000)
    if (!is.finite(scale_v) || scale_v <= 0)
      stop("compensation_asinh_scale must be positive and finite.", call. = FALSE)
    Y <- asinh(earnings / scale_v)
    transform_label <- sprintf("asinh(earnings/%g)", scale_v)
    natural_lower <- 0
  } else {
    stop("Unsupported compensation_transform: ", transform, call. = FALSE)
  }
  audit_extra <- data.frame(
    metric = c("compensation_exact_only", "n_observed_after_exact_only_rule",
               "compensation_transform"),
    value = c(as.character(exact_only), as.character(sum(is.finite(earnings))), transform),
    stringsAsFactors = FALSE)
  audit_out <- rbind(
    base::transform(comp$audit, value = as.character(value)),
    audit_extra)
  message(sprintf("    [outcome] Constructed %d observed, %d missing. %s range: [%s, %s].",
    sum(!is.na(Y)), sum(is.na(Y)), transform_label,
    ifelse(any(!is.na(Y)), sprintf("%.2f", min(Y, na.rm = TRUE)), "NA"),
    ifelse(any(!is.na(Y)), sprintf("%.2f", max(Y, na.rm = TRUE)), "NA")))
  list(
    Y = Y,
    source = source_used,
    audit = audit_out,
    raw = list(exact = comp$exact, bracket = comp$bracket),
    support = list(
      natural_lower = natural_lower,
      natural_upper = if (identical(transform, "identity"))
        family_cfg$natural_upper_bound %||% Inf else Inf,
      lower_rule = "natural",
      upper_rule = family_cfg$upper_bound_rule %||% "weighted_observed_quantile",
      transformation = transform,
      exact_only = exact_only)
  )
}

# ---- Health Status (binary) -----------------------------------------------
# Add Health self-rated health uses 1=Excellent through 5=Poor. At least good
# is 1 for 1-3 and 0 for 4-5; refusal/DK/nonresponse codes remain missing.
construct_outcome_health_status <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  wave <- as.integer(wave)
  message(sprintf("    [outcome] Health Status, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (!wave %in% c(3L, 4L) || is.null(src))
    stop(sprintf("HealthStatus is not configured for wave %d.", wave), call. = FALSE)
  if (!identical(member, "at_least_good"))
    stop(sprintf("Unknown HealthStatus member: %s", member), call. = FALSE)
  main_df <- join_outcome_fields(
    main_df, wave, unname(src), pipeline_cfg,
    sprintf("Wave-%d health-status source", wave))
  raw_source <- main_df[[unname(src)]]
  raw <- to_numeric_codes(raw_source)
  assert_only_known_codes(raw_source, c(1:5, 96L, 98L, 99L), paste0("H", wave, "GH1"))
  observed <- is.finite(raw) & raw %in% 1:5
  Y <- rep(NA_integer_, nrow(main_df))
  Y[observed] <- as.integer(raw[observed] %in% 1:3)
  if (!any(!is.na(Y)))
    stop("HealthStatus constructor produced no observed outcomes.", call. = FALSE)
  audit <- data.frame(
    metric = c("definition", "member", "primary", "source", "n_observed",
               "n_one_at_least_good", "n_zero_fair_or_poor", "n_missing"),
    value = c("At least good self-rated health (1-3 vs 4-5)", member, "TRUE",
              unname(src), sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE),
              sum(Y == 0L, na.rm = TRUE), sum(is.na(Y))),
    stringsAsFactors = FALSE)
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  list(Y = Y, audit = audit,
       support = list(natural_lower = 0, natural_upper = 1,
                      lower_rule = "fixed_binary", upper_rule = "fixed_binary"))
}

# ---- Mental Health (no implemented source mapping) -------------------------
construct_outcome_mental_health <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Mental Health, wave %d. [PLACEHOLDER]", wave))
  rep(NA_real_, nrow(main_df))
}

# ---- Substance Use (no implemented source mapping) -------------------------
construct_outcome_substance_use <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Substance Use, wave %d. [PLACEHOLDER]", wave))
  rep(NA_real_, nrow(main_df))
}

# ---- Pass-through (negative-control / placebo) outcome --------------------
# Uses a PRE-EXISTING numeric column of the built dataset directly as Y, WITHOUT
# running any headline constructor. This is the correct mechanism for a
# negative-control outcome: it swaps the outcome FAMILY so the dispatcher does
# not build the headline outcome and relabel it (setting only analysis$outcome_var
# would do exactly that -- construct_outcome dispatches on cfg$outcome$family, and
# build_main_dataset writes the constructed Y into whatever analysis$outcome_var
# names). The named column must already be present in the built dataset; this
# constructor never fabricates, joins, or transforms it (no log_transform is
# applied here -- set cfg$outcome$log_transform = FALSE for a raw column). The
# downstream [0,1] bounding is computed from this column's own observed values,
# so it adapts automatically; set cfg$analysis$outcome_type ("continuous" or
# "binary") to match the column. Fails loud if the column is absent or non-numeric.
construct_outcome_pass_through <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  src <- family_cfg$source_var
  if (is.null(src) || !is.character(src) || length(src) != 1L || !nzchar(src))
    stop("PassThrough outcome: cfg$outcome$families$PassThrough$source_var must be a single existing column name.", call. = FALSE)
  if (!src %in% names(main_df))
    stop(sprintf("PassThrough outcome: source column '%s' is not present in the built dataset. Add it to the covariate read or pick a present column.", src), call. = FALSE)
  raw <- main_df[[src]]
  raw_chr <- trimws(as.character(raw))
  raw_observed <- !is.na(raw) & !is.na(raw_chr) & raw_chr != ""
  Y <- to_num(raw)
  bad_numeric <- raw_observed & !is.finite(Y)
  if (any(bad_numeric)) {
    examples <- unique(raw_chr[bad_numeric])
    examples <- examples[seq_len(min(length(examples), 5L))]
    stop(sprintf(
      "PassThrough outcome column '%s' contains %d observed nonnumeric value(s) (examples: %s).",
      src, sum(bad_numeric), paste(examples, collapse = ", ")), call. = FALSE)
  }
  if (all(!is.finite(Y)))
    stop(sprintf("PassThrough outcome column '%s' has no finite numeric values.", src), call. = FALSE)
  Y[!is.finite(Y)] <- NA_real_
  message(sprintf("    [outcome] PassThrough negative-control column '%s', wave %d: %d observed, %d missing.",
    src, wave, sum(!is.na(Y)), sum(is.na(Y))))
  Y
}

construct_outcome <- function(main_df, cfg) {
  fam_name <- cfg$outcome$family
  wave     <- cfg$outcome$current_wave %||% cfg$outcome$waves
  if (length(wave) != 1L || !is.numeric(wave))
    stop("construct_outcome(): cfg$outcome$current_wave must be a single integer.", call. = FALSE)
  wave <- as.integer(wave)
  message(sprintf("  [outcome dispatcher] family = '%s', wave = %d.", fam_name, wave))
  fam_cfg <- cfg$outcome$families[[fam_name]]
  if (is.null(fam_cfg))
    stop(sprintf("Unknown outcome family '%s' (not in cfg$outcome$families).", fam_name), call. = FALSE)
  supported <- supported_outcome_waves(fam_name)
  if (!length(supported) || !wave %in% supported)
    stop(sprintf(
      "Outcome dispatcher hard-blocked family='%s', wave=%d; supported wave(s): %s.",
      fam_name, wave,
      if (length(supported)) paste(supported, collapse = ", ") else "none"),
      call. = FALSE)
  if (!is_verified_outcome_spec(cfg, fam_name, wave) &&
      !isTRUE(cfg$safety$allow_unverified_outcome_specs %||% FALSE)) {
    stop(sprintf(
      paste0("Outcome dispatcher blocked an unverified specification: family='%s', wave=%d. ",
             "Verify the constructor against the codebook or deliberately set ",
             "cfg$safety$allow_unverified_outcome_specs=TRUE."),
      fam_name, wave), call. = FALSE)
  }
  member <- cfg$outcome$family_member
  if (identical(fam_cfg$type, "binary_nested") && is.null(member)) {
    member <- names(fam_cfg$members)[length(fam_cfg$members)]
    message(sprintf("  [outcome dispatcher] No family_member set; defaulting to highest threshold '%s'.", member))
  }
  outcome_result <- switch(fam_name,
    EducationalAttainment   = construct_outcome_educational_attainment(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    LaborForceParticipation = construct_outcome_labor_force_participation(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    HoursWorked             = construct_outcome_hours_worked(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    UsualHours              = construct_outcome_usual_hours(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    Compensation            = construct_outcome_compensation(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    HealthStatus            = construct_outcome_health_status(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    MentalHealth            = construct_outcome_mental_health(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    SubstanceUse            = construct_outcome_substance_use(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    PassThrough             = construct_outcome_pass_through(main_df, wave, fam_cfg, cfg$outcome, member, cfg),
    stop(sprintf("Unknown outcome family '%s'.", fam_name), call. = FALSE))
  if (is.list(outcome_result) && !is.null(outcome_result$Y)) {
    Y <- outcome_result$Y
  } else {
    Y <- outcome_result
    outcome_result <- list(Y = Y)
  }
  if (isTRUE(cfg$outcome$stop_on_all_missing_outcome) && !isTRUE(cfg$safety$allow_placeholder_outcomes %||% FALSE) && all(is.na(Y))) {
    stop(sprintf(
      "Outcome constructor for family '%s' wave %d returned all missing values. This usually means the selected outcome family is still a placeholder or the configured source variables are unavailable.",
      fam_name, wave), call. = FALSE)
  }
  # The constructor may have left-joined source columns into main_df via a
  # local copy; re-attach them by re-reading here would duplicate work. We
  # assume the return vector Y is aligned row-for-row with main_df, which it
  # is because dplyr::left_join preserves row order for existing keys and the
  # constructors do not reorder.
  outcome_result$fam_name <- fam_name
  outcome_result$wave <- wave
  outcome_result$member <- member
  outcome_result
}

# hard-coded transform of H1GH50 (usual bedtime). H1GH50 is stored as
# a 12-hour clock STRING ("HH:MMA"/"HH:MMP", hours 00-12, minutes 00-59) with
# string sentinels "999996"/"999998"/"999999". It is parsed to minutes-since-
# midnight (AM 12->0, PM 12->12, PM h->h+12; hour 00 treated as 12 on the
# clock face) and encoded as H1GH50__tsin / H1GH50__tcos = sin/cos of the
# 24h angle, preserving the midnight wraparound. Sentinel and unparseable
# values become NA in both columns and are handled by the existing
# missingness machinery. The transform is gated by transform_time_variables.
transform_time_variables_in_df <- function(df, cfg) {
  if (!isTRUE(cfg$analysis$transform_time_variables %||% FALSE)) return(df)
  v <- "H1GH50"
  if (!(v %in% names(df))) {
    msg("  [time] H1GH50 not present; nothing to transform.", cfg = cfg)
    return(df)
  }
  raw <- trimws(as.character(df[[v]]))                  # column is character
  sentinels <- c("999996", "999998", "999999")          # refused / DK / N/A
  is_sent   <- raw %in% sentinels | is.na(raw) | raw == ""
  # Parse "HH:MM" + A/P. Hours 00-12 are all accepted (this encoding uses 00),
  # minutes 00-59, suffix A or P (case-insensitive). The suffix may be followed
  # by an optional "M"/"m" ("AM"/"PM") and preceded by optional whitespace, to
  # absorb extract-to-extract variation in how the meridiem is coded.
  m <- regmatches(raw, regexec("^([0-9]{1,2}):([0-5][0-9])\\s*([AaPp])[Mm]?$", raw))
  hour12 <- rep(NA_real_, length(raw))
  minute <- rep(NA_real_, length(raw))
  ampm   <- rep(NA_character_, length(raw))
  for (i in seq_along(m)) {
    g <- m[[i]]
    if (length(g) == 4L) {
      hour12[i] <- as.numeric(g[2]); minute[i] <- as.numeric(g[3])
      ampm[i]   <- toupper(g[4])
    }
  }
  # Treat hour 00 as 12 on the 12-hour face (same clock position) so the
  # AM/PM rule below applies uniformly.
  hour12[is.finite(hour12) & hour12 == 0] <- 12
  parsed_ok <- !is.na(hour12) & !is.na(minute) & !is.na(ampm) &
               hour12 >= 1 & hour12 <= 12 & !is_sent
  # 12h -> 24h: AM 12 -> 0, AM h -> h ; PM 12 -> 12, PM h -> h+12.
  hour24 <- ifelse(ampm == "A",
                   ifelse(hour12 == 12, 0, hour12),
                   ifelse(hour12 == 12, 12, hour12 + 12))
  minutes <- ifelse(parsed_ok, hour24 * 60 + minute, NA_real_)
  # Cyclical encoding preserves the midnight wraparound (bedtimes cluster on
  # both sides of midnight).
  ang <- 2 * pi * minutes / 1440
  df[[paste0(v, "__tsin")]] <- sin(ang)
  df[[paste0(v, "__tcos")]] <- cos(ang)
  df[[v]] <- NULL
  msg(sprintf("  [time] H1GH50 (12h A/P) -> sin/cos: %d parsed, %d sentinel, %d UNPARSEABLE of %d.",
      sum(parsed_ok), sum(is_sent), sum(!parsed_ok & !is_sent), length(raw)), cfg = cfg)
  df
}

validate_final_sample_design_fields <- function(df, cfg) {
  psu_var <- cfg$analysis$cluster_var
  strata_var <- cfg$analysis$strata_var
  assert_required_columns(df, c(psu_var, strata_var), "final analytic sample")
  psu <- as.character(df[[psu_var]])
  strata <- as.character(df[[strata_var]])
  if (anyNA(psu) || any(!nzchar(trimws(psu))))
    stop(sprintf("Final analytic sample has missing or blank %s values.", psu_var), call. = FALSE)
  if (anyNA(strata) || any(!nzchar(trimws(strata))))
    stop(sprintf(paste0("Final analytic sample has missing or blank %s values. ",
                        "No rows were dropped; repair the merge/design field before running."),
                 strata_var), call. = FALSE)
  psu_strata_n <- tapply(strata, psu, function(z) length(unique(z)))
  if (any(psu_strata_n != 1L))
    stop(sprintf("At least one %s maps to multiple %s values.", psu_var, strata_var), call. = FALSE)
  psu_per_stratum <- tapply(psu, strata, function(z) length(unique(z)))
  expected_h <- as.integer(cfg$analysis$expected_strata_n %||% 4L)
  if (is.finite(expected_h) && expected_h > 0L && length(psu_per_stratum) != expected_h)
    stop(sprintf("Expected %d observed %s levels, found %d.",
                 expected_h, strata_var, length(psu_per_stratum)), call. = FALSE)
  if (any(psu_per_stratum < 2L))
    stop(sprintf("Every observed %s must contain at least two sampled %s values.",
                 strata_var, psu_var), call. = FALSE)
  list(n_strata = length(psu_per_stratum),
       psu_per_stratum = as.integer(psu_per_stratum),
       strata_labels = names(psu_per_stratum))
}

validate_expected_final_sample_gates <- function(df, cfg) {
  if (!isTRUE(cfg$analysis$enforce_expected_sample_gates %||% FALSE))
    return(invisible(list(enforced = FALSE)))

  expected_n <- as.integer(cfg$analysis$expected_final_n)
  expected_j <- as.integer(cfg$analysis$expected_cluster_n)
  found_t <- sum(df[[cfg$analysis$exposure_var]] == 1L)
  found_j <- length(unique(df[[cfg$analysis$cluster_var]]))

  cutpoint_gate <- isTRUE(cfg$analysis$enforce_expected_cutpoint_gate %||% TRUE)
  treated_gate <- isTRUE(cfg$analysis$enforce_expected_treated_gate %||% TRUE)
  expected_cutpoint <- as.numeric(cfg$analysis$expected_exposure_cutpoint)
  expected_t <- as.integer(cfg$analysis$expected_treated_n)
  found_cutpoint <- as.numeric(cfg$exposure$cutpoint)

  cutpoint_ok <- !cutpoint_gate || isTRUE(all.equal(found_cutpoint, expected_cutpoint))
  treated_ok <- !treated_gate || found_t == expected_t
  failed <- nrow(df) != expected_n || found_j != expected_j ||
    !cutpoint_ok || !treated_ok

  if (failed) {
    expected_parts <- c(sprintf("n=%d", expected_n), sprintf("PSUs=%d", expected_j))
    if (cutpoint_gate)
      expected_parts <- c(expected_parts, sprintf("cutpoint=%s", as.character(expected_cutpoint)))
    if (treated_gate)
      expected_parts <- c(expected_parts, sprintf("treated=%d", expected_t))
    stop(sprintf(paste0(
      "Final analytic-sample gate failed after design-field validation: ",
      "expected %s; found n=%d, PSUs=%d, treated=%d at cutpoint %s."),
      paste(expected_parts, collapse = ", "),
      nrow(df), found_j, found_t, as.character(found_cutpoint)),
      call. = FALSE)
  }

  invisible(list(
    enforced = TRUE,
    n = nrow(df),
    treated = found_t,
    psus = found_j,
    exposure_cutpoint = found_cutpoint,
    cutpoint_gate_applied = cutpoint_gate,
    treated_gate_applied = treated_gate
  ))
}

derive_mortality_indicator_from_data <- function(mortality, cfg) {
  mort <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
  id_var <- cfg$analysis$id_var
  source_var <- mort$source_var
  source_month_var <- mort$source_month_var
  death_year_var <- mort$derived_death_year_var
  death_month_var <- mort$derived_death_month_var
  raw_window_var <- mort$death_in_window_var
  assert_required_columns(
    mortality, c(id_var, source_var, source_month_var), "mortality linkage")
  mortality <- mortality[, c(id_var, source_var, source_month_var), drop = FALSE]
  mortality <- coerce_join_key(mortality, id_var, "mortality linkage",
                               require_complete = TRUE)
  assert_unique_key(mortality, id_var, "mortality linkage",
                    require_complete = TRUE)

  raw_year <- mortality[[source_var]]
  year_native_missing <- character_native_missing_mask(raw_year)
  year_num <- to_numeric_codes(raw_year)
  integer_year <- is.finite(year_num) & abs(year_num - round(year_num)) <= 1e-8
  year_int <- rep(NA_integer_, length(year_num))
  year_int[integer_year] <- as.integer(round(year_num[integer_year]))
  valid_year <- integer_year &
    year_int >= as.integer(mort$valid_year_min) &
    year_int <= as.integer(mort$valid_year_max)
  no_death_code <- is.finite(year_num) &
    year_num %in% as.numeric(mort$no_death_codes %||% numeric(0))
  usable_death_year <- valid_year & !no_death_code
  recognized_no_death <- no_death_code |
    (year_native_missing & isTRUE(mort$native_missing_means_no_death))
  recognized_year <- usable_death_year | recognized_no_death
  unrecognized_year <- !recognized_year
  if (any(unrecognized_year)) {
    examples <- unique(trimws(as.character(raw_year[unrecognized_year])))
    examples[is.na(examples) | examples == ""] <- "<native missing>"
    examples <- head(examples, 8L)
    message_text <- sprintf(
      "Mortality source %s contains %d unrecognized value(s); examples: %s.",
      source_var, sum(unrecognized_year), paste(examples, collapse = ", "))
    if (isTRUE(mort$fail_on_unrecognized_codes)) stop(message_text, call. = FALSE)
    warning(message_text,
      " Treating them as no recorded death because the gate is disabled.",
      call. = FALSE)
  }

  raw_month <- mortality[[source_month_var]]
  month_native_missing <- character_native_missing_mask(raw_month)
  month_num <- to_numeric_codes(raw_month)
  integer_month <- is.finite(month_num) & abs(month_num - round(month_num)) <= 1e-8
  month_int <- rep(NA_integer_, length(month_num))
  month_int[integer_month] <- as.integer(round(month_num[integer_month]))
  invalid_month_code <- is.finite(month_num) &
    month_num %in% as.numeric(mort$invalid_month_codes %||% integer(0))
  valid_month <- integer_month &
    month_int >= as.integer(mort$valid_month_min) &
    month_int <= as.integer(mort$valid_month_max)
  recognized_month <- month_native_missing | invalid_month_code | valid_month
  unrecognized_month <- !recognized_month
  if (any(unrecognized_month)) {
    examples <- unique(trimws(as.character(raw_month[unrecognized_month])))
    examples[is.na(examples) | examples == ""] <- "<native missing>"
    examples <- head(examples, 8L)
    stop(sprintf(
      paste0("Mortality month source %s contains %d nonmissing code(s) that are ",
             "neither valid months 1-12 nor configured invalid code(s); examples: %s."),
      source_month_var, sum(unrecognized_month), paste(examples, collapse = ", ")),
      call. = FALSE)
  }

  death_year <- rep(NA_integer_, length(year_int))
  death_year[usable_death_year] <- year_int[usable_death_year]
  death_month <- rep(NA_integer_, length(month_int))
  death_month[usable_death_year & valid_month] <-
    month_int[usable_death_year & valid_month]

  death_in_window <- as.integer(usable_death_year &
    year_int >= as.integer(mort$death_year_start) &
    year_int <= as.integer(mort$death_year_end))
  if (anyNA(death_in_window) || any(!death_in_window %in% c(0L, 1L)))
    stop("Internal mortality derivation error: incomplete/nonbinary raw-window indicator.",
         call. = FALSE)

  out <- mortality[, id_var, drop = FALSE]
  out[[death_year_var]] <- death_year
  out[[death_month_var]] <- death_month
  out[[raw_window_var]] <- death_in_window
  audit <- data.frame(
    source_variable = source_var,
    source_year_variable = source_var,
    source_month_variable = source_month_var,
    derived_death_year_variable = death_year_var,
    derived_death_month_variable = death_month_var,
    raw_window_variable = raw_window_var,
    death_year_start = as.integer(mort$death_year_start),
    death_year_end = as.integer(mort$death_year_end),
    n_mortality_records = nrow(mortality),
    n_valid_calendar_years = sum(valid_year),
    n_calendar_years_eligible_as_death = sum(usable_death_year),
    n_no_death_codes_overlapping_year_range = sum(no_death_code & valid_year),
    n_native_missing = sum(year_native_missing),
    n_native_missing_year = sum(year_native_missing),
    n_explicit_no_death_codes = sum(no_death_code),
    n_unrecognized_codes = sum(unrecognized_year),
    n_unrecognized_year_codes = sum(unrecognized_year),
    n_valid_death_months = sum(usable_death_year & is.finite(death_month)),
    n_unknown_death_months = sum(usable_death_year & !is.finite(death_month)),
    n_month_present_without_usable_death_year = sum(valid_month & !usable_death_year),
    n_native_missing_death_months = sum(usable_death_year & month_native_missing),
    n_997_death_months = sum(usable_death_year & invalid_month_code & month_num == 997),
    n_unrecognized_month_codes = sum(unrecognized_month),
    n_deaths_in_raw_window = sum(death_in_window),
    native_missing_means_no_death = isTRUE(mort$native_missing_means_no_death),
    stringsAsFactors = FALSE)
  list(data = out, audit = audit)
}

normalize_interview_timing <- function(interview_timing, mc, id_var,
                                       strict = TRUE) {
  iy <- mc$interview_year_var %||% NULL
  im <- mc$interview_month_var %||% NULL
  if (is.null(iy) || is.null(im))
    stop("Interview timing normalization requires configured year and month variables.",
         call. = FALSE)
  assert_required_columns(
    interview_timing, c(id_var, iy, im),
    sprintf("Wave-%d interview timing", mc$outcome_wave))
  timing <- interview_timing[, c(id_var, iy, im), drop = FALSE]
  timing <- coerce_join_key(
    timing, id_var, sprintf("Wave-%d interview timing", mc$outcome_wave),
    require_complete = TRUE)
  assert_unique_key(
    timing, id_var, sprintf("Wave-%d interview timing", mc$outcome_wave),
    require_complete = TRUE)

  raw_y <- timing[[iy]]
  raw_m <- timing[[im]]
  native_y <- character_native_missing_mask(raw_y)
  native_m <- character_native_missing_mask(raw_m)
  num_y <- to_numeric_codes(raw_y)
  num_m <- to_numeric_codes(raw_m)
  int_y <- is.finite(num_y) & abs(num_y - round(num_y)) <= 1e-8
  int_m <- is.finite(num_m) & abs(num_m - round(num_m)) <= 1e-8
  valid_y <- int_y & num_y >= as.integer(mc$interview_year_valid_min) &
    num_y <= as.integer(mc$interview_year_valid_max)
  valid_m <- int_m & num_m >= as.integer(mc$interview_month_valid_min) &
    num_m <= as.integer(mc$interview_month_valid_max)
  invalid_y <- !native_y & !valid_y
  invalid_m <- !native_m & !valid_m
  if (isTRUE(strict) && any(invalid_y))
    stop(sprintf(
      "Wave-%d interview year contains nonmissing values outside the configured valid range: %s",
      mc$outcome_wave,
      paste(head(sort(unique(as.character(raw_y[invalid_y]))), 10L), collapse = ", ")),
      call. = FALSE)
  if (isTRUE(strict) && any(invalid_m))
    stop(sprintf(
      "Wave-%d interview month contains nonmissing values outside 1-12: %s",
      mc$outcome_wave,
      paste(head(sort(unique(as.character(raw_m[invalid_m]))), 10L), collapse = ", ")),
      call. = FALSE)

  year <- rep(NA_integer_, nrow(timing))
  month <- rep(NA_integer_, nrow(timing))
  year[valid_y] <- as.integer(round(num_y[valid_y]))
  month[valid_m] <- as.integer(round(num_m[valid_m]))
  orphan_month <- is.finite(month) & !is.finite(year)
  if (isTRUE(strict) && any(orphan_month))
    stop(sprintf("Wave-%d interview timing has a valid month without a valid year.",
                 mc$outcome_wave), call. = FALSE)
  month[orphan_month] <- NA_integer_
  timing[[iy]] <- year
  timing[[im]] <- month

  complete <- is.finite(year) & is.finite(month)
  if (!any(complete))
    stop(sprintf(
      "Wave-%d interview timing contains no complete valid year-month values; cannot define the fieldwork fallback.",
      mc$outcome_wave), call. = FALSE)
  ym_key <- year[complete] * 12L + (month[complete] - 1L)
  first_key <- min(ym_key)
  first_year <- as.integer(first_key %/% 12L)
  first_month <- as.integer(first_key %% 12L + 1L)
  last_key <- max(ym_key)
  last_year <- as.integer(last_key %/% 12L)
  last_month <- as.integer(last_key %% 12L + 1L)

  audit <- data.frame(
    outcome_wave = as.integer(mc$outcome_wave),
    interview_year_variable = iy,
    interview_month_variable = im,
    n_wave_timing_rows = nrow(timing),
    n_complete_interview_dates = sum(complete),
    n_year_only_interview_dates = sum(is.finite(year) & !is.finite(month)),
    n_missing_interview_years = sum(!is.finite(year)),
    n_invalid_nonmissing_years = sum(invalid_y),
    n_invalid_nonmissing_months = sum(invalid_m),
    earliest_interview_year = first_year,
    earliest_interview_month = first_month,
    latest_interview_year = last_year,
    latest_interview_month = last_month,
    stringsAsFactors = FALSE)
  list(data = timing, fieldwork_start_year = first_year,
       fieldwork_start_month = first_month,
       fieldwork_end_year = last_year,
       fieldwork_end_month = last_month, audit = audit)
}

classify_mortality_timing <- function(raw_window, death_year, death_month,
                                      interview_year, interview_month,
                                      mc, fieldwork_start_year = NA_integer_,
                                      fieldwork_start_month = NA_integer_,
                                      fieldwork_end_year = NA_integer_,
                                      fieldwork_end_month = NA_integer_) {
  raw_window <- as.integer(raw_window)
  if (anyNA(raw_window) || any(!raw_window %in% c(0L, 1L)))
    stop("Mortality timing classification requires a complete 0/1 raw-window indicator.",
         call. = FALSE)
  n <- length(raw_window)
  if (length(death_year) != n || length(death_month) != n ||
      length(interview_year) != n || length(interview_month) != n)
    stop("Mortality timing classification vector lengths differ.", call. = FALSE)

  mode <- as.character(mc$timing_mode %||% "fixed_window")
  death <- integer(n)
  status <- rep("no_death_or_outside_horizon", n)
  candidate <- raw_window == 1L

  if (identical(mode, "fixed_window")) {
    death[candidate] <- 1L
    status[candidate] <- "fixed_window_death"
    return(list(death = death, status = status))
  }
  if (!identical(mode, "interview_month"))
    stop("Unknown mortality timing mode: ", mode, call. = FALSE)
  if (!is.finite(fieldwork_end_year) || !is.finite(fieldwork_end_month) ||
      fieldwork_end_month < 1L || fieldwork_end_month > 12L)
    stop("Interview-month mortality requires a finite fieldwork-end year/month.",
         call. = FALSE)

  iy_ok <- is.finite(interview_year)
  im_ok <- is.finite(interview_month)
  dm_ok <- is.finite(death_month)

  idx <- which(candidate & iy_ok & death_year < interview_year)
  death[idx] <- 1L; status[idx] <- "before_interview_year"
  idx <- which(candidate & iy_ok & death_year > interview_year)
  status[idx] <- "after_interview_year"

  same_year <- candidate & iy_ok & death_year == interview_year
  idx <- which(same_year & dm_ok & im_ok & death_month < interview_month)
  death[idx] <- 1L; status[idx] <- "before_interview_month"
  idx <- which(same_year & dm_ok & im_ok & death_month > interview_month)
  status[idx] <- "after_interview_month"
  idx <- which(same_year & dm_ok & im_ok & death_month == interview_month)
  status[idx] <- "same_interview_month_unordered"
  idx <- which(same_year & !(dm_ok & im_ok))
  status[idx] <- "same_interview_year_month_unknown"

  no_interview <- candidate & !iy_ok
  idx <- which(no_interview & death_year < fieldwork_end_year)
  death[idx] <- 1L; status[idx] <- "before_fieldwork_end_year_no_interview"
  idx <- which(no_interview & death_year > fieldwork_end_year)
  status[idx] <- "after_fieldwork_end_year_no_interview"
  same_end_year <- no_interview & death_year == fieldwork_end_year
  idx <- which(same_end_year & dm_ok & death_month <= fieldwork_end_month)
  death[idx] <- 1L
  status[idx] <- "on_or_before_fieldwork_end_month_no_interview"
  idx <- which(same_end_year & dm_ok & death_month > fieldwork_end_month)
  status[idx] <- "after_fieldwork_end_month_no_interview"
  idx <- which(same_end_year & !dm_ok)
  status[idx] <- "fieldwork_end_year_death_month_unknown_no_interview"

  if (anyNA(death) || any(!death %in% c(0L, 1L)))
    stop("Internal mortality timing classification produced a nonbinary indicator.",
         call. = FALSE)
  list(death = death, status = status)
}

merge_mortality_indicator_from_data <- function(main_sample, mortality, cfg,
                                                interview_timing = NULL) {
  mort <- derive_mortality_indicator_from_data(mortality, cfg)
  mc <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
  id_var <- cfg$analysis$id_var
  death_var <- mc$death_before_outcome_var
  death_year_var <- mc$derived_death_year_var
  death_month_var <- mc$derived_death_month_var
  raw_window_var <- mc$death_in_window_var
  timing_status_var <- mc$timing_status_var
  interview_year_var <- mc$interview_year_var %||% NULL
  interview_month_var <- mc$interview_month_var %||% NULL
  role_vars <- unique(c(
    death_var, death_year_var, death_month_var, raw_window_var,
    timing_status_var, interview_year_var %||% character(0),
    interview_month_var %||% character(0)))
  conflicts <- canonical_role_key(role_vars) %in% canonical_role_key(names(main_sample))
  if (any(conflicts))
    stop("Mortality-derived or interview-timing variable already exists before linkage: ",
         paste(role_vars[conflicts], collapse = ", "), call. = FALSE)

  joined <- left_join_unique(
    main_sample, mort$data, id_var,
    x_label = "main analytic sample", y_label = "mortality linkage",
    require_complete_x = TRUE, require_complete_y = TRUE)
  matched <- !is.na(joined[[raw_window_var]])
  if (isTRUE(cfg$preprocessing$drop_invalid_weights_at_build %||% TRUE) &&
      cfg$analysis$weight_var %in% names(joined)) {
    linkage_required <- suppressWarnings(as.numeric(joined[[cfg$analysis$weight_var]]))
    linkage_required <- is.finite(linkage_required) & linkage_required > 0
  } else linkage_required <- rep(TRUE, nrow(joined))
  unmatched <- !matched
  n_unmatched <- sum(unmatched)
  n_unmatched_required <- sum(unmatched & linkage_required)
  if (n_unmatched_required > 0L && isTRUE(mc$require_complete_linkage %||% TRUE))
    stop(sprintf("Mortality linkage did not cover %d valid-weight analytic respondent(s).",
                 n_unmatched_required), call. = FALSE)
  if (n_unmatched > 0L) {
    warning(sprintf(
      paste0("Mortality linkage omitted %d row(s), including %d valid-weight analytic row(s); ",
             "treating unmatched rows as no recorded death under the configured linkage rule."),
      n_unmatched, n_unmatched_required), call. = FALSE)
    joined[[raw_window_var]][unmatched] <- 0L
    joined[[death_year_var]][unmatched] <- NA_integer_
    joined[[death_month_var]][unmatched] <- NA_integer_
  }

  mode <- as.character(mc$timing_mode %||% "fixed_window")
  interview_year <- rep(NA_integer_, nrow(joined))
  interview_month <- rep(NA_integer_, nrow(joined))
  timing_audit <- data.frame()
  fieldwork_start_year <- NA_integer_
  fieldwork_start_month <- NA_integer_
  fieldwork_end_year <- NA_integer_
  fieldwork_end_month <- NA_integer_

  if (!is.null(interview_year_var) && !is.null(interview_month_var) &&
      !is.null(interview_timing)) {
    norm <- normalize_interview_timing(
      interview_timing, mc, id_var,
      strict = identical(mode, "interview_month"))
    timing <- norm$data
    fieldwork_start_year <- norm$fieldwork_start_year
    fieldwork_start_month <- norm$fieldwork_start_month
    fieldwork_end_year <- norm$fieldwork_end_year
    fieldwork_end_month <- norm$fieldwork_end_month
    joined <- left_join_unique(
      joined, timing, id_var,
      x_label = "mortality-linked analytic sample", y_label = "interview timing",
      require_complete_x = TRUE, require_complete_y = TRUE)
    interview_year <- as.integer(joined[[interview_year_var]])
    interview_month <- as.integer(joined[[interview_month_var]])
    timing_audit <- norm$audit
  } else {
    if (!is.null(interview_year_var)) joined[[interview_year_var]] <- interview_year
    if (!is.null(interview_month_var)) joined[[interview_month_var]] <- interview_month
  }

  if (identical(mode, "interview_month") && is.null(interview_timing))
    stop(sprintf(
      "Wave-%d interview-month mortality requires the full wave interview timing table.",
      mc$outcome_wave), call. = FALSE)

  classified <- classify_mortality_timing(
    raw_window = joined[[raw_window_var]],
    death_year = as.integer(joined[[death_year_var]]),
    death_month = as.integer(joined[[death_month_var]]),
    interview_year = interview_year,
    interview_month = interview_month,
    mc = mc,
    fieldwork_start_year = fieldwork_start_year,
    fieldwork_start_month = fieldwork_start_month,
    fieldwork_end_year = fieldwork_end_year,
    fieldwork_end_month = fieldwork_end_month)
  joined[[death_var]] <- as.integer(classified$death)
  joined[[timing_status_var]] <- as.character(classified$status)

  death <- joined[[death_var]] == 1L
  raw_candidate <- joined[[raw_window_var]] == 1L
  mort$audit$n_linkage_left_rows <- nrow(joined)
  mort$audit$n_valid_weight_rows_requiring_linkage <- sum(linkage_required)
  mort$audit$n_all_rows_matched <- sum(matched)
  mort$audit$n_all_rows_unmatched <- n_unmatched
  mort$audit$n_required_rows_unmatched <- n_unmatched_required
  mort$audit$n_nonrequired_rows_unmatched <- sum(unmatched & !linkage_required)
  mort$audit$n_linked_deaths_in_raw_window <- sum(raw_candidate)
  mort$audit$n_deaths_classified_before_outcome <- sum(death)
  mort$audit$mortality_timing_mode <- mode
  mort$audit$mortality_classification_rule <- mortality_timing_rule_text(mc)
  mort$audit$fieldwork_start_year <- fieldwork_start_year
  mort$audit$fieldwork_start_month <- fieldwork_start_month
  mort$audit$fieldwork_end_year <- fieldwork_end_year
  mort$audit$fieldwork_end_month <- fieldwork_end_month
  mort$audit$n_interview_year_observed <- sum(is.finite(interview_year))
  mort$audit$n_interview_month_observed <- sum(is.finite(interview_month))
  mort$audit$n_complete_interview_dates <-
    sum(is.finite(interview_year) & is.finite(interview_month))
  mort$audit$n_death_month_unknown_in_raw_window <-
    sum(raw_candidate & !is.finite(joined[[death_month_var]]))
  mort$audit$n_same_interview_month_unordered <-
    sum(joined[[timing_status_var]] == "same_interview_month_unordered")
  mort$audit$n_same_interview_year_month_unknown <-
    sum(joined[[timing_status_var]] == "same_interview_year_month_unknown")

  status_tab <- as.data.frame(table(joined[[timing_status_var]]),
                              stringsAsFactors = FALSE)
  names(status_tab) <- c("timing_status", "n")
  status_tab$outcome_wave <- as.integer(mc$outcome_wave)
  status_tab$timing_mode <- mode
  status_tab$fieldwork_start_year <- fieldwork_start_year
  status_tab$fieldwork_start_month <- fieldwork_start_month
  status_tab$fieldwork_end_year <- fieldwork_end_year
  status_tab$fieldwork_end_month <- fieldwork_end_month
  status_tab$interview_year_variable <- interview_year_var %||% NA_character_
  status_tab$interview_month_variable <- interview_month_var %||% NA_character_
  if (nrow(timing_audit)) {
    for (nm in setdiff(names(timing_audit), names(status_tab)))
      status_tab[[nm]] <- timing_audit[[nm]][1L]
  }

  list(data = joined, audit = mort$audit,
       interview_timing_audit = status_tab)
}

merge_mortality_indicator <- function(main_sample, cfg) {
  mc <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
  if (!isTRUE(mc$enabled)) return(main_sample)
  mortality <- read_xpt_df(cfg$paths$mortality)

  timing <- NULL
  iy <- mc$interview_year_var %||% NULL
  im <- mc$interview_month_var %||% NULL
  if (!is.null(iy) && !is.null(im)) {
    wave_df <- read_wave_inhome(cfg$outcome$current_wave, cfg)
    timing_vars <- c(cfg$analysis$id_var, iy, im)
    assert_required_columns(
      wave_df, timing_vars,
      sprintf("Wave-%d interview timing", cfg$outcome$current_wave))
    timing <- wave_df[, timing_vars, drop = FALSE]
  }

  linked <- merge_mortality_indicator_from_data(
    main_sample, mortality, cfg, interview_timing = timing)
  if (isTRUE(cfg$global$save_stage_csvs)) {
    write_run_csv(linked$audit, cfg, mc$linkage_audit_csv)
    if (!is.null(mc$interview_timing_audit_csv) &&
        !is.null(linked$interview_timing_audit) &&
        nrow(linked$interview_timing_audit))
      write_run_csv(
        linked$interview_timing_audit, cfg, mc$interview_timing_audit_csv)
  }
  linked$data
}

apply_mortality_composite <- function(main_sample, cfg) {
  mc <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
  death_var <- mc$death_before_outcome_var
  outcome_var <- cfg$analysis$outcome_var
  assert_required_columns(
    main_sample, c(death_var, outcome_var), "mortality-composite outcome")
  death <- to_numeric_codes(main_sample[[death_var]])
  if (anyNA(death) || any(!death %in% c(0, 1)))
    stop("Death-before-outcome indicator must be complete and exactly 0/1.",
         call. = FALSE)
  death <- as.integer(death)
  if (!is.numeric(main_sample[[outcome_var]]) &&
      !is.integer(main_sample[[outcome_var]]))
    stop("Mortality-composite outcome must be numeric before death recoding.",
         call. = FALSE)
  support <- attr(main_sample, "outcome_support")
  if (is.list(support)) {
    lower <- suppressWarnings(as.numeric(
      support$natural_lower %||% support$lower %||% NA_real_))
    upper <- suppressWarnings(as.numeric(
      support$natural_upper %||% support$upper %||% NA_real_))
    if ((is.finite(lower) && lower > 0) || (is.finite(upper) && upper < 0))
      stop("Mortality-composite zero lies outside the configured outcome support.",
           call. = FALSE)
  }

  original_y <- as.numeric(main_sample[[outcome_var]])
  observed_death <- death == 1L & is.finite(original_y)
  n_observed_death <- sum(observed_death)
  death_year_var <- mc$derived_death_year_var
  death_month_var <- mc$derived_death_month_var
  interview_year_var <- mc$interview_year_var %||% NULL
  interview_month_var <- mc$interview_month_var %||% NULL
  timing_status_var <- mc$timing_status_var %||% NULL
  audit <- data.frame(
    n_total = nrow(main_sample),
    n_deaths_before_outcome = sum(death == 1L),
    n_deaths_with_observed_original_outcome = n_observed_death,
    n_deaths_with_missing_original_outcome =
      sum(death == 1L & !is.finite(original_y)),
    n_deaths_recoded_to_observed_zero = sum(death == 1L),
    contradiction_gate_enabled = isTRUE(
      mc$fail_on_death_with_observed_original_outcome %||% TRUE),
    mortality_timing_mode = as.character(mc$timing_mode %||% "fixed_window"),
    composite_definition = paste0(
      "Configured outcome set to zero when pre-outcome death is classified by: ",
      mortality_timing_rule_text(mc)),
    stringsAsFactors = FALSE)

  if (n_observed_death > 0L) {
    contradiction <- data.frame(
      respondent_id = main_sample[[cfg$analysis$id_var]][observed_death],
      death_indicator = death[observed_death],
      original_outcome = original_y[observed_death],
      original_outcome_source = if ("EarningsSource" %in% names(main_sample))
        as.character(main_sample$EarningsSource[observed_death]) else NA_character_,
      death_year = if (death_year_var %in% names(main_sample))
        main_sample[[death_year_var]][observed_death] else NA_integer_,
      death_month = if (death_month_var %in% names(main_sample))
        main_sample[[death_month_var]][observed_death] else NA_integer_,
      interview_year = if (!is.null(interview_year_var) &&
          interview_year_var %in% names(main_sample))
        main_sample[[interview_year_var]][observed_death] else NA_integer_,
      interview_month = if (!is.null(interview_month_var) &&
          interview_month_var %in% names(main_sample))
        main_sample[[interview_month_var]][observed_death] else NA_integer_,
      timing_status = if (!is.null(timing_status_var) &&
          timing_status_var %in% names(main_sample))
        as.character(main_sample[[timing_status_var]][observed_death]) else NA_character_,
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$global$save_stage_csvs %||% TRUE))
      write_run_csv(contradiction, cfg, mc$contradiction_audit_csv)
    contradiction_message <- sprintf(
      paste0("Mortality linkage contradiction: %d respondent(s) were classified as ",
             "dead before the configured outcome but had a finite original outcome. ",
             "Review mortality/interview timing before permitting zero recoding."),
      n_observed_death)
    if (isTRUE(mc$fail_on_death_with_observed_original_outcome %||% TRUE))
      stop(contradiction_message, call. = FALSE)
    warning(contradiction_message,
            " The contradiction gate is disabled, so the values will be overwritten with zero.",
            call. = FALSE)
  }

  out <- main_sample
  out[[outcome_var]][death == 1L] <- 0
  if ("EarningsSource" %in% names(out)) {
    out$EarningsSource <- as.character(out$EarningsSource)
    out$EarningsSource[death == 1L] <- "mortality_zero"
  }
  list(data = out, audit = audit)
}


build_main_dataset <- function(w1_all, cfg) {
  msg("\n===== STAGE: Build main dataset =====", cfg = cfg)
  id_var <- cfg$analysis$id_var
  assert_required_columns(w1_all,
    c(id_var, cfg$analysis$cluster_var, cfg$analysis$strata_var,
      cfg$analysis$weight_var), "w1_all")
  main_sample <- w1_all
  sample_flow <- list(wave1_merged = nrow(main_sample))
  global_missing_dictionary <- attr(w1_all, "global_missing_dictionary")
  if (is.null(global_missing_dictionary) || !length(global_missing_dictionary))
    stop("Wave-I merge is missing the frozen global missing-code dictionary; rebuild the Wave-I cache.", call. = FALSE)
  # Use the same outcome-blind, all-Wave-I dictionary for the Wave-II
  # completion diagnostic and every later preprocessing step. Do not relearn
  # semantic reserve-code rules from the completer/noncompleter comparison.
  cfg$preprocessing$global_missing_dictionary <- global_missing_dictionary
  collision_audit <- attr(w1_all, "join_suffix_collision_audit")
  strata_source_audit <- attr(w1_all, "design_strata_source_audit")
  canonical_strata_audit <- attr(w1_all, "canonical_design_field_audit")
  variable_source_registry <- get_variable_source_registry(w1_all, cfg, required = TRUE)
  cfg$preprocessing$variable_source_registry <- variable_source_registry
  full_survey_design_frame <- attr(w1_all, "full_survey_design_frame")
  if (is.null(full_survey_design_frame) || !is.data.frame(full_survey_design_frame))
    stop("Wave I merge is missing the valid-weight full survey-design frame; rebuild the Wave I cache.", call. = FALSE)
  msg(sprintf("  Input W1 merged table: %d rows x %d cols.", nrow(main_sample), ncol(main_sample)), cfg = cfg)
  if (isTRUE(cfg$global$save_stage_csvs)) {
    dict_audit <- missing_dictionary_to_data_frame(global_missing_dictionary, w1_all)
    p_dict <- write_run_csv(
      dict_audit, cfg,
      cfg$diagnostics$global_missing_dictionary_csv %||% "global_missing_code_dictionary.csv")
    msg(sprintf("  [missing dictionary] Audit written: %s", basename(p_dict)), cfg = cfg)
    if (!is.null(strata_source_audit) && is.data.frame(strata_source_audit)) {
      p_strata_source <- write_run_csv(
        strata_source_audit, cfg,
        cfg$diagnostics$design_strata_source_audit_csv %||% "design_strata_source_audit.csv")
      msg(sprintf("  [design] Stratum source audit written: %s", basename(p_strata_source)), cfg = cfg)
    }
    if (!is.null(canonical_strata_audit) && is.data.frame(canonical_strata_audit)) {
      p_strata_canonical <- write_run_csv(
        canonical_strata_audit, cfg,
        cfg$diagnostics$canonical_design_field_audit_csv %||% "canonical_design_field_audit.csv")
      msg(sprintf("  [design] Canonical stratum audit written: %s", basename(p_strata_canonical)), cfg = cfg)
    }
  }

  # ---- Build the common CES-D exposure (Depressed) -------------------------
  msg("  [exposure] Reading Wave 2 CES-D items to build 'Depressed'...", cfg = cfg)
  inhome_w2 <- read_xpt_df(cfg$paths$wave2_inhome)
  mh_cols   <- cfg$exposure$cesd_items
  assert_required_columns(inhome_w2, c(id_var, mh_cols), "inhome_w2")
  main_sample <- left_join_unique(
    main_sample, inhome_w2 %>% dplyr::select(dplyr::all_of(c(id_var, mh_cols))), id_var,
    x_label = "wave1 merged sample", y_label = "wave2 Feelings Scale")
  n_before <- nrow(main_sample)
  complete_cesd <- Reduce(`&`, lapply(mh_cols, function(nm) {
    z <- to_numeric_codes(main_sample[[nm]])
    is.finite(z) & !(z %in% cfg$exposure$nonresponse_codes) & z %in% 0:3
  }))
  wave2_completion_status <- data.frame(
    AID = as.character(main_sample[[id_var]]),
    wave2_cesd_complete = as.integer(complete_cesd),
    stringsAsFactors = FALSE)
  names(wave2_completion_status)[1L] <- id_var
  if (isTRUE(cfg$diagnostics$enable_wave2_completion_diagnostic %||% TRUE)) {
    completion_diag <- build_wave2_completion_diagnostic(main_sample, complete_cesd, cfg)
    if (isTRUE(cfg$global$save_stage_csvs)) {
      write_run_csv(completion_diag$balance, cfg,
                    cfg$diagnostics$wave2_completion_balance_csv %||%
                      "wave2_completion_balance.csv")
      write_run_csv(completion_diag$summary, cfg,
                    cfg$diagnostics$wave2_completion_summary_csv %||%
                      "wave2_completion_summary.csv")
    }
  }
  if (isTRUE(cfg$exposure$drop_missing_exposure)) {
    main_sample <- main_sample[complete_cesd, , drop = FALSE]
    msg(sprintf("  [exposure] Dropped %d of %d rows with missing/refused CES-D items; %d remain.",
      n_before - nrow(main_sample), n_before, nrow(main_sample)), cfg = cfg)
  }
  sample_flow$complete_wave2_cesd <- nrow(main_sample)
  if (isTRUE(cfg$analysis$enforce_expected_sample_gates %||% FALSE) &&
      nrow(main_sample) != as.integer(cfg$analysis$expected_complete_cesd_n))
    stop(sprintf("Complete-CES-D sample gate failed: expected %d rows, found %d.",
                 as.integer(cfg$analysis$expected_complete_cesd_n), nrow(main_sample)), call. = FALSE)
  # Preserve the original storage classes of Wave-I covariates. Only the
  # explicitly documented H2FS exposure items are converted here; blanket
  # numeric coercion can silently destroy character categories and identifiers.
  main_sample <- as.data.frame(main_sample, check.names = FALSE)
  if (!setequal(cfg$exposure$reverse_score_items, c("H2FS4", "H2FS8", "H2FS11", "H2FS15")))
    stop("Exposure reverse-score item set does not match the verified Wave-II Feelings Scale specification.", call. = FALSE)
  for (col in mh_cols) {
    z <- to_numeric_codes(main_sample[[col]])
    bad <- !is.finite(z) | !(z %in% 0:3)
    if (any(bad))
      stop(sprintf("Exposure item %s contains %d retained value(s) outside 0:3 after the missing-item restriction.", col, sum(bad)), call. = FALSE)
    main_sample[[col]] <- z
  }
  for (col in cfg$exposure$reverse_score_items)
    main_sample[[col]] <- 3 - main_sample[[col]]
  main_sample$MHSum <- rowSums(main_sample[, mh_cols, drop = FALSE], na.rm = FALSE)
  if (any(!is.finite(main_sample$MHSum)) ||
      any(main_sample$MHSum < 0 | main_sample$MHSum > 57))
    stop("Constructed Wave-II Feelings Scale total escaped its theoretical 0:57 range.", call. = FALSE)
  main_sample[[cfg$analysis$exposure_var]] <-
    as.integer(main_sample$MHSum >= cfg$exposure$cutpoint)
  msg(sprintf("  [exposure] MHSum in [%d, %d]; Depressed (cutpoint=%d): %d of %d = %.2f%%.",
    min(main_sample$MHSum, na.rm = TRUE), max(main_sample$MHSum, na.rm = TRUE),
    cfg$exposure$cutpoint,
    sum(main_sample[[cfg$analysis$exposure_var]] == 1L, na.rm = TRUE),
    nrow(main_sample),
    100 * mean(main_sample[[cfg$analysis$exposure_var]], na.rm = TRUE)), cfg = cfg)

  if (mortality_enabled_for_wave(cfg, cfg$outcome$current_wave)) {
    mc_now <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
    msg(sprintf("  [mortality] Wave %d: deriving %s via %s...",
                cfg$outcome$current_wave, mc_now$death_before_outcome_var,
                mortality_timing_rule_text(mc_now)), cfg = cfg)
    main_sample <- merge_mortality_indicator(main_sample, cfg)
  }

  # ---- Build outcome via the family dispatcher ---------------------------
  msg("  [outcome] Constructing outcome via family dispatcher...", cfg = cfg)
  oc <- construct_outcome(main_sample, cfg)
  main_sample[[cfg$analysis$outcome_var]] <- oc$Y
  if (identical(cfg$outcome$family, "Compensation") &&
      !is.null(oc$source) && length(oc$source) == nrow(main_sample)) {
    main_sample$EarningsSource <- oc$source
    if (isTRUE(cfg$global$save_stage_csvs) && !is.null(oc$audit)) {
      audit_path <- write_run_csv(
        oc$audit, cfg,
        cfg$diagnostics$earnings_construction_audit_csv %||% "earnings_construction_audit.csv")
      msg(sprintf("  [outcome] Earnings construction audit written: %s", basename(audit_path)), cfg = cfg)
    }
    attr(main_sample, "earnings_construction_audit") <- oc$audit
    sample_flow$earnings_exact_before_weight_drop <- sum(main_sample$EarningsSource == "exact")
    sample_flow$earnings_bracket_before_weight_drop <- sum(main_sample$EarningsSource == "bracket_midpoint")
    sample_flow$earnings_missing_before_weight_drop <- sum(!is.finite(main_sample[[cfg$analysis$outcome_var]]))
    sample_flow$valid_zero_exact_before_weight_drop <- sum(
      main_sample$EarningsSource == "exact" & main_sample[[cfg$analysis$outcome_var]] == 0,
      na.rm = TRUE)
  }
  if (!identical(cfg$outcome$family, "Compensation") && !is.null(oc$audit)) {
    if (isTRUE(cfg$global$save_stage_csvs))
      write_run_csv(oc$audit, cfg, "outcome_construction_audit.csv")
    attr(main_sample, "outcome_construction_audit") <- oc$audit
  }
  attr(main_sample, "outcome_support") <- oc$support %||% NULL
  # Optional policy composite for truncation by death. Deaths in the configured
  # window are observed zeros, including respondents whose original outcome was
  # missing; ordinary nondeath outcome missingness continues through delta_Y/pi.
  if (mortality_enabled_for_wave(cfg, cfg$outcome$current_wave) &&
      isTRUE(resolve_mortality_spec(cfg, cfg$outcome$current_wave)$composite_zero_at_death %||% FALSE)) {
    mortality_composite <- apply_mortality_composite(main_sample, cfg)
    main_sample <- mortality_composite$data
    attr(main_sample, "mortality_composite_audit") <- mortality_composite$audit
    mort_now <- resolve_mortality_spec(cfg, cfg$outcome$current_wave)
    sample_flow$mortality_zero_before_weight_drop <-
      sum(main_sample[[mort_now$death_before_outcome_var]] == 1L, na.rm = TRUE)
    if (isTRUE(cfg$global$save_stage_csvs))
      write_run_csv(
        mortality_composite$audit, cfg,
        resolve_mortality_spec(cfg, cfg$outcome$current_wave)$output_csv)
  }
  # Censoring indicator
  main_sample[[cfg$analysis$outcome_observed_var]] <-
    as.integer(make_observed_mask(main_sample[[cfg$analysis$outcome_var]]))
  msg(sprintf("  [outcome] %d rows with observed Y (%.1f%%); %d censored.",
    sum(main_sample[[cfg$analysis$outcome_observed_var]] == 1L),
    100 * mean(main_sample[[cfg$analysis$outcome_observed_var]]),
    sum(main_sample[[cfg$analysis$outcome_observed_var]] == 0L)), cfg = cfg)
  sample_flow$outcome_observed_before_weight_drop <-
    sum(main_sample[[cfg$analysis$outcome_observed_var]] == 1L)

  if (!is.null(collision_audit) && nrow(collision_audit) > 0L) {
    action_counts <- table(collision_audit$action)
    msg(sprintf(
      "  [cleanup] Audited %d Wave I .y collision column(s): %s.",
      nrow(collision_audit),
      paste(sprintf("%s=%d", names(action_counts), as.integer(action_counts)), collapse = "; ")),
      cfg = cfg)
    if (isTRUE(cfg$global$save_stage_csvs)) {
      p_collision <- write_run_csv(
        collision_audit, cfg,
        cfg$diagnostics$join_suffix_collision_audit_csv %||% "join_suffix_collision_audit.csv")
      msg(sprintf("  [cleanup] Join-suffix collision audit written: %s", basename(p_collision)), cfg = cfg)
    }
  }

  # Drop rows with invalid sampling weights during dataset construction so
  # every downstream stage uses the same analytic sample.
  if (isTRUE(cfg$preprocessing$drop_invalid_weights_at_build)) {
    w <- suppressWarnings(as.numeric(main_sample[[cfg$analysis$weight_var]]))
    keep_w <- is.finite(w) & w > 0
    n_drop <- sum(!keep_w); n_total <- length(w)
    sample_flow$invalid_weight_dropped <- n_drop
    if (n_drop > 0L) {
      msg(sprintf("  [weights] Dropping %d of %d rows (%.2f%%) with non-positive or missing %s.",
        n_drop, n_total, 100 * n_drop / n_total, cfg$analysis$weight_var), cfg = cfg)
      main_sample <- main_sample[keep_w, , drop = FALSE]
      msg(sprintf("  [weights] Analytic sample now has %d rows; weights range [%.4g, %.4g].",
        nrow(main_sample),
        min(main_sample[[cfg$analysis$weight_var]]),
        max(main_sample[[cfg$analysis$weight_var]])), cfg = cfg)
    } else {
      msg(sprintf("  [weights] All %d rows have valid %s (no drops).",
        n_total, cfg$analysis$weight_var), cfg = cfg)
    }
  }

  design_audit <- validate_final_sample_design_fields(main_sample, cfg)
  attr(main_sample, "design_field_audit") <- design_audit
  sample_flow$strata_final <- design_audit$n_strata

  sample_gate_audit <- validate_expected_final_sample_gates(main_sample, cfg)
  attr(main_sample, "expected_sample_gate_audit") <- sample_gate_audit

  weight_winsor_cap_used <- NULL
  weight_renorm_factor_used <- 1
  # winsorize sampling weights at the configured upper quantile, on the
  # FINALIZED analytic sample (clean positive weights only). This dampens the
  # influence of very-high-weight respondents. Runs after the invalid-weight
  # drop so the quantile is not distorted by zero/negative/missing weights.
  wq <- cfg$analysis$weight_winsor_quantile
  if (!is.null(wq) && is.finite(wq) && wq > 0 && wq < 1) {
    w_cur <- suppressWarnings(as.numeric(main_sample[[cfg$analysis$weight_var]]))
    cap <- unname(stats::quantile(w_cur, probs = wq, na.rm = TRUE, type = 8))
    n_wins <- sum(w_cur > cap, na.rm = TRUE)
    w_new <- pmin(w_cur, cap)
    weight_winsor_cap_used <- cap
    # Optional renormalization to preserve the original mean weight, so the
    # winsorized weights represent the same effective population total.
    if (isTRUE(cfg$analysis$weight_winsor_renormalize %||% FALSE)) {
      mean_old <- mean(w_cur, na.rm = TRUE)
      mean_new <- mean(w_new, na.rm = TRUE)
      if (is.finite(mean_new) && mean_new > 0) {
        weight_renorm_factor_used <- mean_old / mean_new
        w_new <- w_new * weight_renorm_factor_used
      }
    }
    main_sample[[cfg$analysis$weight_var]] <- w_new
    msg(sprintf("  [weights] Winsorized %d of %d weights at q%.2f (cap=%.4g); range now [%.4g, %.4g]%s.",
      n_wins, length(w_cur), wq, cap,
      min(w_new, na.rm = TRUE), max(w_new, na.rm = TRUE),
      if (isTRUE(cfg$analysis$weight_winsor_renormalize %||% FALSE)) ", renormalized to original mean" else ""),
      cfg = cfg)
  }

  # Preserve the complete valid-weight Wave-I survey design and mark the
  # complete-CES-D analytic sample as a domain. This lets survey::subset retain
  # PSUs outside the analytic domain when linearizing the ATT EIF, as recommended
  # for Add Health subpopulation analyses. No respondent outside the analytic
  # domain enters nuisance fitting or the point estimate.
  design_w <- suppressWarnings(as.numeric(full_survey_design_frame[[cfg$analysis$weight_var]]))
  if (!is.null(weight_winsor_cap_used))
    design_w <- pmin(design_w, weight_winsor_cap_used) * weight_renorm_factor_used
  full_survey_design_frame[[cfg$analysis$weight_var]] <- design_w
  design_ids <- as.character(full_survey_design_frame[[cfg$analysis$id_var]])
  analytic_ids <- as.character(main_sample[[cfg$analysis$id_var]])
  if (anyDuplicated(analytic_ids) || any(!analytic_ids %in% design_ids))
    stop("Final analytic respondents do not map uniquely into the full survey-design frame.", call. = FALSE)
  full_survey_design_frame$.analysis_domain <- design_ids %in% analytic_ids
  if (sum(full_survey_design_frame$.analysis_domain) != nrow(main_sample))
    stop("Survey-design domain membership does not equal the final analytic sample size.", call. = FALSE)
  match_domain <- match(analytic_ids, design_ids)
  if (!isTRUE(all.equal(
      as.numeric(full_survey_design_frame[[cfg$analysis$weight_var]][match_domain]),
      as.numeric(main_sample[[cfg$analysis$weight_var]]), tolerance = 1e-12)))
    stop("Survey-design domain weights do not match the analytic-sample weights.", call. = FALSE)

  sample_flow$valid_weight_final <- nrow(main_sample)
  sample_flow$treated_final <- sum(main_sample[[cfg$analysis$exposure_var]] == 1L)
  sample_flow$outcome_observed_final <- sum(main_sample[[cfg$analysis$outcome_observed_var]] == 1L)
  sample_flow$psu_final <- length(unique(main_sample[[cfg$analysis$cluster_var]]))
  sample_flow$strata_final <- length(unique(main_sample[[cfg$analysis$strata_var]]))
  sample_flow$control_final <- sum(main_sample[[cfg$analysis$exposure_var]] == 0L)
  sample_flow$outcome_missing_final <- sum(main_sample[[cfg$analysis$outcome_observed_var]] == 0L)
  sample_flow$earnings_exact_final <- if ("EarningsSource" %in% names(main_sample))
    sum(main_sample$EarningsSource == "exact") else NA_integer_
  sample_flow$earnings_bracket_final <- if ("EarningsSource" %in% names(main_sample))
    sum(main_sample$EarningsSource == "bracket_midpoint") else NA_integer_
  mort_now <- if (mortality_enabled_for_wave(cfg, cfg$outcome$current_wave)) resolve_mortality_spec(cfg, cfg$outcome$current_wave) else NULL
  sample_flow$mortality_zero_final <- if (!is.null(mort_now) && mort_now$death_before_outcome_var %in% names(main_sample))
    sum(main_sample[[mort_now$death_before_outcome_var]] == 1L) else 0L
  sample_flow$valid_zero_exact_final <- if ("EarningsSource" %in% names(main_sample))
    sum(main_sample$EarningsSource == "exact" &
        main_sample[[cfg$analysis$outcome_var]] == 0, na.rm = TRUE) else NA_integer_
  sample_flow$full_design_psu <- length(unique(full_survey_design_frame[[cfg$analysis$cluster_var]]))

  # parse and cyclically encode the H1GH50 bedtime (12-hour clock
  # string) on the finalized analytic sample, before it is cached and before
  # screening.
  main_sample <- transform_time_variables_in_df(main_sample, cfg)
  # Derived bedtime sine/cosine columns use native missingness only; all raw
  # Wave I semantic rules remain the globally frozen dictionary.
  for (nm in intersect(c("H1GH50__tsin", "H1GH50__tcos"), names(main_sample))) {
    if (!nm %in% names(global_missing_dictionary))
      global_missing_dictionary[[nm]] <- make_native_only_missing_rule(main_sample[[nm]], nm)
    variable_source_registry <- append_derived_variable_registry(
      variable_source_registry, nm, "H1GH50", "wave1_inhome")
  }
  main_sample <- canonicalize_mandatory_W_columns(main_sample, cfg)
  attr(main_sample, "global_missing_dictionary") <- global_missing_dictionary
  attr(main_sample, "variable_source_registry") <- variable_source_registry
  cfg$preprocessing$variable_source_registry <- variable_source_registry
  candidate_alias_audit <- build_candidate_alias_audit(main_sample, cfg)
  validate_candidate_governance(main_sample, cfg, candidate_alias_audit)
  attr(main_sample, "candidate_alias_audit") <- candidate_alias_audit
  if (isTRUE(cfg$global$save_stage_csvs)) {
    p_registry <- write_run_csv(
      variable_source_registry, cfg,
      cfg$causal_governance$variable_source_registry_csv %||%
        "variable_source_registry.csv")
    p_alias <- write_run_csv(
      candidate_alias_audit, cfg,
      cfg$causal_governance$candidate_alias_audit_csv %||%
        "candidate_alias_audit.csv")
    mandatory_audit <- attr(main_sample, "mandatory_W_canonicalization_audit")
    if (is.data.frame(mandatory_audit) && nrow(mandatory_audit))
      write_run_csv(
        mandatory_audit, cfg,
        cfg$causal_governance$mandatory_W_canonicalization_audit_csv %||%
          "mandatory_W_canonicalization_audit.csv")
    msg(sprintf("  [governance] Source registry written: %s", basename(p_registry)), cfg = cfg)
    msg(sprintf("  [governance] Candidate-alias audit written: %s", basename(p_alias)), cfg = cfg)
  }
  attr(main_sample, "sample_flow") <- sample_flow
  attr(main_sample, "survey_design_frame") <- full_survey_design_frame
  attr(main_sample, "wave2_completion_status") <- wave2_completion_status

  msg(sprintf("===== Main dataset ready: %d rows x %d cols. =====",
    nrow(main_sample), ncol(main_sample)), cfg = cfg)
  main_sample
}

# Which columns are eligible to enter screening/estimation as W. Excludes
# the exposure, outcome, IDs, cluster, weights, outcome-observed indicator,
# and any helper columns the exposure/outcome constructors created.
get_candidate_vars <- function(df, cfg) {
  audit <- build_candidate_alias_audit(df, cfg)
  governed <- validate_candidate_governance(df, cfg, audit)
  governed$candidates
}

# =============================================================================
