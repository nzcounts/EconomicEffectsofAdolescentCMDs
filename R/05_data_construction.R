# Generated from the reviewed v8.28 production source.
# Original lines: 3338-5231.
# Module role: Restricted-data construction and outcomes.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 3) BASE DATA CONSTRUCTION
# =============================================================================
# Plain-English role: read the Add Health .xpt files, join them on AID/PSUSCID,
# then build the exposure (depression) and outcome (raw annual earnings) from raw
# items. Everything downstream starts from the single main_df produced here.

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
  unique(c(
    mort$source_var %||% character(0),
    mort$interview_year_var %||% character(0),
    mort$derived_death_year_var %||% character(0),
    mort$death_in_window_var %||% character(0),
    mort$death_before_outcome_var %||% character(0)))
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
# OUTCOME FAMILY DISPATCHER (v6)
# =============================================================================
# Each family has a constructor that takes (main_df, wave, family_cfg, outcome_cfg)
# and returns a numeric or integer vector Y of length nrow(main_df), with NA
# for rows where the outcome is not observed at the requested wave.
# construct_outcome is the dispatcher called by build_main_dataset. It
# reads cfg$outcome$family and cfg$outcome$family_member to pick which
# constructor to call, writes the result to main_df[[cfg$analysis$outcome_var]],
# and sets the censoring indicator.

# ---- Placeholder helper: read the Wave-N in-home file when available --------
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
# PLACEHOLDER: fill in the recoding rules for your Add Health item values.
construct_outcome_educational_attainment <- function(main_df, wave, family_cfg,
                                                    outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Educational Attainment, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("EducationalAttainment source variable for wave %d is not defined in cfg.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  # Attach the source variable if not already present
  if (!src %in% names(main_df)) {
    if (is.null(pipeline_cfg)) stop("Outcome constructor requires the full pipeline configuration to read its source file.", call. = FALSE)
    inhome <- read_wave_inhome(wave, pipeline_cfg)
    id_var <- pipeline_cfg$analysis$id_var
    assert_required_columns(inhome, c(id_var, src), sprintf("wave%d inhome", wave))
    main_df <- left_join_unique(
      main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, src))), id_var,
      x_label = "main_df", y_label = sprintf("wave%d outcome source", wave))
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # PLACEHOLDER threshold coding: update these numeric cutoffs to match your
  # Add Health coding scheme for this source variable. The values below are
  # illustrative and MUST be verified against the codebook.
  Y <- switch(member,
    at_least_hs           = as.integer(raw >= 2),
    at_least_some_college = as.integer(raw >= 4),
    at_least_college_grad = as.integer(raw >= 6),
    some_grad_school      = as.integer(raw >= 7),
    stop(sprintf("Unknown EducationalAttainment member: %s", member), call. = FALSE))
  Y[!is.finite(raw)] <- NA_integer_
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Labor Force Participation (binary) -----------------------------------
# PLACEHOLDER: fill in the recoding rule.
construct_outcome_labor_force_participation <- function(main_df, wave, family_cfg,
                                                       outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Labor Force Participation, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("LaborForceParticipation source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    if (is.null(pipeline_cfg)) stop("Outcome constructor requires the full pipeline configuration to read its source file.", call. = FALSE)
    inhome <- read_wave_inhome(wave, pipeline_cfg)
    id_var <- pipeline_cfg$analysis$id_var
    assert_required_columns(inhome, c(id_var, src), sprintf("wave%d inhome", wave))
    main_df <- left_join_unique(
      main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, src))), id_var,
      x_label = "main_df", y_label = sprintf("wave%d outcome source", wave))
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # the previous body silently coded EVERY finite value other
  # than 1 (including refusal/DK/legitimate-skip codes) as 0, i.e. it miscoded
  # missing as non-participation. Require an EXPLICIT codebook mapping and
  # refuse to construct LFP otherwise, so it can never run on a guessed rule:
  # family_cfg$codes$participate = codes meaning "in labor force / working" -> 1
  # family_cfg$codes$nonparticipate = codes meaning "not in labor force" -> 0
  # family_cfg$codes$missing = codes meaning refused / DK / skip / missing -> NA
  codes <- family_cfg$codes
  if (is.null(codes) || is.null(codes$participate) || is.null(codes$nonparticipate)) {
    stop(sprintf(paste0(
      "LaborForceParticipation has no codebook mapping. Set family_cfg$codes$participate, ",
      "$nonparticipate (and $missing) for source '%s' (wave %d) from the Add Health codebook ",
      "before constructing LFP. Refusing to guess (would miscode missing as 0)."), src, wave),
      call. = FALSE)
  }
  Y <- rep(NA_integer_, length(raw))
  Y[is.finite(raw) & raw %in% codes$participate]    <- 1L
  Y[is.finite(raw) & raw %in% codes$nonparticipate] <- 0L
  # Any finite value not in participate/nonparticipate/missing is an UNMAPPED
  # code -- fail loudly rather than silently dropping or zero-coding it.
  mapped   <- c(codes$participate, codes$nonparticipate, codes$missing)
  unmapped <- unique(raw[is.finite(raw) & !(raw %in% mapped)])
  if (length(unmapped) > 0L) {
    stop(sprintf("LaborForceParticipation source '%s' (wave %d) has unmapped finite codes: %s. Add them to family_cfg$codes.",
      src, wave, paste(sort(unmapped), collapse = ", ")), call. = FALSE)
  }
  message(sprintf("    [outcome] Constructed %d observed (%d in-LF, %d not-in-LF), %d missing. Participation: %.1f%%.",
    sum(!is.na(Y)), sum(Y == 1L, na.rm = TRUE), sum(Y == 0L, na.rm = TRUE),
    sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Usual Hours (continuous) ---------------------------------------------
# PLACEHOLDER: fill in the recoding rule.
construct_outcome_usual_hours <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Usual Hours, wave %d.", wave))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("UsualHours source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    if (is.null(pipeline_cfg)) stop("Outcome constructor requires the full pipeline configuration to read its source file.", call. = FALSE)
    inhome <- read_wave_inhome(wave, pipeline_cfg)
    id_var <- pipeline_cfg$analysis$id_var
    assert_required_columns(inhome, c(id_var, src), sprintf("wave%d inhome", wave))
    main_df <- left_join_unique(
      main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, src))), id_var,
      x_label = "main_df", y_label = sprintf("wave%d outcome source", wave))
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # PLACEHOLDER: treat explicit refusal codes (>= 996) as missing.
  Y <- ifelse(is.finite(raw) & raw < 996, raw, NA_real_)
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Hours range: [%s, %s].",
    sum(!is.na(Y)), sum(is.na(Y)),
    ifelse(any(!is.na(Y)), sprintf("%.1f", min(Y, na.rm = TRUE)), "NA"),
    ifelse(any(!is.na(Y)), sprintf("%.1f", max(Y, na.rm = TRUE)), "NA")))
  Y
}

# ---- Compensation (continuous; log-transformed when configured) -----------
construct_outcome_compensation <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
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

# ---- Health Status (nested binary) ----------------------------------------
# PLACEHOLDER: Add Health self-rated health uses 1=Excellent ... 5=Poor.
# We invert below so that "at least good" means a healthier rating.
construct_outcome_health_status <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Health Status, wave %d, member = '%s'.", wave, member))
  src <- family_cfg$sources[[as.character(wave)]]
  if (is.null(src)) {
    warning(sprintf("HealthStatus source for wave %d is not defined.", wave))
    return(rep(NA_real_, nrow(main_df)))
  }
  if (!src %in% names(main_df)) {
    if (is.null(pipeline_cfg)) stop("Outcome constructor requires the full pipeline configuration to read its source file.", call. = FALSE)
    inhome <- read_wave_inhome(wave, pipeline_cfg)
    id_var <- pipeline_cfg$analysis$id_var
    assert_required_columns(inhome, c(id_var, src), sprintf("wave%d inhome", wave))
    main_df <- left_join_unique(
      main_df, inhome %>% dplyr::select(dplyr::all_of(c(id_var, src))), id_var,
      x_label = "main_df", y_label = sprintf("wave%d outcome source", wave))
  }
  raw <- suppressWarnings(as.numeric(as.character(main_df[[src]])))
  # Treat refusal/DK codes (>= 6) as missing. 1=Excellent, 5=Poor.
  raw[!is.finite(raw) | raw >= 6] <- NA_real_
  Y <- switch(member,
    at_least_fair      = as.integer(raw <= 4),   # fair, good, very good, excellent
    at_least_good      = as.integer(raw <= 3),   # good, very good, excellent
    at_least_very_good = as.integer(raw <= 2),   # very good, excellent
    excellent          = as.integer(raw <= 1),
    stop(sprintf("Unknown HealthStatus member: %s", member), call. = FALSE))
  Y[is.na(raw)] <- NA_integer_
  message(sprintf("    [outcome] Constructed %d observed, %d missing. Prevalence: %.1f%%.",
    sum(!is.na(Y)), sum(is.na(Y)), 100 * mean(Y, na.rm = TRUE)))
  Y
}

# ---- Mental Health (PLACEHOLDER) ------------------------------------------
construct_outcome_mental_health <- function(main_df, wave, family_cfg, outcome_cfg, member, pipeline_cfg = NULL) {
  message(sprintf("    [outcome] Mental Health, wave %d. [PLACEHOLDER]", wave))
  rep(NA_real_, nrow(main_df))
}

# ---- Substance Use (PLACEHOLDER) ------------------------------------------
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
# clock face) and REPLACED by H1GH50__tsin / H1GH50__tcos = sin/cos of the
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
  mort <- cfg$mortality_sensitivity
  id_var <- cfg$analysis$id_var
  source_var <- mort$source_var
  death_year_var <- mort$derived_death_year_var
  raw_window_var <- mort$death_in_window_var
  assert_required_columns(mortality, c(id_var, source_var), "mortality linkage")
  mortality <- mortality[, c(id_var, source_var), drop = FALSE]
  mortality <- coerce_join_key(mortality, id_var, "mortality linkage",
                               require_complete = TRUE)
  assert_unique_key(mortality, id_var, "mortality linkage",
                    require_complete = TRUE)

  raw <- mortality[[source_var]]
  native_missing <- character_native_missing_mask(raw)
  year <- to_numeric_codes(raw)
  integer_year <- is.finite(year) & abs(year - round(year)) <= 1e-8
  year_int <- rep(NA_integer_, length(year))
  year_int[integer_year] <- as.integer(round(year[integer_year]))
  valid_year <- integer_year & year_int >= as.integer(mort$valid_year_min) &
    year_int <= as.integer(mort$valid_year_max)
  no_death_code <- is.finite(year) &
    year %in% as.numeric(mort$no_death_codes %||% numeric(0))
  usable_death_year <- valid_year & !no_death_code
  recognized_no_death <- no_death_code |
    (native_missing & isTRUE(mort$native_missing_means_no_death))
  recognized <- usable_death_year | recognized_no_death
  unrecognized <- !recognized
  if (any(unrecognized)) {
    examples <- unique(trimws(as.character(raw[unrecognized])))
    examples[is.na(examples) | examples == ""] <- "<native missing>"
    examples <- head(examples, 8L)
    message_text <- sprintf(
      "Mortality source %s contains %d unrecognized value(s); examples: %s.",
      source_var, sum(unrecognized), paste(examples, collapse = ", "))
    if (isTRUE(mort$fail_on_unrecognized_codes)) stop(message_text, call. = FALSE)
    warning(message_text, " Treating them as no recorded death because the gate is disabled.",
            call. = FALSE)
  }

  death_year <- rep(NA_integer_, length(year_int))
  death_year[usable_death_year] <- year_int[usable_death_year]
  death_in_window <- as.integer(usable_death_year &
    year_int >= as.integer(mort$death_year_start) &
    year_int <= as.integer(mort$death_year_end))
  if (anyNA(death_in_window) || any(!death_in_window %in% c(0L, 1L)))
    stop("Internal mortality derivation error: incomplete/nonbinary raw-window indicator.",
         call. = FALSE)

  out <- mortality[, id_var, drop = FALSE]
  out[[death_year_var]] <- death_year
  out[[raw_window_var]] <- death_in_window
  audit <- data.frame(
    source_variable = source_var,
    derived_death_year_variable = death_year_var,
    raw_window_variable = raw_window_var,
    death_year_start = as.integer(mort$death_year_start),
    death_year_end = as.integer(mort$death_year_end),
    n_mortality_records = nrow(mortality),
    n_valid_calendar_years = sum(valid_year),
    n_calendar_years_eligible_as_death = sum(usable_death_year),
    n_no_death_codes_overlapping_year_range = sum(no_death_code & valid_year),
    n_native_missing = sum(native_missing),
    n_explicit_no_death_codes = sum(no_death_code),
    n_unrecognized_codes = sum(unrecognized),
    n_deaths_in_raw_window = sum(death_in_window),
    native_missing_means_no_death = isTRUE(mort$native_missing_means_no_death),
    stringsAsFactors = FALSE)
  list(data = out, audit = audit)
}


merge_mortality_indicator_from_data <- function(main_sample, mortality, cfg,
                                                wave4_timing = NULL) {
  mort <- derive_mortality_indicator_from_data(mortality, cfg)
  mc <- cfg$mortality_sensitivity
  id_var <- cfg$analysis$id_var
  death_var <- mc$death_before_outcome_var
  death_year_var <- mc$derived_death_year_var
  raw_window_var <- mc$death_in_window_var
  interview_var <- mc$interview_year_var
  role_vars <- c(death_var, death_year_var, raw_window_var, interview_var)
  conflicts <- canonical_role_key(role_vars) %in% canonical_role_key(names(main_sample))
  if (any(conflicts))
    stop("Mortality-derived or Wave-IV audit variable already exists before linkage: ",
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
  }

  # Primary mortality classification is intentionally independent of IYEAR4.
  # The settled composite is exactly the recognized NDIDD19Y death-year window.
  raw_window <- as.integer(joined[[raw_window_var]])
  if (anyNA(raw_window) || any(!raw_window %in% c(0L, 1L)))
    stop("Internal mortality linkage error: raw-window death indicator is incomplete/nonbinary.",
         call. = FALSE)
  joined[[death_var]] <- raw_window

  # IYEAR4 is audit-only. Absence of a Wave-IV record or a missing IYEAR4 value
  # is expected for nonrespondents/decedents and NEVER changes the death indicator.
  interview_year <- rep(NA_integer_, nrow(joined))
  if (!is.null(wave4_timing)) {
    assert_required_columns(wave4_timing, c(id_var, interview_var),
                            "Wave-IV interview-year audit")
    timing <- wave4_timing[, c(id_var, interview_var), drop = FALSE]
    timing <- coerce_join_key(timing, id_var, "Wave-IV interview-year audit",
                              require_complete = TRUE)
    assert_unique_key(timing, id_var, "Wave-IV interview-year audit",
                      require_complete = TRUE)
    joined <- left_join_unique(
      joined, timing, id_var,
      x_label = "mortality-linked analytic sample",
      y_label = "Wave-IV interview-year audit",
      require_complete_x = TRUE, require_complete_y = TRUE)

    interview_raw <- joined[[interview_var]]
    interview_num <- to_numeric_codes(interview_raw)
    valid_interview <- is.finite(interview_num) &
      abs(interview_num - round(interview_num)) <= 1e-8 &
      interview_num >= as.integer(mc$interview_year_valid_min) &
      interview_num <= as.integer(mc$interview_year_valid_max)
    invalid_interview <- !character_native_missing_mask(interview_raw) & !valid_interview
    if (any(invalid_interview))
      stop("Wave-IV interview year contains nonmissing values outside the configured valid range: ",
           paste(head(sort(unique(as.character(interview_raw[invalid_interview]))), 10L),
                 collapse = ", "), call. = FALSE)
    interview_year[valid_interview] <- as.integer(round(interview_num[valid_interview]))
    joined[[interview_var]] <- interview_year
  } else {
    joined[[interview_var]] <- interview_year
  }

  death <- joined[[death_var]] == 1L
  mort$audit$n_linkage_left_rows <- nrow(joined)
  mort$audit$n_valid_weight_rows_requiring_linkage <- sum(linkage_required)
  mort$audit$n_all_rows_matched <- sum(matched)
  mort$audit$n_all_rows_unmatched <- n_unmatched
  mort$audit$n_required_rows_unmatched <- n_unmatched_required
  mort$audit$n_nonrequired_rows_unmatched <- sum(unmatched & !linkage_required)
  mort$audit$n_linked_deaths_in_raw_window <- sum(death)
  mort$audit$n_deaths_classified_before_outcome <- sum(death)
  mort$audit$mortality_classification_rule <- sprintf(
    "%s in inclusive %d-%d window; independent of %s",
    mc$source_var, as.integer(mc$death_year_start), as.integer(mc$death_year_end),
    interview_var)
  mort$audit$n_interview_year_observed <- sum(is.finite(interview_year))
  mort$audit$n_interview_year_missing <- sum(!is.finite(interview_year))
  mort$audit$n_deaths_with_interview_year <- sum(death & is.finite(interview_year))
  mort$audit$n_deaths_without_interview_year <- sum(death & !is.finite(interview_year))
  mort$audit$n_nondeaths_with_interview_year <- sum(!death & is.finite(interview_year))
  mort$audit$n_nondeaths_without_interview_year <- sum(!death & !is.finite(interview_year))

  make_interview_rows <- function(year, death_indicator) {
    observed <- is.finite(year)
    years <- sort(unique(year[observed]))
    rows <- list()
    for (v in years) {
      rows[[length(rows) + 1L]] <- data.frame(
        interview_year = as.integer(v),
        mortality_window_status = "all",
        n = sum(observed & year == v), stringsAsFactors = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(
        interview_year = as.integer(v),
        mortality_window_status = "death_1997_2007",
        n = sum(observed & year == v & death_indicator == 1L), stringsAsFactors = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(
        interview_year = as.integer(v),
        mortality_window_status = "no_death_1997_2007",
        n = sum(observed & year == v & death_indicator == 0L), stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      interview_year = NA_integer_, mortality_window_status = "all",
      n = sum(!observed), stringsAsFactors = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(
      interview_year = NA_integer_, mortality_window_status = "death_1997_2007",
      n = sum(!observed & death_indicator == 1L), stringsAsFactors = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(
      interview_year = NA_integer_, mortality_window_status = "no_death_1997_2007",
      n = sum(!observed & death_indicator == 0L), stringsAsFactors = FALSE)
    out <- do.call(rbind, rows)
    out$interview_year_variable <- interview_var
    out$role <- "audit_only_never_used_for_mortality_classification"
    out
  }
  interview_year_audit <- make_interview_rows(interview_year, joined[[death_var]])

  list(data = joined, audit = mort$audit,
       interview_year_audit = interview_year_audit)
}


merge_mortality_indicator <- function(main_sample, cfg) {
  mortality <- read_xpt_df(cfg$paths$mortality)
  wave4 <- read_wave_inhome(4L, cfg)
  timing_vars <- c(cfg$analysis$id_var,
                   cfg$mortality_sensitivity$interview_year_var)
  assert_required_columns(wave4, timing_vars, "Wave-IV interview-year audit")
  linked <- merge_mortality_indicator_from_data(
    main_sample, mortality, cfg,
    wave4_timing = wave4[, timing_vars, drop = FALSE])
  if (isTRUE(cfg$global$save_stage_csvs)) {
    write_run_csv(linked$audit, cfg,
                  cfg$mortality_sensitivity$linkage_audit_csv %||%
                    "mortality_linkage_1997_2007_audit.csv")
    write_run_csv(linked$interview_year_audit, cfg,
                  cfg$mortality_sensitivity$interview_year_audit_csv %||%
                    "wave4_interview_year_audit.csv")
  }
  linked$data
}


apply_mortality_composite <- function(main_sample, cfg) {
  death_var <- cfg$mortality_sensitivity$death_before_outcome_var
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
  death_year_var <- cfg$mortality_sensitivity$derived_death_year_var
  interview_year_var <- cfg$mortality_sensitivity$interview_year_var
  audit <- data.frame(
    n_total = nrow(main_sample),
    n_deaths_before_outcome = sum(death == 1L),
    n_deaths_with_observed_original_outcome = n_observed_death,
    n_deaths_with_missing_original_outcome =
      sum(death == 1L & !is.finite(original_y)),
    n_deaths_recoded_to_observed_zero = sum(death == 1L),
    contradiction_gate_enabled = isTRUE(
      cfg$mortality_sensitivity$fail_on_death_with_observed_original_outcome %||% TRUE),
    composite_definition = sprintf(
      paste0("Configured labor-market outcome set to zero for recognized %s death year ",
             "during inclusive %d-%d; classification is independent of %s"),
      cfg$mortality_sensitivity$source_var %||% "NDIDD19Y",
      as.integer(cfg$mortality_sensitivity$death_year_start),
      as.integer(cfg$mortality_sensitivity$death_year_end),
      interview_year_var %||% "IYEAR4"),
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
      interview_year = if (interview_year_var %in% names(main_sample))
        main_sample[[interview_year_var]][observed_death] else NA_integer_,
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$global$save_stage_csvs %||% TRUE))
      write_run_csv(
        contradiction, cfg,
        cfg$mortality_sensitivity$contradiction_audit_csv %||%
          "mortality_death_with_observed_outcome_records.csv")
    contradiction_message <- sprintf(
      paste0("Mortality linkage contradiction: %d respondent(s) were coded as ",
             "dead during %d-%d but had a finite original outcome. Review the ",
             "mortality linkage and death-year coding before permitting zero recoding."),
      n_observed_death,
      as.integer(cfg$mortality_sensitivity$death_year_start),
      as.integer(cfg$mortality_sensitivity$death_year_end))
    if (isTRUE(
        cfg$mortality_sensitivity$fail_on_death_with_observed_original_outcome %||% TRUE))
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

  # ---- Build exposure (CES-D -> Depressed): unchanged across outcomes/waves ---
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

  if (isTRUE(cfg$mortality_sensitivity$enabled %||% FALSE)) {
    msg(sprintf("  [mortality] Deriving %s from %s for years %d-%d...",
                cfg$mortality_sensitivity$death_before_outcome_var,
                cfg$mortality_sensitivity$source_var,
                as.integer(cfg$mortality_sensitivity$death_year_start),
                as.integer(cfg$mortality_sensitivity$death_year_end)), cfg = cfg)
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
  attr(main_sample, "outcome_support") <- oc$support %||% NULL
  # Optional policy composite for truncation by death. Deaths in the configured
  # window are observed zeros, including respondents whose original outcome was
  # missing; ordinary nondeath outcome missingness continues through delta_Y/pi.
  if (isTRUE(cfg$mortality_sensitivity$enabled %||% FALSE) &&
      isTRUE(cfg$mortality_sensitivity$composite_zero_at_death %||% FALSE)) {
    mortality_composite <- apply_mortality_composite(main_sample, cfg)
    main_sample <- mortality_composite$data
    attr(main_sample, "mortality_composite_audit") <- mortality_composite$audit
    sample_flow$mortality_zero_before_weight_drop <-
      sum(main_sample$EarningsSource == "mortality_zero", na.rm = TRUE)
    if (isTRUE(cfg$global$save_stage_csvs))
      write_run_csv(
        mortality_composite$audit, cfg,
        cfg$mortality_sensitivity$output_csv %||%
          "mortality_composite_zero_at_death_audit.csv")
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

  # v6 Fix C: drop rows with invalid sampling weights at dataset-build time
  # so every downstream stage sees the same analytic sample.
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
  sample_flow$mortality_zero_final <- if ("EarningsSource" %in% names(main_sample))
    sum(main_sample$EarningsSource == "mortality_zero") else NA_integer_
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
