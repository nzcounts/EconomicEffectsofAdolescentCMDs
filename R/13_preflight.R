# 11) PREFLIGHT UNIT TEST (synthetic data)
# =============================================================================
# Plain-English role: a fast end-to-end run on a tiny synthetic dataset to
# detect obvious breakage (missing functions, signature bugs, TMLE failing
# to produce a finite number) before you launch the multi-hour real run.
# Runtime depends on the enabled production learner library and local hardware.

run_fold_engine_stress_test <- function(cfg_template, repetitions = 12L) {
  ctl <- fold_control_from_cfg(cfg_template)
  for (r in seq_len(as.integer(repetitions))) {
    set.seed(81000L + r)
    J <- 132L
    raw_sizes <- pmax(20L, as.integer(round(stats::rlnorm(J, log(90), 0.65))))
    sizes <- pmax(1L, as.integer(round(raw_sizes * 13500 / sum(raw_sizes))))
    sizes[1L] <- sizes[1L] + (13500L - sum(sizes))
    if (sizes[1L] < 1L) stop("Fold stress-test cluster-size construction failed.", call. = FALSE)
    cluster <- rep(seq_len(J), sizes)
    re_a <- stats::rnorm(J, 0, 0.5)
    p_a <- stats::plogis(-2.30 + re_a[cluster])
    A <- stats::rbinom(length(cluster), 1L, p_a)
    re_d <- stats::rnorm(J, 0, 0.25)
    p_d <- stats::plogis(1.50 - 0.30 * A + re_d[cluster])
    delta <- stats::rbinom(length(cluster), 1L, p_d)
    weights <- exp(stats::rnorm(length(cluster), 0, 0.70))
    folds <- do.call(make_cluster_folds_balanced, c(list(
      cluster = cluster, A = A, delta = delta, weights = weights,
      k = 5L, seed = 20260402L + r,
      balance_on_weights = FALSE), ctl))
    fd <- attr(folds, "fold_diagnostics")
    if (is.null(fd) || fd$size_ratio > ctl$max_size_ratio + 1e-12 ||
        fd$size_deviation_prop > ctl$max_size_deviation_prop + 1e-12)
      stop(sprintf("Fold stress test %d failed fold-size limits: [%s].",
                   r, paste(tabulate(folds, nbins = max(folds)), collapse = ",")), call. = FALSE)
    if (any(vapply(split(folds, cluster), function(z) length(unique(z)) != 1L, logical(1))))
      stop(sprintf("Fold stress test %d split a PSU.", r), call. = FALSE)
    for (f in seq_len(max(folds))) {
      for (ix in list(which(folds == f), which(folds != f))) {
        tab <- table(factor(A[ix], levels = 0:1), factor(delta[ix], levels = 0:1))
        active <- table(factor(A, levels = 0:1), factor(delta, levels = 0:1)) > 0
        if (any(tab[active] < ctl$min_active_cell_n))
          stop(sprintf("Fold stress test %d lacks active A-by-delta support.", r), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

preflight_step <- function(label, expr) {
  message(sprintf("[preflight] %s", label))
  tryCatch(force(expr), error = function(e) {
    stop(sprintf("Preflight section '%s' FAILED: %s", label, conditionMessage(e)),
         call. = FALSE)
  })
}

run_configuration_path_tests <- function(cfg_template) {
  equal <- function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-10, check.attributes = FALSE))
  fails <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
  checked <- character(0)
  test <- function(label, expr) {
    if (!isTRUE(force(expr))) stop("Configuration test failed: ", label, call. = FALSE)
    checked <<- c(checked, label)
  }
  cc <- cfg_template
  cc$analysis$protected_W_mode <- "items"
  cc$analysis$dsp4_mode <- "audit_only"
  cc$analysis$adaptive_g_truncation_apply <- FALSE
  cc$global$save_stage_csvs <- TRUE
  cc$final_tmle$protected_W_items <- paste0("H1FS", 1:19)
  cc$final_tmle$protected_W_summary <- "MHSum_W1"
  cc$causal_governance$additional_mandatory_W <- character(0)
  test("item mode does not force total", identical(get_mandatory_W(cc), paste0("H1FS", 1:19)))
  cc$analysis$protected_W_mode <- "summary"
  test("summary protection", identical(get_mandatory_W(cc), "MHSum_W1"))
  cc$analysis$protected_W_mode <- "both"
  test("combined protection", length(get_mandatory_W(cc)) == 20L)
  raw <- as.data.frame(matrix(0, 3L, 19L)); names(raw) <- paste0("H1FS", 1:19)
  raw[1, c(4,8,11,15)] <- 3
  raw[2, setdiff(1:19,c(4,8,11,15))] <- 3
  raw[3, 1] <- 8
  test("CES-D endpoints and complete-item missingness", equal(score_wave1_cesd(raw), c(0,57,NA)))
  test("CES-D one-row input", equal(score_wave1_cesd(raw[1,,drop=FALSE]), 0))
  registry <- build_wave1_variable_source_registry(list(wave1_inhome = raw), cc)
  registry <- append_derived_variable_registry(registry, "MHSum_W1", names(raw), "wave1_inhome")
  test("all 19 summary parents verified and stored", nrow(registry) == 20L &&
         registry$derived_from[registry$raw_variable == "MHSum_W1"] == paste(names(raw), collapse=";"))
  test("invalid summary parent rejected", fails(append_derived_variable_registry(registry, "bad", "absent", "wave1_inhome")))
  p <- c(.2,.2,.6,.9); y <- c(0,1,0,1); w <- c(1,3,2,1)
  iso <- fit_probability_calibrator(p,y,w,"isotonic")
  test("weighted isotonic ties and pooling", equal(predict_probability_calibrator(iso,p), c(.5,.5,.5,1)))
  order2 <- c(4,2,1,3)
  iso2 <- fit_probability_calibrator(p[order2],y[order2],w[order2],"isotonic")
  test("isotonic permutation invariance", equal(predict_probability_calibrator(iso2,p), predict_probability_calibrator(iso,p)))
  iso3 <- fit_probability_calibrator(rep(p,w),rep(y,w),rep(1,sum(w)),"isotonic")
  test("survey weights agree with replicated-data answer", equal(predict_probability_calibrator(iso3,p), predict_probability_calibrator(iso,p)))
  test("isotonic boundaries", equal(predict_probability_calibrator(iso,c(0,1)), c(.5,1)))
  constant <- fit_probability_calibrator(rep(.5,4),y,w,"platt")
  test("constant-score weighted calibration", equal(predict_probability_calibrator(constant,p),rep(4/7,4)))
  pp <- rep(stats::plogis(seq(-2,2,length.out=5)),each=10)
  yy <- unlist(lapply(c(1,3,5,7,9), function(k) c(rep(1,k),rep(0,10-k))))
  pl <- fit_probability_calibrator(pp,yy,rep(1,50),"platt")
  test("Platt finite monotone predictions", all(is.finite(pl$coefficients)) && pl$coefficients[2] > 0)
  test("calibration rejects invalid weights", fails(fit_probability_calibrator(p,y,c(1,0,1,1),"isotonic")))
  mock <- list(Z=cbind(pp,pp), coef=c(.4,.6))
  cal <- calibrate_outer_predictions(mock,yy,rep(1,50),c(.3,.6,.8),"platt",1L,"g")
  test("outer calibration uses training scores", equal(cal$prediction,predict_probability_calibrator(pl,c(.3,.6,.8))) &&
         cal$audit$calibration_labels == "outer_training_only")
  badmock <- mock; badmock$Z[1,1] <- NA
  test("incomplete inner scores rejected", fails(calibrate_outer_predictions(badmock,yy,rep(1,50),.5,"platt",1L,"g")))
  # Verify numeric total preprocessing for every representation, including a
  # missing total. Its full 0:57 range must survive training preprocessing.
  dat <- as.data.frame(lapply(1:19, function(j) rep(0:3, length.out=120)))
  names(dat) <- paste0("H1FS",1:19)
  dat$MHSum_W1 <- c(NA, seq(0,57,length.out=119))
  dat[[cc$analysis$exposure_var]] <- rep(0:1,60)
  cc$preprocessing$global_missing_dictionary <- setNames(lapply(names(dat), function(nm)
    make_native_only_missing_rule(dat[[nm]],nm)),names(dat))
  for (nm in paste0("H1FS",1:19)) cc$preprocessing$global_missing_dictionary[[nm]]$as_factor <- TRUE
  for (mode in c("items","summary","both")) {
    cc$analysis$protected_W_mode <- mode
    pack <- build_final_W_train_valid(dat,dat,c(paste0("H1FS",1:19),"MHSum_W1"),cc)
    test(paste("design has mandatory columns",mode), all(get_mandatory_W(cc) %in% pack$processed_raw_map))
  }
  rec <- learn_final_missing_recipe(dat["MHSum_W1"],cc$preprocessing)
  test("total remains numeric", rec$MHSum_W1$type == "numeric")
  prep <- prep_numeric_train(0:57,cc$final_preprocess,cc$preprocessing,variable_name="MHSum_W1")
  test("total tails not winsorized", all(is.infinite(prep$win_q)))
  cc$outcome$family <- "HoursWorked"; cc$outcome$current_wave <- 4L; cc$outcome$waves <- 4L
  cc$mortality_sensitivity$enabled <- FALSE
  d <- data.frame(AID=c("1","2","3"),Y=c(NA,20,10))
  names(d)[1:2] <- c(cc$analysis$id_var,cc$analysis$outcome_var)
  s <- data.frame(AID=c("3","1","2"),wave=4,family="HoursWorked",status_zero=c(0,1,1),
                  reference_verified=1,reason=c("eligible","incarcerated","nonresident"),
                  reference_period="current Wave-IV endpoint",evidence_source="synthetic adjudication")
  names(s)[1] <- cc$analysis$id_var
  cc$analysis$dsp4_mode <- "composite"
  z <- apply_verified_labor_status(d,cc,s)
  test("verified status joins by ID and handles observed/missing", equal(z$data[[cc$analysis$outcome_var]],c(0,0,10)))
  cc$analysis$dsp4_mode <- "exclude_sensitivity"
  z <- apply_verified_labor_status(d,cc,s)
  test("exclusion flags without changing outcomes", equal(z$data[[cc$analysis$outcome_var]],d[[cc$analysis$outcome_var]]) && equal(z$data$.status_excluded,c(TRUE,TRUE,FALSE)))
  test("status coverage required", fails(apply_verified_labor_status(d,cc,s[-1,])))
  test("duplicate status IDs rejected", fails(apply_verified_labor_status(d,cc,rbind(s,s[1,]))))
  bads <- s; bads$reference_verified[2] <- 0
  test("unverified status rejected", fails(apply_verified_labor_status(d,cc,bads)))
  unresolved <- s
  unresolved$status_zero[2] <- 0L
  unresolved$reference_verified[2] <- 0L
  unresolved$reason[2] <- "unresolved"
  for (status_mode in c("composite", "exclude_sensitivity")) {
    cc$analysis$dsp4_mode <- status_mode
    z_unresolved <- apply_verified_labor_status(d, cc, unresolved)
    test(paste("unresolved status preserved", status_mode),
         is.na(z_unresolved$data[[cc$analysis$outcome_var]][1]) &&
           !z_unresolved$data$.status_excluded[1] && z_unresolved$audit$n_unresolved == 1L)
  }
  bads <- s; bads$reason[2] <- "deceased"
  test("deaths not disposition-zeroed", fails(apply_verified_labor_status(d,cc,bads)))
  cc$outcome$family <- "Compensation"
  test("annual earnings recoding blocked", fails(apply_verified_labor_status(d,cc,s)))
  cc$outcome$family <- "HoursWorked"; cc$analysis$dsp4_mode <- "audit_only"; cc$analysis$enable_dsp4_audit <- TRUE
  dsp <- data.frame(AID=c("1","2","3"),DSP4=c(459,477,NA))
  names(dsp)[1] <- cc$analysis$id_var
  aud <- build_disposition_audit(d,cc,dsp)
  test("disposition missing-code audit retains all rows", sum(aud$n) == 3L && sum(aud$n_outcome_observed) == 2L)
  cc$analysis$adaptive_g_truncation_apply <- TRUE
  test("unsupported post-estimation clip selection fails early", fails(validate_cfg(cc)))
  cc$analysis$adaptive_g_truncation_apply <- FALSE
  cc$global$pipeline_source_path <- cfg_template$global$pipeline_source_path
  f0 <- make_main_dataset_cache_fingerprint(cc)
  cc$analysis$restrict_wave1_depressed <- !isTRUE(cc$analysis$restrict_wave1_depressed)
  test("subgroup switch invalidates dataset cache", !identical(f0,make_main_dataset_cache_fingerprint(cc)))
  f0 <- make_main_dataset_cache_fingerprint(cc); cc$analysis$wave1_depression_cutpoint <- 21
  test("baseline threshold invalidates dataset cache", !identical(f0,make_main_dataset_cache_fingerprint(cc)))
  f0 <- make_main_dataset_cache_fingerprint(cc); cc$analysis$dsp4_mode <- "composite"
  test("status definition invalidates dataset cache", !identical(f0,make_main_dataset_cache_fingerprint(cc)))
  ac <- list(psi_att=-2,att_se=.5,inference_df=20,delta_Y=rep(1,4),
             Y_bounded_orig=1:4,weights=rep(1,4),pi_AW=rep(1,4))
  bias <- build_ovb_robustness_values(list(att_components=ac),cc)
  test("fixed-bias point-null identity", equal(bias$adjusted_att[bias$assumed_estimator_bias == -2],0))
  test("fixed-bias conditional intervals retain width", length(unique(round(bias$conditional_ci_upper-bias$conditional_ci_lower,10))) == 1L)
  data.frame(test=checked,status="pass",stringsAsFactors=FALSE)
}

# Fast regression checks for the recorded-status path and saved calibration data.
run_status_and_review_self_test <- function(cfg) {
  cc <- cfg
  cc$outcome$family <- "HoursWorked"; cc$outcome$current_wave <- 4L; cc$outcome$waves <- 4L
  cc$analysis$dsp4_mode <- "composite"
  cc$mortality_sensitivity$enabled <- TRUE
  id <- cc$analysis$id_var; yv <- cc$analysis$outcome_var
  dv <- resolve_mortality_spec(cc,4L)$death_before_outcome_var
  m <- data.frame(id=as.character(1:20), y=c(rep(35,8),rep(NA_real_,10),35,0))
  names(m) <- c(id,yv); m[[dv]] <- as.integer(1:20==14); m[[yv]][14] <- 0
  w <- data.frame(id=as.character(c(1:8,19,20)),PRISON4=c(0,0,1,0,0,0,0,0,0,0),
    H4MI1=c(0,1,0,1,1,1,1,1,0,0), H4MI3=c(7,1,7,0,1,1,1,1,1,1),
    H4MI6A=c(7,1,7,1,1,1,0,8,1,1), H4MI6B=c(7,0,7,0,1,1,1,8,0,0),
    H4MI6C=c(7,0,7,0,0,0,0,8,0,0), H4MI7=c(7,7,7,7,1,2,7,7,1,1))
  names(w)[1] <- id
  dsp <- data.frame(id=as.character(1:20),DSP4=c(rep(491,8),477,613,486,460,459,459,478,482,NA,480,491,491))
  names(dsp)[1] <- id
  s <- build_documented_military_prison_status(m,cc,w,dsp)
  stopifnot(identical(which(s$status_zero==1L),c(2L,3L,5L,9L,10L,11L,15L,16L)),
    all(s$reference_verified[c(8,12,13,17:20)]==0),
    s$reason[14]=="mortality_handled_separately",all(s$military_response_conflict[19:20]))
  shuffled <- build_documented_military_prison_status(m,cc,w[nrow(w):1,],dsp[20:1,])
  stopifnot(identical(s,shuffled))
  fail <- function(expr) inherits(try(force(expr),silent=TRUE),"try-error")
  stopifnot(fail(build_documented_military_prison_status(m,cc,rbind(w,w[1,]),dsp)))
  z <- apply_verified_labor_status(m,cc,s)
  out <- z$data; ob <- resolve_composite_observation(out,cc)
  stopifnot(identical(which(ob$structural_zero),c(2L,3L,5L,9L,10L,11L,14L,15L,16L)),
    all(is.na(out[[yv]][c(8,12,13,17:19)])), out[[yv]][20]==0,
    all(out[[yv]][c(1,4,6,7)]==35), ob$S[19]==1L, ob$response[19]==0L,
    z$audit$n_unresolved_nonzero_masked==2L,
    identical(as.character(z$details[[id]]),as.character(m[[id]])))
  bad <- out; bad[[yv]][19] <- 35
  stopifnot(fail(resolve_composite_observation(bad,cc)))
  bad <- s; bad$reference_verified[2] <- 0L
  stopifnot(fail(apply_verified_labor_status(m,cc,bad)))
  ca <- cc; ca$analysis$enable_dsp4_audit <- TRUE
  da <- build_disposition_audit(out,ca,dsp)
  stopifnot(sum(da$n_disposition_death_without_dated_confirmation)==1L)
  no_mort <- cc; no_mort$mortality_sensitivity$enabled <- FALSE
  m0 <- m; m0[[dv]] <- NULL
  stopifnot(nrow(build_documented_military_prison_status(m0,no_mort,w,dsp))==20L)
  cc$analysis$dsp4_mode <- "auto_verified"; cc$analysis$auto_military_prison_status <- TRUE
  stopifnot(resolve_status_mode(cc)=="composite")
  local_apply <- apply_verified_labor_status
  env <- new.env(parent=environment(local_apply))
  env$read_xpt_df <- function(path) {
    if (identical(path,cc$paths$wave4_inhome)) w else
      if (identical(path,cc$paths$wave4_disposition)) dsp else stop("Unexpected status source")
  }
  environment(local_apply) <- env
  stopifnot(identical(local_apply(m,cc)$data,out))
  conflicting <- w; conflicting$H4MI6A[5] <- 0
  stopifnot(build_documented_military_prison_status(m,cc,conflicting,dsp)$reason[5]=="unresolved")
  batch <- cc; batch$outcome$family <- "LaborForceParticipation"
  batch$outcome$current_wave <- NULL; batch$outcome$waves <- c(3L,4L)
  batch <- apply_outcome_runtime_defaults(batch)
  stopifnot(batch$analysis$dsp4_mode=="auto_verified")
  batch$outcome$current_wave <- 3L
  stopifnot(resolve_status_mode(batch)=="audit_only")
  batch$outcome$current_wave <- 4L
  stopifnot(resolve_status_mode(batch)=="composite")
  batch$outcome$current_wave <- NULL; batch$outcome$waves <- "all"
  stopifnot(resolve_status_mode(batch)=="audit_only")
  cc$outcome$family <- "Compensation"
  stopifnot(resolve_status_mode(cc)=="audit_only")
  # A deterministic response fold still has an auditable calibration archive.
  ar <- capture_calibration_training(NULL,c(1,1),c(2,3),c("a","b"),c("1","2"),1,constant=1)
  stopifnot(all(ar$data$score==1),identical(ar$data$respondent_id,c("a","b")))
  sl <- list(Z=cbind(c(.1,.3,.6,.9),c(.2,.4,.5,.8)),coef=c(.4,.6))
  ar <- capture_calibration_training(sl,c(0,0,1,1),rep(1,4),letters[1:4],1:4,1)
  stopifnot(isTRUE(all.equal(ar$data$score,as.numeric(sl$Z %*% sl$coef))))
  cat("Status and calibration archive regression checks passed.\n")
  invisible(TRUE)
}

run_preflight_unit_test <- function(cfg_template) {
  message("=== Preflight unit test: raw-dollar joint ATT + percentage inference ===")
  run_v840_self_test(cfg_template)
  run_v842_self_test(cfg_template)
  run_v843_self_test(cfg_template)
  run_exposure_mode_self_test(cfg_template)
  # Synthetic cross-outcome fixtures must not inherit a real-data status-file
  # restriction. Entry points validate the actual requested config first;
  # status transformations themselves have explicit fixture tests above.
  cfg_template$final_tmle$g_lower_rule <- "fixed"
  cfg_template$analysis$dsp4_mode <- "audit_only"
  cfg_template$analysis$auto_military_prison_status <- FALSE
  cfg_template$analysis$restrict_wave1_depressed <- FALSE
  # Synthetic fixtures contain no Wave-III file; the scope logic has its own test.
  # A non-default scope also switches the policy translation off for the real
  # run; the synthetic fixtures still exercise the complete policy path.
  cfg_template$exposure$exposure_mode <- "wave2_only"
  cfg_template$policy$enable_policy_components <- TRUE
  cfg_template$policy$enable_att_prevalence_translation <- TRUE
  # The fixed reference fixture has exactly these baseline columns. Real-data
  # representation choices are covered separately by configuration-path tests.
  cfg_template$analysis$protected_W_mode <- cfg_template$analysis$protected_W_mode %||% "summary"
  cfg_template$final_tmle$protected_W_items <- paste0("H1FS", 1:19)
  cfg_template$causal_governance$additional_mandatory_W <- character(0)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  preflight_step("source-code fingerprint", {
    pipeline_script_fingerprint(
      cfg_template, strict = isTRUE(cfg_template$global$require_script_md5 %||% FALSE))
  })
  preflight_step("configuration paths, calibration, scoring and status guards", {
    run_configuration_path_tests(cfg_template)
    run_status_and_review_self_test(cfg_template)
  })
  set.seed(1L)
  preflight_step("600-row sequential composite known-answer test", {
    run_sequential_composite_self_test(cfg_template)
  })
  preflight_step("whole-PSU fold-engine stress test", {
    run_fold_engine_stress_test(cfg_template)
  })
  set.seed(1L)
  n <- 1200L; J <- 80L
  cluster <- sample(rep(seq_len(J), length.out = n), size = n, replace = FALSE)
  
  # Baseline Wave I Feelings Scale block, the exact protected set used in the
  # production analysis. These are generated before A and Y so they are genuine
  # pre-exposure confounders in the synthetic target trial.
  h1fs <- replicate(19L, sample(0:3, n, replace = TRUE,
                                prob = c(.58, .27, .11, .04)))
  colnames(h1fs) <- paste0("H1FS", 1:19)
  mh0 <- rowSums(h1fs[, c(1:3,5:7,9:10,12:14,16:19), drop=FALSE]) -
    rowSums(h1fs[, c(4,8,11,15), drop=FALSE])
  W1 <- stats::rnorm(n); W2 <- stats::rbinom(n, 1, 0.35)
  AH_PVT <- 100 + 15 * (0.45 * W1 + stats::rnorm(n, sd = 0.9))
  pA <- stats::plogis(-3.0 + 0.055 * mh0 + 0.40 * W1 + 0.35 * W2 -
                        0.006 * (AH_PVT - 100))
  A <- stats::rbinom(n, 1, pA)
  # Guarantee enough treated observations for nested whole-cluster CV.
  min_preflight_treated <- 140L
  if (sum(A) < min_preflight_treated)
    A[order(pA, decreasing = TRUE)[seq_len(min_preflight_treated)]] <- 1L
  
  Y <- 30000 - 6500 * A + 320 * mh0 + 2800 * W1 + 1800 * W2 +
    stats::rnorm(n, sd = 11000)
  Y <- pmax(0, Y)
  Y[1:15] <- 0
  extreme_values <- c(150000, 180000, 220000, 280000, 350000,
                      450000, 550000, 700000, 850000, 920000, 999995)
  extreme_idx <- c(which(A == 0L)[1:6], which(A == 1L)[1:5])
  Y[extreme_idx] <- extreme_values
  delta <- stats::rbinom(n, 1, stats::plogis(1.9 - 0.35*A + 0.015*mh0 + 0.2*W1))
  for (a_value in 0:1) {
    eligible_a <- setdiff(which(A == a_value), extreme_idx)
    if (length(eligible_a) < 10L)
      stop("Preflight synthetic data lack enough rows in treatment arm.", call. = FALSE)
    delta[eligible_a[seq_len(5L)]] <- 0L
    delta[eligible_a[6:10]] <- 1L
  }
  delta[extreme_idx] <- 1L
  preflight_cell_counts <- table(
    factor(A, levels = 0:1),
    factor(delta, levels = 0:1)
  )
  if (any(preflight_cell_counts < 5L))
    stop ("Preflight synthetic data lack adequate A-by-delta support.", call. = FALSE)
  Y_obs <- ifelse(delta == 1L, Y, NA_real_)
  
  region_by_cluster <- setNames(rep(1:4, length.out = J), as.character(seq_len(J)))
  df <- data.frame(
    AID = seq_len(n), PSUSCID = cluster,
    REGION = as.integer(region_by_cluster[as.character(cluster)]),
    GSWGT1 = exp(stats::rnorm(n, log(1000), 0.65)),
    Depressed = A, Y = Y_obs, delta_Y = delta,
    W1 = W1, W2 = W2, AH_PVT = AH_PVT,
    W3 = stats::rnorm(n), W4 = sample(1:4, n, replace = TRUE),
    check.names = FALSE)
  for (j in seq_len(ncol(h1fs))) df[[colnames(h1fs)[j]]] <- h1fs[,j]
  df$MHSum_W1 <- score_wave1_cesd(df)
  synthetic_registry_vars <- c("W1", "W2", "W3", "W4", "AH_PVT", colnames(h1fs), "MHSum_W1")
  synthetic_registry <- data.frame(
    source = "synthetic_wave1",
    raw_variable = synthetic_registry_vars,
    canonical_variable = canonical_join_variable_name(synthetic_registry_vars),
    derived_from = NA_character_,
    stringsAsFactors = FALSE)
  synthetic_registry <- validate_variable_source_registry(synthetic_registry)
  attr(df, "variable_source_registry") <- synthetic_registry
  
  # Add valid-weight Wave-I respondents outside the complete-CES-D analytic
  # domain so preflight exercises survey-domain rather than subset-only EIF
  # variance estimation. They retain the same PSU-to-REGION mapping but never
  # enter nuisance fitting or the ATT point estimate.
  n_outside <- 240L
  outside_cluster <- sample(seq_len(J), n_outside, replace = TRUE)
  outside <- data.frame(
    AID = n + seq_len(n_outside),
    PSUSCID = outside_cluster,
    REGION = as.integer(region_by_cluster[as.character(outside_cluster)]),
    GSWGT1 = exp(stats::rnorm(n_outside, log(1000), 0.65)),
    .analysis_domain = FALSE,
    check.names = FALSE)
  full_design <- df[, c("AID", "PSUSCID", "REGION", "GSWGT1"), drop = FALSE]
  full_design$.analysis_domain <- TRUE
  full_design <- rbind(full_design, outside)
  attr(df, "survey_design_frame") <- full_design
  
  cfg_design_test <- cfg_template
  cfg_design_test$analysis$enforce_expected_sample_gates <- TRUE
  cfg_design_test$analysis$expected_final_n <- nrow(df)
  cfg_design_test$analysis$expected_treated_n <- sum(df$Depressed == 1L)
  cfg_design_test$analysis$expected_cluster_n <- length(unique(df$PSUSCID))
  cfg_design_test$analysis$expected_exposure_cutpoint <- cfg_design_test$exposure$cutpoint
  design_test <- validate_final_sample_design_fields(df, cfg_design_test)
  if (design_test$n_strata != 4L)
    stop("Preflight FAILED: synthetic REGION/PSU design audit is incorrect.", call. = FALSE)
  # The merge repair must coalesce REGION.x/REGION.y into one canonical REGION,
  # reject genuine conflicts, and keep REGION out of W and the dictionary.
  region_merge_ok <- data.frame(AID = 1:4, REGION.x = c(1, 2, NA, 4),
                                REGION.y = c(1, 2, 3, 4), check.names = FALSE)
  region_merge_ok <- canonicalize_merged_design_field(region_merge_ok, "REGION")
  if (!identical(as.integer(region_merge_ok$REGION), 1:4) ||
      any(grepl("^REGION\\.[xy]", names(region_merge_ok), ignore.case = TRUE)))
    stop("Preflight FAILED: REGION suffix canonicalization is incorrect.", call. = FALSE)
  region_merge_bad <- data.frame(AID = 1:2, REGION.x = c(1, 2),
                                 REGION.y = c(1, 3), check.names = FALSE)
  if (!inherits(try(canonicalize_merged_design_field(region_merge_bad, "REGION"),
                    silent = TRUE), "try-error"))
    stop("Preflight FAILED: conflicting REGION sources did not stop.", call. = FALSE)
  preflight_step("canonical design-field audit append", {
    design_merge <- data.frame(
      AID = 1:4,
      PSUSCID.x = c(10, 10, 20, 20),
      PSUSCID.y = c(10, 10, 20, 20),
      REGION.x = c(1, 1, 2, 2),
      REGION.y = c(1, 1, 2, 2),
      check.names = FALSE)
    design_merge <- canonicalize_merged_design_field(design_merge, "PSUSCID")
    design_merge <- canonicalize_merged_design_field(design_merge, "REGION")
    design_audit <- attr(design_merge, "canonical_design_field_audit")
    if (!is.data.frame(design_audit) || nrow(design_audit) != 2L ||
        !setequal(design_audit$field, c("PSUSCID", "REGION")))
      stop("canonical design-field audit did not preserve both fields.", call. = FALSE)
  })
  
  preflight_step("redundancy assignment and denominator", {
    zz <- seq(-2, 2, length.out = 120L)
    red_X <- data.frame(red_a = zz, red_b = zz,
                        independent = rep(c(-1, 1), 60L))
    red_test <- cluster_dedupe_candidates(
      names(red_X), red_X, cor_threshold = 0.99,
      linkage = "complete", max_cluster_vars = 20L, enable = TRUE)
    red_denom <- length(unique(c(red_test$kept, red_test$dropped)))
    red_frac <- length(unique(red_test$dropped)) / red_denom
    dropped_mapped <- red_test$dropped %in% red_test$assignments$member_variable
    if (red_denom != 3L || length(red_test$dropped) != 1L ||
        !isTRUE(all(dropped_mapped)) ||
        !isTRUE(all.equal(red_frac, 1 / 3, tolerance = 1e-12)))
      stop("redundancy assignments or all-candidate denominator are incorrect.", call. = FALSE)
  })
  
  preflight_step("combined nuisance learner library activation", {
    cfg_sens <- cfg_template
    cfg_sens$learners$g$use_glmnet_h1fs <- TRUE
    cfg_sens$learners$pi$use_glmnet_A_unpenalized <- TRUE
    lib_g <- build_sl_library(cfg_sens, "g")
    lib_pi <- build_sl_library(cfg_sens, "pi")
    if (!"SL.glmnet.h1fs" %in% lib_g ||
        !"SL.glmnet.pi_A_unpenalized" %in% lib_pi ||
        "SL.glmnet.h1fs" %in% lib_pi ||
        "SL.glmnet.pi_A_unpenalized" %in% lib_g)
      stop("Combined nuisance sensitivity learners were not scoped to the intended nuisance models.", call. = FALSE)
  })
  
  preflight_step("real H1FS-prioritized g glmnet machinery", {
    if (!requireNamespace("glmnet", quietly = TRUE))
      stop("glmnet is required for the H1FS-prioritized g learner preflight.",
           call. = FALSE)
    set.seed(9081)
    ng <- 300L
    clg <- rep(seq_len(30L), each = 10L)
    Xg <- data.frame(
      H1FS1 = stats::rnorm(ng),
      H1FS2 = stats::rnorm(ng),
      W1 = stats::rnorm(ng),
      check.names = FALSE)
    Ag <- stats::rbinom(
      ng, 1L,
      stats::plogis(-1.4 + 0.9 * Xg$H1FS1 - 0.7 * Xg$H1FS2 +
                      0.05 * Xg$W1))
    if (min(table(Ag)) < 40L)
      stop("Synthetic g learner data lack treatment support.", call. = FALSE)
    cfg_g <- cfg_template
    cfg_g$learners$g$use_glmnet_h1fs <- TRUE
    register_custom_learners(cfg_g)
    .SL_RUNTIME_ENV$h1fs_processed_columns <- c("H1FS1", "H1FS2")
    g_fun <- get("SL.glmnet.h1fs", envir = .GlobalEnv, inherits = FALSE)
    g_fit <- with_sl_glmnet_context(
      "g", 9082L, 3L,
      g_fun(
        Y = Ag, X = Xg, newX = Xg,
        family = stats::binomial(), obsWeights = rep(1, ng),
        id = as.character(clg)))
    if (!is.list(g_fit) || length(g_fit$pred) != ng ||
        any(!is.finite(g_fit$pred)) ||
        !inherits(g_fit$fit, "SL.glmnet.h1fs"))
      stop("H1FS-prioritized g learner returned an invalid fit or predictions.",
           call. = FALSE)
    pf <- g_fit$fit$penalty_factor
    expected_h <- cfg_g$learners$glmnet_h1fs$h1fs_penalty_multiplier
    if (!is.numeric(pf) ||
        !all(c("H1FS1", "H1FS2", "W1") %in% names(pf)) ||
        any(abs(pf[c("H1FS1", "H1FS2")] - expected_h) > 1e-12) ||
        abs(pf["W1"] - 1) > 1e-12)
      stop("H1FS-prioritized g learner did not apply the intended penalty factors.",
           call. = FALSE)
  })
  
  preflight_step("pi counterfactual block signed contrast", {
    set.seed(9091)
    npi <- 240L
    Api <- rep(c(0L,1L), each=npi/2)
    Wpi <- data.frame(W1=rnorm(npi), W2=rnorm(npi))
    dpi <- rbinom(npi,1,stats::plogis(-0.4 + 1.4*Api + 0.05*Wpi$W1))
    Xtr <- data.frame(A=Api,Wpi,check.names=FALSE)
    fit <- glm(stats::as.formula("dpi ~ A + W1 + W2"), data=Xtr, family=binomial())
    p1 <- predict(fit,newdata=data.frame(A=1L,Wpi),type="response")
    p0 <- predict(fit,newdata=data.frame(A=0L,Wpi),type="response")
    pA <- ifelse(Api == 1L, p1, p0)
    blocks <- extract_pi_counterfactual_blocks(c(p1, p0, pA), npi)
    signed <- mean(blocks$pi_1W - blocks$pi_0W)
    observed_match <- max(abs(blocks$pi_AW - ifelse(Api == 1L, blocks$pi_1W, blocks$pi_0W)))
    if (!is.finite(signed) || signed < 0.15 || !is.finite(observed_match) || observed_match > 1e-12)
      stop("pi signed counterfactual preflight failed, observed-treatment extraction failed, or blocks may be reversed.",call.=FALSE)
  })
  
  
  preflight_step("real SuperLearner pi treatment-response machinery", {
    if (!requireNamespace("SuperLearner", quietly = TRUE) ||
        !requireNamespace("glmnet", quietly = TRUE))
      stop("SuperLearner and glmnet are required for the pi machinery preflight.",
           call. = FALSE)
    set.seed(9092)
    npi <- 360L
    Jpi <- 36L
    clpi <- rep(seq_len(Jpi), each = npi / Jpi)
    Api <- rep(c(0L, 1L), length.out = npi)
    Wpi <- data.frame(
      W1 = stats::rnorm(npi),
      W2 = stats::rnorm(npi),
      check.names = FALSE)
    p_true_1 <- stats::plogis(-0.7 + 1.8 + 0.05 * Wpi$W1 - 0.04 * Wpi$W2)
    p_true_0 <- stats::plogis(-0.7 +       0.05 * Wpi$W1 - 0.04 * Wpi$W2)
    dpi <- stats::rbinom(
      npi, 1L, ifelse(Api == 1L, p_true_1, p_true_0))
    # Guarantee all A-by-delta cells and every cluster-fold have support.
    if (any(table(Api, dpi) < 10L))
      stop("Synthetic pi machinery data lack A-by-delta support.", call. = FALSE)
    
    cfg_pi <- cfg_template
    cfg_pi$learners$pi$use_glmnet_A_unpenalized <- TRUE
    register_custom_learners(cfg_pi)
    Xpi <- data.frame(A = Api, Wpi, check.names = FALSE)
    new_pi <- rbind(
      data.frame(A = rep(1L, npi), Wpi, check.names = FALSE),
      data.frame(A = rep(0L, npi), Wpi, check.names = FALSE),
      data.frame(A = Api, Wpi, check.names = FALSE))
    valid_pi <- build_cluster_valid_rows(
      cluster_vec = as.character(clpi), A_vec = Api, V = 3L,
      seed = 9093L, weights = rep(1, npi), delta = dpi,
      balance_on_weights = FALSE,
      fold_control = fold_control_from_cfg(cfg_pi, "internal"))
    cvpi <- list(
      V = length(valid_pi), validRows = valid_pi,
      stratifyCV = FALSE, shuffle = FALSE)
    pi_fun <- get(
      "SL.glmnet.pi_A_unpenalized", envir = .GlobalEnv,
      inherits = FALSE)
    direct_pi <- with_sl_glmnet_context(
      "pi", 9094L, 3L,
      pi_fun(
        Y = dpi, X = Xpi, newX = new_pi,
        family = stats::binomial(), obsWeights = rep(1, npi),
        id = as.character(clpi)))
    direct_pf <- direct_pi$fit$penalty_factor
    if (!is.list(direct_pi) || length(direct_pi$pred) != 3L * npi ||
        any(!is.finite(direct_pi$pred)) ||
        !inherits(direct_pi$fit, "SL.glmnet.pi_A_unpenalized") ||
        !is.numeric(direct_pf) || !all(c("A", "W1", "W2") %in% names(direct_pf)) ||
        abs(direct_pf["A"]) > 1e-12 ||
        any(abs(direct_pf[c("W1", "W2")] - 1) > 1e-12))
      stop("Unpenalized-A pi learner did not apply the intended penalty factors or predictions.",
           call. = FALSE)
    fit_pi <- with_sl_glmnet_context(
      "pi", 9095L, 3L,
      SuperLearner::SuperLearner(
        Y = dpi, X = Xpi, newX = new_pi,
        family = stats::binomial(),
        SL.library = c("SL.mean", "SL.glmnet.pi_A_unpenalized"),
        obsWeights = rep(1, npi), id = as.character(clpi),
        cvControl = cvpi,
        control = list(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE),
        verbose = FALSE))
    assert_superlearner_fit(
      fit_pi, c("SL.mean", "SL.glmnet.pi_A_unpenalized"),
      expected_prediction_n = 3L * npi, target = "pi",
      outer_fold = 0L)
    blocks_pi <- extract_pi_counterfactual_blocks(
      as.numeric(fit_pi$SL.predict), npi)
    wpi <- rep(1, npi)
    signed_est <- weighted_mean_safe(
      blocks_pi$pi_1W - blocks_pi$pi_0W, wpi)
    signed_truth <- weighted_mean_safe(p_true_1 - p_true_0, wpi)
    observed_match <- max(abs(
      blocks_pi$pi_AW -
        ifelse(Api == 1L, blocks_pi$pi_1W, blocks_pi$pi_0W)))
    if (!is.finite(signed_est) || !is.finite(signed_truth) ||
        signed_truth <= 0 || signed_est <= 0.10 ||
        sign(signed_est) != sign(signed_truth) ||
        !is.finite(observed_match) || observed_match > 1e-10)
      stop(paste0(
        "Real pi SuperLearner machinery did not recover the known positive ",
        "A effect, or observed-treatment block extraction failed."),
        call. = FALSE)
  })
  
  preflight_step("weighted AUROC tie handling", {
    auc_tie <- weighted_auc_tie_corrected(
      y = c(1L, 0L), score = c(0.5, 0.5), w = c(1, 1))
    auc_perfect <- weighted_auc_tie_corrected(
      y = c(0L, 1L), score = c(0.1, 0.9), w = c(2, 3))
    auc_reverse <- weighted_auc_tie_corrected(
      y = c(0L, 1L), score = c(0.9, 0.1), w = c(2, 3))
    if (!isTRUE(all.equal(auc_tie, 0.5, tolerance = 1e-12)) ||
        !isTRUE(all.equal(auc_perfect, 1, tolerance = 1e-12)) ||
        !isTRUE(all.equal(auc_reverse, 0, tolerance = 1e-12)))
      stop("Weighted AUROC tie correction is incorrect.", call. = FALSE)
  })
  
  preflight_step("named learner-risk parsing", {
    metric_test <- parse_named_metric_string(
      "SL.mean_All=0.125;SL.glmnet.fixed_All=0.100",
      fold = 1L, nuisance = "Q", value_name = "cv_risk")
    if (nrow(metric_test) != 2L ||
        !identical(metric_test$learner,
                   c("SL.mean_All", "SL.glmnet.fixed_All")) ||
        !isTRUE(all.equal(metric_test$cv_risk, c(0.125, 0.100))))
      stop("named learner-risk parser is incorrect.", call. = FALSE)
  })
  
  preflight_step("sparse balance and love-plot exclusion", {
    sparse_x <- rep(NA_real_, 40L)
    sparse_x[c(1:4, 21:24)] <- c(1:4, 2:5)
    sparse_group <- rep(0:1, each = 20L)
    sparse_bal <- balance_one_variable(
      sparse_x, sparse_group, rep(1, 40L), rep(1, 40L), "sparse_x")
    if (!isTRUE(sparse_bal$sparse_observed_support) ||
        sparse_bal$n_observed_group1 != 4L || sparse_bal$n_observed_group0 != 4L)
      stop("sparse numeric-balance support was not flagged.", call. = FALSE)
    fake_fit <- list(selection_log = do.call(rbind, lapply(1:5, function(ff)
      data.frame(variable = c("too_sparse", "kept_var"), fold = ff,
                 prefilter_reason = c("too_missing", "kept"),
                 selected = c(FALSE, TRUE), kept_in_final_W = c(FALSE, TRUE),
                 stringsAsFactors = FALSE))))
    excluded <- derive_love_plot_exclusions(fake_fit, cfg_template)
    if (!identical(excluded, "too_sparse"))
      stop("love-plot exclusion did not isolate the always-too-missing variable.", call. = FALSE)
  })
  validate_expected_final_sample_gates(df, cfg_design_test)
  bad_region <- df; bad_region$REGION[1L] <- NA_integer_
  if (!inherits(try(validate_final_sample_design_fields(bad_region, cfg_design_test), silent = TRUE), "try-error"))
    stop("Preflight FAILED: missing REGION did not trigger a design-field error.", call. = FALSE)
  bad_gate_cfg <- cfg_design_test; bad_gate_cfg$analysis$expected_final_n <- nrow(df) + 1L
  if (!inherits(try(validate_expected_final_sample_gates(df, bad_gate_cfg), silent = TRUE), "try-error"))
    stop("Preflight FAILED: an incorrect exact sample gate did not stop.", call. = FALSE)
  alternate_cutpoint_cfg <- cfg_design_test
  alternate_cutpoint_cfg$exposure$cutpoint <- 20
  alternate_cutpoint_cfg$analysis$expected_exposure_cutpoint <- 20
  alternate_cutpoint_cfg$analysis$enforce_expected_treated_gate <- FALSE
  if (inherits(try(validate_expected_final_sample_gates(df, alternate_cutpoint_cfg),
                   silent = TRUE), "try-error"))
    stop("Preflight FAILED: an alternative-cutpoint run could not retain invariant sample gates while disabling only the treated-count gate.",
         call. = FALSE)
  alternate_cutpoint_cfg$analysis$enforce_expected_treated_gate <- TRUE
  alternate_cutpoint_cfg$analysis$expected_treated_n <- sum(df$Depressed == 1L) + 1L
  if (!inherits(try(validate_expected_final_sample_gates(df, alternate_cutpoint_cfg),
                    silent = TRUE), "try-error"))
    stop("Preflight FAILED: the treated-count gate did not stop under an intentionally mismatched alternative-cutpoint count.",
         call. = FALSE)
  
  cfg_pre <- cfg_template
  cfg_pre$stages <- list(
    run_preflight_unit_test = FALSE,
    run_read_wave1_phase = FALSE,
    run_build_main_dataset_phase = FALSE,
    run_final_cv_tmle = TRUE,
    run_diagnostics = FALSE,
    run_multiseed_att = FALSE)
  cfg_pre$cache$use_cached_wave1 <- FALSE
  cfg_pre$cache$use_cached_main_dataset <- FALSE
  cfg_pre$safety$require_publication_ready_marker <- FALSE
  cfg_pre$global$output_dir <- tempfile("preflight_raw_pct_"); dir.create(cfg_pre$global$output_dir)
  cfg_pre$global$checkpoint_subdir <- "fresh_preflight_checkpoints"
  cfg_pre$global$verbose <- TRUE
  cfg_pre$outcome$log_transform <- FALSE
  cfg_pre$outcome$family <- "Compensation"
  cfg_pre$outcome$waves <- 4L
  cfg_pre$outcome$compensation_transform <- "identity"
  cfg_pre$outcome$continuous_upper_quantile <- cfg_template$outcome$continuous_upper_quantile
  cfg_pre$outcome$continuous_bound_eps <- 0
  cfg_pre$outcome$current_wave <- 4L
  cfg_pre$analysis$dsp4_mode <- "audit_only"
  preflight_dv <- resolve_mortality_spec(cfg_pre,4L)$death_before_outcome_var
  df[[preflight_dv]] <- 0L
  # Exercise the eligibility fit and known-zero path in the REAL SL driver.
  death_candidates <- setdiff(seq_len(nrow(df)),c(extreme_idx,1:15))
  death_rows <- c(head(death_candidates[df$Depressed[death_candidates]==1L],15L),
                  head(death_candidates[df$Depressed[death_candidates]==0L],25L))
  df[[preflight_dv]][death_rows] <- 1L
  df$Y[death_rows] <- 0;df$delta_Y[death_rows] <- 1L
  cfg_pre$outcome$family_member <- NULL
  cfg_pre$analysis$outcome_type <- "continuous"
  cfg_pre$analysis$enforce_expected_sample_gates <- FALSE
  cfg_pre$final_tmle$primary_estimand <- "att"
  cfg_pre$final_tmle$att_estimator <- "tmle"
  cfg_pre$final_tmle$report_att <- TRUE
  cfg_pre$final_tmle$trim_enable <- FALSE
  cfg_pre$final_tmle$vfolds <- 5L
  cfg_pre$final_tmle$internal_superlearner_folds <- 3L
  cfg_pre$final_tmle$rough_folds <- 2L
  cfg_pre$final_tmle$rough_top_n_outcome <- 6L
  cfg_pre$final_tmle$rough_top_n_missingness <- 4L
  cfg_pre$final_tmle$rough_top_n_joint_AY <- 4L
  cfg_pre$final_tmle$rough_top_n_exposure_only <- 0L
  cfg_pre$final_tmle$rough_top_n_exposure_for_lasso <- 6L
  cfg_pre$final_tmle$rough_candidate_pool_max <- 10L
  cfg_pre$final_tmle$rough_max_total_vars <- 8L
  cfg_pre$final_tmle$nested_lasso_after_rough <- TRUE
  cfg_pre$final_tmle$lasso_screen_min_vars <- 2L
  cfg_pre$final_tmle$lasso_screen_max_vars <- 8L
  cfg_pre$final_tmle$lasso_screen_max_processed_cols <- 30L
  cfg_pre$final_tmle$final_max_processed_columns <- 30L
  cfg_pre$final_tmle$use_epp_cap <- FALSE
  cfg_pre$final_tmle$hard_max_processed_columns <- 150L
  cfg_pre$final_tmle$lasso_screen_folds <- 3L
  cfg_pre$final_tmle$cluster_aware_internal_cv <- TRUE
  cfg_pre$final_tmle$outer_fold_balance_on_weights <- FALSE
  cfg_pre$final_tmle$internal_fold_balance_on_weights <- FALSE
  cfg_pre$final_tmle$fold_max_attempts <- 30L # synthetic fixture only; retain hard fold gates
  cfg_pre$final_tmle$fold_max_size_ratio <- 1.60
  cfg_pre$final_tmle$fold_max_size_deviation_prop <- 0.35
  cfg_pre$final_tmle$fold_internal_max_size_ratio <- 1.75
  cfg_pre$final_tmle$fold_internal_max_size_deviation_prop <- 0.45
  cfg_pre$final_tmle$fold_min_active_cell_n <- 1L
  cfg_pre$final_tmle$use_fold_checkpoints <- FALSE
  cfg_pre$preprocessing$global_missing_dictionary <- NULL
  cfg_pre$preprocessing$variable_source_registry <- synthetic_registry
  # Freeze semantic missing-code rules once on the complete synthetic baseline
  # distribution, before outer-fold creation and before any screening.
  preflight_dictionary_source <- df[, setdiff(
    names(df), c(cfg_pre$analysis$exposure_var, cfg_pre$analysis$outcome_var,
                 cfg_pre$analysis$outcome_observed_var)), drop = FALSE]
  attr(preflight_dictionary_source, "variable_source_registry") <- synthetic_registry
  preflight_dictionary_vars <- setdiff(
    names(preflight_dictionary_source),
    get_common_exclusion_vars(cfg_pre, include_analysis_outputs = FALSE))
  if (cfg_pre$analysis$strata_var %in% preflight_dictionary_vars ||
      cfg_pre$analysis$strata_var %in% get_candidate_vars(df, cfg_pre))
    stop("Preflight FAILED: REGION entered the dictionary or candidate covariates.", call. = FALSE)
  preflight_dictionary <- build_global_missing_code_dictionary(
    preflight_dictionary_source, preflight_dictionary_vars, cfg_pre$preprocessing)
  attr(df, "global_missing_dictionary") <- preflight_dictionary
  attr(df, "variable_source_registry") <- synthetic_registry
  cfg_pre$preprocessing$global_missing_dictionary <- preflight_dictionary
  
  preflight_step("main-cache fingerprint covers compensation transformations", {
    f0 <- make_main_dataset_cache_fingerprint(cfg_pre, df)
    cfg_changed <- cfg_pre
    cfg_changed$outcome$compensation_exact_only <- !cfg_pre$outcome$compensation_exact_only
    f1 <- make_main_dataset_cache_fingerprint(cfg_changed, df)
    if (identical(f0, f1))
      stop("Main-data fingerprint ignored compensation_exact_only.", call. = FALSE)
    cfg_changed <- cfg_pre
    cfg_changed$outcome$compensation_transform <- "log1p"
    f2 <- make_main_dataset_cache_fingerprint(cfg_changed, df)
    if (identical(f0, f2))
      stop("Main-data fingerprint ignored compensation_transform.", call. = FALSE)
  })
  
  # Stage-switch/cache regression test. A disabled build stage must load a
  # fingerprint-compatible artifact, preserve the frozen dictionary and survey
  # design attributes, and reject a stale cache rather than passing NULL or
  # rebuilding behind the user's back.
  cache_test_dir <- tempfile("preflight_stage_cache_")
  dir.create(cache_test_dir, recursive = TRUE)
  on.exit(unlink(cache_test_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wave1_cache_path <- file.path(cache_test_dir, "wave1_test.rds")
  main_cache_path <- file.path(cache_test_dir, "main_test.rds")
  w1_cache_test <- preflight_dictionary_source
  attr(w1_cache_test, "global_missing_dictionary") <- preflight_dictionary
  attr(w1_cache_test, "variable_source_registry") <- synthetic_registry
  attr(w1_cache_test, "full_survey_design_frame") <- full_design
  save_wave1_cache(w1_cache_test, wave1_cache_path, cfg_pre)
  w1_loaded <- obtain_wave1_for_run(
    cfg_pre, wave1_cache_path, allow_build = FALSE, supplied = NULL)
  if (!is.data.frame(w1_loaded) || nrow(w1_loaded) != nrow(w1_cache_test) ||
      is.null(attr(w1_loaded, "global_missing_dictionary")) ||
      is.null(attr(w1_loaded, "variable_source_registry")) ||
      is.null(attr(w1_loaded, "full_survey_design_frame")))
    stop("Preflight FAILED: disabled Wave-I stage did not load a valid cache.", call. = FALSE)
  main_cache_test <- df
  attr(main_cache_test, "global_missing_dictionary") <- preflight_dictionary
  attr(main_cache_test, "variable_source_registry") <- synthetic_registry
  attr(main_cache_test, "survey_design_frame") <- full_design
  attr(main_cache_test, "wave2_completion_status") <- data.frame(
    AID = main_cache_test[[cfg_pre$analysis$id_var]],
    wave2_cesd_complete = 1L, stringsAsFactors = FALSE)
  names(attr(main_cache_test, "wave2_completion_status"))[1L] <- cfg_pre$analysis$id_var
  save_main_dataset_cache(main_cache_test, main_cache_path, cfg_pre, w1_cache_test)
  main_loaded <- obtain_main_dataset_for_run(
    cfg_pre, w1_loaded, main_cache_path, allow_build = FALSE)
  if (!is.data.frame(main_loaded) || nrow(main_loaded) != nrow(main_cache_test) ||
      is.null(attr(main_loaded, "global_missing_dictionary")) ||
      is.null(attr(main_loaded, "variable_source_registry")) ||
      is.null(attr(main_loaded, "survey_design_frame")))
    stop("Preflight FAILED: disabled main-dataset stage did not load a valid cache.", call. = FALSE)
  stale_cfg <- cfg_pre
  stale_cfg$safety$stop_on_stale_wave1_cache <- TRUE
  stale_cfg$analysis$extra_exclude_from_candidates <- "PRETEND_NEW_EXCLUSION"
  stale_caught <- tryCatch({
    obtain_wave1_for_run(stale_cfg, wave1_cache_path, allow_build = FALSE)
    FALSE
  }, error = function(e) TRUE)
  if (!stale_caught)
    stop("Preflight FAILED: disabled Wave-I stage accepted a stale cache fingerprint.", call. = FALSE)
  
  # Exercise every learner enabled in the primary production library, using
  # smaller preflight iteration counts so the test remains practical. Optional
  # sensitivity-only learners cannot gate the primary run.
  cfg_pre$learners$ranger$num.trees <- 50L
  cfg_pre$learners$xgboost$ntrees <- 25L
  cfg_pre$learners$glmnet$nlambda <- 30L
  cfg_pre$learners$glmnet$internal_folds <- 3L
  
  # Explicit H4EC2/H4EC3 construction test: zero stays zero; the valid maximum
  # remains valid; refusal/DK exact values fall back only to valid brackets.
  fam <- cfg_pre$outcome$families$Compensation
  ec <- compute_earnings(
    c(0, 999995, 9999996, 9999998),
    c(97, 97, 1, 12), fam)
  if (!isTRUE(all.equal(ec$earnings, c(0, 999995, 2500, 175000), tolerance=0)))
    stop("Preflight FAILED: explicit compensation construction is incorrect.", call. = FALSE)
  if (!identical(ec$source, c("exact", "exact", "bracket_midpoint", "bracket_midpoint")))
    stop("Preflight FAILED: compensation source classification is incorrect.", call. = FALSE)
  
  # Exact codebook rules, valid look-alikes, suffix-safe joins and real skips.
  check_codes <- function(nm, values, general, skip = integer(0)) {
    rule <- learn_conservative_missing_rule(values, cfg_pre$preprocessing, nm)
    masks <- missing_masks_from_rule(values, rule)
    if (!identical(which(masks$general), as.integer(general)) ||
        !identical(which(masks$skip), as.integer(skip)))
      stop("Preflight FAILED: exact codebook rule for ", nm, call. = FALSE)
  }
  check_codes("C1LE004.x", c(8, 9, 98, NA), c(3, 4))
  check_codes("C1EC003M", c(-1.3, 1.14, 2, 8), c(3, 4))
  check_codes("C1EC002", c(33, 55.7, 92, 98), c(3, 4))
  check_codes("S1CT001", c(.05, .55, 8), 3)
  check_codes("STC9506", c(200, 4000, 99998, 999998), 4)
  check_codes("STC9509", c(3300, 6700, 99998), 3)
  check_codes("STC9523", c(4, 6, 7, 8, 98), 5)
  check_codes("W1PCC290", c(6, 7, 8, 9, 10, 98), 6)
  check_codes("W1PCS192", c(-.2, .4, 7, 8), 4, 3)
  check_codes("RIWA001", c(1, 6, 7, 8, 98), 5)
  check_codes("RIWA002", c(1, 8, 13, 995, 996, 998), 4:6)
  check_codes("RPDA003", c(0, 1, 96, 98), 3:4)
  check_codes("H1GH51", c(6, 8, 9, 20, 96, 98, 99), 5:7)
  check_codes("H1TO5", c(0, 6, 7, 8, 9, 30, 96, 97, 98, 99), c(7, 9, 10), 8)
  check_codes("H1HR10A", c(0, 8, 19, 96, 97, 98, 99), c(4, 6, 7), 5)
  check_codes("H1TO7", c(8, 96, 97, 98, 99), integer(0), 3)
  check_codes("H1EE7", c(8, 996, 997, 998, 999), integer(0), 3)
  check_codes("H1IR8A", c(0, 6, 7, 8), integer(0), 3)
  check_codes("H1IR13", c(6, 7, 8, 97, 99), 5, 4)
  check_codes("RIWA002", c(NA_real_, NA_real_), 1:2)
  
  # Explicit no qualifying job -> zero; unresolved work/job-count -> missing.
  hours_routes <- data.frame(
    H4LM6 = c(NA, NA, NA, 1, NA), H4LM11 = c(0, NA, 1, 7, 1),
    H4LM12 = c(97, NA, 1, 98, NA), H4LM13 = c(997, NA, 997, 45, NA),
    H4LM19 = c(997, NA, 40, 997, 40))
  hours_check <- construct_outcome_hours_worked(
    hours_routes, 4L, cfg_pre$outcome$families$HoursWorked, cfg_pre$outcome, NULL)
  if (!identical(hours_check$Y, c(0, NA_real_, 40, 45, NA_real_)) ||
      as.numeric(hours_check$audit$value[
        hours_check$audit$metric == "n_route_missing"]) != 2)
    stop("Preflight FAILED: hours missingness/route classification.", call. = FALSE)
  
  # Source-informed exact-code classifier tests, grounded in Wave I
  # codebook patterns: H1FS-type 0-3 scales use 6/8/9; items with substantive
  # category 6 and observed 96/98 use the high family; long categorical items
  # such as H1HR5A/PB7 use 97; numeric counts/earnings use wider 996-999 codes.
  qx <- c(rep(0:3, each = 20), 6, 7, 8, 9, NA)
  qr <- learn_conservative_missing_rule(qx, cfg_pre$preprocessing, "H1FS1")
  qm <- missing_masks_from_rule(qx, qr)
  if (!setequal(qr$general_codes, c(6, 8, 9)) ||
      !setequal(qr$skip_codes, 7) || sum(qm$general) != 4L || sum(qm$skip) != 1L)
    stop("Preflight FAILED: low questionnaire exact-code classification is incorrect.", call. = FALSE)
  
  # H1PR3-like support: category 6 is substantive because 96/98 identify the
  # high scheme. Exact 6 must survive.
  qhigh <- c(rep(1:6, each = 20), 96, 97, 98, 99)
  qhr <- learn_conservative_missing_rule(qhigh, cfg_pre$preprocessing, "H1PR3")
  qhm <- missing_masks_from_rule(qhigh, qhr)
  if (!setequal(qhr$general_codes, c(96, 98, 99)) ||
      !setequal(qhr$skip_codes, 97) || any(qhm$general[qhigh == 6]) || any(qhm$skip[qhigh == 6]))
    stop("Preflight FAILED: valid category 6 versus high 96-99 scheme is incorrect.", call. = FALSE)
  
  # A substantive category 6 without observed 7/8/9 uses the high family;
  # exact 6 must not be reclassified as low-scheme refusal.
  q6_only <- rep(1:6, each = 20)
  q6r <- learn_conservative_missing_rule(q6_only, cfg_pre$preprocessing, "H1Q6")
  q6m <- missing_masks_from_rule(q6_only, q6r)
  if (any(q6m$general[q6_only == 6]) || any(q6m$skip[q6_only == 6]) ||
      !all(c(96, 98, 99) %in% q6r$general_codes) || !(97 %in% q6r$skip_codes))
    stop("Preflight FAILED: substantive category 6 was not protected by the high-code convention.", call. = FALSE)
  
  # Common skips must remain recognized; prevalence is not a disqualifier.
  common_skip <- c(rep(1:5, each = 10), rep(7, 400), 6, 8, 9)
  csr <- learn_conservative_missing_rule(common_skip, cfg_pre$preprocessing, "H1TEST")
  csm <- missing_masks_from_rule(common_skip, csr)
  if (mean(csm$skip) < 0.80 || !setequal(csr$skip_codes, 7))
    stop("Preflight FAILED: common questionnaire skip was rejected by prevalence.", call. = FALSE)
  
  # Learned family must transfer to validation without suffix matching.
  qtrain <- c(rep(1:5, each = 20), 96, 98, 99)
  qtrain_rule <- learn_conservative_missing_rule(qtrain, cfg_pre$preprocessing, "H1TEST2")
  cfg_pre$preprocessing$global_missing_dictionary$H1TEST2 <- qtrain_rule
  qvalid_masks <- missing_masks_from_rule(c(97, 196), qtrain_rule)
  if (!qvalid_masks$skip[1L] || qvalid_masks$general[2L] || qvalid_masks$skip[2L])
    stop("Preflight FAILED: learned exact-code family was not applied consistently to validation values.", call. = FALSE)
  
  qtrain_factor <- c(qtrain, rep(97,3L))
  
  frec <- learn_final_missing_recipe(data.frame(H1TEST2 = factor(qtrain_factor)), cfg_pre$preprocessing)
  fapp <- apply_final_missing_recipe(
    data.frame(H1TEST2 = factor(c(97, 1), levels = c(1:5, 97))),
    frec, cfg_pre$preprocessing)
  if (any(c("H1TEST2_missA", "H1TEST2_miss97") %in% names(fapp)) ||
      as.character(fapp$H1TEST2[1L]) != (cfg_pre$preprocessing$factor_skip_label %||% "Skip") ||
      as.character(fapp$H1TEST2[2L]) == (cfg_pre$preprocessing$factor_missing_label %||% "Missing"))
    stop("Preflight FAILED: factor recipe did not preserve Skip as a level without duplicate indicators.", call. = FALSE)
  
  # Fully nonnumeric categorical fields are valid native-missing-only factors;
  # removing blanket numeric coercion must not make dictionary construction fail.
  text_x <- c("urban", "rural", NA, "suburban", "urban")
  text_rule <- learn_conservative_missing_rule(text_x, cfg_pre$preprocessing, "CSTAREA")
  text_masks <- missing_masks_from_rule(text_x, text_rule)
  blank_text <- factor(c("North", "", "   ", NA, "South"))
  blank_rule <- learn_conservative_missing_rule(blank_text, cfg_pre$preprocessing, "CSTBLANK")
  blank_masks <- missing_masks_from_rule(blank_text, blank_rule)
  if (!identical(which(blank_masks$general), c(2L, 3L, 4L)) || any(blank_masks$skip))
    stop("Preflight failed: blank/whitespace text was not preserved as native missingness.", call. = FALSE)
  collision_caught <- tryCatch({
    assert_no_missing_indicator_name_collisions(
      c("X", "X_missA"), "X", context = "preflight collision test")
    FALSE
  }, error = function(e) TRUE)
  if (!collision_caught)
    stop("Preflight failed: raw/derived missingness-indicator collision was not rejected.", call. = FALSE)
  arbitrary_binary_caught <- tryCatch({
    normalize_binary_var(factor(c("no", "yes")), "preflight binary labels")
    FALSE
  }, error = function(e) TRUE)
  if (!arbitrary_binary_caught)
    stop("Preflight failed: arbitrary factor-level ordering was accepted as binary 0/1 coding.", call. = FALSE)
  constant_indicator <- normalize_binary_var(rep(1L, 5L),
                                             "preflight constant observation indicator",
                                             require_both = FALSE)
  if (!identical(constant_indicator, rep(1L, 5L)))
    stop("Preflight failed: a valid constant 0/1 observation indicator was rejected.", call. = FALSE)
  constant_exposure_caught <- tryCatch({
    normalize_binary_var(rep(1L, 5L), "preflight constant exposure")
    FALSE
  }, error = function(e) TRUE)
  if (!constant_exposure_caught)
    stop("Preflight failed: an exposure lacking one treatment arm was accepted.", call. = FALSE)
  numeric_factor_outcome <- prepare_modeled_outcome(
    factor(c("0", "1", "2", NA), levels = c("0", "1", "2")),
    requested = "continuous", name = "preflight numeric-coded factor outcome")
  if (!identical(numeric_factor_outcome$values, c(0, 1, 2, NA_real_)))
    stop("Preflight failed: numeric-coded factor outcome was converted through internal factor indices.", call. = FALSE)
  reserved_factor_caught <- tryCatch({
    z_badlabel <- factor(c("A", "Missing", "B"))
    assert_reserved_factor_labels_safe(
      z_badlabel, rep(FALSE, length(z_badlabel)), rep(FALSE, length(z_badlabel)),
      c("Missing", "Skip", "_Other_"), "BADLABEL", "preflight reserved-label test")
    FALSE
  }, error = function(e) TRUE)
  if (!reserved_factor_caught)
    stop("Preflight failed: a substantive raw factor level collided with a generated reserved label.", call. = FALSE)
  missing_recipe_var_caught <- tryCatch({
    apply_final_missing_recipe(data.frame(other = 1:2), frec, cfg_pre$preprocessing)
    FALSE
  }, error = function(e) TRUE)
  if (!missing_recipe_var_caught)
    stop("Preflight failed: missing raw variables were silently replaced during final missingness preprocessing.", call. = FALSE)
  alias_ok <- canonicalize_join_key_alias(
    data.frame(ASCHLCDE = c(10, 11)), "PSUSCID", "ASCHLCDE", "preflight school key")
  if (!identical(as.integer(alias_ok$PSUSCID), c(10L, 11L)))
    stop("Preflight failed: school-cluster alias was not canonicalized.", call. = FALSE)
  alias_conflict_caught <- tryCatch({
    canonicalize_join_key_alias(
      data.frame(PSUSCID = c(10, 11), ASCHLCDE = c(10, 12)),
      "PSUSCID", "ASCHLCDE", "preflight school-key conflict")
    FALSE
  }, error = function(e) TRUE)
  if (!alias_conflict_caught)
    stop("Preflight failed: conflicting school-cluster aliases were not rejected.", call. = FALSE)
  if (isTRUE(text_rule$numeric_coded) || !isTRUE(text_rule$as_factor) ||
      sum(text_masks$general) != 1L || any(text_masks$skip))
    stop("Preflight FAILED: nonnumeric categorical native-missing handling is incorrect.", call. = FALSE)
  
  # Broad bounded measures and dense counts must retain 96-99/6-9 when they
  # are ordinary values rather than questionnaire codes.
  broad <- rep(0:99, 2)
  br <- learn_conservative_missing_rule(broad, cfg_pre$preprocessing, "A10")
  bm <- missing_masks_from_rule(broad, br)
  if (any(bm$general | bm$skip))
    stop("Preflight FAILED: valid broad 0-99 percentage values were misclassified as missing.", call. = FALSE)
  
  count_0_9 <- rep(0:9, each = 20)
  c09r <- learn_conservative_missing_rule(count_0_9, cfg_pre$preprocessing, "CSTCOUNT")
  c09m <- missing_masks_from_rule(count_0_9, c09r)
  if (any(c09m$general | c09m$skip))
    stop("Preflight FAILED: dense substantive 0-9 count was misclassified.", call. = FALSE)
  
  cont <- c(seq(0, 25000, by = 250), 196, 1297, 22198)
  cr <- learn_conservative_missing_rule(cont, cfg_pre$preprocessing, "CST90550")
  cm <- missing_masks_from_rule(cont, cr)
  if (any(cm$general | cm$skip))
    stop("Preflight FAILED: contextual/continuous values were misclassified by suffix.", call. = FALSE)
  
  # Wider families remain available. Documented ambiguous response values are
  # preserved, while separately identifiable legitimate skips remain skips.
  earnings_item <- c(0:900, rep(997, 2000), 996, 998, 999)
  er <- learn_conservative_missing_rule(earnings_item, cfg_pre$preprocessing, "H1MONEYTEST")
  em <- missing_masks_from_rule(earnings_item, er)
  if (!all(c(996, 998, 999) %in% er$general_codes) || !(997 %in% er$skip_codes) ||
      any(em$general[earnings_item == 196]) || any(em$skip[earnings_item == 197]))
    stop("Preflight FAILED: 996-999 wider family or ordinary 196/197 handling is incorrect.", call. = FALSE)
  overlap_rule <- learn_conservative_missing_rule(earnings_item, cfg_pre$preprocessing, "H1EE7")
  overlap_masks <- missing_masks_from_rule(earnings_item, overlap_rule)
  if (length(overlap_rule$general_codes) ||
      !setequal(overlap_rule$skip_codes, 997) ||
      any(overlap_masks$general[earnings_item %in% c(996, 998, 999)]) ||
      any(overlap_masks$skip[earnings_item %in% c(996, 998, 999)]))
    stop("Preflight FAILED: ambiguous values or identifiable skips were misclassified.", call. = FALSE)
  
  hours_item <- c(rep(0:99, 2), 996, 998)
  hir <- learn_conservative_missing_rule(hours_item, cfg_pre$preprocessing, "H1DA10")
  him <- missing_masks_from_rule(hours_item, hir)
  if (any(him$general[hours_item %in% 96:99]) ||
      !all(c(996, 998) %in% hir$general_codes))
    stop("Preflight FAILED: 0-99 numeric item versus 996-999 sentinel handling is incorrect.", call. = FALSE)
  
  # H1TO37-like long factor: ages 0-18 include valid 6-9, while the
  # field-width reserve block is 996-999. The wider family must take precedence
  # and must not activate the low 6-9 scheme merely because the variable is a
  # declared long factor.
  age_item <- c(rep(0:18, each = 4), 996, rep(997, 200), 998, 999)
  air <- learn_conservative_missing_rule(age_item, cfg_pre$preprocessing, "H1TO37")
  aim <- missing_masks_from_rule(age_item, air)
  if (any(aim$general[age_item %in% 6:9]) || any(aim$skip[age_item %in% 6:9]) ||
      !all(c(996, 998, 999) %in% air$general_codes) || !(997 %in% air$skip_codes) ||
      any(c(6, 7, 8, 9) %in% c(air$general_codes, air$skip_codes)))
    stop("Preflight FAILED: wider reserve family did not protect substantive 6-9 values.", call. = FALSE)
  
  # A variable containing only a documented wider special-code family is
  # still classified correctly even when no ordinary response is observed.
  wide_only <- c(9996, 9997, 9998, 9999)
  wor <- learn_conservative_missing_rule(wide_only, cfg_pre$preprocessing, "H1WIDEONLY")
  if (!all(c(9996, 9998, 9999) %in% wor$general_codes) || !(9997 %in% wor$skip_codes))
    stop("Preflight FAILED: all-special wider family was not recognized.", call. = FALSE)
  
  # If both a lower-family value (such as substantive 996) and a wider reserve
  # block (9996-9999) appear, select only the widest supported family.
  width_hierarchy <- c(0:999, 9996, 9997, 9998, 9999)
  whr <- learn_conservative_missing_rule(width_hierarchy, cfg_pre$preprocessing, "H1WIDTH")
  whm <- missing_masks_from_rule(width_hierarchy, whr)
  if (any(whm$general[width_hierarchy %in% 996:999]) ||
      any(whm$skip[width_hierarchy %in% 996:999]) ||
      !all(c(9996, 9998, 9999) %in% whr$general_codes) || !(9997 %in% whr$skip_codes))
    stop("Preflight FAILED: widest exact field-width family was not selected.", call. = FALSE)
  
  # Seven-digit exact earnings-style sentinels are supported for questionnaire
  # numeric sources, even when frequent; ordinary values below the sentinel block survive.
  wide7 <- c(0, 1000, 999995, rep(9999996, 100), 9999997, 9999998, 9999999)
  w7r <- learn_conservative_missing_rule(wide7, cfg_pre$preprocessing, "H1MONEY")
  if (!all(c(9999996, 9999998, 9999999) %in% w7r$general_codes) ||
      !(9999997 %in% w7r$skip_codes) ||
      999995 %in% c(w7r$general_codes, w7r$skip_codes) ||
      any(c(96, 97, 98, 99) %in% c(w7r$general_codes, w7r$skip_codes)))
    stop("Preflight FAILED: seven-digit exact sentinel handling is incorrect.", call. = FALSE)
  
  # Existing long-factor declarations still force factorization. COMMID remains
  # a factor but is explicitly excluded from questionnaire missing-code logic.
  lf <- data.frame(PB7 = c(1:28, 96, 97), COMMID.x = c(1:28, 96, 97))
  cfg_lf <- cfg_pre$preprocessing
  cfg_lf$global_missing_dictionary <- build_global_missing_code_dictionary(
    lf, names(lf), cfg_lf)
  lft <- classify_factors_by_uniques(lf, cfg_lf)
  if (!is.factor(lft$PB7) || !is.factor(lft$COMMID.x))
    stop("Preflight FAILED: long-factor declarations were not preserved.", call. = FALSE)
  pbr <- learn_conservative_missing_rule(lf$PB7, cfg_pre$preprocessing, "PB7")
  cmr <- learn_conservative_missing_rule(lf$COMMID.x, cfg_pre$preprocessing, "COMMID.x")
  if (!(97 %in% pbr$skip_codes) || length(cmr$general_codes) || length(cmr$skip_codes))
    stop("Preflight FAILED: long-factor questionnaire/identifier distinction is incorrect.", call. = FALSE)
  
  ntrain <- data.frame(H1MONEY = c(rep(0:100, 3), 9996))
  cfg_money <- cfg_pre$preprocessing
  cfg_money$global_missing_dictionary <- build_global_missing_code_dictionary(
    ntrain, names(ntrain), cfg_money)
  nrec <- learn_final_missing_recipe(ntrain, cfg_money)
  napp <- apply_final_missing_recipe(data.frame(H1MONEY = c(9998, 22198, 50)),
                                     nrec, cfg_money)
  if (napp$H1MONEY_missA[1L] != 1L || napp$H1MONEY_missA[2L] != 0L || napp$H1MONEY[2L] != 22198)
    stop("Preflight FAILED: numeric recipe did not preserve ordinary values while applying an exact sentinel family.", call. = FALSE)
  
  # Rare binary missingness indicators must remain exact 0/1 columns and must
  # never be winsorized to constants.
  rare_indicator <- c(rep(0, 399), 1)
  rare_support <- list(n_obs = length(rare_indicator), p_obs = 1)
  rare_prep <- prep_numeric_train(
    rare_indicator, cfg_pre$final_preprocess, cfg_pre$preprocessing,
    support = rare_support, variable_name = "H1TEST_missA")
  rare_out <- apply_numeric_transform(rare_indicator, rare_prep)
  if (!identical(as.numeric(rare_out[, 1L]), as.numeric(rare_indicator)) ||
      length(unique(rare_out[, 1L])) != 2L)
    stop("Preflight FAILED: rare numeric missingness indicator was transformed or erased.", call. = FALSE)
  
  # Join tests: the master person key must be complete and unique; missing keys
  # are allowed only on supplemental/right tables and must never match.
  jx <- data.frame(AID = c(1, 2, 3), left = 1:3)
  jy <- data.frame(AID = c(1, 2, NA, NA), right = c(10, 20, 30, 40))
  jo <- left_join_unique(jx, jy, "AID", "preflight_left", "preflight_right")
  if (nrow(jo) != 3L || !is.na(jo$right[3L]) || !identical(jo$right[c(1L,2L)], c(10,20)))
    stop("Preflight FAILED: one-to-one join matched or multiplied missing supplemental keys.", call. = FALSE)
  
  missing_master <- try(
    left_join_unique(data.frame(AID = c(1, NA, 2)), jy, "AID",
                     "preflight_missing_master", "preflight_right"), silent = TRUE)
  if (!inherits(missing_master, "try-error"))
    stop("Preflight FAILED: a missing master AID was not rejected.", call. = FALSE)
  
  malformed_key <- try(
    coerce_join_key(data.frame(AID = c("1", "bad", "2")), "AID", "preflight_malformed"),
    silent = TRUE)
  if (!inherits(malformed_key, "try-error"))
    stop("Preflight FAILED: malformed nonmissing join keys were silently converted to NA.", call. = FALSE)
  
  mx <- data.frame(AID = 1:4, PSUSCID = c(10, NA, 10, 20))
  my <- data.frame(PSUSCID = c(10, 20, NA, NA), school = c("a", "b", "x", "y"))
  mo <- left_join_many_to_one(mx, my, "PSUSCID", "preflight_people", "preflight_school", row_id = "AID")
  if (nrow(mo) != 4L || !is.na(mo$school[2L]) ||
      !identical(as.character(mo$school[c(1L,3L,4L)]), c("a","a","b")))
    stop("Preflight FAILED: many-to-one join mishandled missing school keys.", call. = FALSE)
  
  dup_check <- try(assert_unique_key(data.frame(AID = c(1, 1, NA)), "AID", "preflight_duplicate"),
                   silent = TRUE)
  if (!inherits(dup_check, "try-error"))
    stop("Preflight FAILED: duplicated nonmissing join keys were not rejected.", call. = FALSE)
  
  collision_test <- data.frame(
    SAME.x = c(1, NA, 3), SAME.y = c(1, 2, 3),
    CONFLICT.x = c(1, 2, 3), CONFLICT.y = c(1, 9, 3),
    NOOVERLAP.x = c(1, NA, NA), NOOVERLAP.y = c(NA, 2, 3),
    ORPHAN.y = c(4, 5, 6), check.names = FALSE)
  # Global dictionary is frozen on the complete Wave I-like data and reused.
  dict_test_df <- data.frame(
    H1LOW = c(1:5, 6, 7, 8, 9),
    CSTCONT = c(196, 1297, 22198, 1, 2, 3, 4, 5, 10),
    stringsAsFactors = FALSE)
  dict_test <- build_global_missing_code_dictionary(
    dict_test_df, names(dict_test_df), cfg_pre$preprocessing)
  if (!identical(dict_test$H1LOW$general_codes, c(6, 8, 9)))
    stop("Preflight FAILED: H1LOW general missing-code family is incorrect.", call. = FALSE)
  if (!identical(dict_test$H1LOW$skip_codes, 7))
    stop("Preflight FAILED: H1LOW structural-skip code is incorrect.", call. = FALSE)
  if (length(dict_test$CSTCONT$general_codes) != 0L)
    stop("Preflight FAILED: contextual continuous values were assigned missing codes.", call. = FALSE)
  cfg_dict <- cfg_pre$preprocessing
  cfg_dict$global_missing_dictionary <- dict_test
  if (!identical(get_missing_rule(dict_test_df$H1LOW, cfg_dict, "H1LOW")$skip_codes, 7))
    stop("Preflight FAILED: frozen H1LOW skip rule was not reused.", call. = FALSE)
  subset_without_skip <- dict_test_df$H1LOW[dict_test_df$H1LOW != 7]
  frozen_subset_rule <- get_missing_rule(subset_without_skip, cfg_dict, "H1LOW")
  if (!identical(frozen_subset_rule$skip_codes, 7))
    stop("Preflight FAILED: a fold-specific subset changed the globally frozen missing-code rule.", call. = FALSE)
  
  preflight_step("weighted screening invariance and responsiveness", {
    # Use enough rows that every CV training subset is non-degenerate. Within
    # each x level both outcomes occur; relative weights, rather than perfect
    # prediction, create the association.
    n_screen_blocks <- 30L
    sx <- data.frame(x = rep(c(-1, -1, 1, 1), times = n_screen_blocks))
    sy <- rep(c(0, 1, 0, 1), times = n_screen_blocks)
    sw <- rep(c(20, 1, 1, 20), times = n_screen_blocks)
    sfold <- rep(rep(seq_len(3L), each = 4L), length.out = nrow(sx))
    
    s1 <- screen_binom_linear(sy, sx, K = 3L, fold = sfold, weights = sw)
    s2 <- screen_binom_linear(sy, sx, K = 3L, fold = sfold, weights = 100 * sw)
    s0 <- screen_binom_linear(sy, sx, K = 3L, fold = sfold,
                              weights = rep(1, length(sw)))
    if (any(!is.finite(c(s0, s1, s2))))
      stop("binary screening returned a nonfinite score.", call. = FALSE)
    if (!isTRUE(all.equal(s1, s2, tolerance = 1e-8)))
      stop("binary screening changed under common weight rescaling.", call. = FALSE)
    if (abs(as.numeric(s1 - s0)) <= 1e-6)
      stop("binary screening did not respond to relative weights.", call. = FALSE)
    
    # Use a non-perfect outcome pattern so the Gaussian screening
    # fixture can test responses to relative observation weights.
    gy <- as.numeric(sy)
    g1 <- screen_gauss_linear(gy, sx, K = 3L, fold = sfold, weights = sw)
    g2 <- screen_gauss_linear(gy, sx, K = 3L, fold = sfold, weights = 100 * sw)
    g0 <- screen_gauss_linear(gy, sx, K = 3L, fold = sfold,
                              weights = rep(1, length(sw)))
    if (any(!is.finite(c(g0, g1, g2))))
      stop("Gaussian screening returned a nonfinite score.", call. = FALSE)
    if (!isTRUE(all.equal(g1, g2, tolerance = 1e-8)))
      stop("Gaussian screening changed under common weight rescaling.", call. = FALSE)
    if (abs(as.numeric(g1 - g0)) <= 1e-6)
      stop("Gaussian screening did not respond to relative weights.", call. = FALSE)
  })
  
  dup_df <- data.frame(H1DUPA = c(1, 2, 6, 8), CSTDUPB = c(1, 2, 6, 8))
  dup_cfg <- cfg_pre$preprocessing
  dup_cfg$global_missing_dictionary <- build_global_missing_code_dictionary(
    dup_df, names(dup_df), dup_cfg)
  # Raw-identical values can have different source-informed meanings: H1DUPA
  # uses the low questionnaire family, whereas CSTDUPB retains 6/8 as ordinary.
  # Exact-duplicate removal must compare post-semantic representations.
  sig_a <- compact_var_signature_for_duplicate(
    dup_df$H1DUPA, get_missing_rule(dup_df$H1DUPA, dup_cfg, "H1DUPA"))
  sig_b <- compact_var_signature_for_duplicate(
    dup_df$CSTDUPB, get_missing_rule(dup_df$CSTDUPB, dup_cfg, "CSTDUPB"))
  if (identical(sig_a, sig_b))
    stop("Preflight FAILED: semantic duplicate signatures collapsed distinct variables.", call. = FALSE)
  
  collision_out <- resolve_join_suffix_collisions(collision_test)
  if ("SAME.y" %in% names(collision_out$data) ||
      !identical(as.numeric(collision_out$data$SAME.x), c(1,2,3)) ||
      !all(c("CONFLICT.x", "CONFLICT.y", "NOOVERLAP.x", "NOOVERLAP.y", "ORPHAN.y") %in% names(collision_out$data)))
    stop("Preflight FAILED: join-suffix collision resolver discarded or mis-coalesced data.", call. = FALSE)
  
  bad_att_cfg <- cfg_pre; bad_att_cfg$final_tmle$att_estimator <- "onestep"
  if (!inherits(try(validate_cfg(bad_att_cfg), silent = TRUE), "try-error"))
    stop("Preflight FAILED: unsupported one-step ATT headline was not rejected.", call. = FALSE)
  bad_outcome_cfg <- cfg_pre
  bad_outcome_cfg$outcome$family <- "UsualHours"
  bad_outcome_cfg$outcome$waves <- 4L
  if (!inherits(try(validate_cfg(bad_outcome_cfg), silent = TRUE), "try-error"))
    stop("Preflight FAILED: unverified outcome constructor was not blocked.", call. = FALSE)
  pass_cfg <- cfg_pre
  pass_cfg$outcome$family <- "PassThrough"
  pass_cfg$outcome$waves <- 4L
  pass_cfg$outcome$families$PassThrough <- list(type = "continuous", source_var = "W1")
  if (inherits(try(validate_cfg(pass_cfg), silent = TRUE), "try-error"))
    stop("Preflight FAILED: explicitly configured PassThrough outcome was incorrectly blocked.", call. = FALSE)
  pt_ok <- construct_outcome_pass_through(
    data.frame(W1 = c("1", "2", NA_character_)), 4L,
    list(source_var = "W1"), list(), NULL)
  if (!identical(pt_ok, c(1, 2, NA_real_)))
    stop("Preflight FAILED: numeric PassThrough values were not preserved.", call. = FALSE)
  pt_bad <- try(construct_outcome_pass_through(
    data.frame(W1 = c("1", "not_numeric")), 4L,
    list(source_var = "W1"), list(), NULL), silent = TRUE)
  if (!inherits(pt_bad, "try-error"))
    stop("Preflight FAILED: nonnumeric PassThrough values were not rejected.", call. = FALSE)
  
  # Fast g/pi clip-sweep unit test: all scenarios must re-target and
  # return finite ATT/SE/percentage values without refitting nuisance models.
  ncs <- 240L
  set.seed(901)
  Acs <- rbinom(ncs, 1, 0.25)
  dcs <- rbinom(ncs, 1, 0.85)
  ycs <- pmin(pmax(0.25 - 0.04 * Acs + rnorm(ncs, 0, 0.08), 0), 1)
  q1cs <- rep(0.23, ncs); q0cs <- rep(0.27, ncs)
  qawcs <- ifelse(Acs == 1L, q1cs, q0cs)
  grawcs <- pmin(pmax(0.25 + rnorm(ncs, 0, 0.12), 0.001), 0.999)
  pirawcs <- pmin(pmax(0.85 + rnorm(ncs, 0, 0.08), 0.001), 0.999)
  cs_cluster <- rep(seq_len(40L), each = 6L)
  cs_region_by_cluster <- setNames(rep(1:4, length.out = 40L), as.character(seq_len(40L)))
  cs_ids <- seq_len(ncs)
  cs_design <- data.frame(
    AID = cs_ids, PSUSCID = cs_cluster,
    REGION = as.integer(cs_region_by_cluster[as.character(cs_cluster)]),
    GSWGT1 = rep(1, ncs), .analysis_domain = TRUE,
    check.names = FALSE)
  fake_fit <- list(
    Qbar1W = q1cs, Qbar0W = q0cs, QbarAW = qawcs,
    gn_raw = grawcs, pi_AW_raw = pirawcs, pi_1W_raw = pirawcs, pi_0W_raw = pirawcs,
    weights = rep(1, ncs), respondent_ids = cs_ids,
    survey_design_frame = cs_design, y_lower = 0, y_range = 1,
    att_components = list(A = Acs, delta_Y = dcs, Y_bounded_orig = ycs,
                          weights = rep(1, ncs), cluster = cs_cluster,
                          strata = cs_design$REGION,
                          # Distinct uncalibrated g predictions exercise the calibration
                          # comparison branch and remain aligned with the synthetic rows.


                          gn_uncalibrated = pmin(pmax(
                            grawcs + 0.01, 0.001), 0.999)))
  # Synthetic alternate-pi predictions and structural zeros exercise
  # the retargeting branches; alternate pi differs by exposure arm.

  fake_fit$pi_1W_alt_raw <- pmin(pmax(pirawcs + 0.02, 0.001), 0.999)
  fake_fit$pi_0W_alt_raw <- pmin(pmax(pirawcs - 0.02, 0.001), 0.999)
  fake_fit$pi_AW_alt_raw <- ifelse(Acs == 1L, fake_fit$pi_1W_alt_raw,
                                   fake_fit$pi_0W_alt_raw)
  fake_fit$pi_alt_status <- rep("fitted", 5L)
  fake_fit$structural_zero <- (seq_len(ncs) %% 40L == 0L) & dcs == 1L
  fake_fit$structural_zero_source <- "preflight_fixture"
  fake_fit$att_components$Y_bounded_orig[fake_fit$structural_zero] <- 0
  # Seed the synthetic headline with the same configured-bound calculation so
  # build_att_g_pi_clip_sensitivity can exercise its exact reconciliation gate.
  fake_primary <- estimate_joint_att_under_clips(
    fake_fit, cfg_pre,
    cfg_pre$final_tmle$g_lower, cfg_pre$final_tmle$g_upper,
    cfg_pre$final_tmle$pi_lower, cfg_pre$final_tmle$pi_upper,
    "preflight_configured_primary")
  fake_fit$att_components$psi_att <- fake_primary$att_estimate
  fake_fit$att_components$att_tmle_estimate <- fake_primary$att_estimate
  cst <- build_att_g_pi_clip_sensitivity(fake_fit, cfg_pre)
  
  # Expected rows are derived from the SAME configuration the sweep reads, so
  # changing a floor list cannot silently desynchronise this check.
  .sym_floors <- sort(unique(as.numeric(
    cfg_pre$diagnostics$att_g_pi_clip_sensitivity_floors %||% c(0.01, 0.025, 0.05))))
  .g_floors <- as.numeric(cfg_pre$diagnostics$att_g_only_clip_floors %||% numeric(0))
  .g_floors <- .g_floors[is.finite(.g_floors) & .g_floors > 0 & .g_floors < 0.5]
  .n_obs_cs <- sum(dcs == 1L)
  if (isTRUE(cfg_pre$diagnostics$att_g_clip_include_sample_size_bound %||% TRUE)) {
    .cc <- as.numeric(cfg_pre$diagnostics$att_g_clip_sample_size_constant %||% 5)
    .ssb <- .cc / (sqrt(.n_obs_cs) * log(.n_obs_cs))
    if (is.finite(.ssb) && .ssb > 0 && .ssb < 0.5) .g_floors <- c(.g_floors, .ssb)
  }
  .g_floors <- sort(unique(.g_floors))
  expected_clip_rows <-
    as.integer(isTRUE(cfg_pre$diagnostics$att_g_pi_clip_include_configured %||% TRUE)) +
    as.integer(isTRUE(cfg_pre$analysis$calibrate_g)) +
    length(.sym_floors) + length(.g_floors) +
    1L +   # alternate-pi row (fixture always supplies alternate predictions)
    0L # no factual structural-zero pi override
  
  if (nrow(cst) != expected_clip_rows ||
      any(!is.finite(cst$att_estimate)) ||
      any(!is.finite(cst$att_se)) ||
      any(!is.finite(cst$prevention_gain_pct)) ||
      any(!cst$prevention_gain_defined))
    stop(sprintf(paste0("Preflight FAILED: identity-scale g/pi clip sensitivity sweep is invalid ",
                        "(rows=%d, expected=%d)."), nrow(cst), expected_clip_rows), call. = FALSE)
  # Require the named sensitivity rows, not merely the expected row count.
  if (isTRUE(cfg_pre$analysis$calibrate_g) &&
      !"uncalibrated_g_primary_bounds" %in% cst$scenario)
    stop("Preflight FAILED: the uncalibrated-g comparison row is missing.", call. = FALSE)
  if (!any(grepl("^g_only_", cst$scenario)))
    stop("Preflight FAILED: the g-only clipping rows are missing.", call. = FALSE)
  if (isTRUE(cfg_pre$diagnostics$att_g_clip_include_sample_size_bound %||% TRUE) &&
      !any(grepl("^g_only_sample_size_bound_", cst$scenario)))
    stop("Preflight FAILED: the sample-size-based g bound row is missing.", call. = FALSE)
  if (!"alt_pi_A_unpenalized_primary_bounds" %in% cst$scenario)
    stop("Preflight FAILED: the alternate-pi retargeted row is missing.", call. = FALSE)
  # g-only rows must leave the pi bounds at their configured values.
  .gonly <- cst[grepl("^g_only_", cst$scenario), , drop = FALSE]
  if (nrow(.gonly) &&
      (any(.gonly$pi_lower != cfg_pre$final_tmle$pi_lower) ||
       any(.gonly$pi_upper != cfg_pre$final_tmle$pi_upper)))
    stop("Preflight FAILED: a g-only clipping row moved the pi bounds.", call. = FALSE)
  # The sample-size bound must match the published formula on observed outcomes.
  .ssrow <- cst[grepl("^g_only_sample_size_bound_", cst$scenario), , drop = FALSE]
  if (nrow(.ssrow) == 1L) {
    .want <- as.numeric(cfg_pre$diagnostics$att_g_clip_sample_size_constant %||% 5) /
      (sqrt(.n_obs_cs) * log(.n_obs_cs))
    if (!isTRUE(all.equal(.ssrow$g_lower, .want, tolerance = 1e-12)) ||
        !isTRUE(all.equal(unique(cst$g_clip_sample_size_bound), .want, tolerance = 1e-12)) ||
        !identical(as.integer(unique(cst$g_clip_sample_size_bound_n_observed_outcomes)),
                   as.integer(.n_obs_cs)))
      stop("Preflight FAILED: the sample-size g bound or its recorded sample count is wrong.",
           call. = FALSE)
  }
  for (tr in c("log1p", "asinh")) {
    cfg_tr <- cfg_pre
    cfg_tr$outcome$compensation_transform <- tr
    z_tr <- estimate_joint_att_under_clips(
      fake_fit, cfg_tr,
      cfg_tr$final_tmle$g_lower, cfg_tr$final_tmle$g_upper,
      cfg_tr$final_tmle$pi_lower, cfg_tr$final_tmle$pi_upper,
      paste0("preflight_", tr))
    if (isTRUE(z_tr$prevention_gain_defined) ||
        any(is.finite(unlist(z_tr[c("prevention_gain_pct", "prevention_gain_se",
                                    "prevention_gain_ci_lower", "prevention_gain_ci_upper",
                                    "prevention_gain_p")]))))
      stop("Preflight FAILED: transformed Compensation clip analysis emitted an arithmetic-dollar prevention gain.",
           call. = FALSE)
  }
  
  set.seed(1L)
  
  # The structurally identical preflight configuration must itself satisfy
  # every production validation rule before any learner is fit. Freeze the
  # source/runtime provenance exactly as a real run does so output metadata and
  # the fitted result exercise the frozen-provenance path rather than a shortcut.
  cfg_pre <- ensure_run_id(cfg_pre)
  cfg_pre <- freeze_run_provenance(cfg_pre)
  validate_cfg(cfg_pre)
  load_required_packages(cfg_pre)
  
  # The primary cap is the pooled GSWGT1-weighted observed-outcome configured quantile using
  # with the configured HF8 rule. Re-scaling all survey weights must not change it.
  cap_a <- compute_continuous_cap(
    df$Y, df$GSWGT1, cfg_pre$outcome$continuous_upper_quantile, cfg_pre)
  cap_b <- compute_continuous_cap(
    df$Y, 17 * df$GSWGT1, cfg_pre$outcome$continuous_upper_quantile, cfg_pre)
  if (!is.finite(cap_a) || !isTRUE(all.equal(cap_a, cap_b, tolerance = 1e-10)))
    stop("Preflight FAILED: weighted outcome cap is invalid or changes under common weight scaling.", call. = FALSE)
  
  # Whole-school fold assignment must preserve clusters and place every active
  # A-by-delta cell in every validation fold and its training complement.
  fold_test <- do.call(make_cluster_folds_balanced, c(list(
    cluster = df$PSUSCID, A = df$Depressed, delta = df$delta_Y,
    k = 3L, seed = 20260402L, weights = df$GSWGT1,
    balance_on_weights = isTRUE(cfg_pre$final_tmle$internal_fold_balance_on_weights)),
    fold_control_from_cfg(cfg_pre, "internal")))
  if (any(vapply(split(fold_test, df$PSUSCID), function(z) length(unique(z)) != 1L, logical(1))))
    stop("Preflight FAILED: an internal fold split a school cluster.", call. = FALSE)
  fold_test_diag <- attr(fold_test, "fold_diagnostics")
  if (is.null(fold_test_diag) ||
      fold_test_diag$size_ratio > cfg_pre$final_tmle$fold_internal_max_size_ratio + 1e-12 ||
      fold_test_diag$size_deviation_prop > cfg_pre$final_tmle$fold_internal_max_size_deviation_prop + 1e-12)
    stop("Preflight FAILED: whole-school folds violate configured size-balance limits.", call. = FALSE)
  for (kk in sort(unique(fold_test))) {
    for (idx in list(which(fold_test == kk), which(fold_test != kk))) {
      tab <- table(factor(df$Depressed[idx], levels = 0:1),
                   factor(df$delta_Y[idx], levels = 0:1))
      active <- table(factor(df$Depressed, levels = 0:1),
                      factor(df$delta_Y, levels = 0:1)) > 0
      if (any(tab[active] < 1L))
        stop("Preflight FAILED: a fold or its training complement lacks an active A-by-delta cell.", call. = FALSE)
    }
  }
  
  # REGION-stratified with-replacement survey inference must be finite and the
  # PSU-only sensitivity must remain available as a separately labeled result.
  inf_smoke <- cluster_inference_from_eif(
    D = stats::rnorm(n), weights = df$GSWGT1, cluster = df$PSUSCID,
    estimate = 0, strata = df$REGION,
    design_frame = attr(df, "survey_design_frame"), domain_ids = df$AID,
    id_var = "AID", cluster_var = "PSUSCID", strata_var = "REGION",
    weight_var = "GSWGT1")
  needed_inf <- c("se", "p", "df", "se_cluster_only", "p_cluster_only")
  if (any(!vapply(needed_inf, function(nm) is.finite(inf_smoke[[nm]]), logical(1))) ||
      length(inf_smoke$ci) != 2L || any(!is.finite(inf_smoke$ci)) ||
      length(inf_smoke$ci_cluster_only) != 2L || any(!is.finite(inf_smoke$ci_cluster_only)) ||
      inf_smoke$design_n != n + n_outside || inf_smoke$domain_n != n ||
      !grepl("domain", inf_smoke$method, ignore.case = TRUE))
    stop("Preflight FAILED: REGION-by-PSU survey-domain inference returned invalid results.", call. = FALSE)
  psu_mismatch_caught <- tryCatch({
    bad_cluster <- as.character(df$PSUSCID)
    bad_cluster[1L] <- paste0(bad_cluster[1L], "_wrong")
    cluster_inference_from_eif(
      D = stats::rnorm(n), weights = df$GSWGT1, cluster = bad_cluster,
      estimate = 0, strata = df$REGION,
      design_frame = attr(df, "survey_design_frame"), domain_ids = df$AID,
      id_var = "AID", cluster_var = "PSUSCID", strata_var = "REGION",
      weight_var = "GSWGT1")
    FALSE
  }, error = function(e) TRUE)
  if (!psu_mismatch_caught)
    stop("Preflight FAILED: analytic/full-design PSU mismatch was not rejected.", call. = FALSE)
  strata_mismatch_caught <- tryCatch({
    bad_strata <- as.character(df$REGION)
    bad_strata[1L] <- if (bad_strata[1L] == "1") "2" else "1"
    cluster_inference_from_eif(
      D = stats::rnorm(n), weights = df$GSWGT1, cluster = df$PSUSCID,
      estimate = 0, strata = bad_strata,
      design_frame = attr(df, "survey_design_frame"), domain_ids = df$AID,
      id_var = "AID", cluster_var = "PSUSCID", strata_var = "REGION",
      weight_var = "GSWGT1")
    FALSE
  }, error = function(e) TRUE)
  if (!strata_mismatch_caught)
    stop("Preflight FAILED: analytic/full-design stratum mismatch was not rejected.", call. = FALSE)
  
  preflight_step("register active primary-production learners", {
    register_custom_learners(cfg_pre)
    # Validate the requested learner library, including regularized-only
    # and richer-Q settings. Per-fit assertions reject fitting failures.

    for (target in c("Q", "g", "pi")) {
      requested <- build_sl_library(cfg_pre, target)
      missing <- requested[!vapply(requested, exists, logical(1),
                                   mode = "function", inherits = TRUE)]
      if (length(missing))
        stop("Unregistered ", target, " learner(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
  })
  
  preflight_step("v8.30 fixed-nuisance MNAR known-answer tests", {
    n_m <- 240L
    idx_m <- seq_len(n_m)
    cluster_m <- rep(seq_len(40L), each = 6L)
    stratum_m <- rep(rep(1:4, each = 10L), each = 6L)
    A_m <- rep(c(1L, 0L), length.out = n_m)
    pi_m <- seq(0.55, 0.92, length.out = n_m)
    delta_m <- as.integer(idx_m %% 5L != 0L)
    y_full_m <- 4 + 14 * pi_m + 0.4 * sin(idx_m / 9)
    y_obs_m <- ifelse(delta_m == 1L, y_full_m, NA_real_)
    w_m <- 0.8 + (idx_m %% 7L) / 10
    toy_ac <- list(
      A = A_m,
      weights = w_m,
      delta_Y = delta_m,
      pi_AW = pi_m,
      pi_1W = pi_m,
      pi_0W = pi_m,
      Y_bounded_orig = y_obs_m,
      Qbar1W_orig = rep(12, n_m),
      Qbar0W_orig = rep(10, n_m),
      psi_att = -2,
      att_se = 0.5,
      y_lower = 0,
      y_upper = 25,
      inference_df = 20,
      cluster = cluster_m,
      strata = stratum_m)
    cmp <- mnar_fixed_nuisance_components(toy_ac)
    zero <- mnar_net_shift_from_deltas(cmp, 0, 0)
    common <- mnar_net_shift_from_deltas(cmp, 3, 3)
    if (abs(unname(zero["net"])) > 1e-12 ||
        abs(unname(common["net"])) > 1e-12)
      stop("Zero/common MNAR shifts did not cancel under pi1=pi0.", call. = FALSE)
    toy_fit <- list(att_components = toy_ac)
    bd1 <- build_att_mnar_breakdown(toy_fit, cfg_pre)
    bd2 <- build_att_mnar_breakdown(toy_fit, cfg_pre)
    if (!identical(bd1, bd2) || !nrow(bd1))
      stop("MNAR breakdown is not deterministic.", call. = FALSE)
    ex <- build_att_manski_bounds(toy_fit, cfg_pre)
    if (!nrow(ex) || ex$att_extreme_mean_lower > ex$att_extreme_mean_upper)
      stop("Extreme-mean sensitivity bounds are invalid.", call. = FALSE)
    cfg_cal <- cfg_pre
    cfg_cal$diagnostics$mnar_calibration_boot_reps <- 100L
    cfg_cal$diagnostics$mnar_calibration_min_valid_boot_reps <- 50L
    cal1 <- build_att_mnar_calibrated(toy_fit, cfg_cal)
    cal2 <- build_att_mnar_calibrated(toy_fit, cfg_cal)
    if (!identical(cal1, cal2) || !nrow(cal1) ||
        any(cal1$measured_gradient_boot_reps_used < 50L))
      stop("Calibrated MNAR bootstrap is invalid or not exactly seed-reproducible.",
           call. = FALSE)
  })
  
  
  res <- preflight_step("end-to-end synthetic CV-TMLE", {
    run_final_cv_tmle(cfg_pre, df)
  })
  if (isTRUE(cfg_pre$final_tmle$residual_Q_candidate)) {
    if (length(res$residual_Q_training_by_fold)!=length(unique(res$outer_fold)) ||
        any(!is.finite(res$Q_without_residual_predictions)))
      stop("Preflight FAILED: residual-Q archive or original-library predictions missing.")
    chk <- build_residual_Q_comparison(res,cfg_pre)
    if (abs(chk$att_estimate[1]-res$result$att_estimate)>1e-7 ||
        abs(chk$att_se[1]-res$result$att_se)>1e-7)
      stop("Preflight FAILED: residual-Q comparison does not reproduce primary ATT/SE.")
    if (any(!is.finite(build_residual_Q_validation(res,cfg_pre)$baseline_minus_residual_mse)))
      stop("Preflight FAILED: residual-Q held-out validation failed.")
  }
  run_v842_postfit_self_test(res,cfg_pre)
  run_v842_binary_candidate_test(cfg_pre)
  run_v843_glmnet_self_test(cfg_pre)
  run_v844_self_test(cfg_pre)
  rr <- res$result
  needed <- c("estimate","se","att_mu1","att_mu0",
              "pct_depression_effect","pct_depression_effect_se",
              "pct_prevention_gain","pct_prevention_gain_se")
  if (any(!vapply(needed, function(z) is.finite(rr[[z]][1]), logical(1))) || rr$se[1] <= 0 ||
      rr$pct_depression_effect_se[1] <= 0 || rr$pct_prevention_gain_se[1] <= 0)
    stop("Preflight FAILED: dollar or percentage estimates/SEs are invalid.", call. = FALSE)
  ac <- res$att_components
  if (is.null(ac)) stop("Preflight FAILED: ATT components were not returned.", call. = FALSE)
  preflight_outer_v <- length(unique(res$outer_fold))
  if (is.null(res$protected_mapping_by_fold) ||
      length(res$protected_mapping_by_fold) != preflight_outer_v ||
      any(vapply(res$protected_mapping_by_fold, function(z)
        !setequal(names(z),resolve_protected_W(cfg_pre)) || any(!is.finite(z)) || any(z < 1L), logical(1))))
    stop("Preflight FAILED: a configured protected baseline variable disappeared in a final fold.", call. = FALSE)
  if (!isTRUE(all.equal(res$D, ac$D_att, tolerance = 1e-12)))
    stop("Preflight FAILED: the generic primary EIF is not the ATT EIF.", call. = FALSE)
  if (!is.finite(rr$inference_df[1]) || !grepl("stratified", rr$inference_method[1], ignore.case = TRUE))
    stop("Preflight FAILED: the headline result is not using REGION-stratified PSU inference.", call. = FALSE)
  if (max(abs(ac$D_att - (ac$D_mu1 - ac$D_mu0)), na.rm=TRUE) > 1e-10)
    stop("Preflight FAILED: D_att != D_mu1 - D_mu0.", call. = FALSE)
  if (!is.finite(ac$eif_mean_scaled) || abs(ac$eif_mean_scaled) > ac$center_tol_scaled)
    stop(sprintf("Preflight FAILED: scaled ATT EIF centering %.3e exceeds %.3e.",
                 ac$eif_mean_scaled, ac$center_tol_scaled), call. = FALSE)
  if (max(ac$target_score_mu1, ac$target_score_mu0) > cfg_pre$final_tmle$target_score_tol)
    stop("Preflight FAILED: a joint ATT targeting score exceeds tolerance.", call. = FALSE)
  if (!any(df$Y == 0, na.rm=TRUE) || max(df$Y, na.rm=TRUE) != 999995)
    stop("Preflight FAILED: raw-dollar heavy-tail test data were not preserved.", call. = FALSE)
  
  
  # Strict SuperLearner failure-flag regression test: the package explicitly
  # records learners that failed during CV or full fitting, even when it can
  # continue by assigning them zero weight.
  fake_sl <- structure(list(
    libraryNames = c("SL.mean_All", "SL.glmnet.fixed_All"),
    SL.predict = rep(0.5, 5), coef = c(SL.mean_All = 1, SL.glmnet.fixed_All = 0),
    cvRisk = c(SL.mean_All = 0.25, SL.glmnet.fixed_All = NA_real_),
    library.predict = matrix(0.5, 5, 2), Z = matrix(0.5, 10, 2),
    errorsInCVLibrary = c(FALSE, TRUE), errorsInLibrary = c(FALSE, FALSE),
    fitLibrary = list(list(), list())), class = "SuperLearner")
  failed_flag_caught <- tryCatch({
    assert_superlearner_fit(fake_sl, c("SL.mean", "SL.glmnet.fixed"),
                            "preflight", 1L, 5L); FALSE
  }, error = function(e) TRUE)
  if (!failed_flag_caught)
    stop("Preflight failed: SuperLearner package failure flags were not enforced.", call. = FALSE)
  
  # Protected factor levels must not be collapsed because a category is rare
  # or has few treated observations.
  h1_test <- factor(c(rep("0", 40), rep("1", 20), rep("2", 8), "3", "Missing"))
  a_test <- c(rep(0L, 60), rep(1L, 10))
  ptest <- prep_factor_train(h1_test, cfg_pre$final_preprocess,
                             cfg_pre$preprocessing, A = a_test,
                             preserve_substantive_levels = TRUE)
  if (!all(c("0", "1", "2", "3") %in% ptest$levels))
    stop("Preflight failed: protected H1FS substantive levels were collapsed.", call. = FALSE)
  
  # The production driver supplies delta = 1 for every g-fold row, so this
  # regression test verifies deterministic reconstruction of those folds
  g_fold_1 <- do.call(make_cluster_folds_balanced, c(list(
    cluster = cluster, A = A, k = 5L, seed = 9123L,
    weights = df$GSWGT1, delta = rep(1L, length(A))),
    fold_control_from_cfg(cfg_pre, "internal")))
  g_fold_2 <- do.call(make_cluster_folds_balanced, c(list(
    cluster = cluster, A = A, k = 5L, seed = 9123L,
    weights = df$GSWGT1, delta = rep(1L, length(A))),
    fold_control_from_cfg(cfg_pre, "internal")))
  if (!identical(g_fold_1, g_fold_2))
    stop("Preflight failed: g-fold construction is not deterministic.", call. = FALSE)
  
  preflight_step("suffix-safe candidate governance and ordinary AH_PVT screening", {
    toy <- data.frame(
      AID = 1:20,
      PSUSCID = rep(1:4, each = 5),
      REGION = rep(1:4, each = 5),
      GSWGT1 = rep(1, 20),
      Depressed = rep(c(0L, 1L), 10),
      Y = seq_len(20),
      delta_Y = 1L,
      AH_PVT = seq(80, 118, length.out = 20),
      H1FS1 = rep(0:3, 5),
      AID.x = 1:20,
      PSUSCID.x = rep(1:4, each = 5),
      H4EC2.x = seq_len(20),
      WSAFE.x = stats::rnorm(20),
      check.names = FALSE)
    reg <- data.frame(
      source = c("wave1_inhome", "wave1_inhome", "wave1_inhome"),
      raw_variable = c("AH_PVT", "H1FS1", "WSAFE"),
      canonical_variable = c("AH_PVT", "H1FS1", "WSAFE"),
      derived_from = NA_character_, stringsAsFactors = FALSE)
    attr(toy, "variable_source_registry") <- validate_variable_source_registry(reg)
    cfg_toy <- cfg_pre
    cfg_toy$analysis$protected_W_mode <- "items"
    cfg_toy$final_tmle$protected_W_items <- "H1FS1"
    cfg_toy$final_tmle$protected_W <- "H1FS1"
    cfg_toy$causal_governance$additional_mandatory_W <- character(0)
    cfg_toy$analysis$extra_exclude_from_candidates <- c(
      cfg_toy$analysis$extra_exclude_from_candidates, "H4EC2")
    candidates <- get_candidate_vars(toy, cfg_toy)
    if (!all(c("AH_PVT", "H1FS1", "WSAFE.x") %in% candidates) ||
        any(c("AID.x", "PSUSCID.x", "H4EC2.x") %in% candidates))
      stop("Suffix-safe candidate governance failed.", call. = FALSE)
    audit <- build_candidate_alias_audit(toy, cfg_toy)
    ah <- audit[audit$actual_variable == "AH_PVT", , drop = FALSE]
    if (nrow(ah) != 1L || !isTRUE(ah$eligible_candidate_W) ||
        isTRUE(ah$mandatory_W) || isTRUE(ah$configured_exclusion))
      stop("AH_PVT is not behaving as an ordinary screened candidate.", call. = FALSE)
    
    # The descriptive registry must not act as an allowlist.
    toy$UNREGISTERED_W <- stats::rnorm(nrow(toy))
    attr(toy, "variable_source_registry") <- validate_variable_source_registry(reg)
    audit2 <- build_candidate_alias_audit(toy, cfg_toy)
    ur <- audit2[audit2$actual_variable == "UNREGISTERED_W", , drop = FALSE]
    if (nrow(ur) != 1L || !isTRUE(ur$eligible_candidate_W) ||
        isTRUE(ur$registered_in_source_audit))
      stop("The descriptive variable-source registry is still acting as an allowlist.",
           call. = FALSE)
    validate_candidate_governance(toy, cfg_toy, audit2)
    
    # Mandatory suffix canonicalization remains appropriate for H1FS only.
    toy_alias <- toy
    toy_alias$H1FS1.x <- toy_alias$H1FS1
    toy_alias$H1FS1 <- NULL
    attr(toy_alias, "variable_source_registry") <- validate_variable_source_registry(reg)
    toy_alias <- canonicalize_mandatory_W_columns(toy_alias, cfg_toy)
    if (!"H1FS1" %in% names(toy_alias) || "H1FS1.x" %in% names(toy_alias))
      stop("Protected-H1FS suffix canonicalization failed.", call. = FALSE)
    
    bad_reg <- reg
    bad_reg$canonical_variable[1L] <- "WRONG"
    if (!inherits(try(validate_variable_source_registry(bad_reg), silent = TRUE),
                  "try-error"))
      stop("Registry canonical-name mismatch was not rejected.", call. = FALSE)
    derived_reg <- append_derived_variable_registry(
      validate_variable_source_registry(reg), "AH_PVT_SQUARED", "AH_PVT", "wave1_inhome")
    dr <- derived_reg[derived_reg$raw_variable == "AH_PVT_SQUARED", , drop = FALSE]
    if (nrow(dr) != 1L || dr$source != "wave1_inhome" ||
        dr$derived_from != "AH_PVT")
      stop("Derived-variable source audit is incorrect.", call. = FALSE)
  })
  
  preflight_step("optional analysis toggles", {
    cfg_off <- cfg_pre
    cfg_off$policy$enable_policy_components <- FALSE
    cfg_off$policy$enable_att_prevalence_translation <- FALSE
    cfg_off$diagnostics$enable_wave2_completion_diagnostic <- FALSE
    cfg_off$diagnostics$enable_mnar_pattern_mixture <- FALSE
    cfg_off$diagnostics$mnar_shift_sd_grid <- numeric(0)
    cfg_off$global$require_script_md5 <- FALSE
    cfg_off$global$pipeline_source_path <- NA_character_
    validate_cfg(cfg_off)
    pipeline_script_fingerprint(cfg_off, strict = FALSE)
    
    cfg_bad_policy <- cfg_off
    cfg_bad_policy$policy$enable_att_prevalence_translation <- TRUE
    if (!inherits(try(validate_cfg(cfg_bad_policy), silent = TRUE), "try-error"))
      stop("Policy translation was allowed without policy-component targeting.",
           call. = FALSE)
    
    cfg_mort_link_only <- cfg_pre
    cfg_mort_link_only$mortality_sensitivity$enabled <- TRUE
    cfg_mort_link_only$mortality_sensitivity$composite_zero_at_death <- FALSE
    validate_cfg(cfg_mort_link_only)
    
    curated_names <- names(final_sensitivity_scenarios())
    if ("S0_main" %in% curated_names)
      stop("Curated sensitivity list must not re-run the already completed primary specification.",
           call. = FALSE)
    if (!"F1f_no_mortality_composite" %in% curated_names)
      stop("Curated sensitivity list is missing the no-mortality-composite comparator.",
           call. = FALSE)
    f1f <- final_sensitivity_scenarios()[["F1f_no_mortality_composite"]]
    cfg_f1f <- merge_cfg_overlay(cfg_pre, f1f$overlay)
    validate_cfg(cfg_f1f)
    f1f_enabled <- vapply(c(3L, 4L), function(w) {
      isTRUE(resolve_mortality_spec(cfg_f1f, w)$enabled)
    }, logical(1L))
    if (!all(f1f_enabled) ||
        isTRUE(cfg_f1f$mortality_sensitivity$composite_zero_at_death))
      stop(paste0(
        "F1f must retain Wave-III/IV mortality linkage while disabling only ",
        "mortality-composite zeroing."), call. = FALSE)
    if (!"F0f_combined_nuisance_balance_pi" %in% curated_names)
      stop("Curated sensitivity list is missing the combined nuisance scenario.", call. = FALSE)
    # Validate the declared CES-D cutoff-24 sensitivity and retain 22
    # as the primary threshold. The altered exposure requires a separate fit.


    alt_cut_hi <- default_sensitivity_scenarios()[["S6b_alt_cutpoint_high"]]
    if (is.null(alt_cut_hi))
      stop("Default sensitivity list is missing the cutoff-24 comparator.", call. = FALSE)
    cfg_cut_hi <- merge_cfg_overlay(cfg_pre, alt_cut_hi$overlay)
    validate_cfg(cfg_cut_hi)
    if (!identical(as.numeric(cfg_cut_hi$exposure$cutpoint), 24) ||
        !identical(as.numeric(cfg_cut_hi$analysis$expected_exposure_cutpoint), 24) ||
        isTRUE(cfg_cut_hi$analysis$enforce_expected_treated_gate))
      stop("Cutoff-24 sensitivity did not update its cutpoint gate and treated-count gate.",
           call. = FALSE)
    if (isTRUE(cfg_pre$analysis$enforce_expected_cutpoint_gate) &&
        cfg_pre$exposure$cutpoint != cfg_pre$analysis$expected_exposure_cutpoint)
      stop("Exposure cutoff and its declared gate disagree.",call.=FALSE)
    default_cut <- default_sensitivity_scenarios()[["S6_alt_cutpoint_low"]]
    cfg_default_cut <- merge_cfg_overlay(cfg_pre, default_cut$overlay)
    validate_cfg(cfg_default_cut)
    if (!identical(as.numeric(cfg_default_cut$exposure$cutpoint), 20) ||
        !identical(as.numeric(cfg_default_cut$analysis$expected_exposure_cutpoint), 20) ||
        isTRUE(cfg_default_cut$analysis$enforce_expected_treated_gate))
      stop("Default cutoff-20 sensitivity did not update its cutpoint gate and treated-count gate.",
           call. = FALSE)
    if (!inherits(try(final_sensitivity_scenarios(
      negative_control_outcome = "W1"), silent = TRUE), "try-error"))
      stop("F3 accepted a negative-control outcome without an explicit outcome type.",
           call. = FALSE)
    f3_scenarios <- final_sensitivity_scenarios(
      negative_control_outcome = "W1",
      negative_control_outcome_type = "continuous")
    f3 <- f3_scenarios$F3_negative_control
    cfg_f3 <- merge_cfg_overlay(cfg_pre, f3$overlay)
    validate_cfg(cfg_f3)
    if (isTRUE(cfg_f3$mortality_sensitivity$enabled) ||
        isTRUE(cfg_f3$policy$enable_policy_components) ||
        isTRUE(cfg_f3$diagnostics$enable_mnar_calibrated) ||
        isTRUE(cfg_f3$safety$require_publication_ready_marker))
      stop("F3 did not disable mortality, policy translation, MNAR diagnostics, and the primary publication gate.",
           call. = FALSE)
    if (!"W1" %in% get_outcome_drop_vars(cfg_f3))
      stop("F3 negative-control source was not excluded from candidate W.",
           call. = FALSE)
  })
  
  
  preflight_step("harmonized month-timed mortality classification", {
    cfg_mort <- cfg_template
    cfg_mort$global$save_stage_csvs <- FALSE
    cfg_mort$mortality_sensitivity$enabled <- TRUE
    cfg_mort$mortality_sensitivity$enabled_waves <- c(3L, 4L)
    cfg_mort$mortality_sensitivity$composite_zero_at_death <- TRUE
    cfg_mort$outcome$family <- "Compensation"
    cfg_mort$outcome$current_wave <- 4L
    cfg_mort$outcome$waves <- 4L
    mc4 <- resolve_mortality_spec(cfg_mort, 4L)
    cfg_lfp4 <- cfg_mort
    cfg_lfp4$outcome$family <- "LaborForceParticipation"
    mc4_lfp <- resolve_mortality_spec(cfg_lfp4, 4L)
    if (!identical(mc4$timing_mode, "interview_month") ||
        !identical(as.integer(mc4$death_year_end), 2009L) ||
        !identical(mc4$timing_mode, mc4_lfp$timing_mode) ||
        !identical(as.integer(mc4$death_year_end),
                   as.integer(mc4_lfp$death_year_end)))
      stop("Wave-IV mortality is not harmonized across outcome families through 2009.",
           call. = FALSE)
    
    m4 <- data.frame(
      AID = 1:8,
      NDIDD19Y = c(2008, 2008, 2008, 2008, 2009, 2009, 2009, 99997),
      NDIDD19M = c(5, 6, 7, 12, 1, 2, 997, 997),
      check.names = FALSE)
    timing4 <- data.frame(
      AID = c(1, 2, 3, 8),
      IYEAR4 = c(2008, 2008, 2008, 2009),
      IMONTH4 = c(6, 6, 6, 1),
      check.names = FALSE)
    main4 <- data.frame(AID = 1:8, GSWGT1 = rep(1, 8), W = 1:8)
    linked4 <- merge_mortality_indicator_from_data(
      main4, m4, cfg_mort, interview_timing = timing4)
    expected4 <- c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L)
    if (!identical(linked4$data[[mc4$death_before_outcome_var]], expected4) ||
        linked4$audit$fieldwork_start_year != 2008L ||
        linked4$audit$fieldwork_start_month != 6L ||
        linked4$audit$fieldwork_end_year != 2009L ||
        linked4$audit$fieldwork_end_month != 1L ||
        linked4$audit$n_same_interview_month_unordered != 1L)
      stop("Wave-IV harmonized mortality known-answer classification failed.",
           call. = FALSE)
    if (linked4$data[[mc4$timing_status_var]][5L] !=
        "on_or_before_fieldwork_end_month_no_interview" ||
        linked4$data[[mc4$timing_status_var]][7L] !=
        "fieldwork_end_year_death_month_unknown_no_interview")
      stop("Wave-IV fieldwork-end mortality status is incorrect.", call. = FALSE)
    
    # A valid death year with NDIDD19M=997 remains a candidate death with an
    # unknown month; other nonmissing invalid month codes fail loudly.
    unknown_m <- derive_mortality_indicator_from_data(
      data.frame(AID = 1L, NDIDD19Y = 2000L, NDIDD19M = 997L), cfg_mort)
    if (!is.na(unknown_m$data[[mc4$derived_death_month_var]][1L]) ||
        unknown_m$data[[mc4$death_in_window_var]][1L] != 1L)
      stop("NDIDD19M=997 was not retained as unknown month on a valid death year.",
           call. = FALSE)
    if (!inherits(try(derive_mortality_indicator_from_data(
      data.frame(AID = 1L, NDIDD19Y = 2000L, NDIDD19M = 998L), cfg_mort),
      silent = TRUE), "try-error"))
      stop("Unrecognized nonmissing NDIDD19M code did not fail loudly.", call. = FALSE)
    
    # Wave III uses the same rule, with its own latest observed fieldwork month.
    cfg_ed3 <- cfg_mort
    cfg_ed3$outcome$family <- "EducationalAttainment"
    cfg_ed3$outcome$family_member <- "at_least_hs"
    cfg_ed3$outcome$current_wave <- 3L
    cfg_ed3$outcome$waves <- 3L
    mc3 <- resolve_mortality_spec(cfg_ed3, 3L)
    m3 <- data.frame(
      AID = 1:7,
      NDIDD19Y = c(2001, 2002, 2001, 2001, 2002, 2002, 99997),
      NDIDD19M = c(12, 4, 9, 10, 5, 997, 997))
    timing3 <- data.frame(
      AID = c(3, 4, 7),
      IYEAR3 = c(2001, 2001, 2002),
      IMONTH3 = c(10, 10, 4))
    main3 <- data.frame(AID = 1:7, GSWGT1 = rep(1, 7), W = 1:7)
    linked3 <- merge_mortality_indicator_from_data(
      main3, m3, cfg_ed3, interview_timing = timing3)
    expected3 <- c(1L, 1L, 1L, 0L, 0L, 0L, 0L)
    if (!identical(linked3$data[[mc3$death_before_outcome_var]], expected3) ||
        linked3$audit$fieldwork_end_year != 2002L ||
        linked3$audit$fieldwork_end_month != 4L)
      stop("Wave-III fieldwork-end mortality known-answer test failed.", call. = FALSE)
    
    # Incomplete mortality linkage remains a hard failure unless explicitly relaxed.
    incomplete_mortality <- m4[1:7, , drop = FALSE]
    if (!inherits(try(merge_mortality_indicator_from_data(
      main4, incomplete_mortality, cfg_mort,
      interview_timing = timing4), silent = TRUE), "try-error"))
      stop("Incomplete mortality linkage did not trigger the enabled linkage gate.",
           call. = FALSE)
    cfg_relaxed_link <- cfg_mort
    cfg_relaxed_link$mortality_sensitivity$require_complete_linkage <- FALSE
    relaxed <- suppressWarnings(merge_mortality_indicator_from_data(
      main4, incomplete_mortality, cfg_relaxed_link,
      interview_timing = timing4))
    if (relaxed$data[[mc4$death_before_outcome_var]][8L] != 0L ||
        relaxed$audit$n_required_rows_unmatched != 1L)
      stop("Relaxed mortality-linkage gate failed its known-answer behavior.",
           call. = FALSE)
    
    # Explicit no-death code overrides an otherwise calendar-like year.
    cfg_override <- cfg_mort
    cfg_override$mortality_sensitivity$no_death_codes <- 2000
    override <- derive_mortality_indicator_from_data(
      data.frame(AID = 1L, NDIDD19Y = 2000L, NDIDD19M = 1L), cfg_override)
    if (override$data[[mc4$death_in_window_var]] != 0L ||
        override$audit$n_no_death_codes_overlapping_year_range != 1L)
      stop("Explicit mortality no-death code did not override calendar-year interpretation.",
           call. = FALSE)
    bad_mort <- data.frame(AID = 1:2, NDIDD19Y = c(2000, 9999), NDIDD19M = c(1, 1))
    if (!inherits(try(derive_mortality_indicator_from_data(
      bad_mort, cfg_mort), silent = TRUE), "try-error"))
      stop("Unrecognized mortality year code did not trigger the enabled gate.",
           call. = FALSE)
    
    # The contradiction gate still prevents overwriting an observed outcome
    # unless the analyst explicitly relaxes it.
    composite_toy <- data.frame(
      AID = 1:4, Y = c(100, NA, 50, NA),
      EarningsSource = c("exact", "missing", "bracket", "missing"),
      DeathW4 = c(1L, 1L, 0L, 0L), NDIY4 = c(2000L, 2001L, NA, NA),
      NDIM4 = c(1L, NA_integer_, NA_integer_, NA_integer_),
      D4Time = c("before_fieldwork_end_year_no_interview",
                 "before_fieldwork_end_year_no_interview",
                 "no_death_or_outside_horizon", "no_death_or_outside_horizon"),
      IYEAR4 = c(NA, NA, 2008L, 2008L), IMONTH4 = c(NA, NA, 5L, 5L),
      stringsAsFactors = FALSE)
    attr(composite_toy, "outcome_support") <- list(lower = 0, upper = 200)
    if (!inherits(try(apply_mortality_composite(
      composite_toy, cfg_mort), silent = TRUE), "try-error"))
      stop("Observed outcomes among pre-outcome deaths did not trigger the contradiction gate.",
           call. = FALSE)
    cfg_relaxed_outcome <- cfg_mort
    cfg_relaxed_outcome$mortality_sensitivity$fail_on_death_with_observed_original_outcome <- FALSE
    comp <- suppressWarnings(apply_mortality_composite(
      composite_toy, cfg_relaxed_outcome))
    if (!identical(comp$data$Y, c(0, 0, 50, NA_real_)) ||
        !identical(comp$data$EarningsSource,
                   c("mortality_zero", "mortality_zero", "bracket", "missing")) ||
        comp$audit$n_deaths_with_observed_original_outcome != 1L ||
        comp$audit$n_deaths_recoded_to_observed_zero != 2L)
      stop("Relaxed mortality contradiction gate failed its audited zero recode.",
           call. = FALSE)
  })
  
  preflight_step("compensation exact-only and zero-safe transforms", {
    cfg_o <- cfg_pre$outcome$families$Compensation
    raw <- data.frame(H4EC2 = c(0, 10000, 9999996),
                      H4EC3 = c(97, 97, 2))
    base <- data.frame(AID = 1:3)
    base <- cbind(base, raw)
    cfg_identity <- cfg_pre$outcome
    cfg_identity$compensation_transform <- "identity"
    cfg_identity$compensation_exact_only <- FALSE
    out_identity <- construct_outcome_compensation(
      base, 4L, cfg_o, cfg_identity, NULL, cfg_pre)
    cfg_exact <- cfg_identity; cfg_exact$compensation_exact_only <- TRUE
    out_exact <- construct_outcome_compensation(
      base, 4L, cfg_o, cfg_exact, NULL, cfg_pre)
    cfg_log <- cfg_identity; cfg_log$compensation_transform <- "log1p"
    out_log <- construct_outcome_compensation(
      base, 4L, cfg_o, cfg_log, NULL, cfg_pre)
    if (!isTRUE(all.equal(out_identity$Y, c(0, 10000, 7500))) ||
        !isTRUE(all.equal(out_exact$Y, c(0, 10000, NA), check.attributes = FALSE)) ||
        !isTRUE(all.equal(out_log$Y, log1p(c(0, 10000, 7500)))))
      stop("Compensation sensitivity transformations failed.", call. = FALSE)
  })
  
  preflight_step("Q clipping and MNAR helper arithmetic", {
    qs <- summarize_prediction_clipping(c(-0.1, 0.2, 1.1), 0.005, 0.995, 1L, "toy")
    if (qs$n_clipped != 2L || !isTRUE(all.equal(qs$fraction_clipped, 2/3)))
      stop("Q clipping summary failed.", call. = FALSE)
    
    mnar_A <- c(1L, 1L, 0L, 0L)
    mnar_w <- c(1, 3, 100, 100)
    mnar_pi1 <- c(0.5, 0.75, 0.5, 0.5)
    mnar_clip <- c(1, 0, 1, 1)
    expected_mnar_clip <- weighted_mean_safe(
      mnar_clip, mnar_w * mnar_A * (1 - mnar_pi1))
    if (!isTRUE(all.equal(expected_mnar_clip, 0.4)))
      stop("MNAR clipping fraction is not using treated missing-potential-outcome contribution weights.",
           call. = FALSE)
  })
  
  preflight_step("verified outcome constructors, nesting, scope, and policy defaults", {
    cfg_y <- cfg_template
    cfg_y$outcome$current_wave <- 4L
    lfp4 <- data.frame(AID=as.character(1:9),
                       H4LM6=c(1,0,0,0,0,0,0,0,0),
                       H4LM11=c(7,1,0,0,0,0,0,0,0),
                       H4LM14=c(97,97,1,2,3,5,4,6,10), stringsAsFactors=FALSE)
    lfp4_y <- construct_outcome_labor_force_participation(
      lfp4, 4L, cfg_y$outcome$families$LaborForceParticipation,
      cfg_y$outcome, NULL, pipeline_cfg=NULL)$Y
    if (!identical(lfp4_y, c(1L,1L,1L,1L,1L,1L,0L,0L,NA_integer_)))
      stop("Wave-IV route-aware LFP known-answer test failed.", call. = FALSE)
    
    hrs4 <- data.frame(AID=as.character(1:6),
                       H4LM6=c(1,0,0,0,0,0), H4LM11=c(7,1,1,0,1,1),
                       H4LM12=c(1,2,1,97,2,98), H4LM13=c(997,70,997,997,997,65),
                       H4LM19=c(45,40,130,997,50,40), stringsAsFactors=FALSE)
    hrs4_y <- construct_outcome_hours_worked(
      hrs4, 4L, cfg_y$outcome$families$HoursWorked,
      cfg_y$outcome, NULL, pipeline_cfg=NULL)$Y
    if (!isTRUE(all.equal(hrs4_y, c(45,70,120,0,NA,65), check.attributes=FALSE)))
      stop("Wave-IV hours known-answer test failed.", call. = FALSE)
    
    edu3 <- data.frame(
      AID = as.character(1:10),
      H3ED1 = c(12,13,17,12,NA,12,18,96,12,13),
      H3ED2 = c(0,0,6,0,0,1,0,0,8,0),
      H3ED3 = c(0,0,9,1,0,0,0,0,9,0),
      H3ED5 = c(0,0,1,0,1,6,0,0,8,0),
      stringsAsFactors = FALSE)
    edu3_expected <- list(
      at_least_hs = c(0L,1L,1L,1L,1L,1L,NA,NA,NA,1L),
      at_least_some_college = c(0L,1L,1L,0L,1L,NA,NA,NA,NA,1L),
      at_least_college_grad = c(0L,0L,1L,0L,1L,NA,NA,0L,NA,0L))
    for (member in names(edu3_expected)) {
      out <- construct_outcome_educational_attainment(
        edu3, 3L, cfg_y$outcome$families$EducationalAttainment,
        cfg_y$outcome, member, pipeline_cfg = NULL)
      if (!identical(out$Y, edu3_expected[[member]]))
        stop("Wave-III education known-answer test failed for ", member, ".",
             call. = FALSE)
      if (!"n_nested_violations" %in% out$audit$metric ||
          as.integer(out$audit$value[out$audit$metric == "n_nested_violations"]) != 0L)
        stop("Wave-III education nesting audit failed for ", member, ".",
             call. = FALSE)
    }
    
    edu4 <- data.frame(
      AID = as.character(1:10),
      H4ED2 = c(0,2,3,5,6,7,13,96,98,NA),
      stringsAsFactors = FALSE)
    edu4_expected <- list(
      at_least_hs = c(0L,0L,1L,1L,1L,1L,1L,NA,NA,NA),
      at_least_some_college = c(0L,0L,0L,0L,1L,1L,1L,NA,NA,NA),
      at_least_college_grad = c(0L,0L,0L,0L,0L,1L,1L,NA,NA,NA))
    for (member in names(edu4_expected)) {
      out <- construct_outcome_educational_attainment(
        edu4, 4L, cfg_y$outcome$families$EducationalAttainment,
        cfg_y$outcome, member, pipeline_cfg = NULL)
      if (!identical(out$Y, edu4_expected[[member]]))
        stop("Wave-IV education known-answer test failed for ", member, ".",
             call. = FALSE)
    }
    if (!inherits(try(construct_outcome_educational_attainment(
      data.frame(AID="x", H4ED2=99), 4L,
      cfg_y$outcome$families$EducationalAttainment,
      cfg_y$outcome, "at_least_college_grad", pipeline_cfg=NULL),
      silent=TRUE), "try-error"))
      stop("Wave-IV education accepted unmapped code 99.", call. = FALSE)
    
    health <- data.frame(
      AID = as.character(1:9), H3GH1 = c(1,2,3,4,5,96,98,99,NA),
      H4GH1 = c(1,2,3,4,5,96,98,99,NA), stringsAsFactors = FALSE)
    health_expected <- c(1L,1L,1L,0L,0L,NA,NA,NA,NA)
    for (wave in c(3L, 4L)) {
      out <- construct_outcome_health_status(
        health, wave, cfg_y$outcome$families$HealthStatus,
        cfg_y$outcome, "at_least_good", pipeline_cfg = NULL)
      if (!identical(out$Y, health_expected))
        stop("Wave-", wave, " health-status known-answer test failed.",
             call. = FALSE)
    }
    if (!inherits(try(construct_outcome_health_status(
      data.frame(AID="x", H3GH1=6), 3L,
      cfg_y$outcome$families$HealthStatus,
      cfg_y$outcome, "at_least_good", pipeline_cfg=NULL),
      silent=TRUE), "try-error"))
      stop("HealthStatus accepted unmapped finite code 6.", call. = FALSE)
    
    # The hard scope cannot be bypassed by the unverified-specification override.
    scope_cases <- list(
      list(family="LaborForceParticipation", waves=3L),
      list(family="HoursWorked", waves=3L),
      list(family="UsualHours", waves=4L),
      list(family="Compensation", waves=3L),
      list(family="EducationalAttainment", waves=5L),
      list(family="HealthStatus", waves=5L))
    for (case in scope_cases) {
      bad <- cfg_template
      bad$outcome$family <- case$family
      bad$outcome$waves <- case$waves
      bad$outcome$family_member <- NULL
      bad$safety$allow_unverified_outcome_specs <- TRUE
      if (!inherits(try(validate_cfg(bad), silent=TRUE), "try-error"))
        stop(sprintf("Hard outcome scope accepted %s at Wave %d.",
                     case$family, case$waves), call. = FALSE)
    }
    for (family in c("EducationalAttainment", "HealthStatus")) {
      good <- cfg_template
      good$outcome$family <- family
      good$outcome$waves <- "all"
      good$outcome$family_member <- NULL
      if (inherits(try(validate_cfg(good), silent=TRUE), "try-error"))
        stop(family, " failed verification for its supported Wave-III/IV scope.",
             call. = FALSE)
    }
    
    # Policy components and the prevention-gain ratio remain enabled for every
    # supported primary outcome; runtime defaults change only the outcome type.
    expected_type <- c(
      EducationalAttainment="binary", HealthStatus="binary",
      LaborForceParticipation="binary", HoursWorked="continuous",
      Compensation="continuous")
    for (family in names(expected_type)) {
      policy_cfg <- cfg_template
      policy_cfg$outcome$family <- family
      policy_cfg$outcome$waves <- supported_outcome_waves(family)[1L]
      policy_cfg$outcome$family_member <- if (identical(family, "EducationalAttainment"))
        "at_least_hs" else if (identical(family, "HealthStatus"))
          "at_least_good" else NULL
      policy_cfg <- apply_outcome_runtime_defaults(policy_cfg)
      if (!identical(policy_cfg$analysis$outcome_type, unname(expected_type[family])) ||
          !isTRUE(policy_cfg$policy$enable_policy_components) ||
          !isTRUE(policy_cfg$policy$enable_att_prevalence_translation) ||
          !isTRUE(compensation_ratio_translation_enabled(policy_cfg)))
        stop("Policy/runtime defaults failed for ", family, ".", call. = FALSE)
    }
    transformed_cfg <- cfg_template
    transformed_cfg$outcome$family <- "Compensation"
    transformed_cfg$outcome$compensation_transform <- "log1p"
    if (compensation_ratio_translation_enabled(transformed_cfg))
      stop("Transformed Compensation incorrectly enabled arithmetic-scale ratios.",
           call. = FALSE)
    
    cfg_m3 <- cfg_template; cfg_m3$outcome$current_wave <- 3L
    cfg_m3$mortality_sensitivity$enabled <- TRUE
    cfg_m3$mortality_sensitivity$enabled_waves <- c(3L,4L)
    mtoy <- data.frame(AID=as.character(1:4),
                       NDIDD19Y=c(2000,2002,2007,99997), NDIDD19M=c(1,2,3,997))
    d3 <- derive_mortality_indicator_from_data(mtoy, cfg_m3)$data$D3Raw
    cfg_m4 <- cfg_m3; cfg_m4$outcome$current_wave <- 4L
    d4 <- derive_mortality_indicator_from_data(mtoy, cfg_m4)$data$D4Raw
    if (!identical(d3, c(1L,1L,0L,0L)) || !identical(d4, c(1L,1L,1L,0L)))
      stop("Wave-specific mortality raw-horizon toggle test failed.", call. = FALSE)
    
    # Outcome-aware sensitivity filtering never applies earnings-specific
    # measurement/transformation scenarios to HoursWorked.
    cfg_h_nomort <- cfg_template
    cfg_h_nomort$outcome$family <- "HoursWorked"
    cfg_h_nomort$outcome$waves <- 4L
    h_nomort_sc <- filter_sensitivity_scenarios_for_outcome(
      cfg_h_nomort, final_sensitivity_scenarios())
    forbidden_h <- c("F1_bound_refit_q0990", "F1b_bound_refit_q1000",
                     "F1c_exact_earnings_only", "F1d_log1p_earnings",
                     "F1e_asinh_earnings")
    if (any(forbidden_h %in% names(h_nomort_sc)))
      stop("Outcome-aware sensitivity filter retained an inapplicable HoursWorked scenario.",
           call. = FALSE)
    if (!"F1f_no_mortality_composite" %in% names(h_nomort_sc))
      stop(paste0(
        "Outcome-aware sensitivity filtering incorrectly removed the ",
        "mortality-composite comparator from HoursWorked."), call. = FALSE)
  })
  
  message(sprintf(paste0("Preflight OK. Dollar ATT=$%.2f (SE $%.2f); prevention gain=%.2f%% ",
                         "(SE %.2f); depression effect=%.2f%%; scaled EIF mean=%.2e; ",
                         "target scores=(%.2e, %.2e)."),
                  rr$estimate[1], rr$se[1], rr$pct_prevention_gain[1],
                  rr$pct_prevention_gain_se[1], rr$pct_depression_effect[1],
                  ac$eif_mean_scaled, ac$target_score_mu1, ac$target_score_mu0))
  invisible(TRUE)
}

# =============================================================================
