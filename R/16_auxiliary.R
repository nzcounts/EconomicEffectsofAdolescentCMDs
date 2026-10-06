# Resume a saved fitted-DGP positivity simulation without rerunning
# the production pipeline. replicates is the total attempted ID count;
# max_seconds is the additional soft time budget for this invocation.
# PSOCK workers require memory for full g fits. Checkpoint inputs are read-only;
# results are written to positivity_resume. Source/fit/environment mismatches
# are rejected, saved attempts are retained, and repeats resume unfinished IDs.
# Do not remove a stale RUNNING lock until all controllers/workers are stopped.


.bp_collect <- function(roots, env) {
  # Export only the pipeline dependencies, never the user's entire workspace.
  found <- list(); queue <- roots
  while (length(queue)) {
    nm <- queue[1L]; queue <- queue[-1L]
    if (nm %in% names(found) || !exists(nm, env, inherits=FALSE)) next
    obj <- get(nm, env, inherits=FALSE)
    if (is.function(obj)) {
      if (!identical(environment(obj), env))
        stop("Pipeline dependency has an unexpected enclosing environment: ", nm)
      queue <- unique(c(queue, codetools::findGlobals(obj, merge=TRUE)))
    }
    found[nm] <- list(obj)
  }
  found
}

.bp_kernel <- function(original) {
  # Change ONLY the outer iterator from seq_len(M) to M. The original loop body,
  # seeds, full learner fits, calibration, targeting, and variance stay intact.
  b <- body(original)
  ix <- which(vapply(as.list(b), function(x) is.call(x) &&
    identical(x[[1L]], as.name("for")) && identical(x[[2L]], as.name("b")) &&
    identical(x[[3L]], quote(seq_len(M))), logical(1)))
  if (length(ix)!=1L) stop("Unsupported bootstrap implementation: expected v8.38 loop.")
  b[[ix]][[3L]] <- as.name("M")
  body(original) <- b
  original
}

.bp_truth <- function(z) {
  f <- z$tmle_fit; a <- f$att_components
  m1 <- f$S1W*(f$Qeligible1W*f$y_range+f$y_lower)
  m0 <- f$S0W*(f$Qeligible0W*f$y_range+f$y_lower)
  sum(a$weights*f$gn_raw*(m1-m0))/sum(a$weights*f$gn_raw)
}

.bp_check_rows <- function(x, ids, floors, upper, truth) {
  req <- c("replicate","q_specification","g_floor","g_upper","estimate","se",
           "ci_lower","ci_upper","covers_truth","frac_g_clipped")
  if (!is.data.frame(x) || !all(req %in% names(x)) || !nrow(x))
    stop("Incomplete saved replicate table.")
  if (!setequal(unique(x$replicate), ids) || any(!is.finite(as.matrix(x[c(
      "replicate","g_floor","g_upper","estimate","se","ci_lower","ci_upper","frac_g_clipped")]))))
    stop("Invalid saved replicate values/IDs.")
  for (b in ids) {
    y <- x[x$replicate==b,,drop=FALSE]
    expected <- expand.grid(q_specification=c("Q_correct","Q_misspecified"),g_floor=floors)
    key <- function(d) paste(d$q_specification, sprintf("%.15g",d$g_floor))
    if (nrow(y)!=nrow(expected) || anyDuplicated(key(y)) || !setequal(key(y),key(expected)))
      stop("Saved replicate has missing, duplicate, or incompatible scenarios.")
  }
  if (any(x$g_upper!=upper) || any(x$se<0) || any(x$ci_lower>x$ci_upper) ||
      any(x$frac_g_clipped<0 | x$frac_g_clipped>1) || anyNA(x$covers_truth) ||
      any(as.logical(x$covers_truth)!=(truth>=x$ci_lower & truth<=x$ci_upper)))
    stop("Saved replicate checks disagree with the current diagnostic.")
  invisible(TRUE)
}

.bp_work <- function(b, ctx) {
  # The statistical routine is unchanged; use the original b, not worker RNG streams.
  previous_rng <- RNGkind()
  had_seed <- exists(".Random.seed",.GlobalEnv,inherits=FALSE)
  previous_seed <- if (had_seed) get(".Random.seed",.GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind,as.list(previous_rng))
    if (had_seed) assign(".Random.seed",previous_seed,.GlobalEnv) else
      if (exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  do.call(RNGkind, as.list(ctx$rng))
  cc <- ctx$z$cfg
  cc$diagnostics$enable_bootstrap_positivity_diagnostic <- TRUE
  cc$diagnostics$bootstrap_positivity_replicates <- as.integer(b)
  # The controller owns the practical budget. A finite internal ceiling avoids
  # overflow in the original first-replicate planning calculation.
  cc$diagnostics$bootstrap_positivity_max_seconds <- 1e7
  cc$global$output_dir <- file.path(ctx$out, paste0("work_",b))
  cc$global$verbose <- FALSE
  started <- proc.time()[3]; warnings <- character()
  ans <- tryCatch(withCallingHandlers(ctx$kernel(ctx$z$tmle_fit,cc),warning=function(w) {
    msg <- conditionMessage(w)
    if (!startsWith(msg,"Fewer than 50 usable bootstrap replicates"))
      warnings <<- c(warnings,msg)
    invokeRestart("muffleWarning")
  }),error=function(e)e)
  err <- NULL; detail <- NULL
  if (inherits(ans,"error")) {
    err <- conditionMessage(ans)
    p <- build_unique_path(cc,"bootstrap_progress.rds")
    if (file.exists(p)) {
      old <- readRDS(p)
      if (NROW(old$errors)) err <- paste(unique(old$errors$error),collapse=" | ")
    }
  } else {
    detail <- ans$replicates
    .bp_check_rows(detail,b,ctx$floors,cc$final_tmle$g_upper,ctx$truth)
  }
  record <- list(schema="positivity_replicate_v1",fingerprint=ctx$fingerprint,
    replicate=as.integer(b),seconds=unname(proc.time()[3]-started),
    rows=detail,error=err,warnings=unique(warnings),origin="resumable")
  atomic_save_rds(record,file.path(ctx$out,sprintf("rep_%06d.rds",b)),overwrite=FALSE)
  list(replicate=b,seconds=record$seconds,success=is.null(err))
}

resume_saved_positivity_diagnostic <- function(checkpoint_path, max_seconds=5400,
    replicates=50L, workers=1L) {
  if (!exists("run_bootstrap_positivity_diagnostic",mode="function"))
    stop("Load the existing v8.38 functions before this add-on.")
  if (length(workers)!=1L || !is.finite(workers) || workers<1 || workers!=as.integer(workers) ||
      length(replicates)!=1L || !is.finite(replicates) || replicates<1 ||
      replicates!=as.integer(replicates) || length(max_seconds)!=1L ||
      !is.finite(max_seconds) || max_seconds<=0) stop("Invalid workers, replicates, or budget.")
  workers <- as.integer(workers); replicates <- as.integer(replicates)
  started <- proc.time()[3]
  checkpoint_path <- normalizePath(checkpoint_path,mustWork=TRUE)
  z <- restore_completed_fit(checkpoint_path); load_required_packages(z$cfg)
  original <- get("run_bootstrap_positivity_diagnostic",mode="function")
  env <- environment(original)
  deps <- .bp_collect(c("run_bootstrap_positivity_diagnostic","load_required_packages",
                       "atomic_save_rds","build_unique_path"),env)
  if (!".SL_RUNTIME_ENV" %in% names(deps)) stop("Missing learner runtime dependency.")
  # Environments are runtime state, not statistical code; reset in each worker.
  deps$.SL_RUNTIME_ENV <- new.env(parent=emptyenv())
  text_code <- function(x) deparse(x,width.cutoff=500L,control=c("keepInteger","keepNA"))
  code <- lapply(deps,function(x) if (is.function(x)) list(text_code(formals(x)),text_code(body(x))) else
    if (is.environment(x)) "runtime_environment" else x)
  # Attach in the original order so package masking does not change calculations.
  packages <- rev(unique(c(.packages(),"codetools")))
  versions <- setNames(vapply(packages,function(p) as.character(utils::packageVersion(p)),""),packages)
  kernel <- .bp_kernel(original); truth <- .bp_truth(z)
  cc <- z$cfg; no <- sum(z$tmle_fit$att_components$delta_Y==1L)
  floors <- cc$diagnostics$bootstrap_positivity_g_floors %||% c(.005,.025,.05)
  if (isTRUE(cc$diagnostics$bootstrap_positivity_include_sample_size_bound))
    floors <- c(floors,(cc$diagnostics$att_g_clip_sample_size_constant %||% 5)/(sqrt(no)*log(no)))
  floors <- sort(unique(c(floors,cc$final_tmle$g_lower)))
  if (!is.finite(truth) || any(!is.finite(floors)|floors<=0|floors>=cc$final_tmle$g_upper))
    stop("Invalid simulated truth or floor configuration.")
  out <- file.path(dirname(checkpoint_path),"positivity_resume")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  lock <- file.path(out,"RUNNING")
  if (!dir.create(lock,showWarnings=FALSE))
    stop("Run lock exists: ",lock,". Do not run two controllers in this folder.")
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  signature <- list(checkpoint_md5=unname(tools::md5sum(checkpoint_path)),
    code_md5=object_md5(code),versions=versions,R=R.version.string,rng=RNGkind(),
    adapter=object_md5(lapply(list(.bp_work,.bp_kernel,.bp_check_rows,.bp_truth),
      function(f) list(text_code(formals(f)),text_code(body(f))))))
  fingerprint <- object_md5(signature)
  manifest_path <- file.path(out,"manifest.rds")
  if (file.exists(manifest_path)) {
    manifest <- readRDS(manifest_path)
    if (!identical(manifest$fingerprint,fingerprint))
      stop("Fit, code, RNG kind, or software changed. Refusing to mix simulations.")
  } else {
    manifest <- list(fingerprint=fingerprint,signature=signature,true_att=truth,
      legacy_import="none",legacy_first_seconds=NA_real_)
    atomic_save_rds(manifest,manifest_path,overwrite=FALSE)
  }
  ctx <- list(z=z,kernel=kernel,truth=truth,floors=floors,out=out,
              fingerprint=fingerprint,rng=signature$rng)
  # Import a schema-compatible checkpoint once; never overwrite it. That checkpoint format has no
  # fit/code fingerprint. Location + truth + scenario checks cannot prove provenance.
  if (identical(manifest$legacy_import,"none")) {
    legacy <- build_unique_path(cc,"bootstrap_progress.rds")
    if (file.exists(legacy)) {
      old <- readRDS(legacy); x <- old$replicates; e <- old$errors
      if (!isTRUE(all.equal(old$true_att,truth,tolerance=1e-12)))
        stop("Legacy checkpoint simulated truth does not match the completed fit.")
      ok <- if (NROW(x)) sort(unique(x$replicate)) else integer()
      bad <- if (NROW(e)) e$replicate else integer()
      attempted <- c(ok,bad)
      if (!length(attempted) || anyNA(attempted) || anyDuplicated(attempted) ||
          any(attempted<1 | attempted!=as.integer(attempted)) ||
          !setequal(attempted,seq_len(max(attempted)))) stop("Invalid legacy replicate IDs.")
      if (length(ok)) .bp_check_rows(x,ok,floors,cc$final_tmle$g_upper,truth)
      for (b in attempted) {
        p <- file.path(out,sprintf("rep_%06d.rds",b))
        record <- list(schema="positivity_replicate_v1",fingerprint=fingerprint,
          replicate=as.integer(b),seconds=if (b==1) old$first_replicate_seconds else NA_real_,
          rows=if (b %in% ok) x[x$replicate==b,,drop=FALSE] else NULL,
          error=if (b %in% bad) as.character(e$error[e$replicate==b]) else NULL,
          warnings=character(),origin="legacy_unfingerprinted")
        if (!file.exists(p)) atomic_save_rds(record,p,overwrite=FALSE)
      }
      manifest$legacy_import <- "same_run_path_truth_and_scenarios_checked; original_has_no_fit_or_code_fingerprint"
      manifest$legacy_first_seconds <- old$first_replicate_seconds
      message("Imported legacy attempts: ",length(attempted),
        ". Original checkpoint lacks a fit/code fingerprint; this limitation is recorded.")
    } else manifest$legacy_import <- "no_legacy_checkpoint_at_original_run_path"
    atomic_save_rds(manifest,manifest_path,overwrite=TRUE)
  }
  read_records <- function() {
    paths <- list.files(out,"^rep_[0-9]+\\.rds$",full.names=TRUE)
    ans <- lapply(paths,readRDS)
    for (j in seq_along(ans)) {
      r <- ans[[j]]
      if (!identical(r$schema,"positivity_replicate_v1") ||
          !identical(r$fingerprint,fingerprint) || length(r$replicate)!=1L ||
          !is.finite(r$replicate) || r$replicate<1 || r$replicate!=as.integer(r$replicate) ||
          basename(paths[j])!=sprintf("rep_%06d.rds",r$replicate)) stop("Invalid replicate file.")
      if (is.null(r$error)) .bp_check_rows(r$rows,r$replicate,floors,cc$final_tmle$g_upper,truth)
    }
    ans
  }
  records <- read_records(); ids <- vapply(records,function(r)r$replicate,integer(1))
  if (any(ids>replicates)) stop("Requested total is below already saved replicate IDs.")
  pending <- setdiff(seq_len(replicates),ids)
  cl <- NULL
  known_times <- vapply(records,function(r) as.numeric(r$seconds %||% NA_real_),numeric(1))
  known_times <- known_times[is.finite(known_times)&known_times>0]
  enough_time <- max_seconds-(proc.time()[3]-started) >
    if (length(known_times)) max(tail(known_times,workers)) else 0
  if (length(pending) && workers>1L && enough_time) {
    cl <- parallel::makePSOCKcluster(min(workers,length(pending)),outfile=file.path(out,"workers.log"))
    on.exit(try(parallel::stopCluster(cl),silent=TRUE),add=TRUE,after=FALSE)
    # Function environment must be global in both master and workers. Do not
    # serialize the controller's local frame (which contains socket connections).
    init <- function(deps,ctx,work,check,libpaths,packages,versions) {
      .libPaths(libpaths)
      for (p in packages) suppressPackageStartupMessages(library(p,character.only=TRUE))
      got <- setNames(vapply(packages,function(p)as.character(utils::packageVersion(p)),""),packages)
      if (!identical(got,versions)) stop("Worker package versions differ.")
      list2env(deps,envir=.GlobalEnv)
      assign(".bp_check_rows",check,.GlobalEnv); assign(".bp_work",work,.GlobalEnv)
      assign(".bp_ctx",ctx,.GlobalEnv)
      TRUE
    }
    environment(init) <- .GlobalEnv
    parallel::clusterCall(cl,init,deps,ctx,.bp_work,.bp_check_rows,.libPaths(),packages,versions)
  }
  while (length(pending)) {
    times <- vapply(records,function(r) as.numeric(r$seconds %||% NA_real_),numeric(1))
    times <- times[is.finite(times)&times>0]
    expected <- if (length(times)) max(tail(times,workers)) else NA_real_
    left <- max_seconds-(proc.time()[3]-started)
    if (left<=0 || (is.finite(expected) && expected>left)) break
    batch <- head(pending,if (is.null(cl)) 1L else length(cl))
    message("Running replicate IDs ",paste(batch,collapse=", "),"; saved successes ",
      sum(vapply(records,function(r)is.null(r$error),logical(1))),"; budget left ",round(left),"s.")
    if (is.null(cl)) {
      for (b in batch) .bp_work(b,ctx)
    } else {
      dispatch <- function(b) .bp_work(b,.bp_ctx)
      environment(dispatch) <- .GlobalEnv
      parallel::parLapply(cl,batch,dispatch)
    }
    records <- read_records()
    ids <- vapply(records,function(r)r$replicate,integer(1))
    pending <- setdiff(seq_len(replicates),ids)
  }
  records <- read_records()
  good <- records[vapply(records,function(r)is.null(r$error),logical(1))]
  detail <- if (length(good)) do.call(rbind,lapply(good,`[[`,"rows")) else data.frame()
  errors <- do.call(rbind,lapply(records,function(r) if (!is.null(r$error))
    data.frame(replicate=r$replicate,error=r$error,seconds=r$seconds)))
  warning_rows <- do.call(rbind,lapply(records,function(r) if (length(r$warnings))
    data.frame(replicate=r$replicate,warning=r$warnings)))
  summary <- if (nrow(detail)) do.call(rbind,lapply(split(detail,
    list(detail$q_specification,detail$g_floor),drop=TRUE),function(x) {
      bias <- mean(x$estimate)-truth; variance <- stats::var(x$estimate)
      cov <- mean(x$covers_truth); n <- nrow(x)
      # Wilson interval avoids a falsely zero uncertainty report at 0%/100% coverage.
      t <- stats::qnorm(.975); den <- 1+t^2/n
      ctr <- (cov+t^2/(2*n))/den
      rad <- t*sqrt(cov*(1-cov)/n+t^2/(4*n^2))/den
      data.frame(q_specification=x$q_specification[1],g_floor=x$g_floor[1],true_att=truth,
        n_replicates=n,mean_estimate=mean(x$estimate),bias=bias,variance=variance,
        mse=bias^2+variance,empirical_sd=sqrt(variance),mean_estimated_se=mean(x$se),
        ci_coverage=cov,coverage_monte_carlo_se=sqrt(cov*(1-cov)/n),
        coverage_mc_lower=max(0,ctr-rad),coverage_mc_upper=min(1,ctr+rad),
        mean_frac_g_clipped=mean(x$frac_g_clipped))
    })) else data.frame()
  status <- data.frame(requested_total=replicates,attempted=length(records),completed=length(good),
    failed=length(records)-length(good),workers=workers,elapsed_this_call=proc.time()[3]-started,
    soft_budget_seconds=max_seconds,remaining_ids=length(pending),legacy_import=manifest$legacy_import,
    interpretation="conditional_fitted_DGP_diagnostic_not_actual_data_coverage_validation")
  ans <- list(summary=summary,replicates=detail,errors=errors,warnings=warning_rows,status=status)
  atomic_save_rds(ans,file.path(out,"results.rds"),overwrite=TRUE)
  for (nm in names(ans)) if (is.data.frame(ans[[nm]]) && ncol(ans[[nm]]))
    atomic_write_plain_csv(ans[[nm]],file.path(out,paste0(nm,".csv")),row.names=FALSE,overwrite=TRUE)
  message("Saved ",length(good)," successes and ",length(records)-length(good)," failures in ",out)
  if (length(good)<50L) warning("Fewer than 50 usable replicates; coverage remains imprecise.",call.=FALSE)
  invisible(ans)
}

# realized lower propensity bound. This is a literature-informed rule,
# not an estimated optimal bound or a guarantee of ATT interval coverage.
resolve_primary_g_bound <- function(cfg, n_observed) {
  rule <- cfg$final_tmle$g_lower_rule %||% "fixed"
  if (!rule %in% c("fixed", "sample_size")) stop("Unknown g_lower_rule.")
  if (!is.null(cfg$final_tmle$g_lower_runtime) &&
      isTRUE(all.equal(cfg$final_tmle$g_lower,cfg$final_tmle$g_lower_runtime$value)))
    cfg$final_tmle$g_lower <- cfg$final_tmle$g_lower_configured
  configured <- cfg$final_tmle$g_lower
  value <- configured
  if (rule == "sample_size") {
    if (length(n_observed)!=1L || !is.finite(n_observed) || n_observed<=1)
      stop("Sample-size g bound requires more than one observed outcome.")
    constant <- cfg$final_tmle$g_lower_sample_size_constant %||% 5
    if (!is.finite(constant) || constant<=0) stop("Invalid g bound constant.")
    value <- constant/(sqrt(n_observed)*log(n_observed))
  }
  if (!is.finite(value) || value<=0 || value>=cfg$final_tmle$g_upper)
    stop("Resolved propensity bounds are invalid for this sample.")
  cfg$final_tmle$g_lower_configured <- configured
  cfg$final_tmle$g_lower <- value
  cfg$final_tmle$g_lower_runtime <- list(rule=rule, value=value, n_observed=n_observed)
  cfg
}

adopt_fit_g_bound <- function(cfg, fit) {
  b <- fit$g_bound_runtime
  if (!is.null(b)) {
    cfg$final_tmle$g_lower_configured <- b$configured
    cfg$final_tmle$g_lower <- b$value
    cfg$final_tmle$g_lower_runtime <- b
  }
  cfg
}

# SuperLearner screening functions only route already processed columns.
# All raw-variable ranking and preprocessing is confined to the outer training
# sample. The added candidate never supplies additional inputs to g or pi.
screen.Q_core <- function(Y, X, family, obsWeights, id, ...) {
  !startsWith(names(X), "BROADQ__")
}
screen.Q_broad <- function(Y, X, family, obsWeights, id, ...) {
  names(X)=="A" | startsWith(names(X), "BROADQ__")
}

build_broad_Q_pack <- function(main_df, tr, te, rough_sel, sel_vars, cfg) {
  tab <- rough_sel$selection_table
  if (is.null(tab) || !all(c("variable","outcome_score") %in% names(tab)))
    stop("Broader Q candidate requires the nested training-only outcome scores.")
  tab <- tab[is.finite(tab$outcome_score),,drop=FALSE]
  tab <- tab[order(-tab$outcome_score,tab$variable),,drop=FALSE]
  candidates <- get_candidate_vars(main_df,cfg)
  # Retain the same governance, missingness and redundancy rules; relax the
  # tight joint-screen cap for ONE regularized outcome candidate only.
  ranked <- intersect(tab$variable,candidates)
  ranked <- head(ranked, as.integer(cfg$final_tmle$broad_Q_max_raw %||% 300L))
  vars <- unique(c(get_mandatory_W(cfg),ranked))
  if (length(vars)<=length(get_mandatory_W(cfg)))
    stop("No scored baseline candidates available to broaden Q.")
  cq <- cfg
  cap <- as.integer(cq$final_tmle$broad_Q_max_processed %||% 520L)
  cq$final_tmle$hard_max_processed_columns <- max(
    cq$final_tmle$hard_max_processed_columns,cap+100L)
  pack <- build_final_W_train_valid(main_df[tr,,drop=FALSE],main_df[te,,drop=FALSE],
    vars,cq,priority_table=NULL,processed_cap_override=cap)
  names(pack$train) <- paste0("BROADQ__",names(pack$train))
  names(pack$valid) <- paste0("BROADQ__",names(pack$valid))
  pack$marginal_scores <- stats::setNames(tab$outcome_score, as.character(tab$variable))
  pack
}

# Recombine the ORIGINAL Q candidates from their saved inner predictions. No
# learner refit, no validation-outcome selection, and no p-value selection.
core_Q_predictions <- function(fit,Y,w) {
  ix <- !grepl("_screen.Q_broad$",fit$libraryNames)
  if (!any(ix) || all(ix)) stop("Cannot identify the core Q library.")
  nm <- fit$libraryNames[ix]
  method <- SuperLearner::method.NNLS()
  co <- method$computeCoef(Z=fit$Z[,ix,drop=FALSE],Y=Y,obsWeights=w,
    libraryNames=nm,verbose=FALSE)
  p <- as.numeric(fit$library.predict[,ix,drop=FALSE] %*% co$coef)
  if (any(!is.finite(p))) stop("Invalid recombined core Q predictions.")
  list(pred=p,coef=co$coef,cvRisk=co$cvRisk)
}

# Recalibrate ALL counterfactual response blocks with the SAME training map.
# The returned copy is for sensitivity evaluation, not automatic primary selection.
recalibrate_saved_fit <- function(fit, cfg, g_method="current", pi_method="current") {
  methods <- c("current","identity","platt","isotonic")
  if (!g_method %in% methods || !pi_method %in% methods) stop("Invalid calibration method.")
  n <- length(fit$att_components$A)
  for (target in c("g","pi")) {
    method <- if (target=="g") g_method else pi_method
    if (method=="current") next
    fields <- if (target=="g") "gn" else c("pi_AW","pi_1W","pi_0W")
    u <- lapply(fields,function(nm) fit[[paste0(nm,"_uncalibrated")]])
    if (any(vapply(u,function(x) length(x)!=n || any(!is.finite(x)),logical(1))))
      stop("Missing or invalid uncalibrated ",target," predictions.")
    p <- u
    if (method!="identity") for (v in sort(unique(fit$outer_fold))) {
      ar <- fit$calibration_training_by_fold[[v]][[target]]
      if (is.null(ar$data)) stop("Missing calibration archive for fold ",v," target ",target)
      d <- ar$data; te <- which(fit$outer_fold==v)
      if (any(d$respondent_id %in% fit$respondent_ids[te]) ||
          any(d$psu %in% as.character(fit$att_components$cluster[te])))
        stop("Calibration training/validation overlap.")
      map <- fit_probability_calibrator(d$score,d$label,d$weight,method)
      for (j in seq_along(fields)) p[[j]][te] <- predict_probability_calibrator(map,u[[j]][te])
    }
    for (j in seq_along(fields)) {
      nm <- fields[j]
      fit[[paste0(nm,"_raw")]] <- p[[j]]
      fit$att_components[[paste0(nm,"_raw")]] <- p[[j]]
      lo <- if (target=="g") cfg$final_tmle$g_lower else cfg$final_tmle$pi_lower
      hi <- if (target=="g") cfg$final_tmle$g_upper else cfg$final_tmle$pi_upper
      fit[[nm]] <- fit$att_components[[nm]] <- pmin(pmax(p[[j]],lo),hi)
    }
  }
  fit
}

retarget_saved_calibration <- function(checkpoint_path,g_method="current",pi_method="current") {
  z <- restore_completed_fit(checkpoint_path)
  cc <- adopt_fit_g_bound(z$cfg,z$tmle_fit)
  f <- recalibrate_saved_fit(z$tmle_fit,cc,g_method,pi_method)
  estimate_joint_att_under_clips(f,cc,cc$final_tmle$g_lower,cc$final_tmle$g_upper,
    cc$final_tmle$pi_lower,cc$final_tmle$pi_upper,paste0("g_",g_method,"_pi_",pi_method))
}

build_offline_calibration_comparison <- function(fit,cfg) {
  cfg <- adopt_fit_g_bound(cfg,fit)
  # Nine cheap combinations; primary is separately reconstructed as a gate.
  grid <- expand.grid(g=c("identity","platt","isotonic"),
    pi=c("identity","platt","isotonic"),stringsAsFactors=FALSE)
  rows <- lapply(seq_len(nrow(grid)),function(i) {
    f <- recalibrate_saved_fit(fit,cfg,grid$g[i],grid$pi[i])
    ans <- estimate_joint_att_under_clips(f,cfg,cfg$final_tmle$g_lower,
      cfg$final_tmle$g_upper,cfg$final_tmle$pi_lower,cfg$final_tmle$pi_upper,
      paste0("g_",grid$g[i],"_pi_",grid$pi[i]))
    ans$g_calibration <- grid$g[i]; ans$pi_calibration <- grid$pi[i]
    ans$interpretation <- "fixed_fitted_nuisance_sensitivity_no_automatic_selection"
    ans
  })
  do.call(rbind,rows)
}

build_offline_calibration_losses <- function(fit,cfg) {
  cfg <- adopt_fit_g_bound(cfg,fit)
  rows <- list()
  for (target in c("g","pi")) for (method in c("identity","platt","isotonic")) {
    f <- if (target=="g") recalibrate_saved_fit(fit,cfg,g_method=method) else
      recalibrate_saved_fit(fit,cfg,pi_method=method)
    f$calibration_region_reference_g <- fit$gn_raw
    x <- build_calibration_region_diagnostic(f,cfg)
    x <- x[x$target==target,,drop=FALSE]
    x$calibration <- method
    rows[[length(rows)+1L]] <- x
  }
  do.call(rbind,rows)
}

build_broad_Q_comparison <- function(fit,cfg) {
  if (is.null(fit$Q_core_predictions)) return(data.frame(status="broader_Q_disabled"))
  cfg <- adopt_fit_g_bound(cfg,fit)
  f <- fit; p <- fit$Q_core_predictions
  if (!all(dim(p)==c(length(f$respondent_ids),3L)) || any(!is.finite(p)))
    stop("Core Q predictions missing or misaligned.")
  f$Qeligible1W <- p[,1]; f$Qeligible0W <- p[,2]
  eval <- function(z,label) estimate_joint_att_under_clips(z,cfg,
    cfg$final_tmle$g_lower,cfg$final_tmle$g_upper,cfg$final_tmle$pi_lower,
    cfg$final_tmle$pi_upper,label)
  rbind(eval(fit,"Q_expanded_library"),eval(f,"Q_original_library_same_other_nuisances"))
}

# Descriptive joint residual check; does not select adjustment variables, estimate
# residual ATT bias, or propagate nuisance-estimation uncertainty into a test.
build_joint_baseline_residual <- function(fit,cfg) {
  a <- fit$att_components
  vars <- intersect(c("PA12","H1ED12","H1GI4","H1GH49"),names(fit$review_data))
  if (!length(vars)) return(data.frame(status="review_variables_unavailable"))
  d <- fit$review_data[vars]
  for (v in vars) {
    k <- missing_masks_from_rule(d[[v]],get_missing_rule(d[[v]],cfg$preprocessing,v))
    d[[v]] <- factor(ifelse(k$general,"Missing",ifelse(k$skip,"Skip",as.character(d[[v]]))))
  }
  d$error <- a$Y_bounded_orig-(fit$Qeligible0W*fit$y_range+fit$y_lower)
  d$keep <- a$A==0 & fit$eligibility==1 & a$delta_Y==1
  vars <- vars[vapply(d[vars],function(x) length(unique(x[d$keep]))>1L,logical(1))]
  if (!length(vars)) return(data.frame(status="no_varying_review_variables"))
  frame <- fit$survey_design_frame
  j <- match(as.character(frame[[cfg$analysis$id_var]]),as.character(fit$respondent_ids))
  d <- cbind(frame,d[j,,drop=FALSE]); d$keep[is.na(j)] <- FALSE
  d$weight <- frame[[cfg$analysis$weight_var]]/ifelse(is.na(j),1,a$pi_AW[j])
  design <- survey::svydesign(stats::reformulate(cfg$analysis$cluster_var),
    strata=stats::reformulate(cfg$analysis$strata_var),weights=~weight,data=d,nest=TRUE)
  fit_r <- survey::svyglm(stats::reformulate(vars,"error"),subset(design,keep))
  co <- coef(summary(fit_r)); jt <- survey::regTermTest(fit_r,stats::reformulate(vars))
  data.frame(term=rownames(co),estimate=co[,1],se=co[,2],statistic=co[,3],
    p_value=co[,4],joint_p=as.numeric(jt$p),n_controls=sum(d$keep),
    psu_controls=length(unique(d[[cfg$analysis$cluster_var]][d$keep])),
    interpretation="fixed_fit_descriptive_residual_association_not_ATT_bias",
    stringsAsFactors=FALSE)
}

# Point-effect thresholds remain useful when the primary CI already crosses zero.
build_att_mnar_point_thresholds <- function(fit,cfg) {
  cmp <- mnar_fixed_nuisance_components(fit$att_components)
  targets <- unique(c(0,cmp$psi_att/2,cfg$diagnostics$mnar_point_targets %||% numeric()))
  if (any(!is.finite(targets))) stop("MNAR point targets must be finite.")
  directions <- list(treated_only=c(1,0),control_only=c(0,1),opposing=c(1,-1))
  limit <- cmp$y_upper-cmp$y_lower
  rows <- list()
  for (target in targets) for (nm in names(directions)) {
    u <- directions[[nm]]
    fun <- function(x) cmp$psi_att+unname(mnar_net_shift_from_deltas(cmp,x*u[1],x*u[2])["net"])-target
    bounds <- c(fun(-limit),fun(limit))
    ok <- min(bounds)<=0 && max(bounds)>=0
    root <- if (abs(fun(0))<1e-10) 0 else if (ok)
      stats::uniroot(fun,c(-limit,limit),tol=1e-9)$root else NA_real_
    rows[[length(rows)+1L]] <- data.frame(target_att=target,direction=nm,
      delta1_outcome_units=root*u[1],delta0_outcome_units=root*u[2],
      delta_abs_sd=abs(root)/cmp$outcome_sd,attainable_within_support=ok,
      adjusted_att=if (is.finite(root)) target+fun(root) else NA_real_,
      primary_att=cmp$psi_att,
      interpretation="bounded_fixed_nuisance_point_threshold_not_confidence_threshold")
  }
  do.call(rbind,rows)
}

build_all_calibration_prognostic <- function(fit,cfg) {
  cfg <- adopt_fit_g_bound(cfg,fit)
  do.call(rbind,lapply(c("identity","platt","isotonic"),function(method) {
    z <- recalibrate_saved_fit(fit,cfg,g_method=method)
    ans <- build_prognostic_score_balance(z,cfg)
    ans$g_calibration <- method
    ans$interpretation <- "same_reference_score_descriptive_balance_not_estimated_bias"
    ans
  }))
}

run_v840_self_test <- function(cfg) {
  cc <- cfg; cc$final_tmle$g_lower_rule <- "sample_size"
  cc$final_tmle$g_lower_runtime <- NULL
  cc$final_tmle$g_lower <- .025
  frozen <- strip_runtime_config_state(cc)
  b <- resolve_primary_g_bound(cc,11374L)
  stopifnot(abs(b$final_tmle$g_lower-5/(sqrt(11374)*log(11374)))<1e-14,
    identical(strip_runtime_config_state(b),frozen),
    identical(resolve_primary_g_bound(b,11374L),b))
  changed <- b; changed$final_tmle$g_lower_rule <- "fixed"
  changed$final_tmle$g_lower <- .01
  stopifnot(resolve_primary_g_bound(changed,11374L)$final_tmle$g_lower==.01,
    strip_runtime_config_state(changed)$final_tmle$g_lower==.01)
  fixed <- cc; fixed$final_tmle$g_lower_rule <- "fixed"
  stopifnot(resolve_primary_g_bound(fixed,11374L)$final_tmle$g_lower==.025)
  X <- data.frame(A=0:1,W1=1:2,BROADQ__W1=1:2,BROADQ__W2=3:4)
  stopifnot(identical(screen.Q_core(X=X),c(TRUE,TRUE,FALSE,FALSE)),
    identical(screen.Q_broad(X=X),c(TRUE,FALSE,TRUE,TRUE)))
  invisible(TRUE)
}

# Aggregate-only comparisons of INITIAL outer-held-out Q predictions.
# Survey weights are applied once, inside review_ratio_summary().
# No diagnostic selects a learner, changes Q, or changes the reported ATT.
q_validation_multiplier <- function(A, S, observed, pi, g, arm) {
  n <- length(A)
  if (!arm %in% c("all","treated","control","att_control") || !n ||
      any(lengths(list(S,observed,pi,g))!=n) || anyNA(c(A,S,observed)) ||
      any(!A %in% 0:1) || any(!S %in% 0:1) || any(!observed %in% 0:1))
    stop("Invalid Q-validation domain or arm.",call.=FALSE)
  use <- S==1L & observed==1L
  if (arm=="treated") use <- use & A==1L
  if (arm %in% c("control","att_control")) use <- use & A==0L
  if (any(!is.finite(pi[use])) || any(pi[use]<=0 | pi[use]>1))
    stop("Invalid response probabilities on Q-validation rows.",call.=FALSE)
  mult <- numeric(n)
  mult[use] <- 1/pi[use]
  if (arm=="att_control") {
    if (any(!is.finite(g[use])) || any(g[use]<=0 | g[use]>=1))
      stop("Invalid ATT propensities on Q-validation rows.",call.=FALSE)
    mult[use] <- mult[use]*g[use]/(1-g[use])
  }
  if (any(!is.finite(mult))) stop("Nonfinite Q-validation weights.",call.=FALSE)
  mult
}

build_Q_validation_comparison <- function(fit,cfg,base,active,gain_name) {
  a <- fit$att_components
  n <- length(a$A)
  S <- fit$eligibility
  if (!n || any(lengths(list(base,active,S,a$Y_bounded_orig,a$delta_Y,
                            a$pi_AW,a$gn,a$weights))!=n) ||
      anyNA(c(S,a$delta_Y)) || any(!S %in% 0:1) || any(!a$delta_Y %in% 0:1) ||
      any(!is.finite(a$weights) | a$weights<=0))
    stop("Misaligned Q-validation inputs.",call.=FALSE)
  use <- S==1L & a$delta_Y==1L
  if (any(!is.finite(base[use])) || any(!is.finite(active[use])) ||
      any(!is.finite(a$Y_bounded_orig[use])))
    stop("Invalid observed outcomes or held-out Q predictions.",call.=FALSE)
  loss_base <- loss_active <- numeric(n)
  loss_base[use] <- (a$Y_bounded_orig[use]-base[use])^2
  loss_active[use] <- (a$Y_bounded_orig[use]-active[use])^2
  gain <- loss_base-loss_active
  out <- lapply(c("all","treated","control","att_control"),function(arm) {
    mult <- q_validation_multiplier(a$A,S,a$delta_Y,a$pi_AW,a$gn,arm)
    z <- review_ratio_summary(gain,mult,fit,cfg)
    keep <- mult>0
    weight <- a$weights[keep]*mult[keep]
    bm <- if (any(keep)) stats::weighted.mean(loss_base[keep],weight) else NA_real_
    am <- if (any(keep)) stats::weighted.mean(loss_active[keep],weight) else NA_real_
    ans <- data.frame(arm=arm,n=z$n,psus=z$psus,ess=z$ess,
      gain=z$estimate,paired_design_se=z$se,se_status=z$se_status,
      baseline_mse=bm,augmented_mse=am,
      loss_units=if (identical(fit$outcome_type %||% cfg$analysis$outcome_type,"binary"))
        "squared_probability_error" else "squared_outcome_units",
      weight_scope=if (arm=="att_control") "survey_times_ATT_odds_times_response_inverse" else
        "survey_times_response_inverse",
      prediction_scope="initial_outer_held_out;not_targeted_predictions",
      interpretation="positive_favors_augmented_Q;fixed_fit_descriptive_not_bias_or_selection_test",
      stringsAsFactors=FALSE)
    names(ans)[names(ans)=="gain"] <- gain_name
    ans
  })
  do.call(rbind,out)
}

build_broad_Q_validation <- function(fit,cfg) {
  if (is.null(fit$Q_core_predictions)) return(data.frame(status="broader_Q_disabled"))
  n <- length(fit$att_components$A)
  p <- fit$Q_core_predictions
  if (!is.matrix(p) || !identical(dim(p),c(as.integer(n),3L)))
    stop("Core-Q validation predictions are missing or misaligned.",call.=FALSE)
  core <- p[,3]*fit$y_range+fit$y_lower
  active <- ifelse(fit$att_components$A==1L,fit$Qeligible1W,fit$Qeligible0W)*
    fit$y_range+fit$y_lower
  build_Q_validation_comparison(fit,cfg,core,active,"core_minus_expanded_mse")
}

build_residual_Q_validation <- function(fit,cfg) {
  if (is.null(fit$Q_without_residual_predictions)) return(data.frame(status="residual_Q_disabled"))
  n <- length(fit$att_components$A)
  p <- fit$Q_without_residual_predictions
  if (!is.matrix(p) || !identical(dim(p),c(as.integer(n),3L)))
    stop("Pre-residual Q validation predictions are missing or misaligned.",call.=FALSE)
  base <- p[,3]*fit$y_range+fit$y_lower
  active <- ifelse(fit$att_components$A==1L,fit$Qeligible1W,fit$Qeligible0W)*
    fit$y_range+fit$y_lower
  build_Q_validation_comparison(fit,cfg,base,active,"baseline_minus_residual_mse")
}

# Fit an outcome residual-screening candidate using only the training
# outcomes supplied to it. Ranking, recipes and tuning are learned within
# that training partition. Treatment-effect estimates do not select features.


fit_residual_Q_candidate <- function(main_df, train_idx, valid_idx, Y, valid_A,
                                     cfg, seed, label="full", family=stats::gaussian()) {
  started <- proc.time()[3]
  valid_A <- normalize_binary_var(valid_A,"residual-Q validation A")
  if (!length(train_idx) || !length(valid_idx) || length(Y)!=length(train_idx) ||
      length(valid_A)!=length(valid_idx) || any(!is.finite(Y)) ||
      any(!valid_A %in% 0:1) || length(intersect(train_idx,valid_idx)))
    stop("Invalid/disjoint residual-Q training and prediction rows.")
  tr <- main_df[train_idx,,drop=FALSE]; te <- main_df[valid_idx,,drop=FALSE]
  av <- cfg$analysis$exposure_var
  A <- normalize_binary_var(tr[[av]],"residual-Q A")
  cl <- as.character(tr[[cfg$analysis$cluster_var]])
  if (length(intersect(unique(cl),unique(as.character(te[[cfg$analysis$cluster_var]])))))
    stop("Residual-Q training and validation share a PSU.")
  w <- normalize_positive_weights(tr[[cfg$analysis$weight_var]],length(Y),"residual-Q weights")
  candidates <- get_candidate_vars(main_df,cfg)
  pf <- prefilter_candidate_vars_for_screen(tr,candidates,cfg)
  mandatory <- get_mandatory_W(cfg)
  vars <- unique(c(mandatory,pf$vars))
  if (any(!vars %in% names(tr))) stop("Residual-Q mandatory covariate absent.")
  raw <- tr[,vars,drop=FALSE]
  mr <- learn_final_missing_recipe(raw,cfg$preprocessing)
  Xscreen <- apply_final_missing_recipe(raw,mr,cfg$preprocessing)
  k <- 2L
  sf <- make_rough_fold_ids(nrow(tr),k,seed,
    cluster_vec=cl,A_vec=A,weights=w,cluster_aware=TRUE,
    fold_control=fold_control_from_cfg(cfg,"internal"))
  score <- screen_gauss_linear(Y,Xscreen,K=max(sf),fold=sf,weights=w)
  rank_names <- function(sc) {
    keep <- intersect(vars,names(sc)[is.finite(sc)])
    keep[order(-sc[keep],keep)]
  }
  marginal <- setdiff(rank_names(score),mandatory)
  pilot_vars <- unique(c(mandatory,head(marginal,cfg$final_tmle$residual_Q_pilot_max_raw)))
  cc <- cfg
  cap <- as.integer(cfg$final_tmle$residual_Q_max_processed)
  cc$final_tmle$hard_max_processed_columns <- max(cc$final_tmle$hard_max_processed_columns,cap+100L)
  pilot <- build_final_W_train_valid(tr,tr,pilot_vars,cc,
    processed_cap_override=cap)
  Xp <- data.frame(A=A,pilot$train,check.names=FALSE)
  # Prediction on training rows is deliberate for ONE iterative screening step;
  # no claim is made that these residuals are independently validated outcomes.
  pfit <- with_sl_glmnet_context("Q",seed+10L,cfg$learners$glmnet$internal_folds %||% 5L,
    with_local_seed(seed+20L,SL.glmnet.fixed(Y,Xp,Xp,family,obsWeights=w,id=cl)))
  residual <- Y-as.numeric(pfit$pred)
  pilot_kept <- pilot$kept_raw_vars
  rm(pfit,Xp,pilot)
  rscore <- screen_gauss_linear(residual,Xscreen,K=max(sf),fold=sf,weights=w)
  rm(Xscreen,raw,mr,residual) # release the all-variable screen before the final dense design
  residual_order <- setdiff(rank_names(rscore),unique(c(mandatory,pilot_kept)))
  residual_order <- head(residual_order,cfg$final_tmle$residual_Q_add_max_raw)
  # Alternate new residual-ranked and pilot variables, so the existing column
  # budget cannot be consumed by the pilot before new candidates are considered.
  pilot_order <- setdiff(pilot_kept,mandatory)
  slots <- max(length(residual_order),length(pilot_order))
  interleave <- if (slots) as.vector(rbind(
    c(residual_order,rep(NA_character_,slots-length(residual_order))),
    c(pilot_order,rep(NA_character_,slots-length(pilot_order))))) else character()
  final_vars <- unique(c(mandatory,interleave[!is.na(interleave)]))
  pack <- build_final_W_train_valid(tr,te,final_vars,cc,
    processed_cap_override=cap)
  X <- data.frame(A=A,pack$train,check.names=FALSE)
  newX <- data.frame(A=valid_A,pack$valid,check.names=FALSE)
  fitted <- with_sl_glmnet_context("Q",seed+30L,cfg$learners$glmnet$internal_folds %||% 5L,
    with_local_seed(seed+40L,SL.glmnet.fixed(Y,X,newX,family,obsWeights=w,id=cl)))
  if (length(fitted$pred)!=length(valid_idx) || any(!is.finite(fitted$pred)))
    stop("Residual-Q produced invalid predictions.")
  audit <- data.frame(stage=label,variable=vars,
    marginal_score=unname(score[vars]),residual_score=unname(rscore[vars]),
    pilot_selected=vars %in% pilot_kept,
    residual_shortlisted=vars %in% residual_order,
    final_selected=vars %in% pack$kept_raw_vars,
    protected=vars %in% resolve_protected_W(cfg),stringsAsFactors=FALSE)
  info <- data.frame(stage=label,n_train=length(Y),n_valid=length(valid_idx),
    training_psus=length(unique(cl)),candidate_raw=length(vars),
    pilot_raw=length(pilot_kept),final_raw=length(pack$kept_raw_vars),
    final_columns=ncol(pack$train),new_raw=sum(!pack$kept_raw_vars %in% pilot_kept),
    seconds=proc.time()[3]-started,stringsAsFactors=FALSE)
  list(pred=as.numeric(fitted$pred),selection=audit,info=info,
       training_ids=as.character(tr[[cfg$analysis$id_var]]))
}

# Use outer-training ensemble residuals to screen a fixed feature design
# before inner whole-PSU CV. This is not fully nested screening-and-learner CV:
# inner predictions combined with weights learned from the same training
# outcomes are a training heuristic, not independent validation.
# Untouched outer PSUs provide the final held-out evaluation.
# The separately defined fully nested candidate is not the primary path.


validate_residual_Q_partition <- function(validRows, n, cluster) {
  if (!is.list(validRows) || length(validRows) < 2L || length(cluster) != n ||
      anyNA(cluster) || any(!nzchar(as.character(cluster))))
    stop("Residual-Q requires at least two valid inner PSU partitions.", call. = FALSE)
  good <- vapply(validRows, function(ii) {
    is.numeric(ii) && length(ii) > 0L && all(is.finite(ii)) &&
      all(ii == as.integer(ii)) && all(ii >= 1L & ii <= n) && !anyDuplicated(ii)
  }, logical(1))
  if (!all(good)) stop("Invalid residual-Q inner validation indices.", call. = FALSE)
  all_rows <- unlist(validRows, use.names = FALSE)
  if (length(all_rows) != n || any(tabulate(all_rows, nbins = n) != 1L))
    stop("Residual-Q inner validation rows do not partition training rows.", call. = FALSE)
  for (ii in validRows) {
    learn <- setdiff(seq_len(n), ii)
    if (!length(learn) || length(intersect(unique(cluster[learn]), unique(cluster[ii]))))
      stop("Residual-Q inner training and validation share a PSU.", call. = FALSE)
  }
  invisible(TRUE)
}

interleave_residual_Q_variables <- function(mandatory, base_vars, residual_vars) {
  mandatory <- unique(as.character(mandatory))
  base_vars <- setdiff(unique(as.character(base_vars)), mandatory)
  residual_vars <- setdiff(unique(as.character(residual_vars)), c(mandatory, base_vars))
  slots <- max(length(base_vars), length(residual_vars))
  if (!slots) return(mandatory)
  z <- as.vector(rbind(
    c(residual_vars, rep(NA_character_, slots - length(residual_vars))),
    c(base_vars, rep(NA_character_, slots - length(base_vars)))))
  unique(c(mandatory, z[!is.na(z)]))
}

validate_residual_Q_archive <- function(ar) {
  if (!identical(ar$schema, "residual_Q_training_v2"))
    stop("Unexpected residual-Q archive schema.", call. = FALSE)
  n <- length(ar$Y)
  ids <- as.character(ar$observed_training_ids)
  psu <- as.character(ar$observed_training_psus)
  if (!n || length(ids) != n || length(psu) != n || anyNA(ids) ||
      anyDuplicated(ids) || length(ar$weights) != n ||
      any(!is.finite(ar$weights) | ar$weights <= 0) || any(!is.finite(ar$Y)))
    stop("Invalid residual-Q archive training rows.", call. = FALSE)
  validate_residual_Q_partition(ar$inner_valid_rows, n, psu)
  if (length(ar$candidate_training_ids) != length(ar$inner_valid_rows))
    stop("Residual-Q archive omits coefficient-training IDs.", call. = FALSE)
  for (j in seq_along(ar$inner_valid_rows)) {
    expected <- ids[setdiff(seq_len(n), ar$inner_valid_rows[[j]])]
    if (!identical(as.character(ar$candidate_training_ids[[j]]), expected))
      stop("Residual-Q coefficient-training IDs do not match the inner partition.", call. = FALSE)
  }
  if (any(ar$outer_validation_ids %in% ar$screening_training_ids) ||
      any(ar$outer_validation_psus %in% ar$screening_training_psus) ||
      !all(ids %in% ar$screening_training_ids))
    stop("Residual-Q archive has an outer screening/evaluation overlap.", call. = FALSE)
  if (!is.matrix(ar$Z) || nrow(ar$Z) != n ||
      !is.matrix(ar$library_predictions) ||
      nrow(ar$library_predictions) != 3L * length(ar$outer_validation_ids) ||
      ncol(ar$Z) != length(ar$library_names) ||
      ncol(ar$library_predictions) != ncol(ar$Z) ||
      !identical(colnames(ar$Z), ar$library_names) ||
      !identical(colnames(ar$library_predictions), ar$library_names) ||
      any(!is.finite(ar$Z)) || any(!is.finite(ar$library_predictions)) ||
      length(ar$coefficient) != ncol(ar$Z) ||
      any(!is.finite(ar$coefficient)) || any(ar$coefficient < -1e-12) ||
      abs(sum(ar$coefficient) - 1) > 1e-8)
    stop("Invalid residual-Q archive prediction or coefficient matrix.", call. = FALSE)
  if (!identical(ar$validation_scope, "outer_screen_fixed_design_inner_PSU_CV") ||
      !identical(ar$pilot_prediction_scope,
        "inner_base_predictions_outer_training_screen_and_mixture;training_heuristic"))
    stop("Residual-Q archive misstates its validation scope.", call. = FALSE)
  invisible(TRUE)
}

augment_Q_with_residual_candidate <- function(Q_fit,main_df,tr,te,idx_obs,Y,
                                               validRows,cfg,seed,fold,
                                               family=stats::gaussian(),Q_broad_pack=NULL) {
  started <- proc.time()[3]
  if (!isTRUE(cfg$final_tmle$residual_Q_candidate))
    stop("Residual-Q augmentation called while disabled.", call. = FALSE)
  if (is.null(Q_broad_pack))
    stop("Residual-Q augmentation requires broad_Q_candidate=TRUE.", call. = FALSE)
  valid_index <- function(ii, upper) {
    is.numeric(ii) && length(ii) > 0L && all(is.finite(ii)) &&
      all(ii == as.integer(ii)) && all(ii >= 1L & ii <= upper) && !anyDuplicated(ii)
  }
  if (!is.data.frame(main_df) || !valid_index(tr, nrow(main_df)) ||
      !valid_index(te, nrow(main_df)) || length(intersect(tr, te)) ||
      !valid_index(idx_obs, length(tr)))
    stop("Invalid residual-Q outer training, validation or observed-row indices.", call. = FALSE)
  obs <- tr[idx_obs]
  n <- length(obs)
  id_var <- cfg$analysis$id_var
  cluster_var <- cfg$analysis$cluster_var
  weight_var <- cfg$analysis$weight_var
  exposure_var <- cfg$analysis$exposure_var
  required <- c(id_var, cluster_var, weight_var, exposure_var)
  if (!all(required %in% names(main_df)))
    stop("Residual-Q is missing design fields.", call. = FALSE)
  ids_all <- as.character(main_df[[id_var]][c(tr, te)])
  psu_train <- as.character(main_df[[cluster_var]][tr])
  psu_valid <- as.character(main_df[[cluster_var]][te])
  if (anyNA(ids_all) || anyDuplicated(ids_all) || any(!nzchar(ids_all)) ||
      anyNA(c(psu_train, psu_valid)) || any(!nzchar(c(psu_train, psu_valid))) ||
      length(intersect(unique(psu_train), unique(psu_valid))))
    stop("Residual-Q outer observations or PSUs overlap or are invalid.", call. = FALSE)
  cl <- psu_train[idx_obs]
  validate_residual_Q_partition(validRows, n, cl)
  if (!family$family %in% c("gaussian", "binomial") || length(Y) != n ||
      any(!is.finite(Y)) || (family$family == "binomial" && any(!Y %in% 0:1)))
    stop("Residual-Q received an unsupported family or invalid outcome.", call. = FALSE)
  if (!is.data.frame(Q_broad_pack$train) || !is.data.frame(Q_broad_pack$valid) ||
      nrow(Q_broad_pack$train) != length(tr) || nrow(Q_broad_pack$valid) != length(te) ||
      !identical(names(Q_broad_pack$train), names(Q_broad_pack$valid)) ||
      !length(Q_broad_pack$kept_raw_vars))
    stop("Broad-Q pack is absent or misaligned with the outer fold.", call. = FALSE)

  nm_base <- as.character(Q_fit$libraryNames)
  Z_base <- Q_fit$Z
  P_base <- Q_fit$library.predict
  if (!length(nm_base) || anyNA(nm_base) || anyDuplicated(nm_base) ||
      !is.matrix(Z_base) || nrow(Z_base) != n || ncol(Z_base) != length(nm_base) ||
      !is.matrix(P_base) || nrow(P_base) != 3L * length(te) ||
      ncol(P_base) != length(nm_base) ||
      (!is.null(colnames(Z_base)) && !identical(colnames(Z_base), nm_base)) ||
      !identical(colnames(P_base), nm_base) ||
      any(!is.finite(Z_base)) || any(!is.finite(P_base)))
    stop("Existing Q prediction matrices are invalid or misaligned.", call. = FALSE)
  # SuperLearner 2.0-29 returns an UNNAMED Z in libraryNames order.
  # A missing dimname is valid; an inconsistent supplied dimname is not.
  colnames(Z_base) <- nm_base
  if (!is.null(Q_fit$validRows) &&
      !identical(lapply(Q_fit$validRows,as.integer),lapply(validRows,as.integer)))
    stop("Residual-Q inner folds differ from the existing Q folds.",call.=FALSE)
  beta <- Q_fit$coef
  if (!is.null(names(beta))) {
    if (anyDuplicated(names(beta)) || !setequal(names(beta), nm_base))
      stop("Existing Q coefficient names do not match its library.", call. = FALSE)
    beta <- beta[nm_base]
  }
  beta <- as.numeric(beta)
  if (length(beta) != ncol(Z_base) || any(!is.finite(beta)) ||
      any(beta < -1e-12) || abs(sum(beta) - 1) > 1e-8)
    stop("Existing Q mixture coefficients are invalid.", call. = FALSE)
  # Keep the original survey-weight scale for NNLS risk/archive compatibility.
  # The existing screening/glmnet wrappers normalize their own fitting weights.
  w <- as.numeric(main_df[[weight_var]][obs])
  if (length(w)!=n || any(!is.finite(w) | w<=0))
    stop("Residual-Q requires positive finite survey weights.",call.=FALSE)
  # Training-side mixture predictions guide screening. No outer-validation
  # outcome is used by this heuristic.
  base_prediction <- as.numeric(Z_base %*% beta)
  residual <- as.numeric(Y) - base_prediction
  tr_df <- main_df[tr, , drop = FALSE]
  obs_df <- main_df[obs, , drop = FALSE]
  te_df <- main_df[te, , drop = FALSE]
  A <- normalize_binary_var(obs_df[[exposure_var]], "residual-Q training A")
  A_te <- normalize_binary_var(te_df[[exposure_var]], "residual-Q validation A")
  candidates <- get_candidate_vars(tr_df, cfg)
  pf <- prefilter_candidate_vars_for_screen(obs_df, candidates, cfg)
  mandatory <- get_mandatory_W(cfg)
  base_vars <- unique(as.character(Q_broad_pack$kept_raw_vars))
  if (!all(c(mandatory, base_vars) %in% names(tr_df)) ||
      !all(base_vars %in% unique(c(mandatory, candidates))))
    stop("Broad-Q starting variables fail the current candidate governance.", call. = FALSE)
  vars <- unique(c(mandatory, pf$vars))
  raw <- obs_df[, vars, drop = FALSE]
  recipe <- learn_final_missing_recipe(raw, cfg$preprocessing)
  Xscreen <- apply_final_missing_recipe(raw, recipe, cfg$preprocessing)
  sf <- make_rough_fold_ids(n, 2L, seed,
    cluster_vec=cl, A_vec=A, weights=w, cluster_aware=TRUE,
    fold_control=fold_control_from_cfg(cfg, "internal"))
  score <- screen_gauss_linear(residual, Xscreen, K=max(sf), fold=sf, weights=w)
  ranked <- intersect(vars, names(score)[is.finite(score)])
  ranked <- ranked[order(-score[ranked], ranked)]
  residual_vars <- head(setdiff(ranked, unique(c(mandatory, base_vars))),
    as.integer(cfg$final_tmle$residual_Q_add_max_raw))
  final_vars <- interleave_residual_Q_variables(mandatory, base_vars, residual_vars)
  rm(raw, recipe, Xscreen, residual)

  cc <- cfg
  cap <- as.integer(cfg$final_tmle$residual_Q_max_processed)
  cc$final_tmle$hard_max_processed_columns <- max(cc$final_tmle$hard_max_processed_columns,
                                                cap + 100L)
  # Reuse broad-Q's SELECTED RAW VARIABLES. Rebuild the JOINT recipe once on
  # the same outer-training rows to enforce the cap on whole raw-variable
  # groups after interleaving; simply cbind-ing two capped matrices is wrong.
  pack <- build_final_W_train_valid(tr_df, te_df, final_vars, cc,
                                    priority_table=NULL, processed_cap_override=cap)
  X <- data.frame(A=A, pack$train[idx_obs, , drop=FALSE], check.names=FALSE)
  newX <- rbind(
    data.frame(A=rep(1L,length(te)), pack$valid, check.names=FALSE),
    data.frame(A=rep(0L,length(te)), pack$valid, check.names=FALSE),
    data.frame(A=A_te, pack$valid, check.names=FALSE))
  if (any(!is.finite(as.matrix(X))) || any(!is.finite(as.matrix(newX))) ||
      !identical(names(X),names(newX)) || pack$n_processed_optional_budgeted > cap)
    stop("Residual-Q fixed design is invalid or exceeds its optional-column budget.", call.=FALSE)
  screen_seconds <- proc.time()[3] - started
  tuning_folds <- as.integer(cfg$learners$glmnet$internal_folds %||% 5L)
  if (!is.finite(tuning_folds) || tuning_folds < 3L)
    stop("Residual-Q requires at least three glmnet tuning folds.",call.=FALSE)

  # Fit the ORIGINAL outcome, not an additive residual correction. These calls
  # use the existing wrapper, existing inner PSU partitions and unchanged
  # elastic-net/lambda settings. Only the feature design stays fixed in inner CV.
  fit_one <- function(learn, predict_X, stage, seed_offset) {
    t0 <- proc.time()[3]
    ans <- with_sl_glmnet_context("Q", seed + 30L, tuning_folds,
      with_local_seed(seed + seed_offset,
        SL.glmnet.fixed(Y[learn], X[learn,,drop=FALSE], predict_X, family,
                        obsWeights=w[learn], id=cl[learn])))
    pp <- as.numeric(ans$pred)
    if (length(pp)!=nrow(predict_X) || any(!is.finite(pp)) ||
        is.null(ans$fit) || isTRUE(ans$fit$fallback %||% FALSE) ||
        (family$family=="binomial" && any(pp<0 | pp>1)))
      stop("Invalid residual-Q fixed-design fit in ",stage,".",call.=FALSE)
    list(pred=pp, seconds=unname(proc.time()[3]-t0))
  }
  oof <- rep(NA_real_,n)
  coefficient_ids <- vector("list",length(validRows))
  infos <- vector("list",length(validRows)+1L)
  info_row <- function(stage,learn,n_valid,seconds,screen_time=0) {
    data.frame(stage=stage,n_train=length(learn),n_valid=n_valid,
      training_psus=length(unique(cl[learn])),candidate_raw=length(unique(c(vars,base_vars))),
      pilot_raw=length(base_vars),final_raw=length(pack$kept_raw_vars),
      final_columns=ncol(pack$train),new_raw=sum(!pack$kept_raw_vars %in% base_vars),
      seconds=seconds+screen_time,screen_seconds=screen_time,fit_seconds=seconds,
      selection_scope="outer_training_fixed_design",pilot_refitted=FALSE,
      unpenalized_predictors="A",A_penalty_factor=0,
      stringsAsFactors=FALSE)
  }
  for (j in seq_along(validRows)) {
    val <- validRows[[j]]
    learn <- setdiff(seq_len(n),val)
    z <- fit_one(learn,X[val,,drop=FALSE],paste0("inner_",j),40L+1000L*j)
    oof[val] <- z$pred
    coefficient_ids[[j]] <- as.character(obs_df[[id_var]][learn])
    infos[[j]] <- info_row(paste0("inner_",j),learn,length(val),z$seconds)
  }
  full <- fit_one(seq_len(n),newX,"outer_full",9000L)
  infos[[length(infos)]] <- info_row("outer_full",seq_len(n),nrow(newX),
                                    full$seconds,screen_seconds)
  if (any(!is.finite(oof))) stop("Residual-Q inner predictions are incomplete.",call.=FALSE)
  nm <- "SL.glmnet.residual_Q_All"
  Z <- cbind(Z_base,oof); colnames(Z) <- c(nm_base,nm)
  P <- cbind(P_base,full$pred); colnames(P) <- colnames(Z)
  method <- SuperLearner::method.NNLS()
  co <- method$computeCoef(Z=Z,Y=Y,obsWeights=w,libraryNames=colnames(Z),verbose=FALSE)
  coefficient <- stats::setNames(as.numeric(co$coef),colnames(Z))
  if (length(coefficient)!=ncol(Z) || any(!is.finite(coefficient)) ||
      any(coefficient < -1e-12) || abs(sum(coefficient)-1)>1e-8 ||
      length(co$cvRisk)!=ncol(Z) || any(!is.finite(co$cvRisk)))
    stop("Residual-Q augmented ensemble has invalid coefficients or risks.",call.=FALSE)
  pred <- as.numeric(P %*% coefficient)
  if (length(pred)!=3L*length(te) || any(!is.finite(pred)))
    stop("Invalid augmented Q prediction.",call.=FALSE)
  audit_vars <- unique(c(base_vars,vars))
  marginal_scores <- Q_broad_pack$marginal_scores
  marginal <- if (is.null(marginal_scores)) rep(NA_real_,length(audit_vars)) else
    unname(marginal_scores[audit_vars])
  audit <- data.frame(stage="outer_fixed",variable=audit_vars,
    marginal_score=marginal,residual_score=unname(score[audit_vars]),
    pilot_selected=audit_vars %in% base_vars,
    residual_shortlisted=audit_vars %in% residual_vars,
    final_selected=audit_vars %in% pack$kept_raw_vars,
    protected=audit_vars %in% resolve_protected_W(cfg),fold=fold,
    pilot_source="existing_broad_Q_selection;no_separate_pilot_fit",
    selection_scope="outer_training_fixed_design",stringsAsFactors=FALSE)
  info <- do.call(rbind,infos)
  info$fold <- fold
  info$ensemble_weight <- unname(coefficient[nm])
  archive <- list(schema="residual_Q_training_v2",fold=fold,
    observed_training_ids=as.character(obs_df[[id_var]]),
    observed_training_psus=cl,
    outer_validation_ids=as.character(te_df[[id_var]]),
    outer_validation_psus=psu_valid,
    screening_training_ids=as.character(tr_df[[id_var]]),
    screening_training_psus=psu_train,
    residual_screen_training_ids=as.character(obs_df[[id_var]]),
    inner_valid_rows=validRows,candidate_training_ids=coefficient_ids,
    full_candidate_training_ids=as.character(obs_df[[id_var]]),
    Y=Y,weights=w,Z=Z,library_predictions=P,library_names=colnames(Z),
    coefficient=coefficient,pilot_coefficients=stats::setNames(beta,nm_base),
    pilot_predictions=base_prediction,kept_raw_vars=pack$kept_raw_vars,
    processed_raw_map=pack$processed_raw_map,
    optional_columns=pack$n_processed_optional_budgeted,optional_column_budget=cap,
    validation_scope="outer_screen_fixed_design_inner_PSU_CV",
    pilot_prediction_scope=
      "inner_base_predictions_outer_training_screen_and_mixture;training_heuristic")
  validate_residual_Q_archive(archive)
  # Count validation/stacking/archive overhead once, not in every inner stage.
  overhead <- max(0,unname(proc.time()[3]-started)-sum(info$seconds))
  info$seconds[nrow(info)] <- info$seconds[nrow(info)] + overhead
  info$other_seconds <- 0
  info$other_seconds[nrow(info)] <- overhead
  list(pred=pred,candidate_pred=full$pred,coef=coefficient,
    cvRisk=stats::setNames(as.numeric(co$cvRisk),colnames(Z)),
    selection=audit,info=info,training=archive)
}

build_residual_Q_comparison <- function(fit,cfg) {
  if (is.null(fit$Q_without_residual_predictions)) return(data.frame(status="residual_Q_disabled"))
  cfg <- adopt_fit_g_bound(cfg,fit)
  p <- fit$Q_without_residual_predictions
  if (!all(dim(p)==c(length(fit$respondent_ids),3L)) || any(!is.finite(p))) stop("Invalid original-Q comparison.")
  old <- fit; old$Qeligible1W <- p[,1]; old$Qeligible0W <- p[,2]
  eval <- function(f,label) estimate_joint_att_under_clips(f,cfg,
    cfg$final_tmle$g_lower,cfg$final_tmle$g_upper,cfg$final_tmle$pi_lower,cfg$final_tmle$pi_upper,label)
  rbind(eval(fit,"Q_with_residual_candidate"),eval(old,"Q_v840_library_same_other_nuisances"))
}


build_residual_Q_selection <- function(fit,cfg) {
  if (is.null(fit$residual_Q_audit_by_fold)) return(data.frame(status="residual_Q_disabled"))
  do.call(rbind,lapply(fit$residual_Q_audit_by_fold,`[[`,"selection"))
}
build_residual_Q_fit_log <- function(fit,cfg) {
  if (is.null(fit$residual_Q_audit_by_fold)) return(data.frame(status="residual_Q_disabled"))
  do.call(rbind,lapply(fit$residual_Q_audit_by_fold,`[[`,"info"))
}

# Two-part MNAR sensitivity for UNRESOLVED Wave-IV hours/LFP observations.
# This is a complementary, fixed-fit PATTERN-PERTURBATION analysis, not a new
# primary estimator and not a new fitted observation/status model.
#
# Let U=1 identify an observed unresolved-status AND unobserved outcome pattern.
# For a=0,1, q_a(W) is the saved targeted eligible-outcome mean on its ORIGINAL
# outcome scale. In U, replace its reference distribution by a mixture with
# mean m_a(W;z_a,d_a) = z_a*0 + (1-z_a)*clip(q_a(W)+d_a,L,H).
# z_a is the EXTRA structural-zero mixture fraction, NOT total P(Y=0).
# The nonstructural component includes ordinary nonworkers/other genuine zeros.
# z=d=0 exactly recovers q; no division by 1-z or identification of an unknown
# existing structural-zero share is asserted. z=0 means no EXTRA zero component;
# it does not assert that the baseline predictive distribution has no genuine zeros.
#
# Standardize the perturbation to the ATT target using OBSERVED pattern weights:
# D1 = sum[w*A*U*(m1-q1)] / sum[w*A]
# D0 = sum[w*(1-A)*g/(1-g)*U*(m0-q0)] / sum[w*A]
# scenario ATT = primary ATT + D1 - D0.
# This follows E[f(W,U(0))|A=1] = E[(1-A)g(W)/(1-g(W))*f(W,U)]/P(A=1)
# under the corresponding exchangeability/positivity assumptions, and uses no
# outcome-response inverse weight because U already denotes the missing pattern.
#
# This is deliberately DIFFERENT from mnar_points, which integrates fitted
# s_a(W)*(1-pi_a(W)) over treated covariates. Neither leverage estimator is known
# to equal the truth. This output adds an observed-pattern sensitivity, keeps
# both approaches labelled, and never overwrites the existing MNAR outputs.
# Documented zeros, observed outcomes and eligible-interview missing outcomes
# are not recoded or included in U. Counterfactual patterns require assumptions;
# actual status is not assumed invariant to exposure.
#
# Only scenario point estimates are reported. No nominal scenario CI is formed
# by shifting the primary CI or by pretending the nuisance/shift uncertainty is
# known. Scenario means outside the aggregate support are flagged, not clipped.
# General pattern-mixture motivation, not a theorem for this custom estimator:
# https://doi.org/10.1111/j.0006-341X.2000.01241.x
# https://doi.org/10.1002/sim.8569

mnar_zero_mixture_mean <- function(q, zero_fraction, delta, lower, upper) {
  scalar <- function(x) is.numeric(x) && length(x)==1L && is.finite(x)
  if (!is.numeric(q) || any(!is.finite(q)) ||
      !scalar(zero_fraction) || zero_fraction<0 || zero_fraction>1 ||
      !scalar(delta) || !scalar(lower) || !scalar(upper) ||
      upper<=lower || lower>0 || upper<0 ||
      any(q<lower-1e-8 | q>upper+1e-8))
    stop("Invalid two-part MNAR mixture inputs.",call.=FALSE)
  # q is already on the configured support; allow rounding tolerance only.
  q <- pmin(pmax(q,lower),upper)
  remainder_mean <- pmin(pmax(q+delta,lower),upper)
  list(mean=(1-zero_fraction)*remainder_mean,
       nonstructural_mean=remainder_mean,
       support_clipped=(q+delta<lower | q+delta>upper))
}

build_att_mnar_two_part <- function(fit,cfg) {
  if (!isTRUE(cfg$diagnostics$enable_mnar_two_part %||% TRUE) ||
      !isTRUE(cfg$diagnostics$enable_mnar_pattern_mixture %||% TRUE))
    return(data.frame(status="disabled"))
  family <- cfg$outcome$family
  wave <- cfg$outcome$current_wave %||% cfg$outcome$waves
  if (!family %in% c("HoursWorked","LaborForceParticipation") ||
      length(wave)!=1L || is.na(wave) || wave!=4L)
    return(data.frame(status="not_applicable",reason="Wave_IV_hours_or_LFP_only"))
  a <- fit$att_components
  d <- fit$status_records
  if (is.null(a) || is.null(d) || !nrow(d))
    return(data.frame(status="unavailable",reason="saved_status_records_required"))
  required <- c("A","weights","gn","delta_Y","eligibility","structural_zero",
    "Qeligible1W_targeted_orig","Qeligible0W_targeted_orig",
    "psi_att","mu1_att","mu0_att","y_lower","y_upper")
  if (!all(required %in% names(a)))
    stop("Two-part MNAR requires complete sequential ATT components.",call.=FALSE)
  scalar_fields <- c("psi_att","mu1_att","mu0_att","y_lower","y_upper")
  if (any(vapply(a[scalar_fields],function(x) !is.numeric(x) || length(x)!=1L ||
                 !is.finite(x),logical(1))) ||
      abs(a$psi_att-(a$mu1_att-a$mu0_att))>1e-7*max(1,abs(a$psi_att)))
    stop("Two-part MNAR ATT component means do not reconcile.",call.=FALSE)
  id_var <- cfg$analysis$id_var
  if (!all(c(id_var,"reason","status_zero") %in% names(d)))
    stop("Two-part MNAR status fields are missing.",call.=FALSE)
  ids <- as.character(fit$respondent_ids)
  source_ids <- as.character(d[[id_var]])
  if (anyNA(ids) || anyDuplicated(ids) || anyNA(source_ids) || anyDuplicated(source_ids))
    stop("Two-part MNAR requires unique nonmissing respondent IDs.",call.=FALSE)
  j <- match(ids,source_ids)
  if (anyNA(j)) stop("Two-part MNAR status records are not aligned.",call.=FALSE)
  d <- d[j,,drop=FALSE]
  A <- a$A; w <- as.numeric(a$weights); g <- as.numeric(a$gn)
  n <- length(A)
  if (!n || any(lengths(list(ids,w,g,a$delta_Y,a$eligibility,a$structural_zero,
                            a$Qeligible1W_targeted_orig,a$Qeligible0W_targeted_orig))!=n) ||
      anyNA(c(A,a$delta_Y,a$eligibility,a$structural_zero,d$reason)) ||
      any(!A %in% 0:1) || any(!a$delta_Y %in% 0:1) ||
      any(!a$eligibility %in% 0:1) || any(!is.finite(w) | w<=0) ||
      any(!is.finite(g) | g<=0 | g>=1))
    stop("Two-part MNAR has invalid or misaligned analysis vectors.",call.=FALSE)
  U <- d$reason=="unresolved" & a$delta_Y==0L & a$eligibility==1L
  if (any(U & (a$structural_zero | d$status_zero %in% 1L)))
    stop("A documented structural zero entered the unresolved MNAR pattern.",call.=FALSE)
  if (!any(U)) return(data.frame(status="no_unresolved_missing_outcomes"))
  # Normalizing survey weights makes scale invariance explicit.
  w <- w/mean(w)
  denominator <- sum(w[A==1L])
  if (!is.finite(denominator) || denominator<=0)
    stop("Two-part MNAR has no treated survey mass.",call.=FALSE)
  i1 <- which(U & A==1L); i0 <- which(U & A==0L)
  w1 <- w[i1]/denominator
  w0 <- w[i0]*g[i0]/(1-g[i0])/denominator
  q1 <- as.numeric(a$Qeligible1W_targeted_orig[i1])
  q0 <- as.numeric(a$Qeligible0W_targeted_orig[i0])
  lo <- as.numeric(a$y_lower); hi <- as.numeric(a$y_upper)
  if (length(lo)!=1L || length(hi)!=1L || !is.finite(lo) || !is.finite(hi) ||
      lo>0 || hi<0 || hi<=lo || any(!is.finite(c(q1,q0))) ||
      any(c(q1,q0)<lo-1e-8 | c(q1,q0)>hi+1e-8))
    stop("Two-part MNAR is inconsistent with the outcome support.",call.=FALSE)
  zgrid <- cfg$diagnostics$mnar_two_part_zero_grid %||% c(0,0.05,0.10,0.20)
  dgrid <- cfg$diagnostics$mnar_two_part_delta_grid
  if (is.null(dgrid)) dgrid <- if (family=="HoursWorked") c(-10,-5,0,5,10) else
    c(-0.10,-0.05,0,0.05,0.10)
  if (!is.numeric(zgrid) || !length(zgrid) || any(!is.finite(zgrid)) ||
      any(zgrid<0 | zgrid>1) || !any(zgrid==0) ||
      !is.numeric(dgrid) || !length(dgrid) || any(!is.finite(dgrid)) || !any(dgrid==0))
    stop("Two-part MNAR grids must be finite and contain the zero reference.",call.=FALSE)
  zgrid <- sort(unique(zgrid)); dgrid <- sort(unique(dgrid))
  if (length(zgrid)^2*length(dgrid)^2>10000L)
    stop("Two-part MNAR grid exceeds its 10000-scenario safety limit.",call.=FALSE)
  grid <- expand.grid(zero1=zgrid,zero0=zgrid,delta1=dgrid,delta0=dgrid,
                      KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
  mean_w <- function(x,ww) if(length(ww) && sum(ww)>0) sum(x*ww)/sum(ww) else NA_real_
  rows <- lapply(seq_len(nrow(grid)),function(k) {
    s <- grid[k,,drop=FALSE]
    m1 <- mnar_zero_mixture_mean(q1,s$zero1,s$delta1,lo,hi)
    m0 <- mnar_zero_mixture_mean(q0,s$zero0,s$delta0,lo,hi)
    shift1 <- sum(w1*(m1$mean-q1)); shift0 <- sum(w0*(m0$mean-q0))
    mu1 <- a$mu1_att+shift1; mu0 <- a$mu0_att+shift0
    att <- a$psi_att+shift1-shift0
    reference <- s$zero1==0 && s$zero0==0 && s$delta1==0 && s$delta0==0
    feasible <- is.finite(mu1) && is.finite(mu0) &&
      mu1>=lo-1e-8 && mu1<=hi+1e-8 && mu0>=lo-1e-8 && mu0<=hi+1e-8
    data.frame(status=if(feasible) "evaluated" else "aggregate_support_violation",
      reference=reference,zero1=s$zero1,zero0=s$zero0,
      delta1_outcome_units=s$delta1,delta0_outcome_units=s$delta0,
      outcome_units=if(family=="HoursWorked") "hours_per_week" else "probability",
      primary_att=a$psi_att,mu1_shift=shift1,mu0_shift=shift0,
      adjusted_att=att,adjusted_mu1=mu1,adjusted_mu0=mu0,
      aggregate_support_ok=feasible,
      n_unresolved_treated=length(i1),n_unresolved_control=length(i0),
      unresolved_control_ATT_ess=if(length(w0)) kish_eff_n(w0) else 0,
      unresolved_treated_survey_ess=if(length(w1)) kish_eff_n(w1) else 0,
      unresolved_mass1=sum(w1),unresolved_mass0=sum(w0),
      excluded_eligible_missing=sum(a$delta_Y==0L & d$reason=="eligible"),
      documented_zeros_unchanged=sum(a$structural_zero),
      reference_mean1=mean_w(q1,w1),reference_mean0=mean_w(q0,w0),
      shifted_nonstructural_mean1=mean_w(m1$nonstructural_mean,w1),
      shifted_nonstructural_mean0=mean_w(m0$nonstructural_mean,w0),
      support_clip_fraction1=mean_w(as.numeric(m1$support_clipped),w1),
      support_clip_fraction0=mean_w(as.numeric(m0$support_clipped),w0),
      perturbation_scope="unresolved_status_AND_missing_outcome_only",
      leverage_method="observed_pattern_ATT_standardization;not_fitted_s_times_1_minus_pi",
      zero_parameter="extra_structural_zero_mixture;not_total_probability_of_zero",
      inference="fixed_fit_scenario_point_estimate;no_scenario_CI",
      interpretation="conditional_sensitivity_not_imputation_or_identified_status_distribution",
      stringsAsFactors=FALSE)
  })
  out <- do.call(rbind,rows)
  ref <- which(out$reference)
  if (length(ref)!=1L || abs(out$adjusted_att[ref]-a$psi_att)>1e-10 ||
      abs(out$mu1_shift[ref])+abs(out$mu0_shift[ref])>1e-10)
    stop("Two-part MNAR zero reference does not reproduce the primary ATT.",call.=FALSE)
  if (any(!out$aggregate_support_ok))
    warning("Some fixed-fit two-part MNAR scenarios violate aggregate outcome support; these rows are flagged, not clipped.",call.=FALSE)
  out
}


# Fast numerical/structural tests added to the normal preflight. They do not use
# restricted records and do not replace the existing end-to-end preflight.
run_v842_self_test <- function(cfg_template) {
  fail <- function(expr) inherits(try(force(expr),silent=TRUE),"try-error")
  psu <- rep(letters[1:6],each=2L)
  vr <- list(1:4,5:8,9:12)
  validate_residual_Q_partition(vr,12L,psu)
  stopifnot(fail(validate_residual_Q_partition(list(c(1:4,1),5:8,9:12),12L,psu)),
    fail(validate_residual_Q_partition(list(1:4,5:8,c(9,10,11,13)),12L,psu)),
    fail(validate_residual_Q_partition(list(c(1,3,5,7),c(2,4,6,8),9:12),12L,psu)))
  stopifnot(identical(interleave_residual_Q_variables("M",c("M","B1","B2"),
    c("R1","R2","B1","M")),c("M","R1","B1","R2","B2")))
  stopifnot(identical(interleave_residual_Q_variables("M",c("M","B1"),character()),
    c("M","B1")))

  A <- c(0L,0L,1L,0L,0L)
  S <- c(1L,1L,1L,0L,1L)
  R <- c(1L,1L,1L,1L,0L)
  pi <- c(.5,1,.8,NA_real_,NA_real_)
  g <- c(.2,.5,.4,NA_real_,NA_real_)
  ordinary <- q_validation_multiplier(A,S,R,pi,g,"control")
  att <- q_validation_multiplier(A,S,R,pi,g,"att_control")
  stopifnot(isTRUE(all.equal(ordinary,c(2,1,0,0,0),tolerance=1e-12)),
    isTRUE(all.equal(att,c(.5,1,0,0,0),tolerance=1e-12)),
    fail(q_validation_multiplier(A,S,R,pi,replace(g,1L,1),"att_control")))
  # Different association of weights with errors cannot be recovered from
  # ordinary MSE alone; check ATT weighting by direct hand arithmetic.
  sw <- c(2,3,4,5,6); gain <- c(4,-2,0,0,0)
  stopifnot(abs(sum(sw*att*gain)/sum(sw*att)-(-.5))<1e-12)

  q <- c(0,10,40,120)
  stopifnot(identical(mnar_zero_mixture_mean(q,0,0,0,120)$mean,q),
    identical(mnar_zero_mixture_mean(q,1,50,0,120)$mean,rep(0,4)),
    isTRUE(all.equal(mnar_zero_mixture_mean(q,.2,5,0,120)$mean,
      c(4,12,36,96),tolerance=1e-12)),
    fail(mnar_zero_mixture_mean(q,-.1,0,0,120)),
    fail(mnar_zero_mixture_mean(q,0,0,1,120)))
  # Binary support, empty arm, and ordinary zero outcomes remain well-defined.
  b <- mnar_zero_mixture_mean(c(0,.2,.8,1),.1,.3,0,1)
  stopifnot(all(b$mean>=0 & b$mean<=1),
    length(mnar_zero_mixture_mean(numeric(),0,0,0,1)$mean)==0L)

  cc <- cfg_template
  cc$outcome$family <- "HoursWorked"; cc$outcome$current_wave <- 4L
  cc$diagnostics$enable_mnar_two_part <- TRUE
  cc$diagnostics$enable_mnar_pattern_mixture <- TRUE
  cc$diagnostics$mnar_two_part_zero_grid <- c(0,.2)
  cc$diagnostics$mnar_two_part_delta_grid <- c(0,5)
  ids <- as.character(1:6)
  # Rows 1 and 3 are unresolved; row 5 is a known structural zero and row 6
  # is an eligible-interview missing value. Only rows 1 and 3 may shift.
  ac <- list(A=c(1L,1L,0L,0L,1L,0L),weights=c(2,1,3,1,1,1),
    gn=rep(.5,6),delta_Y=c(0L,1L,0L,1L,1L,0L),
    eligibility=c(1L,1L,1L,1L,0L,1L),
    structural_zero=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE),
    Qeligible1W_targeted_orig=rep(20,6),Qeligible0W_targeted_orig=rep(30,6),
    psi_att=-2,mu1_att=20,mu0_att=22,y_lower=0,y_upper=120)
  details <- data.frame(id=ids,
    reason=c("unresolved","eligible","unresolved","eligible","incarcerated","eligible"),
    status_zero=c(0L,0L,0L,0L,1L,0L),stringsAsFactors=FALSE)
  names(details)[1L] <- cc$analysis$id_var
  fit <- list(att_components=ac,respondent_ids=ids,status_records=details)
  # Compare VALUES. serialize() bytes change when R expands the deferred
  # as.character(1:6) vector on first use, although the object is unchanged.
  snapshot <- unserialize(serialize(fit,NULL))
  x <- build_att_mnar_two_part(fit,cc)
  stopifnot(sum(x$reference)==1L,x$adjusted_att[x$reference]==-2,
    all(x$excluded_eligible_missing==1L),all(x$documented_zeros_unchanged==1L),
    identical(snapshot,fit))
  r <- x$zero1==.2 & x$zero0==0 & x$delta1_outcome_units==0 & x$delta0_outcome_units==0
  # Treated denominator is 2+1+1=4; hidden-zero shift is (2/4)*(-4)=-2.
  stopifnot(sum(r)==1L,abs(x$adjusted_att[r]-(-4))<1e-12)
  r <- x$zero1==0 & x$zero0==.2 & x$delta1_outcome_units==0 & x$delta0_outcome_units==0
  # Control perturbation is (3/4)*(-6)=-4.5; ATT increases by 4.5.
  stopifnot(sum(r)==1L,abs(x$adjusted_att[r]-2.5)<1e-12)
  scaled <- fit; scaled$att_components$weights <- 37*ac$weights
  xs <- build_att_mnar_two_part(scaled,cc)
  stopifnot(isTRUE(all.equal(x$adjusted_att,xs$adjusted_att,tolerance=1e-12)))
  reordered <- fit; reordered$status_records <- details[6:1,,drop=FALSE]
  stopifnot(isTRUE(all.equal(x,build_att_mnar_two_part(reordered,cc),tolerance=1e-12)))
  bad <- fit; bad$status_records$status_zero[1L] <- 1L
  stopifnot(fail(build_att_mnar_two_part(bad,cc)))
  cc$outcome$family <- "Compensation"
  stopifnot(build_att_mnar_two_part(fit,cc)$status=="not_applicable")
  cat("v8.42 partition, ATT-loss, and two-part MNAR numerical tests passed.\n")
  invisible(TRUE)
}

# Verify the actual output of the existing 1200-row continuous preflight,
# including serialization and exact reproduction of stored candidate blocks.
run_v842_postfit_self_test <- function(fit,cfg) {
  before <- serialize(fit,NULL)
  if (isTRUE(cfg$final_tmle$residual_Q_candidate)) {
    for (ar in fit$residual_Q_training_by_fold) {
      validate_residual_Q_archive(ar)
      idx <- match(ar$outer_validation_ids,as.character(fit$respondent_ids))
      if (anyNA(idx)) stop("Preflight: archived outer IDs do not align.",call.=FALSE)
      v <- pmin(pmax(ar$library_predictions[,ncol(ar$library_predictions)],
        cfg$final_tmle$Q_pred_lower %||% .005),cfg$final_tmle$Q_pred_upper %||% .995)
      if (!isTRUE(all.equal(unname(matrix(v,ncol=3L)),
        unname(fit$Q_residual_candidate_predictions[idx,,drop=FALSE]),tolerance=1e-12)))
        stop("Preflight: residual candidate counterfactual blocks changed order.",call.=FALSE)
      base <- ar$Z[,-ncol(ar$Z),drop=FALSE]
      if (!isTRUE(all.equal(as.numeric(base %*% ar$pilot_coefficients),
                           ar$pilot_predictions,tolerance=1e-12)))
        stop("Preflight: archived ensemble screening signal is inconsistent.",call.=FALSE)
    }
    q <- build_residual_Q_validation(fit,cfg)
    if (!identical(q$arm,c("all","treated","control","att_control")) ||
        anyDuplicated(names(q)) || any(!is.finite(q$baseline_minus_residual_mse)))
      stop("Preflight: aggregate residual-Q diagnostics failed.",call.=FALSE)
  }
  q <- build_broad_Q_validation(fit,cfg)
  if (isTRUE(cfg$final_tmle$broad_Q_candidate) &&
      (!identical(q$arm,c("all","treated","control","att_control")) ||
       any(!is.finite(q$core_minus_expanded_mse))))
    stop("Preflight: aggregate broad-Q diagnostics failed.",call.=FALSE)
  # Diagnostics must not mutate fitted values or archives.
  if (!identical(before,serialize(fit,NULL)))
    stop("Preflight: v8.42 diagnostics modified the fitted object.",call.=FALSE)
  path <- tempfile(fileext=".rds")
  on.exit(unlink(path),add=TRUE)
  saveRDS(list(schema="addhealth_completed_fit_v1",cfg=cfg,tmle_fit=fit),path)
  restored <- restore_completed_fit(path)
  if (!identical(restored$tmle_fit,fit))
    stop("Preflight: v8.42 checkpoint round trip failed.",call.=FALSE)
  cat("v8.42 continuous integration, aggregate diagnostics and checkpoint tests passed.\n")
  invisible(TRUE)
}

# A small additional binary fixture exercises the candidate path, not a
# production refit or seed sensitivity. The main preflight already exercises
# the complete enabled production library on its continuous fixture.
run_v842_binary_candidate_test <- function(cfg_template) {
  if (!isTRUE(cfg_template$final_tmle$residual_Q_candidate)) return(invisible(TRUE))
  on.exit(register_custom_learners(cfg_template),add=TRUE)
  with_local_seed(84203L, {
    cc <- cfg_template
    cc$analysis$protected_W_mode <- "summary"
    cc$causal_governance$additional_mandatory_W <- character()
    cc$learners$glmnet$nlambda <- 20L
    cc$learners$glmnet$internal_folds <- 3L
    cc$final_tmle$residual_Q_max_processed <- 24L
    cc$final_tmle$residual_Q_add_max_raw <- 4L
    cc$final_tmle$hard_max_processed_columns <- 150L
    cc$final_tmle$rough_prefilter_two_arm_min_n <- 0L
    cc$final_tmle$fold_max_attempts <- 30L
    n <- 320L
    df <- data.frame(AID=as.character(seq_len(n)),PSUSCID=rep(seq_len(40L),each=8L),
      REGION=rep(rep(1:4,length.out=40L),each=8L),
      GSWGT1=rep(c(1,1.5,2,2.5),length.out=n),
      Depressed=rep(c(0L,0L,1L,0L,1L,0L,1L,0L),40L),
      Y=rep(c(0L,1L,0L,1L,1L,0L,1L,0L),40L),delta_Y=1L,
      MHSum_W1=sample(0:40,n,replace=TRUE),W1=stats::rnorm(n),
      W2=stats::rnorm(n),W3=sample(1:4,n,replace=TRUE),stringsAsFactors=FALSE)
    # Resolve configured design field names without changing production names.
    names(df)[match(c("AID","PSUSCID","REGION","GSWGT1","Depressed","Y","delta_Y"),names(df))] <-
      c(cc$analysis$id_var,cc$analysis$cluster_var,cc$analysis$strata_var,
        cc$analysis$weight_var,cc$analysis$exposure_var,cc$analysis$outcome_var,
        cc$analysis$outcome_observed_var)
    cv <- c("MHSum_W1","W1","W2","W3")
    registry <- data.frame(source="synthetic_wave1",raw_variable=cv,
      canonical_variable=canonical_join_variable_name(cv),derived_from=NA_character_,
      stringsAsFactors=FALSE)
    registry <- validate_variable_source_registry(registry)
    attr(df,"variable_source_registry") <- registry
    cc$preprocessing$variable_source_registry <- registry
    cc$preprocessing$global_missing_dictionary <- build_global_missing_code_dictionary(
      df[,cv,drop=FALSE],cv,cc$preprocessing)
    attr(df,"global_missing_dictionary") <- cc$preprocessing$global_missing_dictionary
    tr <- 1:240; te <- 241:320; obs <- seq_along(tr)
    vr <- list(1:80,81:160,161:240)
    broad <- build_final_W_train_valid(df[tr,,drop=FALSE],df[te,,drop=FALSE],
      c("MHSum_W1","W1"),cc,processed_cap_override=12L)
    # Fixed external pilot predictions make this a test of the augmentation,
    # not an assertion about the performance of a trained baseline ensemble.
    p <- stats::plogis(-.2+.2*df$W1)
    Z <- cbind(rep(.5,length(tr)),p[tr])
    P <- cbind(rep(.5,3L*length(te)),rep(p[te],3L))
    lib <- c("SL.mean_All","SL.glmnet.fixed_screen.Q_broad")
    # Match the real package: Z has no dimnames; library.predict has names.
    colnames(P) <- lib
    old <- list(Z=Z,library.predict=P,libraryNames=lib,
      coef=stats::setNames(c(.4,.6),lib),validRows=vr)
    register_custom_learners(cc)
    Y <- df[[cc$analysis$outcome_var]][tr]
    rng <- .Random.seed
    z1 <- with_local_seed(84204L,augment_Q_with_residual_candidate(
      old,df,tr,te,obs,Y,vr,cc,84205L,1L,stats::binomial(),Q_broad_pack=broad))
    stopifnot(identical(.Random.seed,rng))
    changed <- df
    changed[[cc$analysis$outcome_var]][te] <- 1L-df[[cc$analysis$outcome_var]][te]
    z2 <- with_local_seed(84204L,augment_Q_with_residual_candidate(
      old,changed,tr,te,obs,Y,vr,cc,84205L,1L,stats::binomial(),Q_broad_pack=broad))
    if (!isTRUE(all.equal(z1$pred,z2$pred,tolerance=1e-12)) ||
        !identical(z1$selection,z2$selection) ||
        !isTRUE(all.equal(z1$coef,z2$coef,tolerance=1e-12)) ||
        any(z1$pred<0 | z1$pred>1) || nrow(z1$info)!=4L ||
        z1$training$optional_columns>24L)
      stop("Preflight: binary/outer-label-isolation residual-Q test failed.",call.=FALSE)
    validate_residual_Q_archive(z1$training)
  })
  # Restore the registered wrappers to the caller's unchanged configuration.
  register_custom_learners(cfg_template)
  cat("v8.42 binary candidate, outer-label isolation and RNG tests passed.\n")
  invisible(TRUE)
}


# focused regression tests. These do not access restricted Add Health data.
run_v843_self_test <- function(cfg_template) {
  fail <- function(expr) inherits(try(force(expr),silent=TRUE),"try-error")
  same <- function(a,b) isTRUE(all.equal(a,b,tolerance=1e-12,check.attributes=FALSE))
  cc <- cfg_template
  cc$analysis$protected_W_mode <- "summary"
  cc$causal_governance$additional_mandatory_W <- character(0)
  cc$causal_governance$excluded_candidate_sources <- "birth_records"
  d <- data.frame(AID=as.character(1:8),PSUSCID=rep(1:4,each=2),REGION=rep(1:2,each=4),
    GSWGT1=rep(1,8),Depressed=rep(0:1,4),Y=1:8,delta_Y=1L,MHSum_W1=1:8,
    WSAFE=1:8,UNREGISTERED_W=8:1,RIWA001=rep(0:1,4),BR_EXTRA=1:8,
    BR_DERIVED=1:8,BR_CHILD=1:8,check.names=FALSE)
  d[["RIWA001_missA.x"]] <- 0L
  d[["BR_EXTRA.x.y"]] <- 1:8
  d[["BROADQ__BR_CHILD_miss97"]] <- 0L
  base <- setdiff(names(d),c("UNREGISTERED_W","RIWA001","BR_EXTRA","BR_DERIVED","BR_CHILD",
    "RIWA001_missA.x","BR_EXTRA.x.y","BROADQ__BR_CHILD_miss97"))
  reg <- data.frame(source=c(rep("wave1_inhome",length(base)),"birth_records","birth_records",
                             "wave1_inhome","derived"),
    raw_variable=c(base,"RIWA001","BR_EXTRA","BR_DERIVED","BR_CHILD"),
    canonical_variable=c(base,"RIWA001","BR_EXTRA","BR_DERIVED","BR_CHILD"),
    derived_from=c(rep(NA_character_,length(base)+2L),"BR_EXTRA","BR_DERIVED"),
    stringsAsFactors=FALSE)
  reg <- validate_variable_source_registry(reg)
  attr(d,"variable_source_registry") <- reg
  cc$preprocessing$variable_source_registry <- reg
  before <- serialize(d,NULL)
  a <- build_candidate_alias_audit(d,cc)
  candidates <- get_candidate_vars(d,cc)
  banned <- c("RIWA001","BR_EXTRA","BR_DERIVED","BR_CHILD","RIWA001_missA.x",
              "BR_EXTRA.x.y","BROADQ__BR_CHILD_miss97")
  stopifnot(!any(banned %in% candidates),all(c("MHSum_W1","WSAFE","UNREGISTERED_W") %in% candidates))
  stopifnot(all(a$excluded_candidate_source[match(banned,a$actual_variable)]))
  stale <- a; stale$eligible_candidate_W[match("BR_DERIVED",stale$actual_variable)] <- TRUE
  stopifnot(fail(validate_candidate_governance(d,cc,stale)))
  stopifnot(fail(assert_no_excluded_source_W(d,cc,c("WSAFE","BR_CHILD"))))
  stopifnot(fail(build_final_W_train_valid(d,d,c("WSAFE","RIWA001"),cc)))
  # Without registry entries, the 14 known birth-record names still cannot leak.
  d2 <- d; attr(d2,"variable_source_registry") <- empty_variable_source_registry()
  stopifnot(excluded_candidate_source_mask(d2,cc,"RIWA001_missA.x"))
  stopifnot(identical(before,serialize(d,NULL)))
  stopifnot(identical(source_candidate_key(c("RIWA001.x_missA.y","BROADQ__BR_EXTRA_miss97")),
                      c("RIWA001","BR_EXTRA")))

  p <- make_glmnet_penalty_factor(c("W1","A",".glmnet_pad"),"A")
  stopifnot(identical(unname(p),c(1,0,1)),identical(names(p),c("W1","A",".glmnet_pad")))
  stopifnot(all(make_glmnet_penalty_factor(c("W1","W2"))==1),
            fail(make_glmnet_penalty_factor(c("W1","W2"),"A")),
            fail(make_glmnet_penalty_factor(c("A","A"),"A")))
  had <- exists("glmnet_context",envir=.SL_RUNTIME_ENV,inherits=FALSE)
  old <- if(had) get("glmnet_context",envir=.SL_RUNTIME_ENV,inherits=FALSE) else NULL
  with_sl_glmnet_context("Q",4L,3L, {
    stopifnot(identical(.SL_RUNTIME_ENV$glmnet_context$unpenalized_columns,"A"))
    parent <- .SL_RUNTIME_ENV$glmnet_context
    stopifnot(fail(with_sl_glmnet_context("g",5L,3L,stop("intentional"))))
    stopifnot(identical(parent,.SL_RUNTIME_ENV$glmnet_context))
    with_sl_glmnet_context("g",6L,3L,
      stopifnot(identical(.SL_RUNTIME_ENV$glmnet_context$unpenalized_columns,"A")),unpenalized="A")
  })
  stopifnot(identical(had,exists("glmnet_context",envir=.SL_RUNTIME_ENV,inherits=FALSE)))
  if(had) stopifnot(identical(old,.SL_RUNTIME_ENV$glmnet_context))

  # Exercise actual run-tag construction and the row adapter on a saved-fit
  # shaped fixture. No nuisance fitting is required for these compatibility tests.
  cc$outcome$family <- "HoursWorked"; cc$outcome$current_wave <- 4L
  cc$outcome$family_member <- NULL; cc$global$run_label <- "seed1"
  tag1 <- build_run_tag(cc)
  cc$global$run_label <- "seed2"; tag2 <- build_run_tag(cc)
  stopifnot(identical(multiseed_analysis_tag(tag1,1L),multiseed_analysis_tag(tag2,2L)),
    identical(multiseed_analysis_tag("hrs_w4__seed1",1L),"hrs_w4"),
    fail(multiseed_analysis_tag(tag1,2L)))
  ac <- list(psi_att=-2,att_se=1,inference_df=20,inference_method="survey",
    A=c(0L,1L,0L,1L),Y_raw=c(1,2,3,4),Y_bounded_orig=c(1,2,3,4),
    delta_Y=rep(1L,4),weights=c(1,2,1,2),cluster=c("1","1","2","2"),
    strata=c("1","1","2","2"))
  frame <- data.frame(AID=as.character(1:4),PSUSCID=ac$cluster,REGION=ac$strata,
    GSWGT1=ac$weights,.analysis_domain=TRUE,stringsAsFactors=FALSE)
  f <- list(att_components=ac,result=data.frame(script_md5="source"),
    respondent_ids=as.character(1:4),eligibility=rep(1L,4),structural_zero=rep(FALSE,4),
    survey_design_frame=frame,y_lower=0,y_upper=120,outcome_type="continuous",
    composite_scope="test",outer_fold=c(1L,1L,2L,2L),
    selected_by_fold=list(c("WSAFE"),c("WSAFE","MHSum_W1")))
  pr <- list(summary=data.frame(run_tag=tag1,att_estimate=-2,att_se=1,
    family="HoursWorked",wave=4L,primary_estimand="att",stringsAsFactors=FALSE),
    results=stats::setNames(list(list(tmle_fit=f)),tag1))
  sig <- list(script=list(md5="source"),same_analysis="yes")
  pp <- prepare_multiseed_att_rows(pr,1L,cc,sig)
  pr2 <- pr; pr2$summary$run_tag <- tag2; names(pr2$results) <- tag2
  ord <- c(4,2,3,1)
  f2 <- f; f2$respondent_ids <- f$respondent_ids[ord]
  for (nm in c("A","Y_raw","Y_bounded_orig","delta_Y","weights","cluster","strata"))
    f2$att_components[[nm]] <- f$att_components[[nm]][ord]
  f2$eligibility <- f$eligibility[ord]; f2$structural_zero <- f$structural_zero[ord]
  f2$survey_design_frame <- frame[ord,,drop=FALSE]
  attr(f2$survey_design_frame,"cache_note") <- "different metadata"
  f2$outer_fold <- c(2L,1L,2L,1L)
  pr2$results[[tag2]]$tmle_fit <- f2
  pp2 <- prepare_multiseed_att_rows(pr2,2L,cc,sig)
  stopifnot(identical(pp$multisplit_sample_md5,pp2$multisplit_sample_md5),
    identical(pp$multisplit_analysis_tag,pp2$multisplit_analysis_tag),
    nrow(aggregate_multiseed_att(rbind(pp,pp2),1:2,1L))==1L)
  stopifnot(identical(names(extract_multiseed_selection_sets(pr,1L)),
                       names(extract_multiseed_selection_sets(pr2,2L))))
  badpr <- pr2; badpr$results[[tag2]]$tmle_fit$att_components$Y_raw[1L] <- 99
  badrows <- prepare_multiseed_att_rows(badpr,2L,cc,sig)
  stopifnot(!identical(pp$multisplit_sample_md5,badrows$multisplit_sample_md5),
    fail(aggregate_multiseed_att(rbind(pp,badrows),1:2,1L)))
  badpr <- pr; badpr$results[[tag1]]$tmle_fit$result$script_md5 <- "changed"
  stopifnot(fail(prepare_multiseed_att_rows(badpr,1L,cc,sig)))

  rows <- data.frame(seed=1:3,att_estimate=c(-2,-1,-3),att_se=c(1,1.5,.5),
    att_inference_df=20,att_inference_method="survey",multisplit_analysis_tag="hrs_w4",
    multisplit_spec_md5="spec",multisplit_source_md5="source",multisplit_sample_md5="sample",
    family="HoursWorked",wave=4L,family_member=NA_character_,primary_estimand="att",
    estimand="ATT_hours",cap_probability=NA_real_,stringsAsFactors=FALSE)
  z <- aggregate_multiseed_att(rows,1:3,1L)
  stopifnot(same(z$att_multisplit_estimate,-2),same(z$att_multisplit_variance,1.25),
    same(z$att_multisplit_se,sqrt(1.25)),
    same(z$att_multisplit_ci_lower,-2-stats::qt(.975,20)*sqrt(1.25)))
  stopifnot(same(z,aggregate_multiseed_att(rows[c(3,1,2),],3:1,1L)))
  shifted <- rows; shifted$att_estimate <- shifted$att_estimate+7
  zz <- aggregate_multiseed_att(shifted,1:3,1L)
  stopifnot(same(zz$att_multisplit_estimate,5),same(zz$att_multisplit_variance,1.25))
  scaled <- rows; scaled$att_estimate <- 4*scaled$att_estimate; scaled$att_se <- 4*scaled$att_se
  zz <- aggregate_multiseed_att(scaled,1:3,1L)
  stopifnot(same(zz$att_multisplit_estimate,-8),same(zz$att_multisplit_variance,20))
  single <- aggregate_multiseed_att(rows[1,,drop=FALSE],1L,1L)
  stopifnot(same(single$att_multisplit_se,1),same(single$att_multisplit_estimate,-2))
  stopifnot(fail(aggregate_multiseed_att(rows[-1,],1:3,1L)),
            fail(aggregate_multiseed_att(rbind(rows,rows[1,]),1:3,1L)),
            fail(validate_multiseed_ids(c(1,2.5))),fail(validate_multiseed_ids(c(1,1))))
  for(nm in c("multisplit_spec_md5","multisplit_source_md5","multisplit_sample_md5","att_inference_method")) {
    bad <- rows; bad[[nm]][1] <- "different"
    stopifnot(fail(aggregate_multiseed_att(bad,1:3,1L)))
  }
  bad <- rows; bad$att_inference_df[1] <- 19
  stopifnot(fail(aggregate_multiseed_att(bad,1:3,1L)))
  for(value in c(NA_real_,Inf,0,-1)) {
    bad <- rows; bad$att_se[1] <- value
    stopifnot(fail(aggregate_multiseed_att(bad,1:3,1L)))
  }
  other <- rows; other$family <- "LaborForceParticipation"; other$multisplit_analysis_tag <- "lfp_w4"
  both <- rbind(rows,other)
  stopifnot(nrow(aggregate_multiseed_att(both,1:3,1L))==2L,
    fail(aggregate_multiseed_att(both[-6,],1:3,1L)))
  path <- tempfile(fileext=".csv"); on.exit(unlink(path),add=TRUE)
  utils::write.csv(rows,path,row.names=FALSE)
  disk <- utils::read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
  for (nm in names(rows)) {
    if (is.character(rows[[nm]])) disk[[nm]] <- as.character(disk[[nm]])
    if (is.numeric(rows[[nm]])) disk[[nm]] <- as.numeric(disk[[nm]])
    if (is.integer(rows[[nm]])) disk[[nm]] <- as.integer(disk[[nm]])
    if (is.logical(rows[[nm]])) disk[[nm]] <- as.logical(disk[[nm]])
  }
  stopifnot(same(z,aggregate_multiseed_att(disk,1:3,1L)))
  # No actual multi-seed production fit is launched by these unit tests.
  cat("v8.43 source-governance, penalty-map/context and multi-split arithmetic tests passed.\n")
  invisible(TRUE)
}

run_v843_glmnet_self_test <- function(cfg_template) {
  if (!requireNamespace("glmnet",quietly=TRUE)) stop("glmnet is required for v8.43 preflight.",call.=FALSE)
  same <- function(a,b) isTRUE(all.equal(a,b,tolerance=1e-10,check.attributes=FALSE))
  cc <- cfg_template
  cc$learners$glmnet$nlambda <- 20L
  cc$learners$glmnet$internal_folds <- 3L
  cc$learners$Q$use_glmnet <- TRUE
  on.exit(register_custom_learners(cfg_template),add=TRUE)
  register_custom_learners(cc)
  with_local_seed(84301L, {
    n <- 360L
    cl <- rep(1:60,each=6)
    A <- rep(c(0L,0L,1L,0L,1L,0L),60)
    X <- data.frame(A=A,W1=stats::rnorm(n),W2=stats::rnorm(n))
    w <- rep(c(1,2,1,3,1,2),60)
    y <- .3+.4*A+.05*X$W1+stats::rnorm(n,sd=.04)
    newX <- rbind(transform(X,A=1L),transform(X,A=0L),X)
    fit <- with_sl_glmnet_context("Q",84302L,3L,
      SL.glmnet.fixed(y,X,newX,stats::gaussian(),obsWeights=w,id=as.character(cl)))
    stopifnot(identical(fit$fit$unpenalized_columns,"A"),
      identical(unname(fit$fit$penalty_factor[c("A","W1","W2")]),c(0,1,1)),
      all(is.finite(fit$pred)),length(fit$pred)==3L*n)
    # The wrapper handles factor-coded 0/1 A without accidentally exempting W.
    xf <- X; xf$A <- factor(xf$A,levels=c(1,0))
    nf <- newX; nf$A <- factor(nf$A,levels=c(1,0))
    fit_f <- with_sl_glmnet_context("Q",84302L,3L,
      SL.glmnet.fixed(y,xf,nf,stats::gaussian(),obsWeights=w,id=as.character(cl)))
    stopifnot(same(fit$pred,fit_f$pred),
              same(fit$pred,predict.SL.glmnet.fixed(fit$fit,nf)))
    yb <- rep(c(0L,1L,1L,0L,0L,1L),60)
    b <- with_sl_glmnet_context("Q",84303L,3L,
      SL.glmnet.fixed(yb,X,newX,stats::binomial(),obsWeights=w,id=as.character(cl)))
    stopifnot(all(is.finite(b$pred)),all(b$pred>=0 & b$pred<=1),b$fit$penalty_factor["A"]==0)
    pi_fit <- with_sl_glmnet_context("pi",84304L,3L,
      SL.glmnet.fixed(yb,X,newX,stats::binomial(),obsWeights=w,id=as.character(cl)))
    stopifnot(pi_fit$fit$penalty_factor["A"]==0)
    aux <- with_sl_glmnet_context("g",84305L,3L,
      SL.glmnet.fixed(yb,X,newX,stats::binomial(),obsWeights=w,id=as.character(cl)),unpenalized="A")
    stopifnot(aux$fit$penalty_factor["A"]==0)
    # The actual propensity model has no predictor A and keeps all penalties 1.
    W <- X[c("W1","W2")]
    g <- with_sl_glmnet_context("g",84306L,3L,
      SL.glmnet.fixed(A,W,W,stats::binomial(),obsWeights=w,id=as.character(cl)))
    stopifnot(all(g$fit$penalty_factor==1),length(g$fit$unpenalized_columns)==0L)
    # Compare penalized and unpenalized exposure fits at identical folds.
    wg <- normalize_positive_weights(w,n,"test weights")
    fd <- do.call(make_cluster_folds_balanced,c(list(cluster=as.character(cl),A=A,k=3L,
      seed=84306L,weights=wg,delta=rep(1L,n),
      balance_on_weights=isTRUE(cc$final_tmle$internal_fold_balance_on_weights)),
      fold_control_from_cfg(cc,"internal")))
    xm <- stats::model.matrix(~ . - 1,W,na.action=stats::na.pass)
    xx <- if(requireNamespace("Matrix",quietly=TRUE)) Matrix::Matrix(xm,sparse=TRUE) else xm
    direct <- glmnet::cv.glmnet(x=xx,y=A,family="binomial",alpha=cc$learners$glmnet$alpha,
      nlambda=cc$learners$glmnet$nlambda,weights=wg,foldid=fd,
      standardize=isTRUE(cc$learners$glmnet$standardize),maxit=cc$learners$glmnet$maxit)
    lam <- if(identical(cc$learners$glmnet$lambda_choice,"1se")) direct$lambda.1se else direct$lambda.min
    gp <- as.numeric(stats::predict(direct,newx=xm,s=lam,type="response"))
    stopifnot(same(g$pred,gp))
  })
  cat("v8.43 actual glmnet continuous/binary, factor-A, auxiliary and unchanged-g tests passed.\n")
  invisible(TRUE)
}


# focused regression tests (synthetic values only).
run_v844_self_test <- function(cfg_template) {
  same <- function(a, b, tol = 1e-9) isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
  fail <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
  # ---- calibrator selection ------------------------------------------------
  with_local_seed(84401L, {
    n <- 3000L; cl <- rep(1:60, each = 50L)
    lp <- -2.2 + 1.2 * stats::rnorm(n); y <- stats::rbinom(n, 1L, stats::plogis(lp))
    w <- stats::runif(n, .5, 2); b <- c(.005, .975)
    vr <- unname(split(seq_len(n), rep(1:3, length.out = 60L)[cl]))
    good <- stats::plogis(lp); squeezed <- stats::plogis(lp / 2.5)
    s1 <- select_probability_calibrator(good, y, w, vr, b, c("identity", "platt"))
    s2 <- select_probability_calibrator(squeezed, y, w, vr, b, c("identity", "platt"))
    pc <- pmin(pmax(good, b[1L]), b[2L]); wn <- w / mean(w)
    direct <- -sum(wn * (y * log(pc) + (1 - y) * log(1 - pc))) / sum(wn)
    stopifnot(same(unname(s1$loss["identity"]), direct),
              abs(s1$loss["platt"] - s1$loss["identity"]) < 0.002,   # nothing to gain when calibrated
              identical(s2$method, "platt"),                          # compressed scores are repaired
              s2$loss["identity"] - s2$loss["platt"] > 0.005,
              same(select_probability_calibrator(squeezed, y, 37 * w, vr, b)$loss, s2$loss))
    o <- sample.int(n); inv <- order(o)
    s3 <- select_probability_calibrator(squeezed[o], y[o], w[o], lapply(vr, function(ii) inv[ii]), b)
    stopifnot(same(s3$loss, s2$loss),
              fail(select_probability_calibrator(squeezed, y, w, list(1:10, 5:n), b)),
              fail(select_probability_calibrator(squeezed, y, w, vr, c(0, 1))),
              fail(select_probability_calibrator(squeezed, y, w, NULL, b)))
    mock <- list(Z = cbind(squeezed, squeezed), coef = c(.3, .7), validRows = vr)
    pv <- c(.02, .2, .6)
    c1 <- calibrate_outer_predictions(mock, y, w, pv, "select", 1L, "g", bounds = b)
    c2 <- calibrate_outer_predictions(mock, y, w, pv, "platt", 1L, "g", bounds = b)
    c3 <- calibrate_outer_predictions(mock, y, w, pv, "select", 1L, "g", bounds = b,
                                      candidates = "identity")
    stopifnot(identical(c1$audit$fitted_method, "platt"), same(c1$prediction, c2$prediction),
              is.finite(c1$audit$cv_logloss_identity), is.finite(c1$audit$cv_logloss_platt),
              is.na(c1$audit$cv_logloss_isotonic), is.na(c2$audit$cv_logloss_platt),
              identical(c3$audit$fitted_method, "identity"), same(c3$prediction, pv),
              identical(c1$audit$calibration_labels, "outer_training_only"),
              identical(names(c1$audit), names(c2$audit)))
    nofolds <- mock; nofolds$validRows <- NULL
    stopifnot(fail(calibrate_outer_predictions(nofolds, y, w, pv, "select", 1L, "g", bounds = b)))
  })
  # ---- control-arm Q learners ----------------------------------------------
  cc <- cfg_template
  cc$learners$glmnet$nlambda <- 20L; cc$learners$glmnet$internal_folds <- 3L
  cc$learners$ranger$num.trees <- 40L; cc$learners$ranger$always_split_A <- TRUE
  cc$learners$xgboost$ntrees <- 30L
  has <- c(glmnet = requireNamespace("glmnet", quietly = TRUE),
           ranger = requireNamespace("ranger", quietly = TRUE),
           xgboost = requireNamespace("xgboost", quietly = TRUE))
  cc$learners$Q$use_glmnet_control_arm <- has[["glmnet"]]
  cc$learners$Q$use_ranger_control_arm <- has[["ranger"]]
  cc$learners$Q$use_xgboost_control_arm <- has[["xgboost"]]
  on.exit(register_custom_learners(cfg_template), add = TRUE)
  register_custom_learners(cc)
  lib <- build_sl_library(cc, "Q")
  ctl <- grep("\\.control_arm$", lib, value = TRUE)
  stopifnot(length(ctl) == sum(has),
            !any(grepl("control_arm", c(build_sl_library(cc, "g"), build_sl_library(cc, "pi")))))
  with_local_seed(84402L, {
    n <- 600L; cl <- as.character(rep(1:60, each = 10L))
    A <- rep(c(0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L), 60L)
    X <- data.frame(A = A, W1 = stats::rnorm(n), W2 = stats::rnorm(n),
                    W3 = stats::rbinom(n, 1L, .4))
    w <- stats::runif(n, .5, 2)
    y <- .30 + .05 * X$W1 - .04 * X$W2 + .03 * X$W3 - .08 * A + stats::rnorm(n, sd = .05)
    yb <- stats::rbinom(n, 1L, stats::plogis(-.4 + .6 * X$W1 - .9 * A))
    newX <- rbind(transform(X, A = 1L), transform(X, A = 0L), X)
    i1 <- seq_len(n); i0 <- n + seq_len(n); ia <- 2L * n + seq_len(n); trt <- A == 1L
    for (nm in ctl) {
      f <- get(nm, envir = .GlobalEnv)
      go <- function(yy, fam) with_local_seed(84403L, with_sl_glmnet_context("Q", 84404L, 3L,
        f(Y = yy, X = X, newX = newX, family = fam, obsWeights = w, id = cl)))
      a <- go(y, stats::gaussian())
      y_shift <- y; y_shift[trt] <- y_shift[trt] + 5
      b <- go(y_shift, stats::gaussian())
      stopifnot(
        # the defining property: exposed outcomes cannot move Q(0,W)
        same(a$pred[i0], b$pred[i0]),
        # the exposed arm moves by exactly the added constant
        same(b$pred[i1] - a$pred[i1], rep(5, n)),
        same(a$pred[i1] - a$pred[i0], rep(a$fit$shift, n)),
        # the shift centres the weighted exposed training residuals
        abs(sum(w[trt] * (y[trt] - a$pred[ia][trt]))) < 1e-8 * sum(w[trt]),
        same(a$pred[ia], ifelse(trt, a$pred[i1], a$pred[i0])),
        same(as.numeric(stats::predict(a$fit, newX)), a$pred),
        a$fit$n_control_train == sum(!trt), a$fit$n_exposed_train == sum(trt))
      p <- go(yb, stats::binomial())
      yb_flip <- yb; yb_flip[trt] <- 1L - yb_flip[trt]
      q <- go(yb_flip, stats::binomial())
      stopifnot(all(is.finite(p$pred)), all(p$pred >= 0 & p$pred <= 1),
                same(p$pred[i0], q$pred[i0]),
                abs(sum(w[trt] * (yb[trt] - p$pred[ia][trt]))) < 1e-6 * sum(w[trt]),
                # Q-only: no A column, or a non-Q tuning context, must stop
                fail(f(Y = y, X = X[c("W1", "W2")], newX = newX[c("W1", "W2")],
                       family = stats::gaussian(), obsWeights = w, id = cl)),
                fail(with_sl_glmnet_context("g", 84405L, 3L,
                  f(Y = y, X = X, newX = newX, family = stats::gaussian(), obsWeights = w, id = cl))),
                fail(with_sl_glmnet_context("Q", 84405L, 3L,
                  f(Y = y[!trt], X = X[!trt, ], newX = newX, family = stats::gaussian(),
                    obsWeights = w[!trt], id = cl[!trt]))))
    }
    if (has[["ranger"]]) {
      rf <- get("SL.ranger.fixed", envir = .GlobalEnv)
      stopifnot(isTRUE(rf(y, X, X, stats::gaussian(), obsWeights = w)$fit$always_split_A),
                !isTRUE(rf(A, X[c("W1", "W2", "W3")], X[c("W1", "W2", "W3")],
                           stats::binomial(), obsWeights = w)$fit$always_split_A))
    }
  })
  cat("v8.44 calibrator-selection and control-arm Q learner tests passed.\n")
  invisible(TRUE)
}

