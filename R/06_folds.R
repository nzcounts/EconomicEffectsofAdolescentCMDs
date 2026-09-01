# 4) CLUSTER-AWARE FOLD CONSTRUCTION
# =============================================================================
# Plain-English role: build cross-validation folds at the CLUSTER level (not
# the row level). Entire clusters stay together in train or validation so
# dependence within a cluster cannot leak across the CV boundary. We also
# greedily balance the number of exposed units across folds, which is
# important when exposure prevalence is only ~9%.

make_cluster_folds_balanced <- function(cluster, A, k = 5L, seed = 1L,
                                        weights = NULL, delta = NULL,
                                        balance_on_weights = FALSE,
                                        max_attempts = 500L,
                                        projected_size_tolerance_prop = 0.02,
                                        max_size_ratio = 1.60,
                                        max_size_deviation_prop = 0.35,
                                        min_active_cell_n = 1L) {
  cluster <- trimws(as.character(cluster))
  if (!length(cluster) || anyNA(cluster) || any(!nzchar(cluster)))
    stop("Fold construction requires complete nonblank cluster identifiers.", call. = FALSE)
  A <- as.integer(A)
  if (length(A) != length(cluster) || anyNA(A) || any(!A %in% c(0L, 1L)))
    stop("Fold construction requires a complete binary treatment vector.", call. = FALSE)
  if (is.null(weights)) weights <- rep(1, length(A))
  weights <- as.numeric(weights)
  if (length(weights) != length(A) || any(!is.finite(weights)) || any(weights <= 0))
    stop("Fold construction requires positive finite weights.", call. = FALSE)
  if (is.null(delta)) delta <- rep(1L, length(A))
  delta <- as.integer(delta)
  if (length(delta) != length(A) || anyNA(delta) || any(!delta %in% c(0L, 1L)))
    stop("Fold construction requires a complete binary outcome-observation vector.", call. = FALSE)
  max_attempts <- as.integer(max_attempts)
  min_active_cell_n <- as.integer(min_active_cell_n)
  if (!is.finite(max_attempts) || max_attempts < 1L ||
      !is.finite(projected_size_tolerance_prop) ||
        projected_size_tolerance_prop < 0 || projected_size_tolerance_prop > 0.20 ||
      !is.finite(max_size_ratio) || max_size_ratio <= 1 ||
      !is.finite(max_size_deviation_prop) || max_size_deviation_prop <= 0 ||
      !is.finite(min_active_cell_n) || min_active_cell_n < 1L)
    stop("Fold construction controls are invalid.", call. = FALSE)

  dat <- data.frame(cluster = cluster, A = A, delta = delta, w = weights,
                    stringsAsFactors = FALSE)
  dat$a1d1 <- as.integer(A == 1L & delta == 1L)
  dat$a1d0 <- as.integer(A == 1L & delta == 0L)
  dat$a0d1 <- as.integer(A == 0L & delta == 1L)
  dat$a0d0 <- as.integer(A == 0L & delta == 0L)
  cell_names <- c("a1d1", "a1d0", "a0d1", "a0d0")
  cl_stats <- stats::aggregate(
    x = list(n = rep(1L, nrow(dat)),
             a_sum = dat$A,
             a_w = dat$w * dat$A,
             obs_a = dat$A * dat$delta,
             a1d1 = dat$a1d1, a1d0 = dat$a1d0,
             a0d1 = dat$a0d1, a0d0 = dat$a0d0),
    by = list(cluster = dat$cluster), FUN = sum)
  n_clusters <- nrow(cl_stats)
  k_requested <- as.integer(k)
  k <- min(k_requested, n_clusters)
  if (!is.finite(k) || k < 2L)
    stop("Need at least two unique clusters to make folds.", call. = FALSE)

  cell_totals <- colSums(cl_stats[cell_names])
  active <- cell_totals > 0
  if (any(active)) {
    clusters_with_cell <- vapply(cell_names, function(z) sum(cl_stats[[z]] > 0), integer(1))
    k_by_cluster_support <- min(clusters_with_cell[active])
    k_by_count_support <- min(floor(cell_totals[active] / min_active_cell_n))
    k <- min(k, k_by_cluster_support, k_by_count_support)
  }
  if (k < 2L)
    stop("Fewer than two folds can satisfy the active treatment-by-observation support requirement.", call. = FALSE)

  target_n <- nrow(dat) / k
  total_cell_counts <- cell_totals
  best_assignment <- NULL
  best_key <- c(Inf, Inf, Inf)

  assignment_diagnostics <- function(out) {
    fold_n <- tabulate(out, nbins = k)
    if (any(fold_n <= 0L)) return(NULL)
    fold_cells <- vapply(seq_len(k), function(f) {
      c(a1d1 = sum(A[out == f] == 1L & delta[out == f] == 1L),
        a1d0 = sum(A[out == f] == 1L & delta[out == f] == 0L),
        a0d1 = sum(A[out == f] == 0L & delta[out == f] == 1L),
        a0d0 = sum(A[out == f] == 0L & delta[out == f] == 0L))
    }, numeric(4L))
    support_validation <- fold_cells >= min_active_cell_n
    support_training <- sweep(fold_cells, 1L, total_cell_counts, function(x, tot) tot - x) >= min_active_cell_n
    support_ok <- all(support_validation[active, , drop = FALSE]) &&
      all(support_training[active, , drop = FALSE])
    size_ratio <- max(fold_n) / min(fold_n)
    size_deviation_prop <- max(abs(fold_n - target_n)) / target_n
    active_targets <- pmax(total_cell_counts[active] / k, 1)
    cell_balance <- if (any(active)) {
      mean((sweep(fold_cells[active, , drop = FALSE], 1L, active_targets, "/") - 1)^2)
    } else 0
    list(fold_n = fold_n, fold_cells = fold_cells,
         support_ok = support_ok, size_ratio = size_ratio,
         size_deviation_prop = size_deviation_prop,
         cell_balance = cell_balance)
  }

  target_a <- sum(A) / k
  target_obs_a <- sum(A * delta) / k
  target_aw <- sum(weights * A) / k
  target_cells <- pmax(total_cell_counts / k, 1)

  for (attempt in seq_len(max_attempts)) {
    cs <- cl_stats
    cs$.tie <- with_local_seed(seed + attempt - 1L, stats::runif(n_clusters))
    # Largest clusters are placed first, as in longest-processing-time bin
    # packing. This prevents treatment balance from creating tiny validation
    # folds when treatment is concentrated in a few large schools.
    cs <- cs[order(-cs$n, -cs$a_sum, -cs$obs_a, -cs$a_w, cs$.tie), , drop = FALSE]
    fold_a <- rep(0, k); fold_aw <- rep(0, k)
    fold_n <- rep(0, k); fold_obs_a <- rep(0, k)
    fold_cells_running <- matrix(0, nrow = length(cell_names), ncol = k,
                                 dimnames = list(cell_names, NULL))
    fold_assign_cluster <- integer(nrow(cs))

    for (i in seq_len(nrow(cs))) {
      projected_n <- fold_n + cs$n[i]
      best_projected_n <- min(projected_n)
      size_tolerance <- max(1, projected_size_tolerance_prop * target_n)
      eligible <- which(projected_n <= best_projected_n + size_tolerance + 1e-12)

      cell_add <- as.numeric(unlist(cs[i, cell_names, drop = FALSE],
                                    use.names = FALSE))
      cell_score <- vapply(eligible, function(f) {
        projected_cells <- fold_cells_running[, f] + cell_add
        mean(((projected_cells[active] - target_cells[active]) /
               target_cells[active])^2)
      }, numeric(1))
      treatment_score <- ((fold_a[eligible] + cs$a_sum[i] - target_a) / max(target_a, 1))^2 +
        ((fold_obs_a[eligible] + cs$obs_a[i] - target_obs_a) / max(target_obs_a, 1))^2
      if (isTRUE(balance_on_weights))
        treatment_score <- treatment_score +
          ((fold_aw[eligible] + cs$a_w[i] - target_aw) / max(target_aw, 1))^2
      score <- cell_score + treatment_score
      best <- eligible[score <= min(score) + 1e-12]
      chosen <- if (length(best) == 1L) best[[1L]] else
        with_local_seed(seed + attempt * 100000L + i, sample(best, 1L))

      fold_assign_cluster[i] <- chosen
      fold_a[chosen] <- fold_a[chosen] + cs$a_sum[i]
      fold_aw[chosen] <- fold_aw[chosen] + cs$a_w[i]
      fold_obs_a[chosen] <- fold_obs_a[chosen] + cs$obs_a[i]
      fold_n[chosen] <- fold_n[chosen] + cs$n[i]
      fold_cells_running[, chosen] <- fold_cells_running[, chosen] + cell_add
    }

    map <- stats::setNames(fold_assign_cluster, cs$cluster)
    out <- unname(as.integer(map[cluster]))
    if (anyNA(out) || length(unique(out)) != k) next
    diag <- assignment_diagnostics(out)
    if (is.null(diag) || !isTRUE(diag$support_ok)) next
    key <- c(diag$size_ratio, diag$size_deviation_prop, diag$cell_balance)
    # Lexicographic comparison prevents improved cell balance from
    # compensating for worse fold-size balance.
    better <- is.null(best_assignment) || key[1] < best_key[1] - 1e-12 ||
      (abs(key[1] - best_key[1]) <= 1e-12 && key[2] < best_key[2] - 1e-12) ||
      (abs(key[1] - best_key[1]) <= 1e-12 && abs(key[2] - best_key[2]) <= 1e-12 &&
         key[3] < best_key[3] - 1e-12)
    if (better) {
      best_assignment <- list(fold = out, diagnostics = diag, attempt = attempt)
      best_key <- key
    }
  }

  if (is.null(best_assignment))
    stop(sprintf(paste0("Could not construct %d whole-cluster folds with at least %d observation(s) ",
                        "in every active A-by-delta cell in validation and training complements after %d attempts."),
                 k, min_active_cell_n, max_attempts), call. = FALSE)
  if (best_assignment$diagnostics$size_ratio > max_size_ratio ||
      best_assignment$diagnostics$size_deviation_prop > max_size_deviation_prop) {
    stop(sprintf(paste0("Best whole-cluster assignment remains too imbalanced: fold sizes [%s], ",
                        "max/min=%.3f (limit %.3f), maximum target deviation=%.3f (limit %.3f)."),
                 paste(best_assignment$diagnostics$fold_n, collapse = ","),
                 best_assignment$diagnostics$size_ratio, max_size_ratio,
                 best_assignment$diagnostics$size_deviation_prop, max_size_deviation_prop),
         call. = FALSE)
  }
  attr(best_assignment$fold, "fold_diagnostics") <- c(best_assignment$diagnostics,
    list(k_requested = k_requested, k_used = k, attempts_used = best_assignment$attempt,
         assignment_method = "LPT_size_primary_with_hard_A_delta_support"))
  best_assignment$fold
}

fold_control_from_cfg <- function(cfg, level = c("outer", "internal")) {
  level <- match.arg(level)
  internal <- identical(level, "internal")
  list(
    max_attempts = as.integer(cfg$final_tmle$fold_max_attempts %||% 500L),
    projected_size_tolerance_prop = as.numeric(
      cfg$final_tmle$fold_projected_size_tolerance_prop %||% 0.02),
    max_size_ratio = as.numeric(if (internal)
      cfg$final_tmle$fold_internal_max_size_ratio %||% 1.75 else
      cfg$final_tmle$fold_max_size_ratio %||% 1.60),
    max_size_deviation_prop = as.numeric(if (internal)
      cfg$final_tmle$fold_internal_max_size_deviation_prop %||% 0.45 else
      cfg$final_tmle$fold_max_size_deviation_prop %||% 0.35),
    min_active_cell_n = as.integer(cfg$final_tmle$fold_min_active_cell_n %||% 1L))
}


# Build whole-cluster fold ids for the rough prescreen. Row-level folds are
# prohibited so school dependence cannot cross screening-validation boundaries.
make_rough_fold_ids <- function(n, K, seed, cluster_vec = NULL, A_vec = NULL,
                                delta_vec = NULL, weights = NULL,
                                cluster_aware = TRUE, fold_control = NULL) {
  if (!isTRUE(cluster_aware))
    stop("Row-level rough-screen folds are disabled; use whole-cluster folds.", call. = FALSE)
  if (is.null(cluster_vec) || is.null(A_vec) || length(cluster_vec) != n || length(A_vec) != n)
    stop("Cluster-aware rough-screen folds require aligned cluster and treatment vectors.", call. = FALSE)
  ctl <- fold_control %||% list()
  do.call(make_cluster_folds_balanced, c(list(
    cluster = cluster_vec, A = A_vec, k = K, seed = seed,
    weights = weights, delta = delta_vec), ctl))
}

# =============================================================================
