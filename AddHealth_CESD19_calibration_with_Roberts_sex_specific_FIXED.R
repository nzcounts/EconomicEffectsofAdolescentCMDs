#!/usr/bin/env Rscript
# Add Health Wave II CES-D-19 measurement calibration, independent of causal pipeline
# Integrated review 2026-11-20. No row-level or PSU-level outputs are exported.
# Primary purpose: compare PHQ-9 >=10 score-severity linking, adolescent clinical
# diagnostic performance, and internal Add Health construct checks.
# CRITICAL: The clinical-validation data do not measure PHQ-9 in Add Health.
#           No part of this script establishes NSDUH episode status or causal effects.
#           External item-bank sampling uncertainty is not quantified by the PSU bootstrap.
# Sources in companion README. Public-item parameters manually transcribed from
# Olino et al. (2012) Table 2; 20 rows cross-checked against the
# published table. See 00_provenance_and_status.txt for limits.
#
# INDEPENDENT-REVIEW REVISION (2026-11-21). Changes relative to the 2026-11-20 file:
#  [A] PRIMARY ESTIMAND is now IRT observed-score EQUIPERCENTILE linking of S19
#      to the original CES-D-20 in the weighted Wave II population, with an explicit
#      PHQ-9 cut-score convention (boundary 9.5 = "PHQ-9 >= 10"; percentile-rank
#      continuization). It depends only on the two marginal score distributions,
#      so it does not depend on the unidentified within-person dependence between
#      Add Health and original-format responses. The former headline rule
#      E[T(S20)|S19] >= 59.9 is retained as a labelled secondary output
#      (it is a regression estimand and uses the PHQ = 10 score-point convention).
#  [A] RESPONSE-FORMAT SENSITIVITY: delta = uniform threshold shift of Add Health
#      labels relative to original CES-D labels. delta is NOT identified by Add
#      Health data (it is absorbed exactly by the latent mean), so cutoffs are
#      reported over cfg$format_shift_scenarios. delta > 0 = Add Health labels
#      harder to endorse -> lower PHQ-equivalent Add Health cutoff.
#  [A] ANCHOR FIT GATE + positive-item-freed refit (item-level transport check).
#  COLLABORATOR RESPONSE (2026-11-21, later): primary F_X is the OBSERVED weighted
#      S19 distribution (model-implied F_X = sensitivity); 16- and 12-anchor models
#      get identical fit summaries (03b, 18b) and a side-by-side table (19) with no
#      automatic selection; the headline prints its conditions and any anchor warning.
#  [A] Test harness parse error fixed (separate test file).
#  [B] Score-distribution fit (observed weighted vs model-implied S19 tail).
#  [B] Clinical table gains a verification column; Garrison 1991 published
#      sex-specific optima added; unverified rows flagged, values unchanged.
#  [C] survey.lonely.psu set globally; bootstrap also records the primary cutoff.

cfg <- list(
  # "xpt" reads original files; "rds" reads a prepared restricted-use data.frame.
  input_mode = "xpt",
  source_xpt = "M:/AddHealth/Data/Standard/Survey/W2/wave2.xpt",
  weights_xpt = "M:/AddHealth/Data/Standard/Weights/Homewt2.xpt",
  # Wave I or Wave II file(s) containing PSUSCID and REGION by AID.
  design_xpts = c("M:/AddHealth/Data/Standard/Survey/W1/allwave1.xpt"),
  input_rds = "",                    # optional; RAW H2FS responses, not reverse scored
  output_dir = "M:/AddHealth/CESD_calibration_fresh",
  vilagut_xls = "",                 # optional uploaded pone.0155431.s001.xls
  wave1_xpt = "",                  # optional H1FS1..19 for stability check (raw responses)
  seed = 20261120L,
  min_complete_n = 1000L,
  response_codes = 0:3,
  weight_var = "GSWGT2",
  primary_phq9_cutoff = 10L,
  # Three original-CES-D reference routes:
  methods = c("prosetta", "adolescent_clinical", "original_cesd_scenarios"),
  original_cesd_scenarios = 19:26,
  # PROsetta Stone 2014 (Choi et al.) Appendix A1/A2. External T values
  # below are *crosswalk linked scores*, NOT sampling-standard errors.
  promis_t_shift_scenarios = c(-2, 0, 2),
  anchors_to_free = character(),    # e.g. "H2FS5" or c("H2FS5","H2FS9")
  # Choose which integer rule to emphasize in comparison, not by preference history.
  decision_rules = c("true_score_ceiling", "true_score_nearest",
                     "posterior_equal_loss", "posterior_balanced_loss",
                     "observed_score_link"),
  theta_grid = seq(-8, 8, length.out = 161L),
  optimizer_maxit = 500L,
  optimizer_restarts = 2L,
  # Keep 1.0: Olino used MULTILOG 7.03, whose GRM slopes are in the logistic metric (D = 1).
  # 1.7 changes the item response functions themselves (b is fixed), so it is a different
  # model, not a metric stress test; leave it at 1.0 for all reported results.
  grm_slope_multiplier = 1.0,
  # Diagnostic options; neither selects the cutoff by maximizing suicide prediction.
  run_wave2_validation = TRUE,
  enable_counseling_validation = TRUE,  # H2HS5: 0=No, 1=Yes (Wave II codebook)
  run_dif_screen = TRUE,
  dif_group_vars = c("BIO_SEX"), # may add verified respondent demographic variables
  run_local_dependence = TRUE,
  run_wave1_stability = FALSE,
  # Full survey PSU bootstrap can be costly: 0 is test/preflight; 100+ for final.
  psu_bootstrap_replicates = 0L,
  run_anchor_loo = FALSE,
  clinical_meta_verified_counts_csv = "", # optional verified exact TP,FP,TN,FN at multiple cutoffs
  clinical_override_csv = "", # optional externally verified cutoff accuracy rows
  clinical_meta_distr = "logistic",
  min_report_cell = 20L,
  allow_outputs = TRUE,
  # ---- review additions ----
  # PHQ-9 cut-score conventions (continuized PHQ-9 boundary). 9.5 is the boundary
  # of the integer screen "PHQ-9 >= 10" under percentile-rank continuization;
  # 10.0 is the score-point convention used by the former headline rule;
  # 10.5 is the boundary of "PHQ-9 >= 11" (Richardson 2010 adolescent optimum).
  phq_boundaries = c(PHQ_ge10=9.5, PHQ_scorepoint10=10.0, PHQ_ge11=10.5),
  primary_phq_boundary = "PHQ_ge10",
  # Uniform Add Health-vs-original label threshold shift, latent SD units.
  format_shift_scenarios = c(-0.2,-0.1,0,0.1,0.2),
  # Applied to fixed anchor thresholds (test/sensitivity hook; keep 0 for primary).
  anchor_threshold_shift = 0,
  # Gate: any anchor whose model-implied category proportion misses the weighted
  # observed proportion by more than this (absolute) triggers a transport warning.
  anchor_fit_tolerance = 0.03,
  positive_items = c("H2FS4","H2FS8","H2FS11","H2FS15"),
  run_positive_free_sensitivity = TRUE
)
options(survey.lonely.psu="adjust")

required <- c("haven", "survey", "tidyselect")
for (p in required) if (!requireNamespace(p, quietly=TRUE))
  stop("Missing package '",p,"'. Install before running this script.")
set.seed(cfg$seed)
if (!all(cfg$decision_rules %in% c("true_score_ceiling","true_score_nearest",
                                   "posterior_equal_loss","posterior_balanced_loss",
                                   "observed_score_link")))
  stop("Unknown decision rule")
if (length(cfg$theta_grid)<61L || min(cfg$theta_grid)> -6 || max(cfg$theta_grid)<6 ||
    any(diff(cfg$theta_grid)<=0)) stop("theta_grid needs ordered broad support")
if (max(abs(diff(cfg$theta_grid)-mean(diff(cfg$theta_grid))))>1e-10)
  stop("theta_grid must be equally spaced for the implemented quadrature")
if (!cfg$input_mode %in% c("xpt","rds")) stop("Unknown input_mode")
if (!cfg$primary_phq9_cutoff %in% 0:27) stop("Invalid PHQ-9 cutoff")
if (!cfg$grm_slope_multiplier %in% c(1.0,1.7)) stop("GRM metric toggle must be 1 or 1.7")
if (!all(cfg$methods %in% c("prosetta","adolescent_clinical","original_cesd_scenarios")))
  stop("Unknown method in cfg$methods")
if (!cfg$primary_phq_boundary %in% names(cfg$phq_boundaries))
  stop("primary_phq_boundary must name an element of phq_boundaries")
if (!any(abs(cfg$format_shift_scenarios)<1e-12))
  stop("format_shift_scenarios must include 0")
if (!is.numeric(cfg$psu_bootstrap_replicates) ||
    cfg$psu_bootstrap_replicates<0) stop("Invalid bootstrap count")

# Published original CES-D-20 graded-response parameters, Olino et al. 2012,
# Table 2. b1<b2<b3 are category-endorsement severity points on that paper's theta.
# Original item sequence: 1..20 inclusive (sleep=11; crying=17).
olino <- data.frame(
  item=1:20,
  a=c(1.04,.86,1.86,1.21,1.04,1.98,.51,.87,1.96,1.28,
      .96,1.62,1.00,1.51,1.00,1.90,1.38,1.83,1.55,1.23),
  b1=c(-.25,-.18,.15,-.10,-1.43,-.37,-2.38,-.76,.81,-.03,
       -.41,-.21,-.53,-.05,.05,-.05,.93,-.34,.29,-.51),
  b2=c(1.26,1.37,.95,1.34,.28,.70,.70,1.23,1.65,1.29,
       .92,1.25,1.00,1.17,2.12,1.09,1.83,.92,1.49,.96),
  b3=c(2.90,3.17,1.86,2.58,2.25,1.67,3.61,2.83,2.43,2.56,
       2.32,2.29,2.65,2.30,4.01,2.11,2.94,1.95,2.55,2.39)
)
stopifnot(nrow(olino)==20L, all(olino$a>0),
          all(olino$b1<olino$b2), all(olino$b2<olino$b3))

# Add Health position -> original CES-D position. NA means new item.
map <- data.frame(addhealth=paste0("H2FS",1:19),
  original=c(1:10,12:16,18:20,NA_integer_),
  type=c(rep("shared",6),"reworded",rep("shared",10),"reworded","new"),
  stringsAsFactors=FALSE)
stopifnot(sum(map$type=="shared")==16L,
          identical(as.integer(sort(setdiff(1:20,na.omit(map$original)))),c(11L,17L)))
anchors <- map$addhealth[map$type=="shared"]
if (!all(cfg$anchors_to_free %in% anchors)) stop("anchors_to_free must name shared items")
anchors <- setdiff(anchors,cfg$anchors_to_free)
if (length(anchors)<10) stop("Too few invariant anchors to identify link")

# Table A1 full original CES-D raw score -> adult PROMIS T;
# Table A2 PHQ-9 raw score -> adult PROMIS T, Choi et al. 2014.
# IMPORTANT: Appendix SE is conditional score precision, not linking calibration CI.
cesd_t <- c(34.5,38.6,41.1,42.9,44.7,46.2,47.5,48.7,49.8,50.8,
            51.7,52.6,53.4,54.1,54.8,55.5,56.2,56.8,57.4,58.0,
            58.6,59.1,59.7,60.2,60.8,61.3,61.8,62.3,62.9,63.4,
            63.9,64.4,64.9,65.4,66.0,66.5,67.0,67.6,68.1,68.7,
            69.2,69.8,70.4,71.0,71.7,72.3,73.0,73.7,74.4,75.2,
            76.0,76.8,77.7,78.7,79.7,80.8,82.0,83.1,84.3,85.4,86.4)
phq_t <- c(37.4,42.7,45.9,48.3,50.5,52.5,54.2,55.8,57.2,58.6,
           59.9,61.1,62.3,63.5,64.7,65.8,66.9,68.0,69.2,70.3,
           71.5,72.7,74.0,75.3,76.7,78.3,80.0,82.3)
stopifnot(length(cesd_t)==61L,length(phq_t)==28L,
          all(diff(cesd_t)>0),all(diff(phq_t)>0),
          identical(unname(phq_t[11]),59.9),
          identical(unname(cesd_t[23]),59.7))
interp_cesd <- function(t) {
  if (any(t<min(cesd_t) | t>max(cesd_t)))
    stop("PROMIS T outside published CES-D crosswalk: no extrapolation")
  approx(x=cesd_t,y=0:60,xout=t,ties="ordered")$y
}

# Accessible corroboration studies; not individual observations. External
# sensitivity/specificity rows are not a joint patient-level ROC dataset.
clinical <- data.frame(
  study=c(rep("Cuijpers_2008",6),"Roberts_1991","Garrison_1991",
          "Garrison_1991","Camacho_2009","Yang_2004","Prescott_1998",
          "Richardson_2010"),
  instrument=c(rep("CESD20",12),"PHQ9"),
  cutoff=c(19,20,21,22,23,24,24,22,16,16,28,16,10),
  sensitivity=c(.9048,.9048,.9048,.9048,.8571,.8571,.837,.5585,
                .72975,.91,.41,.787,.8947),
  specificity=c(.6528,.6806,.7292,.7431,.7569,.7743,.752,.7959,
                .58615,.44,.90,NA_real_,.721),
  # NA means study-level diagnostic case counts *at that cutoff* not verified.
  case_count=c(rep(21,6),rep(NA_real_,6),19),
  nondisease_count=c(rep(222,6),rep(NA_real_,6),423), # 222 nominal; weighting lowers effective precision
  population=c(rep("Dutch adolescent school/internet; two-phase verification",6),
               "Oregon adolescents","US school adolescents","US school adolescents",
               "Colombian school adolescents","Taiwanese adolescents",
               "Hawaii adolescents; specificity unavailable","US health-system adolescents"),
  source=c(rep("https://doi.org/10.1007/s00787-007-0631-2",6),
           rep("https://doi.org/10.1371/journal.pone.0155431.s001",6),
           "https://pmc.ncbi.nlm.nih.gov/articles/PMC3217785/"),
  stringsAsFactors=FALSE
)
# Review 2026-11-21: verification status of each row against primary sources
# reachable during review (abstract/full text). Values are NOT altered.
clinical$verification <- c(
  "unverified (full text needed)","unverified (full text needed)",
  "unverified (full text needed)",
  "abstract-verified: 90.48/74.31; criterion MINI mood disorder (check current vs lifetime); 19/21, 165/222 consistent",
  "unverified (full text needed)","unverified (full text needed)",
  "unverified (Vilagut supplement)",
  "DOES NOT MATCH published sex-specific optima; likely Vilagut reconstruction pooled over sex",
  "DOES NOT MATCH published sex-specific optima; likely Vilagut reconstruction pooled over sex",
  "unverified (Vilagut supplement)",
  "secondary source: 90th-percentile cut (0.41/0.90); raw value 28 unverified",
  "secondary source gives specificity 0.74 at 16; unverified",
  "consistent with Table 1 counts (17/19; 305/423) but >=10 row not verified in full text; >=11 abstract-verified 89.5/77.5; criterion DISC-IV MDD past month")
clinical <- rbind(clinical, data.frame(
  study=c("Garrison_1991_boys","Garrison_1991_girls"),instrument="CESD20",
  cutoff=c(12,22),sensitivity=c(.85,.83),specificity=c(.49,.77),
  case_count=NA_real_,nondisease_count=NA_real_,
  population="US school adolescents (two-stage; K-SADS); published sex-specific optimum",
  source="Garrison et al. 1991 JAACAP 30:636-641, abstract",
  verification="abstract-verified",stringsAsFactors=FALSE))
# No assumed independence of multiple Cuijpers cutoffs. Each comparison below
# is performed separately, with uncertainty for observed cases only.

# ---- Robust data reading, AID matching, missingness and complete domains ----
as_num <- function(z) {
  if (is.factor(z)) z <- as.character(z)
  if (inherits(z,"haven_labelled")) z <- unclass(z)
  suppressWarnings(as.numeric(z))
}
as_key <- function(z,label) {
  if (is.factor(z)) z <- as.character(z)
  s <- trimws(as.character(z))
  if (anyNA(s) || any(!nzchar(s))) stop(label,": missing AID")
  # SAS XPT IDs are commonly numeric even when a different imported file
  # represents the same integer ID as character. Normalize such keys without
  # scientific notation or loss of precision; preserve genuinely nonnumeric IDs.
  candidate <- suppressWarnings(as.numeric(s))
  if (all(is.finite(candidate))) {
    if (any(candidate!=floor(candidate)) || any(abs(candidate)>9007199254740991))
      stop(label,": numeric respondent IDs are noninteger or exceed exact precision")
    s <- format(candidate,scientific=FALSE,trim=TRUE,digits=16)
  }
  s
}
read_xpt_small <- function(path,fields,required=c("AID")) {
  if (!nzchar(path) || !file.exists(path)) stop("Missing input file: ",path)
  names0 <- names(haven::read_xpt(path,n_max=0L))
  if (anyDuplicated(toupper(names0))) stop("Ambiguous names in ",path)
  want <- unique(toupper(fields))
  missing <- setdiff(toupper(required),toupper(names0))
  if (length(missing)) stop("Missing required fields in ",path,": ",paste(missing,collapse=", "))
  actual <- names0[match(intersect(want,toupper(names0)),toupper(names0))]
  z <- haven::read_xpt(path,col_select=tidyselect::all_of(actual))
  z <- as.data.frame(z)
  names(z) <- toupper(names(z))
  z
}
merge_unique <- function(base,aux,nm) {
  if (!"AID" %in% names(aux)) stop("Missing AID in ",nm)
  aux$AID <- as_key(aux$AID,nm)
  if (anyDuplicated(aux$AID)) stop("Duplicate AIDs in ",nm)
  match_idx <- match(base$AID,aux$AID)
  for (k in setdiff(names(aux),"AID")) {
    new <- aux[[k]][match_idx]
    if (k %in% names(base)) {
      old <- base[[k]]
      identical_both <- !is.na(old) & !is.na(new)
      if (is.numeric(old) || is.numeric(new)) {
        old <- as_num(old);new <- as_num(new)
        if (any(identical_both & abs(old-new) > 1e-7*pmax(1,abs(old),abs(new)),na.rm=TRUE))
          stop("Conflicting ",k," from ",nm)
      } else if (any(identical_both & as.character(old)!=as.character(new))) {
        stop("Conflicting ",k," from ",nm)
      }
      old[is.na(old)] <- new[is.na(old)]
      base[[k]] <- old
    } else base[[k]] <- new
  }
  base
}
read_data <- function() {
  items <- map$addhealth
  val <- c("H2SU1","H2SU2","H2SU3","H2HS5","H2GH23","H2GH26",
           "H2GH24","H2GH25","H2GH27","H2GH40","H2GH41",
           "H2GH1","H2GH6C","BIO_SEX",
           "H2GI4","H2GI1Y","RACE1","AGE_W2")
  if (cfg$input_mode=="rds") {
    if (!file.exists(cfg$input_rds)) stop("Missing input RDS")
    z <- as.data.frame(readRDS(cfg$input_rds))
    names(z) <- toupper(names(z))
  } else {
    z <- read_xpt_small(cfg$source_xpt,c("AID",items,val,"PSUSCID","REGION",cfg$weight_var),
                        required=c("AID",items))
    w <- read_xpt_small(cfg$weights_xpt,c("AID",cfg$weight_var,"PSUSCID","REGION"),
                        required=c("AID",cfg$weight_var))
    z$AID <- as_key(z$AID,"Wave II")
    if (anyDuplicated(z$AID)) stop("Duplicate Wave II AIDs")
    z <- merge_unique(z,w,"Wave II weights")
    for (f in cfg$design_xpts) {
      if (!file.exists(f)) stop("Configured design file does not exist: ",f)
      d <- read_xpt_small(f,c("AID","PSUSCID","REGION","BIO_SEX","RACE1"))
      z <- merge_unique(z,d,basename(f))
    }
  }
  needed <- c("AID",items,cfg$weight_var,"PSUSCID","REGION")
  if (!all(needed %in% names(z)))
    stop("Input lacks required fields: ",paste(setdiff(needed,names(z)),collapse=", "))
  z$AID <- as_key(z$AID,"joined input")
  if (anyDuplicated(z$AID)) stop("Duplicate AIDs in merged input")
  z$WEIGHT <- as_num(z[[cfg$weight_var]])
  z$PSUSCID <- as.character(z$PSUSCID)
  z$REGION <- as.character(z$REGION)
  z$valid_design <- is.finite(z$WEIGHT) & z$WEIGHT>0 &
    !is.na(z$PSUSCID) & nzchar(z$PSUSCID) &
    !is.na(z$REGION) & nzchar(z$REGION)
  z$complete <- TRUE
  for (j in seq_along(items)) {
    nm <- items[j]
    x <- as_num(z[[nm]])
    unknown <- !is.na(x) & !(x %in% c(0:3,6,8,9))
    if (any(unknown)) stop("Unexpected raw response code in ",nm,
                           ": ",paste(head(unique(x[unknown]),8),collapse=", "))
    x[!x %in% 0:3] <- NA_real_
    if (j %in% c(4L,8L,11L,15L)) x <- 3-x
    z[[nm]] <- x
    z$complete <- z$complete & !is.na(x)
  }
  z$sum19 <- rowSums(z[items],na.rm=FALSE)
  z$analysis_domain <- z$valid_design & z$complete
  if (sum(z$analysis_domain)<cfg$min_complete_n)
    stop("Too few valid-weight CES-D complete cases; inspect paths/coding")
  if (any(z$sum19[z$analysis_domain] <0 | z$sum19[z$analysis_domain]>57))
    stop("Invalid CES-D-19 score")
  ps <- split(z$REGION[z$valid_design],z$PSUSCID[z$valid_design])
  if (any(vapply(ps,function(x)length(unique(x))>1L,logical(1))))
    stop("PSU appears in multiple REGION strata")
  z
}

# ---- One-factor Samejima graded-response marginal maximum likelihood ------
# Fixed anchors identify the external adolescent theta metric. Weighted ML
# estimates uncalibrated items and the Add Health latent normal mean/variance.
# Gaussian mixture integration is via deterministic dense quadrature; numerical
# truncation is checked. This is not an unconstrained mirt fit mislabeled as linking.
item_probs <- function(theta,a,b) {
  if (length(b)!=3L || !is.finite(a) || a<=0 || any(diff(b)<=0))
    stop("Invalid GRM item parameters")
  cc <- plogis(outer(theta,b,function(t,z)a*(t-z)))
  z <- rbind(1-cc[,1],cc[,1]-cc[,2],cc[,2]-cc[,3],cc[,3])
  pmax(z,1e-14)
}
# Returned 4-by-K matrix; each column sums to 1 (up to floating error).
parameter_matrix <- function(a,b) {
  c(a=a,b1=b[1],b2=b[2],b3=b[3])
}
get_bank <- function(anchor_names=anchors) {
  p <- matrix(NA_real_,nrow=19,ncol=4,
              dimnames=list(map$addhealth,c("a","b1","b2","b3")))
  for (j in seq_len(19)) {
    o <- map$original[j]
    if (!is.na(o)) p[j,] <- as.numeric(unlist(olino[o,c("a","b1","b2","b3")],use.names=FALSE)) +
                     c(0,rep(cfg$anchor_threshold_shift,3))
    else p[j,] <- c(1.3,.6,1.7,2.8)
  }
  p[,"a"] <- p[,"a"]*cfg$grm_slope_multiplier
  p
}
pack_four <- function(p) {
  c(log(p[1]),p[2],log(p[3]-p[2]),log(p[4]-p[3]))
}
unpack_four <- function(q) {
  c(exp(q[1]),q[2],q[2]+exp(q[3]),q[2]+exp(q[3])+exp(q[4]))
}
row_logsumexp <- function(m) {
  hi <- apply(m,1,max)
  hi+log(rowSums(exp(m-hi)))
}
irt_loglike <- function(x,parmat,theta) {
  n <- nrow(x)
  out <- matrix(0,n,length(theta))
  for (j in seq_len(ncol(x))) {
    name <- colnames(x)[j]
    v <- parmat[name,]
    prob <- item_probs(theta,v[1],v[2:4])
    out <- out + log(prob[x[,j]+1L,,drop=FALSE])
  }
  out
}
grid_prior <- function(theta,mu,sd) {
  if (!is.finite(mu) || !is.finite(sd) || sd<=0) stop("Bad latent prior")
  p <- dnorm(theta,mean=mu,sd=sd)
  if (sum(p)<=0 || any(!is.finite(p))) stop("Invalid quadrature prior")
  p/sum(p)
}
fit_external_grm <- function(x,w,anchor_names=anchors,
                             maxit=cfg$optimizer_maxit,restarts=cfg$optimizer_restarts) {
  x <- as.matrix(x)
  storage.mode(x) <- "integer"
  if (nrow(x)!=length(w) || ncol(x)==0 || anyNA(x) ||
      any(!(x %in% 0:3)) || any(!is.finite(w)) || any(w<=0))
    stop("Invalid item data or calibration weights")
  if (!all(anchor_names %in% colnames(x))) stop("Unknown anchor item")
  free <- setdiff(colnames(x),anchor_names)
  P0 <- get_bank()
  theta <- cfg$theta_grid
  n <- nrow(x)
  w <- w/mean(w)
  # Anchor contribution is invariant over optimizer iterations.
  const <- matrix(0,n,length(theta))
  for (nm in anchor_names) {
    p <- P0[nm,]
    pm <- item_probs(theta,p[1],p[2:4])
    const <- const + log(pm[x[,nm]+1L,,drop=FALSE])
  }
  initial <- unlist(lapply(free,function(nm)pack_four(P0[nm,])),use.names=FALSE)
  initial <- c(initial,0,0)  # latent mean, log SD
  lower <- c(rep(c(log(.15),-6,log(.06),log(.06)),length(free)),
             -4,log(.3))
  upper <- c(rep(c(log(5),4,log(8),log(8)),length(free)),
             4,log(3.5))
  decode <- function(coef) {
    PP <- P0
    if (length(free)) for (j in seq_along(free)) {
      ii <- (4*j-3):(4*j)
      PP[free[j],] <- unpack_four(coef[ii])
    }
    list(P=PP,mu=coef[length(coef)-1],sd=exp(coef[length(coef)]))
  }
  objective <- function(coef) {
    dd <- decode(coef)
    pi <- grid_prior(theta,dd$mu,dd$sd)
    if (sum(pi[c(1,length(pi))])>1e-4) return(1e15)
    LL <- const
    for (nm in free) {
      v <- dd$P[nm,]
      pm <- item_probs(theta,v[1],v[2:4])
      LL <- LL + log(pm[x[,nm]+1L,,drop=FALSE])
    }
    LL <- sweep(LL,2,log(pmax(pi,1e-250)),"+")
    ans <- -sum(w*row_logsumexp(LL))
    if (!is.finite(ans)) return(1e15)
    ans
  }
  trials <- vector("list",max(1L,as.integer(restarts)))
  for (tt in seq_along(trials)) {
    start <- initial
    if (tt>1L) start[length(start)-1] <- .25*(tt-1L)
    trials[[tt]] <- optim(start,objective,method="L-BFGS-B",
      lower=lower,upper=upper,control=list(maxit=as.integer(maxit),factr=1e7))
  }
  valid <- vapply(trials,function(z)isTRUE(z$convergence==0L) && is.finite(z$value),logical(1))
  if (!any(valid)) stop("External-anchor ML did not converge; examine config/data")
  good <- which(valid)
  k <- good[which.min(vapply(trials[good],function(z)z$value,numeric(1)))]
  best <- trials[[k]]
  if (any(abs(best$par-lower)<1e-4) || any(abs(best$par-upper)<1e-4))
    stop("ML optimum hits optimizer constraint; cannot trust calibration")
  dd <- decode(best$par)
  if (sum(grid_prior(theta,dd$mu,dd$sd)[c(1,length(theta))])>1e-4)
    stop("Latent prior truncates at quadrature edges")
  list(P=dd$P,mu=dd$mu,sd=dd$sd,logLik=-best$value,
       anchor_names=anchor_names,free_names=free,
       convergence=best$convergence,iterations=best$counts[1],
       fit_items=colnames(x),theta=theta)
}
posterior_irt <- function(x,fit,include_loglik=FALSE) {
  x <- as.matrix(x[,fit$fit_items,drop=FALSE])
  LL <- irt_loglike(x,fit$P,fit$theta)
  pi <- grid_prior(fit$theta,fit$mu,fit$sd)
  z <- sweep(LL,2,log(pmax(pi,1e-250)),"+")
  hi <- apply(z,1,max)
  post <- exp(z-hi)
  post <- post/rowSums(post)
  out <- list(mean_theta=as.numeric(post %*% fit$theta),
              sd_theta=sqrt(pmax(0,as.numeric(post%*% (fit$theta^2))-
                                      as.numeric(post%*% fit$theta)^2)),post=post)
  if (include_loglik) out$LL <- LL
  out
}
expected_item <- function(theta,p) {
  sum(plogis(p[1]*(theta-p[2:4])))
}
expected_sum <- function(theta,par) {
  sum(apply(par,1,function(v)expected_item(theta,v)))
}
orig_theta <- function(c20) {
  pp <- as.matrix(olino[,c("a","b1","b2","b3")])
  pp[,1] <- pp[,1]*cfg$grm_slope_multiplier
  uniroot(function(t)expected_sum(t,pp)-c20,
          interval=c(-8,8),tol=1e-7)$root
}
link_target <- function(c20,fit,score,ww,post,score_link=NULL) {
  # c20 is a *published crosswalk* equivalent original observed sum.
  # The theta/TCC and posterior classification rules below are alternatives,
  # NOT independent validations of the PROsetta observed-score link.
  theta0 <- orig_theta(c20)
  target <- expected_sum(theta0,fit$P[fit$fit_items,,drop=FALSE])
  high <- as.numeric(post$post[,fit$theta>=theta0,drop=FALSE] %*%
                       rep(1,sum(fit$theta>=theta0)))
  tot <- sum(ww)
  den1 <- sum(ww*high)
  den0 <- sum(ww*(1-high))
  result <- data.frame(cutoff=0:57,
    expected_error=NA_real_,balanced_error=NA_real_,
    fpr_latent=NA_real_,fnr_latent=NA_real_,
    screening_prevalence=NA_real_)
  for (k in 0:57) {
    ispos <- as.numeric(score>=k)
    result$expected_error[k+1L] <- sum(ww*(ispos*(1-high)+(1-ispos)*high))/tot
    result$fpr_latent[k+1L] <- if(den0>0) sum(ww*ispos*(1-high))/den0 else NA_real_
    result$fnr_latent[k+1L] <- if(den1>0) sum(ww*(1-ispos)*high)/den1 else NA_real_
    result$balanced_error[k+1L] <- .5*(result$fpr_latent[k+1L]+result$fnr_latent[k+1L])
    result$screening_prevalence[k+1L] <- sum(ww*ispos)/tot
  }
  os <- if (is.null(score_link)) modeled_observed_score_link(fit) else score_link
  target_T <- stats::approx(0:60,cesd_t,xout=c20)$y
  # Match the published observed-score-to-PROMIS-T metric directly.
  # E[T | S19=s] follows the conditional original-20 score distribution;
  # it is not T(E[S20|S19=s]), and it is not direct PHQ-9 classification.
  eligible <- which(os$expected_PROMIS_T >= target_T-1e-10)
  if (!length(eligible)) stop("No Add Health score reaches the reference T-score")
  if (any(diff(os$expected_PROMIS_T) < -1e-7))
    stop("Observed-score link is not monotone; examine the IRT calibration")
  cutoff <- c(true_score_ceiling=ceiling(target-1e-10),
              true_score_nearest=round(target),
              posterior_equal_loss=result$cutoff[which.min(result$expected_error)],
              posterior_balanced_loss=result$cutoff[which.min(result$balanced_error)],
              observed_score_link=min(eligible)-1L)
  cutoff <- pmin(57L,pmax(0L,as.integer(cutoff)))
  names(cutoff) <- c("true_score_ceiling","true_score_nearest",
                     "posterior_equal_loss","posterior_balanced_loss",
                     "observed_score_link")
  # Distinct classification interpretation: P(S20 >= first published CES-D
  # raw score at or above reference T | S19=s) >= 0.5.
  cond <- attr(os,"conditional_original20")
  orig_boundary <- which(cesd_t >= target_T-1e-10)[1L]-1L
  class_probability <- rowSums(cond[,seq.int(orig_boundary+1L,61L),drop=FALSE])
  cls <- which(class_probability >= 0.5)
  prob_link <- if (length(cls)) min(cls)-1L else NA_integer_
  list(original_cesd=c20,theta_ref=theta0,expected_addhealth=target,
       candidate_cutoffs=cutoff,risk=result,latent_positive=sum(ww*high)/tot,
       observed_prob_link=prob_link,original_boundary=orig_boundary,
       observed_target_T=target_T,score_link=os)
}

# ---- Design-based aggregate summaries, and external clinical comparison -----
survey_prevalences <- function(z,score,cuts) {
  domain <- z$valid_design & z$complete
  d <- survey::svydesign(ids=~PSUSCID,strata=~REGION,weights=~WEIGHT,
       nest=TRUE,data=z[z$valid_design,,drop=FALSE])
  dom <- subset(d,complete)
  ss <- dom$variables$sum19
  out <- lapply(sort(unique(cuts)),function(cut) {
    dom$variables$positive <- as.numeric(ss>=cut)
    e <- survey::svymean(~positive,dom,na.rm=FALSE)
    ci <- as.numeric(stats::confint(e))
    np <- sum(ss>=cut)
    data.frame(cutoff=cut,n_positive=if(np>=cfg$min_report_cell)np else NA_integer_,
               n_negative=if(length(ss)-np>=cfg$min_report_cell)length(ss)-np else NA_integer_,
               prevalence=as.numeric(stats::coef(e)),se=as.numeric(survey::SE(e)),
               ci_lower=ci[1],ci_upper=ci[2])
  })
  do.call(rbind,out)
}
clinical_compare <- function(clinical_tbl=clinical) {
  phq <- subset(clinical_tbl,instrument=="PHQ9" & cutoff==cfg$primary_phq9_cutoff)
  if (nrow(phq)!=1L) stop("Missing unique adolescent PHQ comparator")
  ces <- subset(clinical_tbl,instrument=="CESD20")
  ces$delta_se <- ces$sensitivity-phq$sensitivity
  ces$delta_sp <- ces$specificity-phq$specificity
  ces$distance <- sqrt(.5*(ces$delta_se^2+ces$delta_sp^2))
  # Descriptive differences ONLY. There is no justified fixed tolerance or
  # independent-patient meta-analysis for these heterogeneous studies.
  ces$interpretation <- "descriptive external clinical evidence; not a linked-score calibration"
  ces
}

# ---- Validation: these indicators remain external to the CES-D classifier -----
# Counts and response labels MUST match the questionnaire version in the enclave.
# No wave-II indicator is admitted as a Wave-I confounder or as a replacement item.
validation_spec <- data.frame(
  variable=c("H2GH40","H2GH41","H2SU1","H2SU2","H2HS5",
             "H2GH23","H2GH24","H2GH25","H2GH26","H2GH27"),
  label=c("missed school due to health/emotional problem, past month",
          "missed social/recreation due to health/emotional problem, past month",
          "serious suicidal ideation, 12m","suicide attempts, 12m",
          "psychological/emotional counseling, 12m",
          "sleep difficulties, 12m","trouble relaxing, 12m","moodiness, 12m",
          "crying frequently, 12m","fearfulness, 12m"),
  status=c("freq","freq","binary","attempt_count","binary",
           rep("freq",5)),
  # >=1 is any impairment; >=2 for often-reported annual symptoms.
  threshold=c(1,1,1,1,1,2,2,2,2,2),
  stringsAsFactors=FALSE
)
safe_binary <- function(x,kind,other=NULL,threshold=2) {
  x <- as_num(x)
  if (kind=="binary") {
    out <- ifelse(x==1,1,ifelse(x==0,0,NA_real_))
  } else if (kind=="attempt_count") {
    out <- ifelse(x %in% 1:4,1,ifelse(x==0,0,NA_real_))
    # Legitimate skip only denotes zero when the routing ideation answer is no.
    if (!is.null(other)) {
      idx <- is.finite(x) & x==7 & is.finite(other) & other==0
      out[idx] <- 0
    }
  } else if (kind=="freq") {
    # 7=legitimate skip for school attendance is not a negative response.
    out <- ifelse(x %in% 0:4,as.numeric(x>=threshold),NA_real_)
  } else stop("Unknown construct indicator type")
  out
}
construct_validation <- function(z,cuts) {
  if (!any(z$valid_design)) return(data.frame())
  des <- survey::svydesign(ids=~PSUSCID,strata=~REGION,weights=~WEIGHT,
                           nest=TRUE,data=z[z$valid_design,,drop=FALSE])
  out <- list();ix <- 0L
  for (i in seq_len(nrow(validation_spec))) {
    row <- validation_spec[i,]
    nm <- row$variable
    if (!nm %in% names(des$variables)) next
    if (nm=="H2HS5" && !cfg$enable_counseling_validation) next
    other <- if("H2SU1" %in% names(des$variables))
      as_num(des$variables$H2SU1) else NULL
    des$variables$indicator <- safe_binary(des$variables[[nm]],row$status,
                                          other,row$threshold)
    if (sum(des$variables$complete & is.finite(des$variables$indicator))<100L) next
    for (cut in sort(unique(cuts))) {
      des$variables$group <- as.integer(des$variables$sum19>=cut)
      for (arm in 0:1) {
        a <- des$variables$complete & is.finite(des$variables$indicator) &
             des$variables$group==arm
        n <- sum(a)
        pos <- sum(des$variables$indicator[a]==1)
        neg <- n-pos
        if (n==0L) next
        releasable <- n>=cfg$min_report_cell &&
                      pos>=cfg$min_report_cell && neg>=cfg$min_report_cell
        estimate <- se <- lo <- hi <- NA_real_
        if (releasable) {
          # Subset the full valid-design frame, not a reconstructed
          # post-exclusion svydesign, to retain domain linearization.
          subdes <- subset(des,complete & is.finite(indicator) & group==arm)
          v <- tryCatch(survey::svymean(~indicator,subdes),
                        error=function(e) NULL)
          if (!is.null(v)) {
            estimate <- as.numeric(stats::coef(v))
            se <- as.numeric(survey::SE(v))
            ci <- as.numeric(stats::confint(v))
            lo <- ci[1];hi <- ci[2]
          }
        }
        ix <- ix+1L
        out[[ix]] <- data.frame(variable=nm,label=row$label,cutoff=cut,
          group=arm,n=if(n>=cfg$min_report_cell)n else NA_integer_,
          positive=if(releasable)pos else NA_integer_,
          weighted_rate=estimate,se=se,ci_lower=lo,ci_upper=hi,
          status="construct association, not diagnostic validity")
      }
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind,out)
}

item_fit_summary <- function(x,w,fit) {
  prior <- grid_prior(fit$theta,fit$mu,fit$sd)
  out <- list();k <- 0L
  for (nm in fit$fit_items) {
    p <- fit$P[nm,]
    pm <- item_probs(fit$theta,p[1],p[2:4])
    for (cat in 0:3) {
      k <- k+1L
      obs <- sum(w*(x[[nm]]==cat))/sum(w)
      pred <- sum(prior*pm[cat+1L,])
      out[[k]] <- data.frame(item=nm,category=cat,observed=obs,
        predicted=pred,abs_error=abs(obs-pred),
        fixed_anchor=nm %in% fit$anchor_names)
    }
  }
  do.call(rbind,out)
}
# Descriptive conditional residual checks; NOT formal DIF or Yen's Q3.
descriptive_item_residuals <- function(x,w,fit,post) {
  n <- nrow(x)
  res <- matrix(NA_real_,n,length(fit$fit_items),
                dimnames=list(NULL,fit$fit_items))
  for (j in seq_along(fit$fit_items)) {
    nm <- fit$fit_items[j]
    p <- fit$P[nm,]
    means_theta <- colSums(item_probs(fit$theta,p[1],p[2:4])*(0:3))
    pred <- as.numeric(post$post %*% means_theta)
    res[,j] <- x[[nm]]-pred
  }
  wm <- colSums(res*w)/sum(w)
  center <- sweep(res,2,wm,"-")
  covar <- crossprod(center,center*w)/sum(w)
  sdv <- sqrt(pmax(0,diag(covar)))
  cor <- covar/outer(sdv,sdv)
  cor[!is.finite(cor)] <- NA_real_
  ii <- which(upper.tri(cor),arr.ind=TRUE)
  out <- data.frame(item1=colnames(res)[ii[,1]],item2=colnames(res)[ii[,2]],
                    residual_cor=cor[ii])
  out[order(-abs(out$residual_cor)),,drop=FALSE]
}
# DIF is *screening only*: difference in response after coarse rest-score control.
# Does NOT replace a formal multigroup IRT DIF model or establish invariance.
dif_screen <- function(z,items) {
  zz <- z[z$analysis_domain,,drop=FALSE]
  out <- list();ct <- 0L
  for (grp in cfg$dif_group_vars) {
    if (!grp %in% names(zz)) next
    g <- as.character(zz[[grp]])
    valid <- !is.na(g) & nzchar(g)
    lev <- sort(unique(g[valid]))
    if (length(lev)<2L || length(lev)>12L) next
    # Pair every category with the most prevalent category to avoid imposing
    # an arbitrary numerical ordering on nominal groups. This is only an
    # item-rest-score screen, not a confirmatory multigroup IRT DIF analysis.
    ref <- names(which.max(table(g[valid])))
    for (nm in items) {
      rest <- zz$sum19-zz[[nm]]
      bins <- cut(rest,breaks=c(-1,9,19,29,56),include.lowest=TRUE)
      for (alternative in setdiff(lev,ref)) for (bn in levels(bins)) {
        i0 <- valid & g==ref & bins==bn
        i1 <- valid & g==alternative & bins==bn
        n0 <- sum(i0);n1 <- sum(i1)
        ct <- ct+1L
        out[[ct]] <- data.frame(group_variable=grp,item=nm,rest_bin=bn,
          reference_group=ref,comparison_group=alternative,
          reference_n=if(n0>=cfg$min_report_cell)n0 else NA_integer_,
          comparison_n=if(n1>=cfg$min_report_cell)n1 else NA_integer_,
          weighted_mean_difference=if(n0>=cfg$min_report_cell && n1>=cfg$min_report_cell)
              stats::weighted.mean(zz[[nm]][i1],zz$WEIGHT[i1])-
              stats::weighted.mean(zz[[nm]][i0],zz$WEIGHT[i0]) else NA_real_)
      }
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind,out)
}

# Optionally inspect the uploaded Vilagut workbook without importing fractional
# reconstructed TP/FP/FN/TN as independent Bernoulli patient observations.
vilagut_audit <- function(path) {
  if (!nzchar(path)) return(data.frame())
  if (!file.exists(path)) stop("Configured Vilagut workbook missing: ",path)
  if (!requireNamespace("readxl",quietly=TRUE))
    stop("Install 'readxl' to audit Vilagut XLS")
  tab <- as.data.frame(readxl::read_excel(path,sheet="cut16orreported(n=28)"))
  names(tab) <- toupper(names(tab))
  needed <- c("STUDY","AGEGRP","SEN_ES","ESP_ES","CUT-OFF")
  if (!all(needed %in% names(tab)))
    stop("Unexpected Vilagut workbook fields: ",paste(names(tab),collapse=", "))
  is_adolescent <- grepl("adolescent",tab$AGEGRP,ignore.case=TRUE)
  a <- tab[!is.na(is_adolescent) & is_adolescent,,drop=FALSE]
  if (nrow(a)!=6L) warning("Expected six adolescent study rows; found ",nrow(a))
  a$anomaly <- with(a, !is.finite(as_num(`CUT-OFF`)) |
                     as_num(`CUT-OFF`)<=0 |
                     !is.finite(as_num(SEN_ES)) | as_num(SEN_ES)<=0 |
                     !is.finite(as_num(ESP_ES)) | as_num(ESP_ES)<=0)
  a
}

safe_csv <- function(df,path) {
  if (!isTRUE(cfg$allow_outputs) || nrow(df)==0) return(invisible(NULL))
  utils::write.csv(df,path,row.names=FALSE,na="")
}
record_text <- function(path,txt) {
  if (isTRUE(cfg$allow_outputs)) writeLines(txt,con=path,useBytes=TRUE)
}

# Analyze ideation with and without the overlapping H2FS19 item. Descriptive,
# design-adjusted construct check; not a diagnostic-validity or causal model.
suicidality_overlap_check <- function(z) {
  if (!"H2SU1" %in% names(z)) return(data.frame())
  zz <- z[z$valid_design,,drop=FALSE]
  zz$ideation <- safe_binary(zz$H2SU1,"binary")
  zz$sum18 <- zz$sum19-zz$H2FS19
  ok <- zz$complete & is.finite(zz$ideation)
  if (sum(ok & zz$ideation==1)<cfg$min_report_cell ||
      sum(ok & zz$ideation==0)<cfg$min_report_cell) return(data.frame())
  full <- survey::svydesign(ids=~PSUSCID,strata=~REGION,weights=~WEIGHT,
                            nest=TRUE,data=zz)
  ds <- subset(full,complete & is.finite(ideation))
  models <- list(total19=ideation~sum19,
                 total18_without_new_item=ideation~sum18,
                 total18_plus_new_item=ideation~sum18+H2FS19)
  out <- list();k <- 0L
  for (nm in names(models)) {
    fit <- tryCatch(survey::svyglm(models[[nm]],design=ds,
                   family=stats::quasibinomial()),error=function(e)NULL)
    if (is.null(fit)) next
    b <- stats::coef(fit)
    V <- stats::vcov(fit)
    for (term in setdiff(names(b),"(Intercept)")) {
      k <- k+1L
      # OR per 5 points for the summed-score terms; per one point for H2FS19.
      scale <- if(term=="H2FS19")1 else 5
      se <- sqrt(V[term,term])
      out[[k]] <- data.frame(model=nm,term=term,
            scale_points=scale,odds_ratio=exp(b[term]*scale),
            ci_lower=exp((b[term]-1.96*se)*scale),
            ci_upper=exp((b[term]+1.96*se)*scale),
            estimated_from="survey logistic construct association, not diagnosis")
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind,out)
}

# ---- Model-implied observed-score linking, preserving common item responses ---
# For a given latent theta, S19=C+A and S20=C+B where C is the exact sum
# of 16 common item scores, A the 3 changed/new Add Health scores, and
# B the 4 original CES-D scores (7, 11, 17, 20). This explicitly preserves
# the common item responses, unlike multiplying two independent whole-test
# score distributions. A and B are conditionally independent given theta;
# their residual association is not identified without paired administration.
score_dist <- function(theta,p) {
  d <- 1
  for (j in seq_len(nrow(p))) {
    q <- item_probs(theta,p[j,1],p[j,2:4])[,1]
    nxt <- numeric(length(d)+3L)
    for (k in 0:3) {
      ii <- seq_along(d)+k
      nxt[ii] <- nxt[ii]+d*q[k+1L]
    }
    d <- nxt
  }
  if (abs(sum(d)-1)>1e-8) stop("Item score convolution failed")
  d
}
score_joint <- function(fit,common,add_only,original_only) {
  if (!all(c(common,add_only) %in% rownames(fit$P)))
    stop("Invalid Add Health items in joint score link")
  M <- 3L*(length(common)+length(add_only))
  N <- 3L*(length(common)+nrow(original_only))
  ans <- matrix(0,nrow=M+1L,ncol=N+1L)
  prior <- grid_prior(fit$theta,fit$mu,fit$sd)
  for (g in seq_along(fit$theta)) {
    t <- fit$theta[g]
    C <- score_dist(t,fit$P[common,,drop=FALSE])
    A <- score_dist(t,fit$P[add_only,,drop=FALSE])
    B <- score_dist(t,original_only)
    for (c in 0:(length(C)-1L)) {
      for (a in 0:(length(A)-1L)) {
        r <- c+a+1L
        j <- c+seq_along(B)
        ans[r,j] <- ans[r,j]+prior[g]*C[c+1L]*A[a+1L]*B
      }
    }
  }
  if (abs(sum(ans)-1)>1e-7 || any(ans<0))
    stop("Joint-score distribution not normalized")
  ans
}
modeled_observed_score_link <- function(fit) {
  shared <- map$addhealth[map$type=="shared"]
  extra <- setdiff(fit$fit_items,shared)
  if (length(shared)!=16L || length(extra)!=3L)
    stop("Unexpected shared/changed item structure")
  p20 <- as.matrix(olino[,c("a","b1","b2","b3")])
  p20[,1] <- p20[,1]*cfg$grm_slope_multiplier
  j <- score_joint(fit,shared,extra,p20[c(7,11,17,20),,drop=FALSE])
  if (!identical(dim(j),c(58L,61L)))
    stop("Expected a 58 by 61 score joint distribution")
  m <- rowSums(j)
  if (any(m<=0) || any(!is.finite(m)))
    stop("Zero or nonfinite modeled score probability")
  cond <- sweep(j,1,m,"/")
  stopifnot(max(abs(rowSums(cond)-1))<1e-7)
  e20 <- as.numeric(cond %*% (0:60))
  et  <- as.numeric(cond %*% cesd_t)
  out <- data.frame(raw_addhealth_score=0:57,
     expected_original20=e20,
     expected_PROMIS_T=et,
     model_predicted_score_probability=m)
  attr(out,"conditional_original20") <- cond
  out
}

# ---- PRIMARY ESTIMAND (review revision): IRT observed-score equipercentile ----
# Notation: X = Add Health S19 (0..57), Y = original CES-D-20 (0..60) that the same
# Wave II adolescents would obtain under standard administration. For a PHQ-9
# boundary q (9.5 = "PHQ-9 >= 10"), y*(q) = PROsetta CES-D equivalent of q
# (linear interpolation of Choi 2014 Tables A1/A2). x*(q) = F_X^{-1}(F_Y(y*(q))),
# both CDFs percentile-rank continuized, in the weighted Wave II population.
# Integer cutoff c = floor(x* + 1): the integer whose continuized boundary c-0.5
# is nearest x*. Hence P(X >= c) ~= P(Y >= y*): the exposed group has the size of
# the group at or above PHQ-9>=10-equivalent CES-D-20 severity.
# F_Y is model-implied (Lord-Wingersky over the 20 Olino items) at the fitted latent
# distribution shifted by delta. F_X is model-implied (primary) or observed-weighted.
lw_marginal <- function(theta,prior,par) {
  par <- as.matrix(par)
  out <- numeric(3L*nrow(par)+1L)
  for (g in seq_along(theta)) if (prior[g]>1e-300)
    out <- out+prior[g]*score_dist(theta[g],par)
  out/sum(out)
}
prank_cdf <- function(m,x) {
  K <- length(m)-1L
  F <- cumsum(m)
  k <- pmin(K,pmax(0L,floor(x+0.5)))
  pmin(1,pmax(0,(F[k+1L]-m[k+1L])+m[k+1L]*(x-(k-0.5))))
}
equip_boundary <- function(m_from,m_to,y_to) {
  if (abs(sum(m_from)-1)>1e-8 || abs(sum(m_to)-1)>1e-8) stop("Score distributions must sum to 1")
  p <- prank_cdf(m_to,y_to)
  xs <- seq(-0.5,length(m_from)-0.5,length.out=40001L)
  stats::approx(prank_cdf(m_from,xs),xs,xout=p,ties=mean,rule=2)$y
}
phq_boundary_to_cesd <- function(q) {
  if (q < 0 || q > 27) stop("PHQ boundary outside 0..27")
  interp_cesd(stats::approx(0:27,phq_t,xout=q)$y)
}
olino20 <- function() {
  p20 <- as.matrix(olino[,c("a","b1","b2","b3")])
  p20[,1] <- p20[,1]*cfg$grm_slope_multiplier
  p20
}
equipercentile_cutoffs <- function(fit,obs_m19=NULL,anchor_set="all16") {
  m19_model <- lw_marginal(fit$theta,grid_prior(fit$theta,fit$mu,fit$sd),
                           fit$P[map$addhealth,,drop=FALSE])
  if (length(m19_model)!=58L) stop("S19 marginal must have 58 cells")
  p20 <- olino20()
  out <- list();k <- 0L
  for (delta in cfg$format_shift_scenarios) {
    m20 <- lw_marginal(fit$theta,grid_prior(fit$theta,fit$mu+delta,fit$sd),p20)
    if (length(m20)!=61L) stop("S20 marginal must have 61 cells")
    for (ref in names(cfg$phq_boundaries)) {
      q <- unname(cfg$phq_boundaries[ref]); ystar <- phq_boundary_to_cesd(q)
      for (src in c("model",if(!is.null(obs_m19))"observed_weighted")) {
        mx <- if (src=="model") m19_model else obs_m19
        xstar <- equip_boundary(mx,m20,ystar)
        cut <- as.integer(min(57,max(0,floor(xstar+1))))
        k <- k+1L
        out[[k]] <- data.frame(anchor_set=anchor_set,phq_reference=ref,phq_boundary=q,
          promis_T=stats::approx(0:27,phq_t,xout=q)$y,cesd20_boundary=ystar,
          format_shift_delta=delta,fx_source=src,s19_boundary=xstar,cutoff=cut,
          prev_S19_at_cutoff_from_fx=sum(mx[(cut+1L):58]),
          prev_S20_target_model=1-prank_cdf(m20,ystar))
      }
    }
  }
  do.call(rbind,out)
}
# Aggregate score-level fit of the latent model (no row-level output).
score_distribution_fit <- function(fit,S,W) {
  m_model <- lw_marginal(fit$theta,grid_prior(fit$theta,fit$mu,fit$sd),
                         fit$P[map$addhealth,,drop=FALSE])
  m_obs <- vapply(0:57,function(s)sum(W[S==s]),numeric(1))/sum(W)
  out <- lapply(10:35,function(k){
    npos <- sum(S>=k)
    ok <- npos>=cfg$min_report_cell && (length(S)-npos)>=cfg$min_report_cell
    data.frame(k=k,observed_weighted_P_S19_ge_k=if(ok)sum(m_obs[(k+1L):58]) else NA_real_,
               model_P_S19_ge_k=sum(m_model[(k+1L):58]),
               difference=if(ok)sum(m_obs[(k+1L):58])-sum(m_model[(k+1L):58]) else NA_real_)
  })
  list(table=do.call(rbind,out),m_obs=m_obs,m_model=m_model)
}
# Collaborator response (2026-11-21): anchor specifications are compared on COMMON
# observed targets (all 19 items' category shares; observed weighted S19 mean, SD
# and tail near the cut). No automatic selection: better internal fit does not
# establish a more valid external link. latent_mu/sd are on Olino's metric, where
# the OADP calibration sample is N(0,1): an external plausibility reference only.
# Headline text: primary cutoff WITH its qualifications (collaborator memo, item 4).
headline_lines <- function(equip,gate,anchor_cmp) {
  sel <- function(set,src,ref) equip[equip$anchor_set==set & equip$fx_source==src &
                                      equip$phq_reference==ref,,drop=FALSE]
  pr <- sel("all16","observed_weighted",cfg$primary_phq_boundary)
  p0 <- pr[abs(pr$format_shift_delta)<1e-12,,drop=FALSE]
  if (nrow(p0)!=1L) stop("Primary equipercentile row missing or duplicated")
  warn <- length(gate$bad)>0L
  out <- c(paste0("PRIMARY CUTOFF: S19 >= ",p0$cutoff,
    " [equipercentile; OBSERVED survey-weighted S19 vs model-implied CES-D-20; 16 fixed anchors; ",
    cfg$primary_phq_boundary," boundary ",cfg$phq_boundaries[cfg$primary_phq_boundary],"; delta = 0]",
    " continuous boundary ",round(p0$s19_boundary,2),
    "; observed weighted share at cutoff ",round(100*p0$prev_S19_at_cutoff_from_fx,1),"%",
    "; modelled CES-D-20 target share ",round(100*p0$prev_S20_target_model,1),"%"),
    paste0("  Conditional equipercentile cutoff under external anchor-invariance and specified ",
      "response-format assumptions; ",if(warn)"anchor-fit warning present" else "no anchor-fit warning","."),
    "  Population-share severity concordance; not a diagnostic, NSDUH-MDE or individual PHQ-9 screen equivalence.")
  if (warn) out <- c(out,paste0("  ANCHOR-FIT WARNING (descriptive flag at ",cfg$anchor_fit_tolerance,
    ", not a validated invariance test): ",paste(gate$bad,collapse=", ")))
  for (ref in setdiff(names(cfg$phq_boundaries),cfg$primary_phq_boundary)) {
    r <- sel("all16","observed_weighted",ref); r <- r[abs(r$format_shift_delta)<1e-12,,drop=FALSE]
    out <- c(out,paste0("  Alternative convention ",ref," (boundary ",cfg$phq_boundaries[ref],"): S19 >= ",r$cutoff))
  }
  pm <- sel("all16","model",cfg$primary_phq_boundary); pm <- pm[abs(pm$format_shift_delta)<1e-12,,drop=FALSE]
  out <- c(out,paste0("  Sensitivity, model-implied S19: S19 >= ",pm$cutoff))
  if (!is.null(anchor_cmp) && nrow(anchor_cmp)>1L)
    out <- c(out,paste0("  Sensitivity, positive items freed (12 anchors): S19 >= ",anchor_cmp$cutoff_observed_Fx[2],
      "; common-anchor max misfit 16 vs 12 = ",round(anchor_cmp$item_max_abs_error_common_anchors[1],3)," vs ",
      round(anchor_cmp$item_max_abs_error_common_anchors[2],3)," (no automatic selection)"))
  out <- c(out,paste0("  Format-shift ASSUMPTION scenarios (not confidence limits), delta ",
    paste(range(cfg$format_shift_scenarios),collapse=" to "),": S19 >= ",min(pr$cutoff)," to ",max(pr$cutoff),
    "; delta is not identified by Add Health data."))
  out
}
anchor_model_summary <- function(fit,label,itemfit,sfit,eq,S,W) {
  e <- eq[eq$phq_reference==cfg$primary_phq_boundary & abs(eq$format_shift_delta)<1e-12,,drop=FALSE]
  c_obs <- e$cutoff[e$fx_source=="observed_weighted"][1]
  c_mod <- e$cutoff[e$fx_source=="model"][1]
  near <- sfit$table[abs(sfit$table$k-c_obs)<=2,,drop=FALSE]
  mm <- sfit$m_model; ks <- 0:57
  m_obs_mean <- sum(W*S)/sum(W); m_obs_sd <- sqrt(sum(W*(S-m_obs_mean)^2)/sum(W))
  mod_mean <- sum(ks*mm); mod_sd <- sqrt(sum((ks-mod_mean)^2*mm))
  data.frame(anchor_set=label,n_fixed_anchors=length(fit$anchor_names),
    latent_mu=fit$mu,latent_sd=fit$sd,
    item_max_abs_error_all19=max(itemfit$abs_error),
    item_mean_abs_error_all19=mean(itemfit$abs_error),
    item_max_abs_error_fixed_anchors=max(itemfit$abs_error[itemfit$fixed_anchor]),
    # Fair comparison: the anchors fixed in BOTH specifications (same items, same
    # fixed parameters); freed items fit by construction and are excluded here.
    item_max_abs_error_common_anchors=max(itemfit$abs_error[itemfit$item %in%
      setdiff(anchors,cfg$positive_items)]),
    item_mean_abs_error_common_anchors=mean(itemfit$abs_error[itemfit$item %in%
      setdiff(anchors,cfg$positive_items)]),
    S19_mean_observed=m_obs_mean,S19_mean_model=mod_mean,
    S19_sd_observed=m_obs_sd,S19_sd_model=mod_sd,
    max_abs_tail_diff_near_cut=if(nrow(near)&&any(is.finite(near$difference)))
      max(abs(near$difference),na.rm=TRUE) else NA_real_,
    implied_P_S20_target=e$prev_S20_target_model[1],
    cutoff_observed_Fx=c_obs,cutoff_model_Fx=c_mod,
    selection="none (sensitivity comparison only)",stringsAsFactors=FALSE)
}
anchor_fit_gate <- function(itemfit,tol=cfg$anchor_fit_tolerance) {
  a <- itemfit[itemfit$fixed_anchor,,drop=FALSE]
  worst <- tapply(a$abs_error,a$item,max)
  bad <- names(worst)[worst>tol]
  list(worst=worst,bad=bad)
}

# ---- Optional genuine multiple-cutoff meta-analysis, only for verified cases ----
# Do not convert Vilagut's fractional reconstructed cells into exact counts.
# The input file must have study, cutoff, TP, FP, TN, FN (actual observed integers),
# with comparable diagnostic targets and consistent within-study denominators.
clinical_multi_threshold_meta <- function(path,outdir) {
  if (!file.exists(path)) stop("Clinical raw-counts file missing: ",path)
  if (!requireNamespace("diagmeta",quietly=TRUE))
    stop("Install CRAN package 'diagmeta' to run verified multi-threshold meta-analysis")
  e <- utils::read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
  needed <- c("study","cutoff","TP","FP","TN","FN")
  if (!all(needed %in% names(e)))
    stop("Exact-count meta file needs columns: ",paste(needed,collapse=", "))
  if (!nrow(e) || anyNA(e[,needed]) || any(!is.finite(as.matrix(e[,needed[-1]]))))
    stop("Clinical exact-count table contains NA or nonnumeric values")
  cnt <- as.matrix(e[,c("TP","FP","TN","FN")])
  if (any(cnt<0) || any(abs(cnt-round(cnt))>1e-9))
    stop("Clinical meta counts must be genuine nonnegative integer patient counts")
  if (anyDuplicated(paste(e$study,e$cutoff))) stop("Duplicate study + cutoff")
  if (length(unique(e$study))<3L || !any(table(e$study)>1L))
    stop("Clinical data insufficient for multiple-threshold meta-analysis")
  for (nm in unique(e$study)) {
    p <- e[e$study==nm,,drop=FALSE]
    p <- p[order(p$cutoff),,drop=FALSE]
    if (length(unique(p$TP+p$FN))!=1L || length(unique(p$FP+p$TN))!=1L)
      stop("Different denominators at different cutoffs for ",nm)
    if (nrow(p)>1L && (any(diff(p$TP)>0) || any(diff(p$FP)>0) ||
                       any(diff(p$FN)<0) || any(diff(p$TN)<0)))
      stop("Nonmonotone diagnostic counts across cutoffs for ",nm)
  }
  d <- diagmeta::diagmeta(TP=e$TP,FP=e$FP,TN=e$TN,FN=e$FN,
       cutoff=e$cutoff,studlab=e$study,distr=cfg$clinical_meta_distr,
       model="CICS",equalvar=FALSE,silent=TRUE)
  safe_csv(as.data.frame(diagmeta::diagstats(d,cutoff=16:30)),
           file.path(outdir,"16_VERIFIED_DIAGMETA_ACCURACY.csv"))
  record_text(file.path(outdir,"16_VERIFIED_DIAGMETA_MODEL.txt"),
              capture.output(summary(d)))
  invisible(d)
}

# ---- Optional longitudinal measurement-consistency check, no adult outcomes ---
wave1_stability <- function(z,cuts) {
  path <- cfg$wave1_xpt
  if (!nzchar(path) || !file.exists(path)) stop("Specify wave1_xpt before enabling stability")
  vars <- paste0("H1FS",1:19)
  old <- read_xpt_small(path,c("AID",vars),required=c("AID",vars))
  old$AID <- as_key(old$AID,"Wave I")
  if (anyDuplicated(old$AID)) stop("Duplicate Wave I AIDs")
  idx <- match(z$AID,old$AID)
  x <- as.data.frame(lapply(old[vars],function(v)as_num(v)[idx]))
  for (j in seq_along(vars)) {
    x[[j]][!(x[[j]] %in% 0:3)] <- NA_real_
    if (j %in% c(4L,8L,11L,15L)) x[[j]] <- 3-x[[j]]
  }
  ss1 <- rowSums(x,na.rm=FALSE)
  use <- z$analysis_domain & is.finite(ss1)
  if (sum(use)<cfg$min_complete_n) stop("Insufficient paired Wave I/II records")
  out <- list();k<-0L
  for (cut in cuts) for (s1 in 0:1) for (s2 in 0:1) {
    keep <- use & as.integer(ss1>=cut)==s1 & as.integer(z$sum19>=cut)==s2
    n <- sum(keep)
    k <- k+1L
    out[[k]] <- data.frame(cutoff=cut,positive_wave1=s1,positive_wave2=s2,
      n=if(n>=cfg$min_report_cell)n else NA_integer_,
      weighted_share=if(n>=cfg$min_report_cell)
        sum(z$WEIGHT[keep])/sum(z$WEIGHT[use]) else NA_real_)
  }
  do.call(rbind,out)
}

# ---- Wrapper: source -> fit -> multi-reference calibration -> diagnostics ----
run_measurement <- function() {
  if (!dir.exists(cfg$output_dir) && isTRUE(cfg$allow_outputs))
    dir.create(cfg$output_dir,recursive=TRUE,showWarnings=FALSE)
  if (isTRUE(cfg$allow_outputs) && !dir.exists(cfg$output_dir))
    stop("Could not create output_dir")
  z <- read_data()
  keep <- z$analysis_domain
  obs <- z[keep,,drop=FALSE]
  vars <- map$addhealth  # all 19 scored items; sensitivity excludes item19 only in construct check
  X <- obs[,vars,drop=FALSE]
  S <- obs$sum19
  W <- obs$WEIGHT
  message("Calibrating ",nrow(obs)," eligible Wave II respondents, ",
          length(unique(obs$PSUSCID))," PSUs ...")
  fit <- fit_external_grm(X,W)
  post <- posterior_irt(X,fit)
  safe_csv(data.frame(item=rownames(fit$P),fit$P,
    role=ifelse(rownames(fit$P) %in% fit$anchor_names,"fixed_anchor","estimated")),
    file.path(cfg$output_dir,"02_item_irt_parameters.csv"))
  itemfit <- item_fit_summary(X,W,fit)
  safe_csv(itemfit,file.path(cfg$output_dir,"03_item_marginal_fit.csv"))
  gate <- anchor_fit_gate(itemfit)
  if (length(gate$bad))
    warning("ANCHOR TRANSPORT QUESTIONABLE: fixed-anchor category misfit > ",
            cfg$anchor_fit_tolerance," for ",paste(gate$bad,collapse=", "),
            ". Treat the PHQ-equivalent cutoff as conditional on anchor invariance.")
  sfit <- score_distribution_fit(fit,S,W)
  safe_csv(sfit$table,file.path(cfg$output_dir,"18_SCORE_DISTRIBUTION_FIT.csv"))
  equip <- equipercentile_cutoffs(fit,obs_m19=sfit$m_obs,anchor_set="all16")
  cmp <- list(anchor_model_summary(fit,"all16",itemfit,sfit,equip,S,W))
  if (isTRUE(cfg$run_positive_free_sensitivity)) {
    pf <- tryCatch({
      fpos <- fit_external_grm(X,W,setdiff(anchors,cfg$positive_items))
      # Same targets and definitions as the 16-anchor model (all 19 items, observed S19).
      itemfit_pf <- item_fit_summary(X,W,fpos)
      sfit_pf <- score_distribution_fit(fpos,S,W)
      safe_csv(itemfit_pf,file.path(cfg$output_dir,"03b_item_marginal_fit_neg12.csv"))
      safe_csv(sfit_pf$table,file.path(cfg$output_dir,"18b_SCORE_DISTRIBUTION_FIT_neg12.csv"))
      eq_pf <- equipercentile_cutoffs(fpos,obs_m19=sfit$m_obs,anchor_set="neg12_positive_items_free")
      list(eq=eq_pf,row=anchor_model_summary(fpos,"neg12_positive_items_free",itemfit_pf,sfit_pf,eq_pf,S,W))
    },error=function(e){warning("Positive-item-free refit failed: ",conditionMessage(e));NULL})
    if (!is.null(pf)) { equip <- rbind(equip,pf$eq); cmp[[2]] <- pf$row }
  }
  anchor_cmp <- do.call(rbind,cmp)
  safe_csv(equip,file.path(cfg$output_dir,"17_EQUIPERCENTILE_CUTOFFS.csv"))
  safe_csv(anchor_cmp,file.path(cfg$output_dir,"19_ANCHOR_MODEL_COMPARISON.csv"))
  if (cfg$run_local_dependence)
    safe_csv(descriptive_item_residuals(X,W,fit,post),
             file.path(cfg$output_dir,"04_local_residual_dependence.csv"))
  if (cfg$run_dif_screen)
    safe_csv(dif_screen(z,fit$fit_items),
             file.path(cfg$output_dir,"05_group_item_restscore_screen.csv"))
  if (nzchar(cfg$clinical_meta_verified_counts_csv)) {
    clinical_multi_threshold_meta(cfg$clinical_meta_verified_counts_csv,cfg$output_dir)
  }
  if (nzchar(cfg$vilagut_xls)) safe_csv(vilagut_audit(cfg$vilagut_xls),
                                        file.path(cfg$output_dir,"06_vilagut_adolescent_source_audit.csv"))
  if (cfg$run_wave1_stability)
    safe_csv(wave1_stability(z,sort(unique(c(18:26)))),
             file.path(cfg$output_dir,"14_WAVEI_WAVEII_TRANSITIONS.csv"))
  references <- list()
  if ("prosetta" %in% cfg$methods) {
    t0 <- phq_t[cfg$primary_phq9_cutoff+1L]
    for (shift in cfg$promis_t_shift_scenarios) {
      key <- sprintf("PROsetta_PHQ%02d_Tshift_%+.1f",cfg$primary_phq9_cutoff,shift)
      references[[key]] <- list(c20=interp_cesd(t0+shift),
                                method="external PROsetta score-severity link",
                                shift=shift)
    }
  }
  if ("adolescent_clinical" %in% cfg$methods) {
    if (cfg$primary_phq9_cutoff != 10L && !nzchar(cfg$clinical_override_csv))
      stop("Adolescent PHQ-9 accuracy for the selected cutoff is not built in: supply clinical_override_csv or disable adolescent_clinical")
    clinical_in <- clinical
    if (nzchar(cfg$clinical_override_csv)) {
      clinical_in <- utils::read.csv(cfg$clinical_override_csv,stringsAsFactors=FALSE)
      if (!all(setdiff(names(clinical),"verification") %in% names(clinical_in)))
        stop("clinical_override_csv missing required columns")
    }
    val <- clinical_compare(clinical_in)
    safe_csv(val,file.path(cfg$output_dir,"07_clinical_adolescent_comparisons.csv"))
    # These studies were not jointly administered with PHQ-9, so their
    # heterogeneous sensitivity/specificity cannot identify a score link.
  }
  if ("original_cesd_scenarios" %in% cfg$methods) {
    for (c20 in cfg$original_cesd_scenarios) {
      references[[paste0("CESD20_reference_",c20)]] <-
        list(c20=c20,method="original CES-D severity scenario",shift=NA_real_)
    }
  }
  if (!length(references)) stop("No reference method enabled")
  linked_scores <- modeled_observed_score_link(fit)
  summary <- list(); risk_all <- list();j <- 0L
  for (nm in names(references)) {
    ref <- references[[nm]]
    lk <- link_target(ref$c20,fit,S,W,post,score_link=linked_scores)
    j <- j+1L
    summary[[j]] <- data.frame(reference=nm,method=ref$method,
      PROMIS_T_shift=ref$shift,original_cesd20_equivalent=lk$original_cesd,
      original_theta=lk$theta_ref,
      expected_addhealth_raw_score=lk$expected_addhealth,
      latent_positive_fraction=lk$latent_positive,
      observed_reference_T=lk$observed_target_T,
      observed_probability_link=lk$observed_prob_link,
      t(lk$candidate_cutoffs),check.names=FALSE)
    t0 <- lk$risk
    t0$reference <- nm
    risk_all[[j]] <- t0
  }
  summary <- do.call(rbind,summary)
  risk_all <- do.call(rbind,risk_all)
  # Row-level respondent data are never saved.
  safe_csv(linked_scores,file.path(cfg$output_dir,"15_IRT_OBSERVED_SCORE_LINK.csv"))
  safe_csv(summary,file.path(cfg$output_dir,"01_REFERENCE_CUTOFF_OPTIONS.csv"))
  # Collaborator response: the PRIMARY input is the OBSERVED survey-weighted S19
  # distribution; headline text is built by headline_lines() (unit-tested).
  hl <- headline_lines(equip,gate,anchor_cmp)
  for (h in hl) message(h)
  primary <- sprintf("PROsetta_PHQ%02d_Tshift_%+.1f",cfg$primary_phq9_cutoff,0)
  if (primary %in% summary$reference) {
    rr <- summary[summary$reference==primary,,drop=FALSE]
    message("SECONDARY (former headline; regression rule, PHQ=10 score point): >=",
            rr$observed_score_link[1],
            "; latent/TCC: >=",rr$true_score_ceiling[1],
            "; P(S20>=boundary|S19)>=.5: >=",rr$observed_probability_link[1])
  }
  # Suppress near-empty score tails in the released decision-risk table.
  canrelease <- vapply(risk_all$cutoff,function(c) {
    nn <- sum(S>=c)
    nn>=cfg$min_report_cell && (length(S)-nn)>=cfg$min_report_cell
  },logical(1))
  for (k in c("expected_error","balanced_error","fpr_latent",
              "fnr_latent","screening_prevalence")) risk_all[[k]][!canrelease] <- NA_real_
  safe_csv(risk_all,file.path(cfg$output_dir,"08_ALL_SCORE_DECISION_RISKS.csv"))
  cuts <- sort(unique(unlist(summary[,cfg$decision_rules,drop=FALSE],use.names=FALSE)))
  cuts <- as.integer(cuts[is.finite(cuts) & cuts>=0 & cuts<=57])
  cuts_all <- sort(unique(c(cuts,equip$cutoff,15:30)))
  prev <- survey_prevalences(z,S,cuts_all)
  safe_csv(prev,file.path(cfg$output_dir,"09_DESIGN_WEIGHTED_PREVALENCE.csv"))
  if (isTRUE(cfg$run_wave2_validation)) {
    v <- construct_validation(z,cuts_all)
    safe_csv(v,file.path(cfg$output_dir,"10_WAVEII_CONSTRUCT_VALIDATION.csv"))
    safe_csv(suicidality_overlap_check(z),
             file.path(cfg$output_dir,"10b_SUICIDALITY_ITEM_OVERLAP.csv"))
  }
  # Aggregate respondents by observed summed score; NOT an individual crosswalk.
  st <- lapply(0:57,function(s) {
    k <- which(S==s)
    if (length(k)<cfg$min_report_cell) return(NULL)
    data.frame(raw_cesd19=s,n=length(k),
      weighted_theta_EAP=stats::weighted.mean(post$mean_theta[k],W[k]),
      weighted_theta_SD=stats::weighted.mean(post$sd_theta[k],W[k]))
  })
  st <- Filter(Negate(is.null),st)
  if (length(st)) safe_csv(do.call(rbind,st),
                             file.path(cfg$output_dir,"11_SAMPLE_SPECIFIC_RAW_SCORE_EAP.csv"))

  # External item-bank perturbations: stress tests, NOT confidence intervals.
  if (isTRUE(cfg$run_anchor_loo)) {
    res <- lapply(anchors,function(omit) {
      message("Anchor removal stress test: ",omit)
      a <- tryCatch({
        ff <- fit_external_grm(X,W,setdiff(anchors,omit),restarts=1L)
        lk <- link_target(interp_cesd(phq_t[cfg$primary_phq9_cutoff+1L]),
                          ff,S,W,posterior_irt(X,ff))
        data.frame(anchor_removed=omit,
                   cutoff_true_score=as.integer(lk$candidate_cutoffs["true_score_ceiling"]),
                   cutoff_observed_link=as.integer(lk$candidate_cutoffs["observed_score_link"]),
                   error="")
      },error=function(e)data.frame(anchor_removed=omit,
                   cutoff_true_score=NA_integer_,
                   cutoff_observed_link=NA_integer_,error=conditionMessage(e)))
      a
    })
    safe_csv(do.call(rbind,res),file.path(cfg$output_dir,"12_ANCHOR_REMOVAL_STRESS.csv"))
  }
  B <- as.integer(cfg$psu_bootstrap_replicates)
  if (B>0L) {
    set.seed(cfg$seed+400L)
    all_design <- z[z$valid_design,,drop=FALSE]
    pools <- lapply(split(all_design$PSUSCID,all_design$REGION),unique)
    if (any(lengths(pools)<2L)) stop("At least two PSUs needed within each stratum")
    bo <- vector("list",B)
    for (b in seq_len(B)) {
      cc <- integer(length(unique(all_design$PSUSCID)))
      names(cc) <- unique(all_design$PSUSCID)
      for (pool in pools) {
        smp <- sample(pool,length(pool),replace=TRUE)
        tab <- table(smp)
        cc[names(tab)] <- as.integer(tab)
      }
      wb <- W*as.integer(cc[obs$PSUSCID])
      ii <- which(wb>0)
      bo[[b]] <- tryCatch({
        ff <- fit_external_grm(X[ii,,drop=FALSE],wb[ii],restarts=1L)
        pp <- posterior_irt(X[ii,,drop=FALSE],ff)
        vv <- link_target(interp_cesd(phq_t[cfg$primary_phq9_cutoff+1L]),
                          ff,S[ii],wb[ii],pp)
        m_obs_b <- vapply(0:57,function(s)sum(wb[ii][S[ii]==s]),numeric(1))/sum(wb[ii])
        eq <- equipercentile_cutoffs(ff,obs_m19=m_obs_b)
        eq <- eq[eq$phq_reference==cfg$primary_phq_boundary & eq$fx_source=="observed_weighted" &
                 abs(eq$format_shift_delta)<1e-12,,drop=FALSE]
        data.frame(replicate=b,
                   cutoff_equipercentile_primary=eq$cutoff[1],
                   s19_boundary_primary=eq$s19_boundary[1],
                   cutoff=as.integer(vv$candidate_cutoffs["observed_score_link"]),
                   cutoff_true_score=as.integer(vv$candidate_cutoffs["true_score_ceiling"]),
                   fit_error="")
      },error=function(e)data.frame(replicate=b,cutoff_equipercentile_primary=NA_integer_,
                                    s19_boundary_primary=NA_real_,cutoff=NA_integer_,
                                    cutoff_true_score=NA_integer_,
                                    fit_error=conditionMessage(e)))
      if (b%%10L==0L) message("PSU bootstrap: ",b," / ",B)
    }
    boot <- do.call(rbind,bo)
    safe_csv(boot,file.path(cfg$output_dir,"13_PSU_BOOTSTRAP_CONDITIONAL_ON_OLINO.csv"))
    if (sum(is.finite(boot$cutoff_equipercentile_primary))<max(30L,as.integer(.8*B)))
      warning("Too few usable PSUs bootstraps: DO NOT interpret cutoff interval")
  }
  metadata <- c(
    "ADD HEALTH WAVE II CES-D-19 CALIBRATION - PROVENANCE & INTERPRETATION",
    paste0("Execution timestamp: ",as.character(Sys.time())),
    paste0("R: ",R.version.string),
    paste0("Eligible CES-D complete valid-weight respondents: ",nrow(obs)),
    paste0("Eligible design PSUs: ",length(unique(obs$PSUSCID))),
    paste0("Strata: ",length(unique(obs$REGION))),
    paste0("Calibration sample uses GSWGT2: ",cfg$weight_var=="GSWGT2"),
    paste0("Fixed original-CES-D anchors: ",paste(fit$anchor_names,collapse=", ")),
    paste0("Estimated changed/additional items: ",paste(fit$free_names,collapse=", ")),
    paste0("Fitted latent mean and SD: ",signif(fit$mu,5),", ",signif(fit$sd,5)),
    paste0("IRT marginal logLik (weighted): ",signif(fit$logLik,8)),
    paste0("IRT slope metric multiplier: ",cfg$grm_slope_multiplier),
    paste0("Selected clinical target PHQ9>=",cfg$primary_phq9_cutoff),
    "Olino 2012 Table 2 parameter rows cross-checked with original published values; external validity is not proven.",
    "PROsetta 2014 published link uses largely adult calibration participants.",
    "PROMIS linked T-score SE is NOT standard error of the crosswalk function.",
    "Continuous original CES-D-20 threshold and Add Health expected sum are not individual diagnostic scores.",
    "Clinical-study cutoffs use independent studies; nearest ROC values do not prove test equivalence.",
    "Cuijpers full cutoff data derive from a two-phase verified sample; independent binomial assumptions are not adequate.",
    "Nominal Cuijpers 21 cases/222 controls used for pointwise beta stress draws; these are NOT design-corrected confidence intervals.",
    "Vilagut supplied reconstructed fractional 2x2 counts MUST NOT be used as actual independent patients.",
    "The Wave II construct indicators have other reference periods and are NOT diagnostic gold standards.",
    paste0("Counseling H2HS5 enabled with verified Wave II 0=No,1=Yes: ",cfg$enable_counseling_validation),
    "No Wave III/IV outcomes and no post-exposure adjustment covariates are used.",
    "Adolescent diagnostic comparisons are descriptive and never automatically select a linked-score cutoff.",
    "Observed-score link preserves the sixteen common CES-D items exactly in the score convolution.",
    "Prediction of remaining original items assumes GRM local independence and unchanged external parameters.",
    "No joint adolescent PHQ-9/CES-D diagnoses are observed; score severity is not clinical classification.",
    "Neither PHQ-9 individual scores nor NSDUH MDE are observed in Add Health Wave II.",
    "Survey PSU bootstrap uses primary observed-score link; excludes uncertainty in external item-bank and crosswalk parameters.",
    "Different candidate cutoffs define different treated ATT populations, not merely alternative numerical estimators.",
    "Export only outputs permitted by your Add Health restricted-use agreement.",
    hl,
    "REVIEW 2026-11-21: primary cutoff = equipercentile, OBSERVED weighted S19 vs model-implied CES-D-20, 16 anchors (17_EQUIPERCENTILE_CUTOFFS.csv).",
    "Model-implied S19 and the 12-anchor (positive items freed) model are sensitivity analyses; anchor models compared on common observed targets in 19_ANCHOR_MODEL_COMPARISON.csv, with no automatic selection.",
    "Format-shift scenarios are assumptions, not confidence limits. The anchor-fit tolerance is a descriptive flag, not a validated invariance test.",
    paste0("Primary PHQ-9 convention: ",cfg$primary_phq_boundary," (continuized boundary ",
           cfg$phq_boundaries[cfg$primary_phq_boundary],")."),
    paste0("Format-shift scenarios (latent SD): ",paste(cfg$format_shift_scenarios,collapse=", "),
           "; delta is unidentified by Add Health data."),
    paste0("Anchor fit gate tolerance ",cfg$anchor_fit_tolerance,"; anchors exceeding: ",
           if(length(gate$bad))paste(gate$bad,collapse=", ") else "none"),
    paste0("Anchor threshold shift applied in this run: ",cfg$anchor_threshold_shift)
  )
  record_text(file.path(cfg$output_dir,"00_provenance_and_status.txt"),metadata)
  message("Finished. Examine 00 provenance, 01 options, 03 fit, 07 clinical, 09 prevalence, and 10 validation.")
  invisible(list(options=summary,prevalence=prev,fit=fit,equipercentile=equip,
                 anchor_gate=gate,score_fit=sfit$table,anchor_comparison=anchor_cmp,
                 clinical=if(exists("val",inherits=FALSE))val else NULL))
}

# Built-in algebra tests; no restricted data used for these checks.
test_math <- function() {
  q <- seq(-3,3,length.out=11)
  for (j in 1:20) {
    p <- item_probs(q,olino$a[j],as.numeric(unlist(olino[j,c("b1","b2","b3")],use.names=FALSE)))
    stopifnot(all(p>0),max(abs(colSums(p)-1))<1e-10)
  }
  stopifnot(abs(interp_cesd(59.9)-22.4)<1e-7)
  pp_test <- as.matrix(olino[,c("a","b1","b2","b3")])
  pp_test[,1] <- pp_test[,1]*cfg$grm_slope_multiplier
  stopifnot(abs(expected_sum(orig_theta(23),pp_test)-23)<1e-6)
  stopifnot(abs(sum(grid_prior(cfg$theta_grid,0,1))-1)<1e-12)
  tmp <- list(theta=cfg$theta_grid,mu=0,sd=1,P=get_bank(),
              fit_items=map$addhealth)
  s0 <- modeled_observed_score_link(tmp)
  stopifnot(nrow(s0)==58L,
            abs(sum(s0$model_predicted_score_probability)-1)<1e-8,
            all(is.finite(s0$expected_original20)),
            all(s0$expected_original20>=0),
            all(s0$expected_original20<=60+1e-7),
            all(is.finite(s0$expected_PROMIS_T)))
  cj <- attr(s0,"conditional_original20")
  stopifnot(identical(dim(cj),c(58L,61L)),
            max(abs(rowSums(cj)-1))<1e-8,
            max(abs(s0$expected_PROMIS_T-as.numeric(cj%*%cesd_t)))<1e-8)
  # Identity test: if the instruments have exactly the same observed items,
  # the joint distribution must be diagonal (perfect score agreement).
  common <- map$addhealth[1:3]
  ident <- score_joint(tmp,common,character(),
                       matrix(numeric(),nrow=0L,ncol=4L))
  stopifnot(nrow(ident)==10L,ncol(ident)==10L,
            max(abs(ident[row(ident)!=col(ident)]))<1e-12,
            abs(sum(ident)-1)<1e-9)
  # Review additions: marginal of S20 from the identity convolution equals the
  # Lord-Wingersky marginal over the 20 original items; equipercentile self-link
  # is the identity; PROsetta boundaries.
  m20_joint <- colSums(cj*s0$model_predicted_score_probability)
  m20_lw <- lw_marginal(tmp$theta,grid_prior(tmp$theta,0,1),olino20())
  stopifnot(max(abs(m20_joint-m20_lw))<1e-10,
            abs(equip_boundary(m20_lw,m20_lw,21.25)-21.25)<2e-3,
            abs(phq_boundary_to_cesd(10)-22.4)<1e-7,
            abs(phq_boundary_to_cesd(9.5)-21.25)<1e-7,
            abs(phq_boundary_to_cesd(10.5)-23.5)<1e-7)
  invisible(TRUE)
}

test_math()
if (identical(Sys.getenv("ADDHEALTH_RUN_CALIBRATION",unset="1"),"1")) {
  options(survey.lonely.psu="adjust")
  result <- run_measurement()
}
# ============================================================================
# SECONDARY ANALYSIS -- ROBERTS ET AL. (1991) SEX-SPECIFIC CES-D-20 CUTOFFS
# APPEND ONLY AFTER THE ENTIRE EXISTING SCRIPT. No existing code is changed.
# This is a separate clinical-reference comparison; it does NOT use PHQ-9.
# Male (BIO_SEX=1): original CES-D-20 >= 22; female (BIO_SEX=2): >= 24.
# Score translation uses the original script's equipercentile method, with
# observed GSWGT2-weighted CES-D-19 scores within each sex and a predicted
# CES-D-20 distribution from the fitted Olino item bank. No refit is required.
# Outputs: 20_ROBERTS_SEX_SPECIFIC_LINK.csv and
#          21_ROBERTS_SEX_SPECIFIC_SUMMARY.csv (new files only).
# ============================================================================

# REVIEW FIX 2 helper: weighted marginal ML for a group's latent N(mu, sd) with
# item parameters fixed at the pooled calibration (fit$P).
group_latent_ml <- function(x, w, fit) {
  x <- as.matrix(x[, fit$fit_items, drop = FALSE])
  LL <- irt_loglike(x, fit$P, fit$theta)
  w <- w / mean(w)
  nll <- function(q) {
    pr <- grid_prior(fit$theta, q[1], exp(q[2]))
    -sum(w * row_logsumexp(sweep(LL, 2, log(pmax(pr, 1e-250)), "+")))
  }
  o <- optim(c(fit$mu, log(fit$sd)), nll, method = "L-BFGS-B",
             lower = c(-4, log(.2)), upper = c(4, log(3.5)))
  if (o$convergence != 0L) stop("Sex-specific latent ML did not converge")
  c(mu = o$par[1], sd = exp(o$par[2]))
}

roberts_sex_specific_secondary <- function(main_result) {
  if (is.null(main_result$fit)) stop("Primary calibration fit is missing")
  if (!identical(cfg$weight_var, "GSWGT2"))
    stop("Roberts comparison requires the same GSWGT2 weighting as primary")

  d <- read_data()
  d <- d[d$analysis_domain, , drop = FALSE]
  if (!"BIO_SEX" %in% names(d)) stop("BIO_SEX is absent")
  sex <- as_num(d$BIO_SEX)
  # REVIEW FIX 1: Wave I BIO_SEX has 1=male, 2=female and missing codes
  # (6=refused; one Wave I case). Treat any other code as missing and count it
  # in n_excluded_missing_sex rather than aborting the whole comparison.
  odd <- !is.na(sex) & !(sex %in% c(1, 2))
  if (any(odd)) message("Roberts comparison: ", sum(odd),
                        " respondent(s) with BIO_SEX not in {1,2} treated as missing")
  valid <- !is.na(sex) & sex %in% c(1, 2)

  fit <- main_result$fit
  X <- d[, fit$fit_items, drop = FALSE]
  S <- as.integer(d$sum19)
  W <- as.numeric(d$WEIGHT)
  if (nrow(X) != length(S) || length(S) != length(W) || anyNA(S) ||
      any(!is.finite(W)) || any(W <= 0)) stop("Invalid analysis data")
  if (length(fit$theta) != length(grid_prior(fit$theta, fit$mu, fit$sd)))
    stop("Invalid fitted theta grid")

  # Posterior probabilities are conditional on each adolescent's 19 responses.
  # Their design-weighted average within sex estimates its theta distribution
  # on the existing common Olino scale; pooled item parameters remain fixed.
  post <- posterior_irt(X, fit)$post
  if (!identical(dim(post), c(nrow(X), length(fit$theta))))
    stop("Unexpected posterior dimensions")
  p20 <- olino20()
  out <- vector("list", 2L)
  details <- vector("list", 2L)
  for (j in 1:2) {
    is_group <- valid & sex == j
    ii <- which(is_group)
    if (length(ii) < max(100L, cfg$min_report_cell))
      stop("Too few complete cases for BIO_SEX=", j)
    wj <- W[ii]
    sj <- S[ii]
    total_w <- sum(wj)

    # Observed within-sex CES-D-19 score distribution (not model-implied).
    m19 <- vapply(0:57, function(s) sum(wj[sj == s]), numeric(1)) / total_w

    # Posterior-mixture latent distribution for this sex, conditional on the
    # already-fitted pooled IRT model. This does not fit a new IRT model.
    latent_w <- colSums(sweep(post[ii, , drop = FALSE], 1, wj, `*`))
    prior_mix <- latent_w / sum(latent_w)
    # REVIEW FIX 2: averaging posteriors computed under the POOLED prior shrinks
    # each sex's latent distribution toward the pooled one (biasing the sex gap
    # toward zero). Instead estimate a sex-specific normal latent mean/SD by
    # weighted marginal ML with ALL item parameters fixed at the pooled
    # calibration (two-group model with invariant items; no item refit).
    lat_j <- group_latent_ml(X[ii, , drop = FALSE], wj, fit)
    prior_j <- grid_prior(fit$theta, lat_j["mu"], lat_j["sd"])
    m20 <- lw_marginal(fit$theta, prior_j, p20)
    m20_mix <- lw_marginal(fit$theta, prior_mix, p20)
    if (length(m19) != 58L || length(m20) != 61L ||
        abs(sum(m19) - 1) > 1e-8 || abs(sum(m20) - 1) > 1e-8)
      stop("Bad score distribution for BIO_SEX=", j)

    original_cut <- if (j == 1L) 22L else 24L
    # For integer CES-D-20 scores, c - 0.5 is the exact continuized boundary
    # between negative (<= c-1) and positive (>= c) scores.
    original_boundary <- original_cut - 0.5
    target_prev <- sum(m20[(original_cut + 1L):61L])
    continuous_s19_boundary <- equip_boundary(m19, m20, original_boundary)
    addhealth_cut <- as.integer(max(0L, min(57L,
                                       floor(continuous_s19_boundary + 1L))))
    actual_prev <- sum(m19[(addhealth_cut + 1L):58L])
    if (!is.finite(continuous_s19_boundary) ||
        !is.finite(actual_prev) || !is.finite(target_prev))
      stop("Nonfinite output for BIO_SEX=", j)

    out[[j]] <- data.frame(
      sex = if (j == 1L) "male" else "female",
      BIO_SEX = j,
      n_complete = length(ii),
      weighted_denominator = total_w,
      roberts_original_CESD20_cutoff = original_cut,
      original_CESD20_boundary = original_boundary,
      addhealth_CESD19_continuous_boundary = continuous_s19_boundary,
      addhealth_CESD19_integer_cutoff = addhealth_cut,
      target_model_CESD20_prevalence = target_prev,
      achieved_observed_CESD19_prevalence = actual_prev,
      prevalence_difference = actual_prev - target_prev,
      latent_mu = unname(lat_j["mu"]), latent_sd = unname(lat_j["sd"]),
      sensitivity_posterior_mixture_cutoff = as.integer(max(0L, min(57L,
        floor(equip_boundary(m19, m20_mix, original_boundary) + 1L)))),
      latent_method = "sex-specific normal latent (ML), pooled item parameters fixed",
      score_method = "observed-score equipercentile; no PHQ-9",
      stringsAsFactors = FALSE
    )
    details[[j]] <- list(indices = ii, cutoff = addhealth_cut,
                         expected_w = total_w * target_prev,
                         observed_w = sum(wj[sj >= addhealth_cut]),
                         denominator_w = total_w)
  }

  ans <- do.call(rbind, out)
  den <- sum(vapply(details, `[[`, numeric(1), "denominator_w"))
  actual <- sum(vapply(details, `[[`, numeric(1), "observed_w")) / den
  modeled <- sum(vapply(details, `[[`, numeric(1), "expected_w")) / den
  summary <- data.frame(
    analysis = "Roberts 1991 sex-specific original CES-D-20 thresholds",
    n_with_valid_sex = sum(valid),
    n_excluded_missing_sex = sum(!valid),
    n_complete_all = nrow(d),
    weighted_denominator_with_valid_sex = den,
    male_original_CESD20_cutoff = 22L,
    female_original_CESD20_cutoff = 24L,
    male_addhealth_CESD19_cutoff = ans$addhealth_CESD19_integer_cutoff[1],
    female_addhealth_CESD19_cutoff = ans$addhealth_CESD19_integer_cutoff[2],
    target_model_combined_prevalence = modeled,
    achieved_observed_combined_prevalence = actual,
    combined_prevalence_difference = actual - modeled,
    description = paste("Secondary clinical-reference sensitivity, not",
                        "diagnostic validation or an individual CES-D-20 conversion"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(cfg$allow_outputs)) {
    safe_csv(ans, file.path(cfg$output_dir,
             "20_ROBERTS_SEX_SPECIFIC_LINK.csv"))
    safe_csv(summary, file.path(cfg$output_dir,
             "21_ROBERTS_SEX_SPECIFIC_SUMMARY.csv"))
  }
  message("Roberts sex-specific comparison: CES-D-19 >= ",
          ans$addhealth_CESD19_integer_cutoff[1], " for males; >= ",
          ans$addhealth_CESD19_integer_cutoff[2], " for females; ",
          "combined weighted prevalence ", round(100 * actual, 2), "%. ",
          "Main PHQ-9 primary result unchanged.")
  invisible(list(by_sex = ans, combined = summary))
}

# Runs only after the existing primary analysis has completed successfully.
# Test harnesses set ADDHEALTH_RUN_CALIBRATION=0, so this does not read data.
if (identical(Sys.getenv("ADDHEALTH_RUN_CALIBRATION", unset = "1"), "1") &&
    identical(Sys.getenv("ADDHEALTH_RUN_ROBERTS_SEXSPEC", unset = "1"), "1")) {
  if (!exists("result", inherits = FALSE)) {
    warning("Roberts secondary analysis skipped: main result not found")
  } else {
    roberts_sex_specific_result <- tryCatch(
      roberts_sex_specific_secondary(result),
      error = function(e) {
        warning("Roberts secondary analysis failed without altering main outputs: ",
                conditionMessage(e))
        NULL
      }
    )
  }
}
