testthat::test_that("configuration overlays reject misspelled keys", {
  bad <- list(glboal = list(output_dir = "runs"))
  testthat::expect_error(assert_known_config_overlay(cfg, bad),
                         "Unknown configuration key")
})

testthat::test_that("configuration overlays merge without mutating defaults", {
  overlay <- list(global = list(pipeline_seed = 42L),
                  stages = list(run_multiseed_att = TRUE))
  merged <- merge_config_overlay(cfg, overlay)
  testthat::expect_equal(merged$global$pipeline_seed, 42L)
  testthat::expect_true(merged$stages$run_multiseed_att)
  testthat::expect_equal(cfg$global$pipeline_seed, 1L)
  testthat::expect_false(cfg$stages$run_multiseed_att)
})

testthat::test_that("example YAML contains every path key", {
  testthat::skip_if_not_installed("yaml")
  example <- yaml::read_yaml(file.path(project_root, "config", "config.example.yml"))
  testthat::expect_setequal(names(example$paths), names(cfg$paths))
  testthat::expect_silent(assert_known_config_overlay(cfg, example))
})

testthat::test_that("default input gate requires only the selected outcome wave", {
  required <- required_input_keys(cfg)
  testthat::expect_true(all(c("wave1_inhome", "wave2_inhome", "wave4_inhome",
                              "mortality") %in% required))
  testthat::expect_false(any(c("wave3_inhome", "wave5_inhome") %in% required))
})
