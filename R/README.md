# Analysis modules

Source every numbered module through `scripts/bootstrap.R` in numeric order.
`00_config_io.R` is the YAML interface. `01`–`15` keep the previous functional
module names; `16_auxiliary.R` holds supplementary definitions and focused tests,
and `17_run_defaults.R` preserves final default assignments. The wrapper applies
local YAML afterward. Do not run an isolated module or add another production file.
