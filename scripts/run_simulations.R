# Run simulations, for forecasts and scenarios

library(tidyverse)

output_dir <- "output"

# if multicore parallel processing is available, use it
if (future::supportsMulticore()) {
  future::plan(future::multicore)
}

# Run simulations ----------------------------------------------------------
# clear existing output
results_path <- file.path(output_dir, "results")
unlink(results_path, recursive = TRUE)

# read in experiment specification
parameter_sets <- griddleR::read_griddle(
  system.file("params.yaml", package = "seirMeasles", mustWork = TRUE)
)

# run experiment & cache results
griddleR::run_cache(
  fun = griddleR::replicated(seirMeasles::simulate),
  parameter_sets = parameter_sets,
  path = results_path
)
