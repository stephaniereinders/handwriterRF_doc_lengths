library(handwriter)
library(handwriterRF)
source("by_graphs/R/analysis.R")
source("by_graphs/R/compare.R")
source("by_graphs/R/plot.R")
source("by_graphs/R/profiles.R")

# Compare Estimates from Lines to Actual Profiles -------------------------

line_dir <- "/Users/stephanie/Documents/non_version_control/handwriting_datasets/CVL/lines/clusters"
line_profiles <- load_profiles(main_dir = line_dir, type = "line", drop_German_prompt = TRUE)

page_dir <- "/Users/stephanie/Documents/non_version_control/handwriting_datasets/CVL/pages_cropped/clusters"
page_profiles <- load_profiles(main_dir = page_dir, type = "page", drop_German_prompt = TRUE)

# Compare different lengths of pseudo-docs versus original docs
compare_all_lengths(
  line_profiles = line_profiles,
  ref_profiles = ref_profiles,
  reps = 20,
  output_dir = file.path("data", "cvl", "slrs")
)
