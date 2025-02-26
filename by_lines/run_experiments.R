library(handwriter)
library(handwriterRF)
source("by_lines/R/analysis.R")
source("by_lines/R/compare.R")
source("by_lines/R/plot.R")
source("by_lines/R/profiles.R")

# Compare Estimates from Lines to Actual Profiles -------------------------

main_dir <- "/Users/stephanie/Documents/non_version_control/handwriting_datasets/CVL/lines/clusters"
line_profiles <- load_profiles(main_dir = main_dir, drop_German_prompt = TRUE)

line_profiles_sum_all_lines <- estimate_profiles_from_all_lines(line_profiles, num_lines = "all")

ref_dir <- "/Users/stephanie/Documents/handwriting_datasets/CVL/pages_cropped/clusters"
ref_profiles <- load_reference_profiles(ref_dir, drop_German_prompt = TRUE)

# Compare estimated profiles to expected profiles
comparison_est_to_exp <- compare_est_to_expected(line_profiles_sum_all_lines, ref_profiles)
get_errors(comparison_est_to_exp$slrs)
saveRDS(comparison_est_to_exp, "data/cvl/comparison_all_lines_v_docs.rds")

# Compare different lengths of pseudo-docs versus original docs
compare_all_lengths(
  line_profiles = line_profiles,
  ref_profiles = ref_profiles,
  reps = 20,
  output_dir = file.path("data", "cvl", "slrs")
)
