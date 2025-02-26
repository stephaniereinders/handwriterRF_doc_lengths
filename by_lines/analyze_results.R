library(ggplot2)
library(handwriter)
library(handwriterRF)

source("R/analysis.R")
source("R/compare.R")
source("R/plot.R")
source("R/profiles.R")


# Analyze Results ---------------------------------------------------------

main_dir <- "/Users/stephanie/Documents/handwriting_datasets/CVL/lines/clusters"
line_profiles <- load_profiles(main_dir = main_dir, drop_German_prompt = TRUE)

plot_num_lines_per_doc(line_profiles)

slrs_dir <- "data/cvl/slrs"
files <- data.frame(filepath = list.files(slrs_dir, pattern = ".rds", full.names = TRUE))
files <- files %>% dplyr::filter(grepl("error", filepath))
df <- make_errors_df(files)

plot_fnr_fpr_by_num_lines(df)
