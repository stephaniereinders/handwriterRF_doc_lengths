library(handwriter)
library(handwriterRF)

load_cluster_assignments <- function(main_dir) {
  if (!file.exists("data/cvl_lines_cluster_assignments.rds")) {
    writers <- list.files(main_dir)
    dfs <- list()
    for (i in 1:length(writers)) {
      writer <- writers[i]
      files <- list.files(file.path(main_dir, writer), pattern = ".rds", full.names = TRUE)
      dfs[[i]] <-  files %>%
        purrr::map_dfr(readRDS)
    }
    clusters <- do.call(rbind, dfs)
    saveRDS(clusters, "data/cvl_lines_cluster_assignments.rds")
  } else {
    clusters <- readRDS("data/cvl_lines_cluster_assignments.rds")
  }
  return(clusters)
}

load_profiles <- function(main_dir) {
  if (!file.exists("data/cvl_lines_profiles.rds")) {
    clusters <- load_cluster_assignments(main_dir = main_dir)
    profiles <- handwriter::get_cluster_fill_counts(clusters)
    saveRDS(profiles, "data/cvl_lines_profiles.rds")
  } else {
    profiles <- readRDS("data/cvl_lines_profiles.rds")
  }
  return(profiles)
}


main_dir <- "/Users/stephanie/Documents/handwriting_datasets/CVL/lines/clusters"

counts <- load_profiles(main_dir = main_dir)



