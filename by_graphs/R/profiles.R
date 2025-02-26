#' Load Writer Profiles
#'
#' This function creates a data frame of writer profiles (cluster fill counts)
#' from CVL prompts - we will refer to these as "pages" - or CVL prompts split
#' into lines. This function requires that the main directory contain a
#' subfolder, with the writer ID as the name, for each writer. Each writer
#' folder must contain cluster assignments for the writer's documents, either
#' pages or lines. The cluster assignments can be obtained with
#' `handwriter::get_clusters_batch()`. The pages must use the naming convention
#' <writerID>_<prompt#>.rds. The lines must use the naming convention
#' <writerID>_<prompt#>_<line#>.rds.
#'
#' `load_profiles()` loads the cluster assignment files for all writers in the
#' main directory into a single data frame and then (1) makes a single data
#' frame of the cluster fill counts with
#' `handwriter::get_cluster_fill_counts()`, (2) filters the data frame for
#' writers in `handwriterRF::test`, (3) extracts the line number from the
#' filename for line documents, (4) optionally, drops the German prompt from the
#' data frame, and (5) saves the data frame as pages_profiles.rds or
#' lines_profiles.rds in the output_dir.
#'
#' If the output file already exists, this function loads it instead of
#' recreating it.
#'
#' @param main_dir The directory containing writer subfolders with cluster
#'   assignments from either CVL pages or lines.
#' @param output_dir The directory to save output files
#' @param type "line" for documents that are a single line and "page" for
#'   documents that are the full document.
#' @param drop_German_prompt Logical. TRUE to remove the German prompt from the
#'   data frame. FALSE to keep the German prompt.
#'
#' @returns A data frame of cluster fill counts as writer profiles
#' 
load_profiles <- function(main_dir, output_dir = "by_graphs/data/cvl", type = "line", drop_German_prompt = TRUE) {
  load_cluster_assignments <- function(main_dir, type, output_dir) {
    # load all cluster assignments in all writer subfolders in main_dir into a
    # single data frame. Save data frame in output_dir as
    # lines_cluster_assignments.rds

    outfile <- switch(type,
                      "line" = file.path(output_dir, "lines_cluster_assignments.rds"),
                      "page" = file.path(output_dir, "pages_cluster_assignment.rds"))
    if (!file.exists(outfile)) {
      writers <- list.files(main_dir)
      dfs <- list()
      for (i in 1:length(writers)) {
        writer <- writers[i]
        files <- list.files(file.path(main_dir, writer), pattern = ".rds", full.names = TRUE)
        dfs[[i]] <- files %>%
          purrr::map_dfr(readRDS)
      }
      clusters <- do.call(rbind, dfs)
      saveRDS(clusters, outfile)
    } else {
      clusters <- readRDS(outfile)
    }
    return(clusters)
  }

  get_test_set <- function(df) {
    # filter for writers in handwriterRF::test
    df <- df %>%
      dplyr::filter(writer %in% handwriterRF::test$writer)
    return(df)
  }

  add_line_col <- function(df) {
    # extract the line number from the file name
    df <- df %>% tidyr::separate_wider_delim(doc, delim = "-", names = c("doc", "line"))
    return(df)
  }

  remove_German_prompt <- function(df) {
    # drop the German prompt
    df <- df %>% dplyr::filter(doc != 6)
    return(df)
  }
  
  outfile <- switch(type,
                    "line" = file.path(output_dir, "lines_profiles.rds"),
                    "page" = file.path(output_dir, "pages_profiles.rds"))
  if (!file.exists(outfile)) {
    clusters <- load_cluster_assignments(main_dir = main_dir, type = type, output_dir = output_dir)
    profiles <- handwriter::get_cluster_fill_counts(clusters)
    profiles <- get_test_set(profiles)
    if (type == "line") {
      profiles <- add_line_col(profiles)
    }
    if (drop_German_prompt) {
      profiles <- remove_German_prompt(profiles)
    }
    saveRDS(profiles, outfile)
  } else {
    profiles <- readRDS(outfile)
  }

  return(profiles)
}
