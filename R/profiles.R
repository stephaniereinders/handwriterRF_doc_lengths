load_profiles <- function(main_dir, output_dir = "data/cvl", drop_German_prompt) {
  load_cluster_assignments <- function(main_dir, output_dir) {
    outfile <- file.path(output_dir, "lines_cluster_assignments.rds")
    if (!file.exists(outfile)) {
      writers <- list.files(main_dir)
      dfs <- list()
      for (i in 1:length(writers)) {
        writer <- writers[i]
        files <- list.files(file.path(main_dir, writer), pattern = ".rds", full.names = TRUE)
        dfs[[i]] <-  files %>%
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
    df <- df %>%
      dplyr::filter(writer %in% handwriterRF::test$writer)
    return(df)
  }

  add_line_col <- function(df) {
    df <- df %>% tidyr::separate_wider_delim(doc, delim = "-", names = c("doc", "line"))
    return(df)
  }

  remove_German_prompt <- function(df) {
    df <- df %>% dplyr::filter(doc != 6)
    return(df)
  }

  outfile <- file.path(output_dir, "lines_profiles.rds")
  if (!file.exists(outfile)) {
    clusters <- load_cluster_assignments(main_dir = main_dir, output_dir = output_dir)
    profiles <- handwriter::get_cluster_fill_counts(clusters)
    profiles <- get_test_set(profiles)
    profiles <- add_line_col(profiles)
    if (drop_German_prompt) {
      profiles <- remove_German_prompt(profiles)
    }
    saveRDS(profiles, outfile)
  } else {
    profiles <- readRDS(outfile)
  }

  return(profiles)
}

load_reference_profiles <- function(main_dir, output_dir = "data/cvl", drop_German_prompt) {
  load_ref_cluster_assignments <- function(main_dir, output_dir) {
    outfile <- file.path(output_dir, "ref_cluster_assignments.rds")
    if (!file.exists(outfile)) {
      writers <- list.files(main_dir)
      dfs <- list()
      for (i in 1:length(writers)) {
        writer <- writers[i]
        files <- list.files(file.path(main_dir, writer), pattern = ".rds", full.names = TRUE)
        dfs[[i]] <-  files %>%
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
    df <- df %>%
      dplyr::filter(writer %in% handwriterRF::test$writer)
    return(df)
  }

  remove_German_prompt <- function(df) {
    df <- df %>% dplyr::filter(doc != 6)
    return(df)
  }

  outfile <- file.path(output_dir, "ref_profiles.rds")
  if (!file.exists(outfile)) {
    clusters <- load_ref_cluster_assignments(main_dir = main_dir, output_dir = output_dir)
    profiles <- handwriter::get_cluster_fill_counts(clusters)
    profiles <- get_test_set(profiles)
    if (drop_German_prompt) {
      profiles <- remove_German_prompt(profiles)
    }
    saveRDS(profiles, outfile)
  } else {
    profiles <- readRDS(outfile)
  }
  return(profiles)
}

estimate_profiles_from_all_lines <- function(line_profiles, num_lines = "all") {
  sum_counts_by_doc <- function(df, w, d) {
    labels <- data.frame(docname = paste(w, d, sep = "_"), "writer" = w, "doc" = d)
    clusters <- df %>%
      # dplyr::filter(writer == w, doc == d) %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::any_of(c("writer", "docname", "doc", "line"))) %>%
      dplyr::summarize(dplyr::across(tidyselect::everything(), ~ sum(.x)))
    counts <- cbind(labels, clusters)
    return(counts)
  }

  if (num_lines == "all") {
    estimated_profiles <- line_profiles %>%
      dplyr::group_by(writer, doc) %>%
      dplyr::group_map(~sum_counts_by_doc(.x, .y$writer, .y$doc), .keep = TRUE)
    estimated_profiles <- do.call(rbind, estimated_profiles)
  } else {
    estimated_profiles <- line_profiles %>%
      dplyr::group_by(writer, doc) %>%
      dplyr::slice_sample(n=num_lines) %>%
      dplyr::group_map(~sum_counts_by_doc(.x, .y$writer, .y$doc), .keep = TRUE)
    estimated_profiles <- do.call(rbind, estimated_profiles)
  }

  return(estimated_profiles)
}
