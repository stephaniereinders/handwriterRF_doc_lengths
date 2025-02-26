compare_lengths <- function(line_profiles, ref_profiles, num_lines1, num_lines2, drop_same_set_comparisons = TRUE) {
  choose_lines_or_ref <- function(line_profiles, ref_profiles, num_lines) {
    if (num_lines == "all") {
      profiles <- ref_profiles
    } else {
      profiles <- line_profiles
    }
    return(profiles)
  }

  add_doc_codes <- function(profiles) {
    profiles$doc_code <- paste(profiles$writer, profiles$doc, sep = "_")
    return(profiles)
  }

  sample_by_doc_codes <- function(profiles, num_lines, exclude = NULL) {
    if (!is.null(exclude)) {
      profiles <- profiles %>%
        dplyr::filter(!(doc_code %in% exclude))
    }

    if (num_lines == "all") {
      profiles <- profiles %>%
        dplyr::group_by(writer) %>%
        dplyr::slice_sample(n=1)
    } else {
      doc_codes <- profiles %>%
        dplyr::group_by(writer) %>%
        dplyr::slice_sample(n=1) %>%
        dplyr::pull(doc_code)
      profiles <- profiles %>%
        dplyr::filter(doc_code %in% doc_codes)
    }

    return(profiles)
  }

  remove_doc_codes <- function(profiles) {
    profiles <- profiles %>%
      dplyr::select(-tidyselect::all_of(c("doc_code")))
    return(profiles)
  }

  make_pseudo_docs <- function(profiles, num_lines) {
    if (num_lines != "all") {
      profiles <- profiles %>%
        dplyr::group_by(writer, doc) %>%
        dplyr::slice_sample(n=num_lines) %>%
        dplyr::group_map(~sum_counts_by_doc(.x, .y$writer, .y$doc), .keep = TRUE)
      profiles <- do.call(rbind, profiles)
    }
    return(profiles)
  }

  sum_counts_by_doc <- function(df, w, d) {
    # NOTE: for pseudo-docs, docname and doc_code are equal, but docname and
    # doc_code are different for ref_profiles.
    labels <- data.frame(docname = paste(w, d, sep = "_"), doc_code = paste(w, d, sep = "_"), "writer" = w, "doc" = d)
    clusters <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::any_of(c("writer", "docname", "doc", "doc_code", "line"))) %>%
      dplyr::summarize(dplyr::across(tidyselect::everything(), ~ sum(.x)))
    counts <- cbind(labels, clusters)
    return(counts)
  }

  add_total_graphs <- function(slrs, profiles) {
    lookup <- profiles %>%
      dplyr::select(tidyselect::all_of(c("docname", "total_graphs")))

    # add total graphs for docname1
    slrs <- slrs %>% dplyr::left_join(lookup, by = c("docname1" = "docname"))
    colnames(slrs)[colnames(slrs) == "total_graphs"] <- "total_graphs1"

    # add total graphs for docname2
    slrs <- slrs %>% dplyr::left_join(lookup, by = c("docname2" = "docname"))
    colnames(slrs)[colnames(slrs) == "total_graphs"] <- "total_graphs2"

    slrs <- slrs %>%
      dplyr::select(tidyselect::any_of(c("docname1", "writer1", "total_graphs1",
                                         "docname2", "writer2", "total_graphs2",
                                         "ground_truth", "score", "slr")))
    return(slrs)
  }

  remove_same_set_comparisons <- function(slrs, profiles1, profiles2) {
    slrs <- slrs %>%
      dplyr::filter(docname1 %in% profiles1$docname, docname2 %in% profiles2$docname)
    return(slrs)
  }

  profiles1 <- choose_lines_or_ref(line_profiles, ref_profiles, num_lines1)
  profiles2 <- choose_lines_or_ref(line_profiles, ref_profiles, num_lines2)

  profiles1 <- add_doc_codes(profiles1)
  profiles2 <- add_doc_codes(profiles2)

  # Randomly select 1 doc from each writer
  profiles1 <- sample_by_doc_codes(profiles = profiles1, num_lines = num_lines1)
  # Randomly select 1 doc from each writer excluding the docs in doc_codes1
  profiles2 <- sample_by_doc_codes(profiles = profiles2, num_lines = num_lines2, exclude = unique(profiles1$doc_code))

  profiles1 <- make_pseudo_docs(profiles = profiles1, num_lines = num_lines1)
  profiles2 <- make_pseudo_docs(profiles = profiles2, num_lines = num_lines2)

  profiles <- rbind(profiles1, profiles2)
  profiles <- remove_doc_codes(profiles)
  profiles <- handwriterRF::get_cluster_fill_rates(profiles)

  slrs <- handwriterRF::compare_writer_profiles(profiles, score_only = FALSE)

  if (drop_same_set_comparisons) {
    slrs <- remove_same_set_comparisons(slrs, profiles1, profiles2)
  }

  slrs <- add_total_graphs(slrs = slrs, profiles = profiles)

  return(slrs)
}

compare_all_lengths <- function(line_profiles, ref_profiles, reps = 10, output_dir = "data/cvl/slrs") {

  doc_v_doc <- compare_lengths(line_profiles = line_profiles, ref_profiles = ref_profiles,
                               num_lines1 = "all", num_lines2 = "all")
  doc_v_doc_err <- get_errors(doc_v_doc)
  saveRDS(doc_v_doc, file.path(output_dir, "slrs_doc_v_doc.rds"))

  for (i in 1:reps) {
    for (j in 1:reps) {
      results <- compare_lengths(line_profiles = line_profiles, ref_profiles = ref_profiles,
                                 num_lines1 = i, num_lines2 = "all")
      errors <- get_errors(results)
      saveRDS(results, file.path(output_dir, paste0("slrs_", i, "lines_v_doc_rep", j, ".rds")))
      saveRDS(errors, file.path(output_dir, paste0("slrs_", i, "lines_v_doc_rep", j, "errors.rds")))
      message(paste0("slrs_", i, "lines_v_doc_rep", j, " finished \n"))
    }
  }
}


