get_known_matches <- function(df) {
  df <- df %>%
    dplyr::filter(ground_truth == "same writer")
  return(df)
}

get_known_nonmatches <- function(df) {
  df <- df %>%
    dplyr::filter(ground_truth == "different writer")
  return(df)
}

get_errors <- function(df, type = "slr") {
  if (type == "slr") {
    errors <- list()

    if ("total_graphs1" %in% colnames(df)) {
      errors$total_graphs_stats <- df %>%
        dplyr::summarize(
          min1 = min(total_graphs1),
          median1 = median(total_graphs1),
          max1 = max(total_graphs1),
          min2 = min(total_graphs2),
          median2 = median(total_graphs2),
          max2 = max(total_graphs2))
    }

    KM_df <- get_known_matches(df)
    KNM_df <- get_known_nonmatches(df)
    errors$KM <- nrow(KM_df)
    errors$KNM <- nrow(KNM_df)

    errors$FN_df <- KM_df %>%
      dplyr::filter(slr < 1)
    errors$FP_df <- KNM_df %>%
      dplyr::filter(slr > 1)
    errors$FN <- nrow(errors$FN_df)
    errors$FP <- nrow(errors$FP_df)

    errors$FNR <- errors$FN / errors$KM
    errors$FPR <- errors$FP / errors$KNM

    errors$error <- (errors$FN + errors$FP) / (errors$KM + errors$KNM)

  }
  return(errors)
}

make_errors_df <- function(error_files) {
  make_single_errors_df <- function(data, filename) {

    df <- data.frame(filename = filename,
                     KM = data$KM,
                     KNM = data$KNM,
                     FN = data$FN,
                     FP = data$FP,
                     FNR = data$FNR,
                     FPR = data$FPR,
                     total_graphs_median1 = data$total_graphs_stats$median1,
                     total_graphs_median2 = data$total_graphs_stats$median2)

    # Get num_lines and rep columns
    df <- df %>% tidyr::separate_wider_delim(cols = filename,
                                             delim = "_",
                                             names = c(NA, "num_lines", NA, NA, "rep"),
                                             cols_remove = FALSE)
    df$num_lines <- readr::parse_number(df$num_lines)
    df$rep <- readr::parse_number(df$rep)

    # Reorder columns
    df <- df %>% dplyr::select(tidyselect::all_of(c("filename")), tidyselect::everything())

    return(df)
  }

  errors <- files$filepath %>%
    purrr::map(readRDS)
  names(errors) <- basename(files$filepath)

  dfs <- lapply(1:length(errors), function(i) make_single_errors_df(errors[[i]], names(errors)[i]))
  df <- do.call(rbind, dfs)

  return(df)
}


