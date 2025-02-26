plot_num_lines_per_doc <- function(line_profiles) {
  get_num_lines_per_doc <- function(line_profiles) {
    df <- line_profiles %>%
      dplyr::group_by(writer, doc) %>%
      dplyr::summarize(n = dplyr::n())
    return(df)
  }

  num_lines <- get_num_lines_per_doc(line_profiles = line_profiles)
  num_lines$n <- factor(num_lines$n)

  num_lines %>% ggplot2::ggplot(ggplot2::aes(n)) +
    ggplot2::geom_histogram(stat = "count") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "# lines per sample")
  ggplot2::ggsave(filename="plots/cvl/histogram_num_lines_per_doc.png")

  num_lines$prompt <- paste0("prompt ", num_lines$doc)
  num_lines %>% ggplot2::ggplot(ggplot2::aes(n)) +
    ggplot2::geom_histogram(stat = "count") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "# lines per sample") +
    ggplot2::facet_wrap(~prompt)
  ggplot2::ggsave(filename="plots/cvl/histogram_num_lines_per_doc_by_prompt.png")
}

plot_fnr_fpr_by_num_lines <- function(df) {
  df %>%
    ggplot(aes(x=num_lines, y=FNR, group=num_lines)) +
    geom_boxplot() +
    labs(x = "# Lines in Pseudo-doc", y = "False Negative Rate") +
    ylim(0,1) +
    theme_bw()
  ggsave("plots/cvl/boxplots_fnr_by_num_lines.png")

  df %>%
    ggplot(aes(x=num_lines, y=FPR, group=num_lines)) +
    geom_boxplot() +
    labs(x = "# Lines in Pseudo-doc", y = "False Positive Rate") +
    ylim(0,1) +
    theme_bw()
  ggsave("plots/cvl/boxplots_fpr_by_num_lines.png")
}


