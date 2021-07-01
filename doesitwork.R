# Use this script for playing around

read_alignment("tests/testthat/Data/protein.fasta") -> tibble_fasta

plot_alignment(tibble_fasta, taxon_labels = TRUE, palette_msa = "floral", graph_title = "Graph", legend_title = "Legend", clist = c(1:260), stack = TRUE) 
# ------------------------------------------------------------------------------------------------------

extract_subalign(tibble_fasta) -> column_test

consensus <- function(info, tibble_from_readalign) {
  info %>%
    dplyr::group_by(column) %>%
    dplyr::count(seq) -> useable_data
  for (c in 1:nrow(tibble_from_readalign)) {
    a <- c
    x <- max(data$n[useable_data$column == c])
    for (i in 1:nrow(useable_data)) {
      if (useable_data$column[i] == a) {
        if (useable_data$n[i] == x) {
          useable_data$seq[i] <- TRUE
        } else {
          useable_data$seq[i] <- FALSE
        }
      }
    } 
  }
  useable_data
}

consensus(column_test, tibble_fasta) -> plot_frame
unique(plot_frame$seq) -> uniques


yes %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = column, y = n, fill = seq) +
  ggplot2::geom_col(position = "stack")




















