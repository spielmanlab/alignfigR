# Use this script for playing around


protein_file <- system.file("extdata", 
                            "protein.fasta", 
                            package = "alignfigR")
read_alignment(protein_file) -> tibble_fasta
tibble_fasta
plot_alignment(tibble_fasta, 
               sites = c(1:225),
               color_palette = "hydrophobicity",
               taxon_labels = TRUE, 
               legend_title = "Legend", 
               graph_title = "Example Alignment") 

plot_site_frequencies(tibble_fasta, 
                      sites = c(1:250),
                      color_palette = "clustal",
                      legend_title = "Legend", 
                      graph_title = "Example Site Frequency")
#rgb(104,0,151, max=255)












consensus <- function(info, tibble_from_readalign) {
  tibble_from_readalign <- tibble::tibble(
   column = 1:4,
   #           1    2    3    4
   #          con  uni  con  tie
   taxon1 = c("A", "C", "C", "A"),
   taxon2 = c("T", "G", "G", "G"),
   taxon3 = c("T", "T", "T", "A"),
   taxon4 = c("T", "A", "C", "G")
  )
  
  extract_subalign(tibble_from_readalign) -> info
  info %>%
    dplyr::group_by(column) %>%
    dplyr::count(seq) %>%
    # to make sure arrange groups on column
    dplyr::arrange(column, desc(n)) %>%
    dplyr::filter(n == max(n))
  
  # if 1 row per column, definitely consensus
  # if >1 row per column and all values are the same:
  #### and all those values == 1 --> everything unique (rare)
  #### and all those values > 1  --> TIE
  # let's resolve ties RANDOMLY
  
    # the first row out of every column's n's aka consensus, maybe!
    dplyr::slice(1)
  
  
  -> column_seq_counts
  for (a in 1:nrow(tibble_from_readalign)) {
    x <- max(data$n[column_seq_counts$column == a])
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



extract_subalign(tibble_fasta) -> column_test
consensus(column_test, tibble_fasta) -> plot_frame
unique(plot_frame$seq) -> uniques


yes %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = column, y = n, fill = seq) +
  ggplot2::geom_col(position = "stack")




















