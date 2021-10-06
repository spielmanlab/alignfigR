# Good Examples
?plot_site_frequencies

protein_file <- system.file("extdata", 
                            "protein.fasta", 
                            package = "alignfigR") 
# Try leaving data_type blank so the function can determine it, or try character, protein, or nucleotide!
read_alignment(protein_file, data_type = "") -> tibble_fasta 
tibble_fasta
filter_taxa_and_sites(tibble_fasta) %>%
make_data_longer() -> data_longer
unique(data_longer$Taxa) -> test2
filter_and_make_data_longer(tibble_fasta)
nucleotide_fasta
plot_alignment(tibble_fasta, 
               sites = c(1:20),
                color_palette = "forest",
              graph_title = "test", legend_title = "test2") 
plot_site_frequencies(tibble_fasta, 
                      sites = c(1:5),
                      color_palette = "clustal",
                      legend_title = "Legend1", 
                      graph_title = "Example Site Frequency")

testing <- function(x) {
  (x+1)
}
testing2 <- function(y) {
  (y+1)
}
testing3 <- function(x,y) {
  testing(x) -> a
  testing2(y) -> b
}
testing4 <- function(a,b) {
  print(a+b)
}
testing5 <- function(x,y) {
  testing3(x,y) %>%
  testing4(a,b)
}
testing5(1,1)



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

determine_palette <- function(data_longer,
                              color_palette,
                              custom_colors,
                              type) {
  unique(data_longer$seq) -> unique_seqs
  define_palette(color_palette,
                 unique_seqs,
                 custom_colors,
                 type)
}
determine_palette(data_longer, color_palette = "Forest", type = "Protein")


filter_and_determine_palette(tibble_fasta, color_palette = "default") -> answers
define_palette(color_palette = "default")
type == "Protein"
answers[1] -> dl
dl$data_longer$Taxa
dl$data_longer[order(dl$data_longer$Taxa),] -> data_alphabetical
answers[2] -> pal
pal$pal
dl$data_longer -> data_longer
prep_site_frequencies(data_longer) -> test
test
data_longer
dplyr::arrange(data_longer)
data_alphabetical
