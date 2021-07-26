#' Allows the user to select what taxa and sites are present in the graphs
#'
#' @param data_longer Tibble output from make_data_longer()
#' @return Returns a tibble that is ready for plot_frequencies()
prep_site_frequencies <- function(data_longer) {
  data_longer[order(data_longer$Taxa),] -> data_alphabetical
  # Counts the number of rows and saves it to number_of_rows
  as.integer(dplyr::count(data_alphabetical)) -> number_of_rows
  # Determines the length of each individual taxon
  number_of_rows/as.integer(dplyr::count(unique(data_alphabetical[1]))) -> length_of_taxa
  data_alphabetical %>%
    dplyr::mutate(column = rep(1:length_of_taxa,
                               number_of_rows/length_of_taxa)) %>%
    dplyr::group_by(column, 
                    seq) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(percent = count/sum(count))
}

#' Creates the plot for plot_site_frequencies
#'
#' @param output_from_prep_site_frequencies The tibble output from prep_site_frequencies()
#' @param pal The output from define_palette
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a plt of the site frequencies
plot_frequencies <- function(output_from_prep_site_frequencies, pal, legend_title = "Legend Title", graph_title = "Graph Title") {
  ggplot2::ggplot(output_from_prep_site_frequencies) +
    ggplot2::aes(x = column, 
                 y = percent, 
                 fill = seq) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = pal,
                               name = legend_title)+
    ggplot2::labs(title = graph_title) +
    ggplot2::theme_minimal()-> plot
  return(plot)
}

#' Creates the site frequency spectra
#'
#' @param fasta_tibble Tibble output from read_alignment
#' @param taxa List of desired or undesired taxa
#' @param exclude_taxa Determines if you wish to only include or exclude the taxa in 'taxa'
#' @param sites List of desired positions in the fasta sequence
#' @param exclude_sites Determines if you wish to only include or exclude the sites in 'sites'
#' @return Returns a tibble of the fasta data with only the desired taxa and sites
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a Site Frequency Plot
plot_site_frequencies <- function(fasta_tibble,
                                  taxa = c(),
                                  exclude_taxa = FALSE,
                                  sites = c(),
                                  exclude_sites = FALSE,
                                  color_palette = "default",
                                  custom_colors = NA,
                                  legend_title = "Legend Title",
                                  graph_title = "Graph Title") {
  fasta_tibble[1,1] -> type_of_data
  fasta_tibble %>%
    dplyr::select(-type_data) -> fasta_tibble
  filter_taxa_and_sites(fasta_tibble,
                        taxa,
                        exclude_taxa,
                        sites,
                        exclude_sites) -> filtered_data
  make_data_longer(filtered_data) -> data_longer
  unique(data_longer$seq) -> unique_seqs
  define_palette(color_palette,
                 unique_seqs,
                 custom_colors) -> pal
  prep_site_frequencies(data_longer) -> output_from_prep_site_frequencies
  plot_frequencies(output_from_prep_site_frequencies, 
                   pal,
                   legend_title = "Legend Title",
                   graph_title = "Legend Title")
} 