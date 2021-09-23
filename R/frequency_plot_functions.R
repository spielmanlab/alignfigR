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
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = pal,
                                name = legend_title,
                               limits = force)+
    ggplot2::labs(title = graph_title) +
    ggplot2::theme_minimal()
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
  filter_and_determine_palette(fasta_tibble,
                               taxa,
                               exclude_taxa,
                               sites,
                               exclude_sites,
                               color_palette,
                               custom_colors) -> output_list
  output_list[1] -> data_longer
  data_longer$data_longer -> data_longer
  output_list[2] -> pal
  pal$pal -> pal
  prep_site_frequencies(data_longer) -> output_from_prep_site_frequencies
  plot_frequencies(output_from_prep_site_frequencies, 
                   pal,
                   legend_title = legend_title,
                   graph_title = graph_title)
} 