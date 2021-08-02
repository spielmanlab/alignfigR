#' Creates a new tibble that is ready to be used in a geom_rect
#'
#' @param data_longer Output from make_data_longer
#' @return Returns a tibble that contains columns x1, x2, y1, and y2; all with correct values for an alignment
create_geom_rect_alignment <- function(data_longer) {
  data_longer[order(data_longer$Taxa),] -> data_alphabetical
  # Counts the number of rows and saves it to number_of_rows
  as.integer(dplyr::count(data_alphabetical)) -> number_of_rows
  # Determines the length of each individual taxon
  number_of_rows/as.integer(dplyr::count(unique(data_alphabetical[1]))) -> length_of_taxa
  data_alphabetical %>%
    # Creates a new column where is it repeating 1:length_of_taxa until it reaches the end
    dplyr::mutate(x1 = rep(1:length_of_taxa,
                           number_of_rows/length_of_taxa),
                  # Creates a new column where x2 is 1 greater than x1
                  x2 = x1 + 1,
                  # Creates a new column where 1 is assigned to the first taxon, 2 to the 2nd and so on
                  y1 = c(t(replicate(length_of_taxa,
                                     1:(number_of_rows/length_of_taxa)))),
                  # Creates a new column where y2 is 1 greater than y1
                  y2 = y1 + 1) -> data_rect_prepped
}



#' Creates the plot for plot_alignment
#'
#' @param data_filtered The Output from filter_taxa_and_sites
#' @param rect_alignment The Output from create_rect_alignment
#' @param pal The output from define_palette
#' @param taxon_labels Determines if you would like rows to be labeled by their respective taxon
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a Multiple Sequence Alignment
create_alignment <- function(data_filtered,
                             rect_alignment,
                             pal,
                             taxon_labels = FALSE,
                             legend_title = "Legend Title",
                             graph_title = "Graph Title") {
  if (taxon_labels == FALSE){
    # defines plot as
    plot <- ggplot2::ggplot() +
      # geom_rect() using rect_alignment from extract_subalign()
      ggplot2::geom_rect(rect_alignment,
                         mapping=ggplot2::aes(xmin=x1-1,
                                              xmax=x2-1,
                                              ymin=y1-1,
                                              ymax=y2-1,
                                              fill = seq),
                         linetype=0)
  } # if taxon_labels is equal to TRUE
  else {
    # defines plot as
    plot <- ggplot2::ggplot() +
      # geom_rect() being run on plot_frame from extract_subalign()
      ggplot2::geom_rect(rect_alignment,
                         mapping=ggplot2::aes(xmin=x1,
                                              xmax=x2,
                                              ymin =y1,
                                              ymax=y2,
                                              fill = seq),
                         linetype=0) +
      # places the taxon identifiers and column along the y axis
      ggplot2::scale_y_discrete(limits = names(data_filtered))
  }

  plot +
    # sets the custom color palette as pal and the name of the legend
    ggplot2::scale_fill_manual(values=pal,
                                name = legend_title,
                               limits = force) +
    # sets the graph title as graph_title
    ggplot2::labs(title = graph_title) +
    ggplot2::theme_minimal()
}




#' Creates a Multiple Sequence Alignment
#'
#' @param fasta_tibble Output from convert_seq_list_to_tibble
#' @param taxa List of desired or undesired taxa
#' @param exclude_taxa Determines if you wish to only include or exclude the taxa in 'taxa'
#' @param sites List of desired positions in the fasta sequence
#' @param exclude_sites Determines if you wish to only include or exclude the sites in 'sites'
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param taxon_labels Determines if you would like rows to be labeled by their respective taxon
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a Multiple Sequence Alignment
plot_alignment <- function(fasta_tibble,
                           taxa = c(),
                           exclude_taxa = FALSE,
                           sites = c(),
                           exclude_sites = FALSE,
                           color_palette,
                           custom_colors = NA,
                           taxon_labels = FALSE,
                           legend_title = "Legend Title",
                           graph_title = "Graph Title") {
  filter_taxa_and_sites(fasta_tibble,
                        taxa,
                        exclude_taxa,
                        sites,
                        exclude_sites) -> filtered_data
  make_data_longer(filtered_data) -> data_longer
  create_geom_rect_alignment(data_longer) -> rect_alignment
  unique(rect_alignment$seq) -> unique_seqs
  define_palette(color_palette,
                 unique_seqs,
                 custom_colors,
                 type) -> pal
  create_alignment(filtered_data,
                   rect_alignment,
                   pal,
                   taxon_labels = taxon_labels,
                   legend_title = legend_title,
                   graph_title = graph_title)
}















