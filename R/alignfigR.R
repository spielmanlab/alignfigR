#' Transforms fasta into a tibble
#'
#' @param file The fasta
#' @return Returns a tibble of the fasta data
read_alignment <- function(file) {
  # separates lines so there's a away to use them independently
  raw_data <- readLines(file,
                        warn = FALSE )
  seq_vector <- c()
  seq_name <- ""
  for (line in raw_data){
    # If the lines begins with a ">"
    if (grepl("^>", line)) {
      # Then the entire line (surrounded in "") is set  equal to seq_name)
      seq_name <- sub("^>", "", line)
      seq_vector[seq_name] <- ""
    }
    else {
      temp_seq <- gsub(" ","",line)
      temp_seq <- gsub("\n","",temp_seq)
      seq_vector[seq_name] <- paste(seq_vector[seq_name], 
                                    temp_seq, 
                                    sep="" )
    }
  }
  # Is this an alignment?
  # Separates the string of proteins by "", allowing the number to be counted
  seq_list <- strsplit(seq_vector, 
                       split = "")
  lengths <- sapply(seq_list, 
                    length)
  # Check to make sure all alignments have the same number of sites
  if ( sum(lengths != lengths[1]) != 0 )
    stop("Your provided file is not an alignment. Please provide an alignment file in FASTA format to use alignfigR.")
  convert_seq_list_to_tibble(seq_list)
}
#' Transforms seq_list into a tibble
#'
#' @param seq_list Dataframe output from read_alignment
#' @return Returns a tibble of the fasta data
convert_seq_list_to_tibble <- function(seq_list) {
  tibble::as_tibble(seq_list) -> new_data
  new_data %>%
    dplyr::mutate(column = 1:nrow(new_data)) %>%
    dplyr::select(column, 
                  dplyr::everything())
}
#' Allows the user to select what taxa and sites are present in the graphs
#'
#' @param fasta_tibble Output from convert_seq_list_to_tibble
#' @param taxa List of desired or undesired taxa
#' @param exclude_taxa Determines if you wish to only include or exclude the taxa in 'taxa'
#' @param sites List of desired positions in the fasta sequence
#' @param exclude_sites Determines if you wish to only include or exclude the sites in 'sites'
#' @return Returns a tibble of the fasta data with only the desired taxa and sites
filter_taxa_and_sites <- function(fasta_tibble, 
                                  taxa = c(), 
                                  exclude_taxa = FALSE, 
                                  sites = c(), 
                                  exclude_sites = FALSE) {
  if (length(taxa) == 0){
    fasta_tibble -> tibble_taxa_filtered
  } else if (exclude_taxa){
    fasta_tibble %>%
      dplyr::select(-tidyselect::all_of(taxa)) -> tibble_taxa_filtered
  } else {
    fasta_tibble %>%
      dplyr::select(column, tidyselect::all_of(taxa)) -> tibble_taxa_filtered
  } 
  if (length(sites) == 0){
    tibble_taxa_filtered -> tibble_fully_filtered
  } else if (exclude_sites) {
    tibble_taxa_filtered %>%
      dplyr::filter(!column %in% sites) -> tibble_fully_filtered
  } else {
    tibble_taxa_filtered %>%
      dplyr::filter(column %in% sites) -> tibble_fully_filtered
  }
  tibble_fully_filtered
}
#' Pivots the output from filter_taxa_and_sites longer
#'
#' @param filtered_data The output from convert_seq_list_to_tibble
#' @return Returns a longer tibble of filtered_data
make_data_longer <- function(filtered_data) {
  filtered_data %>%
    # Removes column 'column'
    dplyr::select(-column) %>%
    # Pivots data longer, selects all columns
    tidyr::pivot_longer(cols = tidyr::everything(), 
                        # Moves columns to 'Taxa'
                        names_to = "Taxa", 
                        # Moves values to 'seq' and sets it equal to data_longer
                        values_to = "seq") -> data_longer
}
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
#' Assigns the color palette for plots, and allows you to select a custom palette
#'
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @return Returns a color palette
define_palette <- function(color_palette, 
                           uniques = NA, 
                           custom_colors = NA){
  if (tolower(color_palette) == "random") {
    # subcolors is equal to colors without null_color
    subcolors <- grDevices::colors()[grDevices::colors() != null_color]
    # palette is equal to a random sampling of subcolors, the same length as uniques
    palette <- sample(subcolors,
                      length(uniques))
    names(palette) <- uniques
    return(palette)
  } else if (tolower(color_palette) == "dna" || tolower(color_palette) == "rna"){
    return(nucleotide_pal)
  } else if (tolower(color_palette) == "custom") {
    # palette is defined as custom_colors
    palette <- custom_colors
    # names of the palette are defined as the bases
    names(palette) <- uniques
    return(palette)
  } else if (tolower(color_palette) == "basic") {
    # the palette is defined below
    return(basic_pal)
  } else if (tolower(color_palette) == "ocean") {
    # the palette is defined below
    return(ocean_pal)
  } else if (tolower(color_palette) == "forest") {
    # the palette is defined below
    return(forest_pal)
  } else if (tolower(color_palette) == "fire") {
    # the palette is defined below
    return(fire_pal)
  } else if (tolower(color_palette) == "floral") {
    # the palette is defined below
    return(floral_pal)
  } else {
    stop("Not a valid palette.")
  }
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
                             legend_title = NA, 
                             graph_title = NA) {
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
                         linetype=0) +   
      # sets the custom color palette as pal and the name of the legend
      ggplot2::scale_fill_manual(values=pal, 
                                 name = legend_title) +
      # sets the graph title as graph_title
      ggplot2::labs(title = graph_title) +
      ggplot2::theme_minimal()
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
      # defines the graph title as graph_title
      ggplot2::labs(title = graph_title) +
      # defiens the custom color palette and names the legend title as legend_title
      ggplot2::scale_fill_manual(values=pal, 
                                 name = legend_title) +
      # places the taxon identifiers and column along the y axis
      ggplot2::scale_y_discrete(limits = names(data_filtered)) +
      ggplot2::theme_minimal()
  }
  # returns the plot
  plot
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
                           legend_title = NA, 
                           graph_title = NA) {
  filter_taxa_and_sites(fasta_tibble, 
                        taxa, 
                        exclude_taxa, 
                        sites, 
                        exclude_sites) -> filtered_data
  make_data_longer(filtered_data) -> data_longer
  create_geom_rect_alignment(data_longer) -> rect_alignment
  unique(rect_alignment$seq) -> unique_seqs
  define_palette(color_palette, 
                 uniques = unique_seqs, 
                 custom_colors = NA) -> pal
  create_alignment(filtered_data, 
                   rect_alignment, 
                   pal, 
                   taxon_labels = taxon_labels, 
                   legend_title = legend_title, 
                   graph_title = graph_title)
}
#' Creates the plot for plot_site_frequencies
#'
#' @param data_longer The Output from make_data_longer
#' @param pal The output from define_palette
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a Plot of Frequencies of protein identifiers at each site
create_site_frequencies <- function(data_longer, 
                                    pal, 
                                    legend_title = NA, 
                                    graph_title = NA) {
  # Reorders the taxa to be alphabetical
  data_longer[order(data_longer$Taxa),] -> data_alphabetical
  # Counts the number of rows and saves it to number_of_rows
  as.integer(dplyr::count(data_alphabetical)) -> number_of_rows 
  # Determines the length of each individual taxon
  number_of_rows/as.integer(dplyr::count(unique(data_alphabetical[1]))) -> length_of_taxa
  data_alphabetical %>%
    dplyr::mutate(column = rep(1:length_of_taxa, 
                               number_of_rows/length_of_taxa)) %>%
  ggplot2::ggplot() +
    ggplot2::aes(x = column, 
                 fill = seq) +
    ggplot2::geom_bar(position = "stack") +
    ggplot2::scale_fill_manual(values = pal,
                               name = legend_title)+
    ggplot2::labs(title = graph_title)-> plot
  return(plot)
}
#' Creates a Plot of Frequencies of protein identifiers at each site
#' 
#' @param fasta_tibble Output from convert_seq_list_to_tibble
#' @param taxa List of desired or undesired taxa
#' @param exclude_taxa Determines if you wish to only include or exclude the taxa in 'taxa'
#' @param sites List of desired positions in the fasta sequence
#' @param exclude_sites Determines if you wish to only include or exclude the sites in 'sites'
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param legend_title A String of what you want the legend title to be
#' @param graph_title A String of what you want the graph title to be
#' @return Returns a Plot of Frequencies of protein identifiers at each site
plot_site_frequencies <- function(fasta_tibble, 
                                  taxa = c(), 
                                  exclude_taxa = FALSE, 
                                  sites = c(), 
                                  exclude_sites = FALSE,
                                  color_palette, 
                                  custom_colors = NA,
                                  legend_title = NA,
                                  graph_title = NA) {
  filter_taxa_and_sites(fasta_tibble, 
                        taxa, 
                        exclude_taxa, 
                        sites, 
                        exclude_sites) -> filtered_data
  make_data_longer(filtered_data) -> data_longer
  unique(data_longer$seq) -> unique_seqs
  define_palette(color_palette, 
                 uniques = unique_seqs, 
                 custom_colors = NA) -> pal
  create_site_frequencies(data_longer,
                          pal,
                          legend_title, 
                          graph_title)
}












