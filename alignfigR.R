library(tidyverse)
null_color <- "grey85"
ZERO <- 1e-12 # effectively zero
#----------------------------------------------------------------------------------------------------------
#' Transforms fasta into a tibble 
#'
#' @param file The fatsa 
#' @return Returns a tibble of the fatsa data
#' @examples 
#' read_alignment("data/protein.fasta") -> tibble_fasta
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
      seq_vector[seq_name] <- paste(seq_vector[seq_name], temp_seq, sep="" )
    }
  }
  # Is this an alignment?
  # Separates the string of proteins by "", allowing the number to be counted
  seq_list <- strsplit(seq_vector, split = "")
  lengths <- sapply(seq_list, length)
  # Check to make sure all alignments have the same number of proteins
  if ( sum(lengths != lengths[1]) != 0 )
    stop("Your provided file is not an alignment. Please provide an alignment file in FASTA format to use alignfigR.")
  # Instead return as a tibble
  tibble::as_tibble(seq_list) -> new_data
  new_data %>%
    dplyr::mutate(column = 1:nrow(new_data)) %>%
    dplyr::select(column, dplyr::everything())
}
read_alignment("data/protein.fasta") -> tibble_fasta

#--------------------------------------------------------------------------------------------------------

#' Gives the ability to select which taxa and columns you want portrayed in the plot 
#'
#' @param alignment The tibble from read_alignment() 
#' @param tlist A string of the taxa you wish to include or exclude
#' @param texcl Determinant if you want to include only the taxa in tlist, or exclude only the tlist
#' @param clist The columns you wish to have portrayed in the plot
#' @param cexcl Determinant if you want to include only the columns in clist, or exclude only the clist
#' @return Returns a tibble which contains the preferred taxa and columns, and is prepped for geom_rect()
extract_subalign <- function(alignment, tlist = c(), texcl = FALSE, clist = c(), cexcl = FALSE) {
  if (length(tlist) == 0){
    alignment -> data
    # If texcl is true, then if will remove all taxa in tlist from the data
  }else if (texcl){
    alignment %>%
      dplyr::select(-tlist) -> data
    # This will only include all mentioned taxa in tlist
  } else {
    alignment %>%
      dplyr::select(column, tlist) -> data
  } 
  data %>%
    dplyr::select(-column) %>%
    # Pivots data longer, selects all columns
    tidyr::pivot_longer(cols = everything(), 
                        # Moves columns to 'Taxa'
                        names_to = "Taxa", 
                        # Moves values to 'seq' and sets it equal to data_longer
                        values_to = "seq") -> data_longer
  # Reorders the taxa to be alphabetical
  data_longer[order(data_longer$Taxa),] -> data_alphabetical
  # Counts the number of rows and saves it to number_of_rows
  as.integer(count(data_alphabetical)) -> number_of_rows 
  # Determines the length of each individual taxon
  number_of_rows/as.integer(count(unique(data_alphabetical[1]))) -> length_of_taxa
  data_alphabetical %>%
    # Creates a new column where is it repeating 1:length_of_taxa until it reaches the end
    dplyr::mutate(x1 = rep(1:length_of_taxa, number_of_rows/length_of_taxa), 
                  # Creates a new column where x2 is 1 greater than x1
                  x2 = x1 + 1, 
                  # Creates a new column where 1 is assigned to the first taxon, 2 to the 2nd and so on
                  y1 = c(t(replicate(length_of_taxa, 
                                     1:(number_of_rows/length_of_taxa)))), 
                  # Creates a new column where y2 is 1 greater than y1
                  y2 = y1 + 1) -> data_rect
  if (length(clist)== 0){
    data_rect -> d
  } else {
    # replaces 'column' column so it can be used to filter the data
    data_rect %>%
      dplyr::mutate(column = rep(1:length_of_taxa, number_of_rows/length_of_taxa)) -> data_column_ready
    if (cexcl) {
      # filters the data for all columns that are not mentioned in clist
      data_column_ready %>%
        dplyr::filter(!column %in% clist) %>%
        # removes column and sets it equal to d
        dplyr::select(-column) -> d
    } else {
      # filters the data for all columns that are mentioned in clist
      data_column_ready %>%
        dplyr::filter(column %in% clist) %>%
        # removes column and sets it equal to d
        dplyr::select(-column) -> d
    }
  }
  d
}

#-------------------------------------------------------------------------------------------------------------

#' Assigns the color palette for plot_alignment(), and allows you to select a custom palette 
#'
#' @param typemsa The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral". 
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @return Returns a color palette
define_palette <- function(typemsa, uniques = NA, custom_colors = NA){
  if (tolower(typemsa) == "random") {
    # subcolors is equal to colors without null_color
    subcolors <- colors()[colors() != null_color]
    # palette is equal to a random sampling of subcolors, the same length as uniques
    palette <- sample(subcolors, 
                      length(uniques))
    names(palette) <- uniques
  } else if (tolower(typemsa) == "dna" || tolower(typemsa) == "rna"){
    bases <- c("A", "C", "G", "T", "U")
    palette <- c("mediumblue", "orangered1", "limegreen", "khaki1", "khaki1")
    # Names of the palette are defined as the bases
    names(palette) <- bases  
  } else if (tolower(typemsa) == "custom") {
    # palette is defined as custom_colors
    palette <- custom_colors
    # names of the palette are defined as the bases
    names(palette) <- uniques
  } else if (tolower(typemsa) == "free") {
    # the palette is defined below
    palette <- c("A" = "limegreen", "G" = "lightgreen",
                 "C" = "hotpink1", "T" = "red", 
                 "U" = "lightsalmon", "J" = "maroon",
                 "B" = "snow", "O" = "mediumorchid",
                 "D" = "lemonchiffon", "E" = "lightseagreen", 
                 "N" = "darkgreen", "Q" = "thistle",
                 "I" = "lightblue1", "L" = "lightcyan", 
                 "M" = "violet", "V" = "powderblue",
                 "F" = "lavender", "W" = "lightcoral", 
                 "Y" = "plum", "V" = "moccasin",
                 "H" = "navy", "Z" = "tan",
                 "K" = "orange", "R" = "lightgoldenrod",
                 "P" = "salmon", "-" = null_color,
                 "S" = "darkred", "X" = "black") 
  } else if (tolower(typemsa) == "ocean") {
    # the palette is defined below
    palette <- c("A" = "turquoise", "G" = "lightgreen",
                 "C" = "turquoise4", "T" = "tan4", 
                 "U" = "steelblue", "J" = "springgreen4",
                 "B" = "seashell", "O" = "seashell4",
                 "D" = "seagreen", "E" = "lightseagreen", 
                 "N" = "skyblue", "Q" = "skyblue4",
                 "I" = "lightblue1", "L" = "lightcyan", 
                 "M" = "midnightblue", "V" = "powderblue",
                 "F" = "blue", "W" = "dodgerblue", 
                 "Y" = "navajowhite", "V" = "moccasin",
                 "H" = "navy", "Z" = "tan",
                 "K" = "lavenderblush2", "R" = "lightgoldenrod",
                 "P" = "deepskyblue4", "-" = null_color,
                 "S" = "honeydew3", "X" = "aquamarine2") 
  } else if (tolower(typemsa) == "forest") {
    # the palette is defined below
    palette <- c("A" = "wheat4", "G" = "darkgreen",
                 "C" = "saddlebrown", "T" = "tan4", 
                 "U" = "brown4", "J" = "springgreen4",
                 "B" = "black", "O" = "goldenrod4",
                 "D" = "seagreen", "E" = "chocolate4", 
                 "N" = "darkseagreen", "Q" = "bisque4",
                 "I" = "burlywood4", "L" = "peru", 
                 "M" = "forestgreen", "V" = "rosybrown4",
                 "F" = "olivedrab4", "W" = "green4", 
                 "Y" = "navajowhite4", "V" = "moccasin",
                 "H" = "khaki4", "Z" = "tan",
                 "K" = "sienna4", "R" = "beige",
                 "P" = "gray20", "-" = null_color,
                 "S" = "darkseagreen4", "X" = "peachpuff4") 
  } else if (tolower(typemsa) == "fire") {
    # the palette is defined below
    palette <- c("A" = "tomato", "G" = "orange",
                 "C" = "yellow", "T" = "red", 
                 "U" = "brown", "J" = "tan",
                 "B" = "saddlebrown", "O" = "goldenrod",
                 "D" = "salmon", "E" = "chocolate", 
                 "N" = "gray45", "Q" = "paleturquoise",
                 "I" = "burlywood4", "L" = "peru", 
                 "M" = "firebrick4", "V" = "sienna4",
                 "F" = "sandybrown", "W" = "gray20", 
                 "Y" = "red4", "V" = "indianred",
                 "H" = "khaki", "Z" = "moccasin",
                 "K" = "sienna", "R" = "blanchedalmond",
                 "P" = "lightgoldenrod", "-" = null_color,
                 "S" = "wheat", "X" = "black")
  } else if (tolower(typemsa) == "floral") {
    # the palette is defined below
    palette <- c("A" = "chartreuse2", "G" = "orange",
                 "C" = "yellow", "T" = "red", 
                 "U" = "pink", "J" = "dodgerblue",
                 "B" = "plum", "O" = "thistle",
                 "D" = "salmon", "E" = "darkblue", 
                 "N" = "violet", "Q" = "paleturquoise",
                 "I" = "steelblue", "L" = "peru", 
                 "M" = "purple", "V" = "purple4",
                 "F" = "palevioletred", "W" = "midnightblue", 
                 "Y" = "palegreen", "V" = "white",
                 "H" = "lavender", "Z" = "honeydew2",
                 "K" = "darkorange3", "R" = "aquamarine",
                 "P" = "mistyrose3", "-" = null_color,
                 "S" = "wheat", "X" = "black")
  }
  palette
}

#------------------------------------------------------------------------------------------------------------

#' Assigns the color palette for plot_alignment(), and allows you to select a custom palette 
#'
#' @param alignment The tibble from read_alignment()
#' @param tlist A string of the taxa you wish to include or exclude
#' @param texcl Determinant if you want to include only the taxa in tlist, or exclude only the tlist
#' @param clist The columns you wish to have portrayed in the plot
#' @param cexcl Determinant if you want to include only the columns in clist, or exclude only the clist
#' @param typemsa The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param taxon_labels Determinant of if taxa identifiers will be present on the graph or not
#' @param graph_title Sets the graph title
#' @param legend_title Sets the legend title
#' @return Returns an MSA of the data
#' @examples 
#' plot_alignment(tibble_fasta, typemsa = "Ocean", taxon_labels = TRUE, graph_title = "Graph", legend_title = "legend")   
plot_alignment <- function(alignment, tlist = c(), texcl = FALSE, clist = c(), cexcl = FALSE, typemsa, uniques = NA, custom_colors = NA, taxon_labels = FALSE, graph_title = NA, legend_title = NA) {
  # runs extract_subalign and defines it as plot_frame
  extract_subalign(alignment, tlist, texcl, clist, cexcl) -> plot_frame
  # defines uniques as the uniques of the sequence in plot_frame
  unique(plot_frame$seq) -> uniques
  # runs define palette and sets it's output as pal
  define_palette(typemsa, uniques, custom_colors) -> pal
  if (taxon_labels == FALSE){
    plot <- ggplot() +
      # geom_rect() using plot_frame from extract_subalign()
      geom_rect(plot_frame, mapping=aes(xmin=x1-1, xmax=x2-1, ymin=
                                  y1-1, ymax=y2-1, fill = seq), linetype=0) +       
      # sets the custom color palette 
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      labs(title = graph_title)
  } else {
    plot <- ggplot() + 
      # geom_rect() being run on plot_frame from extract_subalign()
      geom_rect(plot_frame, mapping=aes(xmin=x1, xmax=x2, ymin =
                                          y1, ymax=y2, fill = seq), linetype=0) +
      labs(title = graph_title) +
      # defines custom color palette and legend title 
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      # places the taxon identifiers and column along the y axis
      scale_y_discrete(limits = names(alignment))
  }
  plot
}
plot_alignment(tibble_fasta, typemsa = "Ocean", taxon_labels = TRUE, graph_title = "Graph", legend_title = "legend")  
  
  
#------------------------------------------------------------------------------------------------------------

#' Reveals the unique protein/nucleotide identifiers found in this column, reveals the counts and the frequency of organisms having that identifier in that column
#'
#' @param alignment_tibble The tibble from read_alignment()
#' @param column_of_interest The specific column you want to see
#' @return Returns a tibble with rows 'value', 'count' and 'percent'
#' @example 
#' calculate_column_percentage(tibble_fasta, 5)
calculate_column_percentage <- function(alignment_tibble, column_of_interest) {
  alignment_tibble %>%
    # Filters the data where column is equal to the column of interest
    dplyr::filter(column == column_of_interest) %>%
    dplyr::select(!column) %>%
    # Makes the data into a long tibble by selecting every column
    tidyr::pivot_longer(everything(), 
                        # Moving the names to a column named 'taxon'
                        names_to = "taxon", 
                        # Moves the values to a column named 'value'
                        values_to = "value") %>%
    dplyr::select(value) %>%
    # Makes a column named count where it is equal to the count of unique values
    dplyr::count(value, name = "count") %>%
    # Creates a new column named 'percent' that is equal to the percent that that unique value had
    dplyr::mutate(percent = count / sum(count)) -> column_percentages # tibble of counts, percents
  
  
  # Check that calculations worked
  expected <- 1 # goal if we did it right
  observed <- sum(column_percentages$percent)
  true_if_worked <- abs(expected - observed) <= ZERO 
  stopifnot(true_if_worked)
  
  # Give back the tibble with three columns: value, count, and percent
  column_percentages
}
 calculate_column_percentage(tibble_fasta, 5)










