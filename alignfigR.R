library(tidyverse)
null_color <- "grey85"
ZERO <- 1e-12 # effectively zero
#----------------------------------------------------------------------------------------------------------

read_alignment <- function(file) {
  # readLines appears to separate lines so there's a away to use them independently
  raw_data <- readLines(file, warn = FALSE )
  # Makes an empty array
  seq_vector <- c()
  # Makes an empty character string
  seq_name <- ""
  # All about getting the organism identifiers in a vector
  for (line in raw_data){
    # If the lines begins with a ">"
    if (grepl("^>", line)) {
      # Then the entire line (surrounded in "") is set  equal to seq_name)
      seq_name <- sub("^>", "", line)
      # Makes an empty vector of the organism identifiers
      seq_vector[seq_name] <- ""
    }
    # For actual sequence not identifiers
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

extract_subalign_improved <- function(alignment, tlist = c(), texcl = FALSE) {
  # If tlist is empty, then alignment = data
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
  # Pipes data into Select
  data %>%
    # Removes column 'column'
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
  # Pipes data_alphabetical into mutate
  data_alphabetical %>%
    # Creates a new column where is it repeating 1:length_of_taxa until it reaches the end
    dplyr::mutate(x1 = rep(1:length_of_taxa, number_of_rows/length_of_taxa), 
                  # Creates a new column where x2 is 1 greater than x1
                  x2 = x1 + 1, 
                  # Creates a new column where 1 is assigned to the first taxon, 2 to the 2nd and so on
                  y1 = c(t(replicate(length_of_taxa, 
                                     1:(number_of_rows/length_of_taxa)))), 
                  # Creates a new column where y2 is 1 greater than y1
                  y2 = y1 + 1)
}
extract_subalign_improved(tibble_fasta) -> d
d
#-------------------------------------------------------------------------------------------------------------
unique(d$seq) -> define_palette_test

define_palette <- function(typemsa, uniques = NA, custom_colors = NA){
  # if typemsa is equal to random
  if (tolower(typemsa) == "random") {
    # set subcolors equal to all colors except the null color
    subcolors <- colors()[colors() != null_color]
    # set palette equal to a sampling of subcolors, the same number as uniques
    palette <- sample(subcolors, length(uniques))
    # sets names of palette equal to uniques
    names(palette) <- uniques
  } # if typemsa is 'dna' or 'rna'
  else if (tolower(typemsa) == "dna" || tolower(typemsa) == "rna"){
    # set bases equal to ACGTU
    bases <- c("A", "C", "G", "T", "U")
    # set palette as a set of 5 colors (two the same for T and U)
    palette <- c("mediumblue", "orangered1", "limegreen", "khaki1", "khaki1")
    # sets names of palette equal to bases
    names(palette) <- bases  
  } # if typemsa is equal to 'custom'
  else if (tolower(typemsa) == "custom") {
    # sets palette equal to custom colors
    palette <- custom_colors
    # assigns uniques as the names of the palette
    names(palette) <- uniques
  } # if typemsa is equal to 'free'
  else if (tolower(typemsa) == "free") {
    # Sets palette as a defined color for each protein/nucleotide
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
  }
  # returns palette
  palette
}

#------------------------------------------------------------------------------------------------------------

plot_alignment <- function(alignment, tlist = c(), texcl = FALSE, typemsa, uniques = NA, custom_colors = NA, taxon_labels = FALSE, graph_title = NA, legend_title = NA) {
  # runs extract_subalign_improved and defines it as plot_frame
  extract_subalign_improved(alignment, tlist, texcl) -> plot_frame
  # defines uniques as the uniques of the sequence in plot_frame
  unique(plot_frame$seq) -> uniques
  # runs define palette and sets it's output as pal
  define_palette(typemsa, uniques, custom_colors) -> pal
  # if taxon_labels is equal to FALSE
  if (taxon_labels == FALSE){
    # defines plot as
    plot <- ggplot() +
      # geom_rect() using plot_frame from extract_subalign()
      geom_rect(plot_frame, mapping=aes(xmin=x1-1, xmax=x2-1, ymin=
                                  y1-1, ymax=y2-1, fill = seq), linetype=0) +       
      # sets the custom color palette as pal and the name of the legend
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      # sets the graph title as graph_title
      labs(title = graph_title)
  } # if taxon_labels is equal to TRUE
  else {
    # defines plot as
    plot <- ggplot() + 
      # geom_rect() being run on plot_frame from extract_subalign()
      geom_rect(plot_frame, mapping=aes(xmin=x1, xmax=x2, ymin =
                                          y1, ymax=y2, fill = seq), linetype=0) +
      # defines the graph title as graph_title
      labs(title = graph_title) +
      # defiens the custom color palette and names the legend title as legend_title
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      # places the taxon identifiers and column along the y axis
      scale_y_discrete(limits = names(alignment))
  }
  # returns the plot
  plot
}
plot_alignment(tibble_fasta, typemsa = "free", taxon_labels = TRUE, graph_title = "Graph", legend_title = "legend")  
  
  
#------------------------------------------------------------------------------------------------------------
calculate_column_percentage <- function(alignment_tibble, column_of_interest) {
  
  # Determine percent of characters per column
  alignment_tibble %>%
    # Filters the data where column is equal to the column of interest
    dplyr::filter(column == column_of_interest) %>%
    # Selects all columns except 'column'
    dplyr::select(!column) %>%
    # Makes the data into a long tibble by selecting every column
    tidyr::pivot_longer(everything(), 
                        # Moving the names to a column named 'taxon'
                        names_to = "taxon", 
                        # Moves the values to a column named 'value'
                        values_to = "value") %>%
    # Selects the value column
    dplyr::select(value) %>%
    # Makes a column named count where it is equal to the count of unique values
    dplyr::count(value, name = "count") %>%
    # # Creates a new column named 'percent' that is equal to the percent that that unique value had
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










