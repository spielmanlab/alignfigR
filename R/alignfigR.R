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
  # Check to make sure all alignments have the same number of proteins
  if ( sum(lengths != lengths[1]) != 0 )
    stop("Your provided file is not an alignment. Please provide an alignment file in FASTA format to use alignfigR.")
  # Instead return as a tibble
  tibble::as_tibble(seq_list) -> new_data
  new_data %>%
    dplyr::mutate(column = 1:nrow(new_data)) %>%
    dplyr::select(column, 
                  dplyr::everything())
}





#' Gives the ability to select which taxa and columns you want portrayed in the plot
#'
#' @param alignment The tibble from read_alignment()
#' @param tlist A string of the taxa you wish to include or exclude
#' @param texcl Determinant if you want to include only the taxa in tlist, or exclude only the tlist
#' @param clist The columns you wish to have portrayed in the plot
#' @param cexcl Determinant if you want to include only the columns in clist, or exclude only the clist
#' @param stack Determines if you want the columns to be stacked by sequence marker
#' @return Returns a tibble which contains the preferred taxa and columns, and is prepped for geom_rect()
extract_subalign <- function(alignment, tlist = c(), texcl = FALSE, clist = c(), cexcl = FALSE, stack = FALSE) {
  # If tlist is empty, then alignment = data
  if (length(tlist) == 0){
    alignment -> data
    # If texcl is true, then if will remove all taxa in tlist from the data
  } else if (texcl){
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
    tidyr::pivot_longer(cols = tidyr::everything(), 
                        # Moves columns to 'Taxa'
                        names_to = "Taxa", 
                        # Moves values to 'seq' and sets it equal to data_longer
                        values_to = "seq") -> data_longer
  # Reorders the taxa to be alphabetical
  data_longer[order(data_longer$Taxa),] -> data_alphabetical
  # Counts the number of rows and saves it to number_of_rows
  as.integer(dplyr::count(data_alphabetical)) -> number_of_rows 
  # Determines the length of each individual taxon
  number_of_rows/as.integer(dplyr::count(unique(data_alphabetical[1]))) -> length_of_taxa
  if (stack) {
    data_alphabetical %>%
      dplyr::mutate(column = rep(1:length_of_taxa, number_of_rows/length_of_taxa)) -> data_rect
  } else {
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
                    y2 = y1 + 1) -> data_rect
  }
  # if clist is empty
  if (length(clist)== 0){
    # data_rect is equal to d
    data_rect -> d
    # if clist is not empty
  } else {
    # replaces 'column' column so it can be used to filter the data
    data_rect %>%
      dplyr::mutate(column = rep(1:length_of_taxa, number_of_rows/length_of_taxa)) -> data_column_ready
    # if cexcl is true
    if (cexcl) {
      # filters the data for all columns that are not mentioned in clist
      data_column_ready %>%
        dplyr::filter(!column %in% clist) -> d
      # if cexcl is false
    } else {
      # filters the data for all columns that are mentioned in clist
      data_column_ready %>%
        dplyr::filter(column %in% clist) -> d
    }
  }
  d
}




#' Assigns the color palette for plot_alignment(), and allows you to select a custom palette
#'
#' @param palette_msa The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @return Returns a color palette
define_palette <- function(palette_msa, 
                           uniques = NA, 
                           custom_colors = NA){
  if (tolower(palette_msa) == "random") {
    # subcolors is equal to colors without null_color
    subcolors <- grDevices::colors()[grDevices::colors() != null_color]
    # palette is equal to a random sampling of subcolors, the same length as uniques
    palette <- sample(subcolors,
                      length(uniques))
    names(palette) <- uniques
    return(palette)
  } else if (tolower(palette_msa) == "dna" || tolower(palette_msa) == "rna"){
    return(nucleotide_pal)
  } else if (tolower(palette_msa) == "custom") {
    # palette is defined as custom_colors
    palette <- custom_colors
    # names of the palette are defined as the bases
    names(palette) <- uniques
    return(palette)
  } else if (tolower(palette_msa) == "basic") {
    # the palette is defined below
    return(basic_pal)
  } else if (tolower(palette_msa) == "ocean") {
    # the palette is defined below
    return(ocean_pal)
  } else if (tolower(palette_msa) == "forest") {
    # the palette is defined below
    return(forest_pal)
  } else if (tolower(palette_msa) == "fire") {
    # the palette is defined below
    return(fire_pal)
  } else if (tolower(palette_msa) == "floral") {
    # the palette is defined below
    return(floral_pal)
  } else {
    stop("Not a valid palette.")
  }
}






#' Assigns the color palette for plot_alignment(), and allows you to select a custom palette
#'
#' @param alignment The tibble from read_alignment()
#' @param tlist A string of the taxa you wish to include or exclude
#' @param texcl Determinant if you want to include only the taxa in tlist, or exclude only the tlist
#' @param clist The columns you wish to have portrayed in the plot
#' @param cexcl Determinant if you want to include only the columns in clist, or exclude only the clist
#' @param stack Determines if you want the columns to be stacked by specific sequence marker
#' @param palette_msa The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param taxon_labels Determinant of if taxa identifiers will be present on the graph or not
#' @param graph_title Sets the graph title
#' @param legend_title Sets the legend title
#' @return Returns an MSA of the data
plot_alignment <- function(alignment, 
                           tlist = c(), 
                           texcl = FALSE, 
                           clist = c(), 
                           cexcl = FALSE, 
                           stack = FALSE, 
                           uniques = NA, 
                           palette_msa, 
                           custom_colors = NA, 
                           taxon_labels = FALSE, 
                           graph_title = NA, 
                           legend_title = NA) {
  # runs extract_subalign_improved and defines it as plot_frame
  extract_subalign(alignment, 
                   tlist, 
                   texcl, 
                   clist, 
                   cexcl, 
                   stack) -> plot_frame
  # defines uniques as the uniques of the sequence in plot_frame
  unique(plot_frame$seq) -> uniques
  # runs define palette and sets it's output as pal
  define_palette(palette_msa, 
                 uniques, 
                 custom_colors) -> pal
  if (stack) {
    plot_frame %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = column, fill = seq) +
      ggplot2::geom_bar(position = "stack") +
      ggplot2::scale_fill_manual(values = pal,
                                 name = legend_title)+
      ggplot2::labs(title = graph_title)-> plot
    return(plot)
  }
  # if taxon_labels is equal to FALSE
  if (taxon_labels == FALSE){
    # defines plot as
    plot <- ggplot2::ggplot() +
      # geom_rect() using plot_frame from extract_subalign()
      ggplot2::geom_rect(plot_frame, 
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
      ggplot2::labs(title = graph_title)
  } # if taxon_labels is equal to TRUE
  else {
    # defines plot as
    plot <- ggplot2::ggplot() + 
      # geom_rect() being run on plot_frame from extract_subalign()
      ggplot2::geom_rect(plot_frame, 
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
      ggplot2::scale_y_discrete(limits = names(alignment))
  }
  # returns the plot
  plot
}  





#' Reveals the unique protein/nucleotide identifiers found in this column, reveals the counts and the frequency of organisms having that identifier in that column
#'
#' @param alignment_tibble The tibble from read_alignment()
#' @param column_of_interest The specific column you want to see
#' @return Returns a tibble with rows 'value', 'count' and 'percent'
calculate_column_percentage <- function(alignment_tibble, 
                                        column_of_interest) {
  alignment_tibble %>%
    # Filters the data where column is equal to the column of interest
    dplyr::filter(column == column_of_interest) %>%
    dplyr::select(!column) %>%
    # Makes the data into a long tibble by selecting every column
    tidyr::pivot_longer(dplyr::everything(),
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
