#' Transforms fasta into a tibble
#'
#' @param file The fasta
#' @param data_type The data_type associated with the data. If left blank, the function will determine the type of data
#' @return Returns a tibble of the fasta data
read_alignment <- function(file, data_type = "") {
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
  convert_seq_list_to_tibble(seq_list) -> data_tibble
  determine_type(data_tibble, data_type)
  data_tibble
}



#' Transforms seq_list into a tibble
#'
#' @param seq_list Dataframe output from read_alignment
#' @return Returns a tibble of the fasta data
convert_seq_list_to_tibble <- function(seq_list) {
  tibble::as_tibble(seq_list) -> new_data
  new_data %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~toupper(.x))) %>%
    dplyr::mutate(column = 1:nrow(new_data)) %>%
    dplyr::select(column,
                  dplyr::everything())
}




#' Allows the user to select what taxa and sites are present in the graphs
#'
#' @param fasta_tibble Tibble output from read_alignment()
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
#' @param data The output from filter_taxa_and_sites
#' @return Returns a longer tibble of filtered_data
make_data_longer <- function(data) {
  data %>%
    # Removes column 'column'
    dplyr::select(-column) %>%
    # Pivots data longer, selects all columns
    tidyr::pivot_longer(cols = tidyr::everything(),
                        # Moves columns to 'Taxa'
                        names_to = "Taxa",
                        # Moves values to 'seq' and sets it equal to data_longer
                        values_to = "seq")
}

#' Combines filter_taxa_and_sites and make_data_longer
#'
#' @param fasta_tibble Tibble output from read_alignment()
#' @param taxa List of desired or undesired taxa
#' @param exclude_taxa Determines if you wish to only include or exclude the taxa in 'taxa'
#' @param sites List of desired positions in the fasta sequence
#' @param exclude_sites Determines if you wish to only include or exclude the sites in 'sites'
#' @return Returns a longer tibble of filtered_data
filter_and_make_data_longer <- function(fasta_tibble,
                                        taxa,
                                        exclude_taxa,
                                        sites,
                                        exclude_sites) {
  filter_taxa_and_sites(fasta_tibble,
                        taxa,
                        exclude_taxa,
                        sites,
                        exclude_sites) %>%
    make_data_longer()
}



#' Allows the package to determine data type, as well as allowing the user to specify this
#'
#' @param data Tibble output from convert_seq_list_to_tibble()
#' @param data_type The data_type associated with the data. If left blank, the function will determine the type of data 
#' @return Returns a tibble of the data that contains a column containing the type of data
determine_type <- function(data, data_type = "") {
if (tolower(data_type) == "protein") {
  type <<- "Protein"
} else if (tolower(data_type) == "nucleotide") {
  type <<- "Nucleotide"
} else if (tolower(data_type) == "character") {
  type <<- "Character"
} else if (data_type != "") {
  stop("Not a Valid Data Type")
} else {
make_data_longer(data) -> data_longer
calculate_total_identifiers(data_longer) -> total_identifiers
if (total_identifiers == 0) {
  type <<- "Character"
} else {
calculate_total_seqs(data_longer) -> total_seqs
calculate_total_nucs(data_longer) -> total_nucs
total_nucs/total_seqs -> percent_nucs
if (percent_nucs < determine_type_threshold) {
  type <<- "Protein"
} else {
  type <<- "Nucleotide"
}
}
}
}

#' Calculates total seqs besides gaps in the data
#'
#' @param data_longer Tibble output from make_data_longer()
#' @return Returns a sum of the number of proteins/nucleotides (ignoring gaps) in the data
calculate_total_seqs <- function(data_longer) {
  data_longer %>%
  dplyr::count(seq) %>%
  dplyr::filter(seq != "-") %>%
  dplyr::select(n) %>%
  sum() 
}

#' Calculates the total number of nucleotides in the data
#'
#' @param data_longer Tibble output from make_data_longer()
#' @return Returns a sum of the number of nucleotides in the data
calculate_total_nucs <- function(data_longer) {
  data_longer %>%
    dplyr::count(seq) %>%
    dplyr::filter(seq %in% nucleotides) %>%
    dplyr::select(n) %>%
    sum()
}

#' Calculates the total number of protein/nucleotide identifiers in the data
#'
#' @param data_longer Tibble output from make_data_longer()
#' @return Returns a sum of the number of nucleotides in the data
calculate_total_identifiers <- function(data_longer) {
  data_longer %>%
    dplyr::filter(seq %in% nucs_protein_identifiers) %>%
    dplyr::count(seq) %>%
    dplyr::select(n) %>%
    sum()
}




