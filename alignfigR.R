library(tidyverse)
null_color <- "grey85"
ZERO <- 1e-12 # effectively zero


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


calculate_column_percentage <- function(alignment_tibble, column_of_interest) {
  
  # Determine percent of characters per column
  alignment_tibble %>%
    dplyr::filter(column == column_of_interest) %>%
    dplyr::select(!column) %>%
    tidyr::pivot_longer(everything(), names_to = "taxon", values_to = "value") %>%
    dplyr::select(value) %>%
    dplyr::count(value, name = "count") %>%
    dplyr::mutate(percent = count / sum(count)) -> column_percentages # tibble of counts, percents
  
  
  # Check that calculations worked
  expected <- 1 # goal if we did it right
  observed <- sum(column_percentages$percent)
  true_if_worked <- abs(expected - observed) <= ZERO 
  stopifnot(true_if_worked)
  
  # Give back the tibble with three columns: value, count, and percent
  column_percentages
}











