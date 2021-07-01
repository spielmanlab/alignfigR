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
  
  #### MOVE THIS INTO ITS OWN FUNCTION!
  # We should call this function: convert_seq_list_to_tibble()
  #### Make sure that read_alignment *still returns a tibble though*
  # Instead return as a tibble
  tibble::as_tibble(seq_list) -> new_data
  new_data %>%
    dplyr::mutate(column = 1:nrow(new_data)) %>%
    dplyr::select(column, 
                  dplyr::everything())
}
