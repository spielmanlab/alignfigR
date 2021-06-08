library(magrittr)

read_alignment <- function(file){
    # readLines appears to separate lines so there's a away to use them independently
    raw_data <- readLines(file, warn = FALSE )
    # Makes an empty array
    seq_vector <- c()
    # Makes an empty character string
    seq_name <- ""
    # All about getting the organism identifiers in a vector
    for (line in raw_data){
        # New sequence record? Reset numbering
      # If the lines begins with a ">"
        if ( grepl("^>", line) ){
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
    # Instead return as a WIDE tibble
    tibble::as_tibble(seq_list) 
}

# Example usage:
read_alignment("~/Desktop/alignfigR/read_alignment/protein.fasta") -> protein_seqs
protein_seqs


























































