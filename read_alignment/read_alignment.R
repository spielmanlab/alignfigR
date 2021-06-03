library(magrittr)

read_alignment <- function(file){
    raw_data <- readLines( file, warn = FALSE )
    seq_vector <- c()
    seq_name <- ""
    for (line in raw_data){
        # New sequence record? Reset numbering
        if ( grepl("^>", line) ){
            seq_name <- sub("^>", "", line)
            seq_vector[seq_name] <- ""
        }
        else {
            temp_seq <- gsub(" ","",line)
            temp_seq <- gsub("\n","",temp_seq)
            seq_vector[seq_name] <- paste( seq_vector[seq_name], temp_seq, sep="" )
        }
    }
    # Is this an alignment?
    seq_list <- strsplit(seq_vector, split = "")
    lengths <- sapply(seq_list, length)
    if ( sum(lengths != lengths[1]) != 0 )
        stop("Your provided file is not an alignment. Please provide an alignment file in FASTA format to use alignfigR.")
    
    ############# LOOKIE HERE!!!!!!! ##############
    # this function used to end above this line and return `seq_list`
    # this morning 6/3/21 i'm making it into a tibble for a bit more convenience.
    # You'll note that I NEVER LOAD A LIBRARY (ish) in this script, but I use the "colon colon" :: to refer to functions based on their package.
    # For now, we load magrittr to be able to use pipe though.
    # This Is The Way of package writing.
        
    # Instead return as a WIDE tibble (this is new code):
    tibble::as_tibble(seq_list) %>%
      dplyr::mutate(position = 1:dplyr::n()) %>% # n() is a dplyr function! dplyr::n
      dplyr::select(position, dplyr::everything()) # everything() is a dplyr function! Parentheses=function.
}

# Want to mess around and write your own function to do this? 100000% !!!! 
# The code is your oyster. 


# Example usage:
read_alignment("protein.fasta") -> protein_seqs
read_alignment("nucleotide.fasta") -> nuc_seqs

