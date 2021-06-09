library(magrittr)
library(tidyverse)
null_color <- "grey85"

read_alignment <- function(file){
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
  tibble::as_tibble(seq_list) 
}

position_alignment <- function(file) {
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
    mutate(position = 1:nrow(new_data)) %>%
    select(position, everything())
}

position_find <- function(file, ID = c(), pos1, pos2) {
  position_alignment(file) -> new_data
  x <- ID
  range <- pos1:pos2
  new_data %>%
    select(position, x) %>%
    filter(position %in% range)
}

extract_subalign <- function(seq_list, plot_step = 1, tlist = c(), clist = c(), texcl = FALSE, cexcl = FALSE, cincl = FALSE)
{
  # Create subset of seqs containing only the desired taxa to plot
  if (length(tlist) == 0){
    sub_seqs_raw <- seq_list
  }else if (texcl){
    # Exclude sequences in the provided list
    sub_seqs_raw <- seq_list[!(names(seq_list) %in% tlist)] 
  } else {
    sub_seqs_raw <- seq_list[tlist]
  }
  # Further subset the sequences to contain only the desired columns
  if (length(clist) == 0){
    clist <- 1:length(sub_seqs_raw[[1]])
  }
  if (cexcl){
    sub_seqs <- lapply(sub_seqs_raw, `[`, (-1*clist))
  }
  if (cincl){
    sub_seqs <- lapply(sub_seqs_raw, `[`, (1*clist))
  } else
  {
    sub_seqs <- lapply(sub_seqs_raw, `[`)
  }
  # Create the data frame to plot
  sub_seqs <- rev(sub_seqs) # For proper plotting direction
  each_length <- length(sub_seqs[[1]]) # length of column 1
  seqnames <- c(t(replicate(each_length, names(sub_seqs))))
  seqletters <- unlist(sub_seqs)
  y1 <- c(t(replicate(each_length, 1:length(sub_seqs))))
  y2 <- y1 + plot_step
  x1 <- rep(1:each_length, length(sub_seqs))
  x2 <- x1 + plot_step
  plot_frame <- data.frame( "x1"  = x1,
                            "y1"  = y1,
                            "x2"  = x2,
                            "y2"  = y2,
                            "name" = seqnames,
                            "seq"  = seqletters)
  rownames(plot_frame) <- NULL
  plot_frame
}

define_palette <- function(typemsa, uniques = NA, custom_colors = NA){
  if (tolower(typemsa) == "random") {
    subcolors <- colors()[colors() != null_color]
    palette <- sample(subcolors, length(uniques))
    names(palette) <- uniques
  } else if (tolower(typemsa) == "dna" || tolower(typemsa) == "rna"){
    bases <- c("A", "C", "G", "T", "U")
    palette <- c("mediumblue", "orangered1", "limegreen", "khaki1", "khaki1")
    names(palette) <- bases  
  } else if (tolower(typemsa) == "custom") {
    palette <- custom_colors
    names(palette) <- uniques
  } else if (tolower(typemsa) == "free") {
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
  palette
}

pos_percent <- function(file, position) {
  position_alignment(file) -> new_data
  position -> x
  r <- 1
  n <- 0
  new_data %>%
    filter(position == x) %>%
    select(!position) -> a
  unlist(a) -> a
  unname(a) -> a
  unique(a) -> uniques
  length(uniques) + 1 -> q
  length(a) -> l
  answer <- c()
  while (r < q) {
    for (i in a) {
      if (i == uniques[r]) {
        n + 1 -> n
      } else {
        n + 0 -> n 
      }
    }
    n / l -> percent
    name <- uniques[r]
    answer[name] <- paste(answer[name], percent, sep = "")
    n <- 0
    r <- r + 1 
  } 
  return(answer)
}

plot_alignment <- function(file, taxa = c(), plot_step = 1, taxon_labels = FALSE, columns = c(), exclude_taxa = FALSE, exclude_columns = FALSE, include_columns = FALSE, legend_title = "Character", graph_title = "Character", typemsa, custom_colors = NA)
{
  seq_list <- read_alignment(file)
  # Extract desired alignment subset
  plot_frame <- extract_subalign(seq_list, plot_step, taxa, columns, exclude_taxa, exclude_columns, include_columns)
  
  # Determine alignment characters for palette construction
  uniques <- unique(plot_frame$seq)
  pal <- define_palette(typemsa, uniques, custom_colors)
  # Plot
  theme_set(theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank()))
  if (taxon_labels == FALSE){
    p <- ggplot() +
      geom_rect(plot_frame, mapping=aes(xmin=x1-1, xmax=x2-1, ymin=
                                          y1-1, ymax=y2-1, fill = seq), linetype=0) +       
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      labs(title = graph_title)
  }
  else {
    p <- ggplot() + 
      geom_rect(plot_frame, mapping=aes(xmin=x1-1, xmax=x2-1, ymin =
                                          y1-1, ymax=y2-1, fill = seq), linetype=0) +
      labs(title = graph_title) +
      scale_fill_manual(values=pal, 
                        name = legend_title) +
      scale_y_discrete(limits = names(seq_list))
  }
  p
}

plot_alignment("~/Desktop/alignfigR/read_alignment/protein.fasta", typemsa = "free", legend_title = "New Legend", taxon_labels = TRUE, graph_title = "New Graph")

position_find("~/Desktop/alignfigR/read_alignment/protein.fasta", ID = c("C9EABACTA301505","C9CABACTO298505"), pos1 = 16, pos2 = 25)

pos_percent("~/Desktop/alignfigR/read_alignment/protein.fasta", 15) 























