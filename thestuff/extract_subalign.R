

extract_subalign <- function(seq_list, plot_step = 1, tlist = c(), clist = c(), texcl = FALSE, cexcl = FALSE, cincl = FALSE)
{
  # Create subset of seqs containing only the desired taxa to plot
  if (length(tlist) == 0){
    sub_seqs_raw <- seq_list
  }else if (texcl){
    sub_seqs_raw <- seq_list[!(names(seq_list) %in% tlist)] # Exclude sequences in the provided list
  }else
  {
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


--------------------------------------------------------------------------------
  




















