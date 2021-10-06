## Explicit pipe definitions ---------------------------------------------------------
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @keywords internal
#' @noRd
#' @importFrom magrittr %>%
NULL


#' Nulls
#' @noRd
column <- count <- value <- x1 <- x2 <- y1 <- y2 <- n <- type_data <- everything <- percent <- NULL
#' Variable that causes issues when placed with NULLS
#' @noRd
utils::globalVariables("type")
#' Defines color for absence of protein/nucleotide
#' @noRd
null_color <- "grey85"

#' Threshold for numerical comparisons
#' @noRd
ZERO <- 1e-12

#' Defines Nucleotides
#' @noRd
c("A", "G", "C", "T") -> nucleotides

#' Defines nucleotide protein identifiers
#' @noRd
nucs_protein_identifiers <- c("A", "G", "C" , "T" , 
                              "U" , "J" , "B" , "O" , 
                              "D" , "E" , "N" , "Q" , "I" , 
                              "L" , "M" , "F" , "W" , "Y" , 
                              "V" ,  "H" , "Z" , "K" , "R" , 
                              "P" , "-", "S" , "X")
#' Defines the threshold for deteriming nucleotide of protein data type
#' 
determine_type_threshold <- 0.9




