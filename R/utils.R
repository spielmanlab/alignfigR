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
column <- count <- value <- x1 <- x2 <- y1 <- y2 <- n <- type_data <- everything <- everything <-  NULL
#' Defines color for absence of protein/nucleotide
null_color <- "grey85"

#' Threshold for numerical comparisons
#' @noRd
ZERO <- 1e-12

#' Defines Nucleotides
c("A", "G", "C", "T") -> nucleotides


nucs_protein_identifiers <- c("A", "G", "C" , "T" , 
                              "U" , "J" , "B" , "O" , 
                              "D" , "E" , "N" , "Q" , "I" , 
                              "L" , "M" , "F" , "W" , "Y" , 
                              "V" ,  "H" , "Z" , "K" , "R" , 
                              "P" , "-", "S" , "X")

# read_alignment() tests
c("type_data", "column", "C9EABACTA301505",   "C9CABACTO298505",  
 "C9DABACTP301521" ,  "Q45745BACTU277497", "C1GBBACTZ253449",
 "C1GABACTU253446" ,  "C1HBBACTM256454"   ,"C1EABACTX258454" , 
 "C1EBBACTA257453",   "C1DABACTA258450"   ,"C1CABACTE258457" , 
 "Q45749BACTU257454", "C1JABACTU258449",   "C1JBBACTU258449"  ,
 "C1ABBACTK259461"  , "C7AABACTU286487" ,  "C1BBBACTU283495"  ,
 "C1BEBACTU283493"  , "C1BABACTK278489"  , "C1IABACTK287497"  ,
 "C1KABACTM284490"  , "C3CABACTK293502"  , "C3AABACTT295499"  ,
 "C3BABACTO304510"  , "C8CABACTP298503"  , "C9AABACTG295510"  ,
 "C20AABACUF288485" , "C19BABACUH293496" , "C19AABACTJ300502" ,
"C4AABACTI322528"  , "CAAABACTI308500"  , "C4BABACTI283470"  ,
"C16AACLOBI252441" , "C17AACLOBI265438" , "C25AABACTJ303514" ,
"C13AABACTU328534" , "C14AABACTS320521" , "C18AAPAEPP344563") -> tibble_fasta_names
# filter_taxa_and_sites()
c("type_data", "column", "Q45745BACTU277497", "C1GBBACTZ253449", 
  "C1GABACTU253446", "C1HBBACTM256454",  "C1EABACTX258454",  
  "C1EBBACTA257453", "C1DABACTA258450",  "C1CABACTE258457" , 
  "Q45749BACTU257454", "C1JABACTU258449",  "C1JBBACTU258449", 
  "C1ABBACTK259461", "C7AABACTU286487",  "C1BBBACTU283495", 
  "C1BEBACTU283493", "C1BABACTK278489",  "C1IABACTK287497",  
  "C1KABACTM284490", "C3CABACTK293502",   "C3AABACTT295499",  
  "C3BABACTO304510",  "C8CABACTP298503",   "C9AABACTG295510",  
  "C20AABACUF288485",  "C19BABACUH293496",  "C19AABACTJ300502", 
  "C4AABACTI322528", "CAAABACTI308500",  "C4BABACTI283470",  
  "C16AACLOBI252441", "C17AACLOBI265438",  "C25AABACTJ303514", 
  "C13AABACTU328534", "C14AABACTS320521",  "C18AAPAEPP344563") -> taxa_exclusion_test

# create_geom_rect_alignment()
c("Taxa", "seq", "x1", "x2", "y1", "y2") -> create_geom_rect_alignment_names
# Should I put this here
utils::globalVariables("percent")
utils::globalVariables("everything")

determine_type_threshold <- 0.9



