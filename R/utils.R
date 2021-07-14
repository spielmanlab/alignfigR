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
column <- count <- value <- x1 <- x2 <- y1 <- y2 <- NULL
#' Defines color for absence of protein/nucleotide
null_color <- "grey85"

#' Threshold for numerical comparisons
#' @noRd
ZERO <- 1e-12

#' Defines Nucleotide Palette
nucleotide_pal <- c("A"= "mediumblue", 
                    "G"="orangered1",
                    "C"= "limegreen", 
                    "T"= "khaki1", 
                    "U"= "khaki1")
#' Defines basic Palette
basic_pal <- c("A" = "limegreen", "G" = "lightgreen",
               "C" = "hotpink1", "T" = "red",
               "U" = "lightsalmon", "J" = "maroon",
               "B" = "snow", "O" = "mediumorchid",
               "D" = "lemonchiffon", "E" = "lightseagreen",
               "N" = "darkgreen", "Q" = "thistle",
               "I" = "darkorange", "L" = "lightcyan",
               "M" = "dodgerblue", 
               "F" = "gray30", "W" = "black",
               "Y" = "plum", "V" = "yellow",
               "H" = "navy", "Z" = "tan",
               "K" = "rosybrown4", "R" = "lightgoldenrod",
               "P" = "salmon", "-" = null_color,
               "S" = "darkred", "X" = "black")
#' Defines Ocean Palette
ocean_pal <- c("A" = "turquoise", "G" = "lightgreen",
               "C" = "turquoise4", "T" = "tan4",
               "U" = "steelblue", "J" = "springgreen4",
               "B" = "seashell", "O" = "seashell4",
               "D" = "seagreen", "E" = "dodgerblue",
               "N" = "skyblue", "Q" = "skyblue4",
               "I" = "lightsalmon", "L" = "lightcyan",
               "M" = "black", 
               "F" = "blue", "W" = "hotpink",
               "Y" = "navajowhite", "V" = "white",
               "H" = "navy", "Z" = "tan",
               "K" = "plum", "R" = "lightgoldenrod",
               "P" = "orange", "-" = null_color,
               "S" = "gray45", "X" = "aquamarine2")
#' Defines Forest Palette
forest_pal <- c("A" = "forestgreen", "G" = "darkgreen",
                "C" = "saddlebrown", "T" = "tan4",
                "U" = "brown4", "J" = "springgreen4",
                "B" = "black", "O" = "goldenrod4",
                "D" = "seagreen", "E" = "peru",
                "N" = "darkseagreen", "Q" = "navy",
                "I" = "wheat4", "L" = "orange",
                "M" = "skyblue", 
                "F" = "olivedrab4", "W" = "firebrick4",
                "Y" = "white", "V" = "indianred",
                "H" = "khaki4", "Z" = "tan",
                "K" = "sienna1", "R" = "beige",
                "P" = "black", "-" = null_color,
                "S" = "moccasin", "X" = "peachpuff4")
#' Defines Fire Palette
fire_pal <- c("A" = "red", "G" = "orange",
              "C" = "yellow", "T" = "firebrick4",
              "U" = "brown", "J" = "tan",
              "B" = "saddlebrown", "O" = "goldenrod",
              "D" = "salmon", "E" = "chocolate",
              "N" = "gray45", "Q" = "paleturquoise",
              "I" = "burlywood4", "L" = "mistyrose3",
              "M" = "wheat", "V" = "sienna4",
              "F" = "sandybrown", "W" = "sienna",
              "Y" = "lavender", "V" = "indianred",
              "H" = "khaki", "Z" = "moccasin",
              "K" = "black", "R" = "white",
              "P" = "lightgoldenrod", "-" = null_color,
              "S" = "tomato", "X" = "black")
#' Defines Floral Palette
floral_pal <- c("A" = "chartreuse2", "G" = "orange",
                "C" = "yellow", "T" = "red",
                "U" = "pink", "J" = "dodgerblue",
                "B" = "plum", "O" = "thistle",
                "D" = "salmon", "E" = "darkblue",
                "N" = "violet", "Q" = "paleturquoise",
                "I" = "steelblue", "L" = "peru",
                "M" = "purple", "V" = "purple4",
                "F" = "palevioletred", "W" = "black",
                "Y" = "palegreen", "V" = "white",
                "H" = "lavender", "Z" = "honeydew2",
                "K" = "darkorange3", "R" = "aquamarine",
                "P" = "mistyrose3", "-" = null_color,
                "S" = "wheat", "X" = "black")
# Defines clustal palette
clustal_pal <- c("A" = "#80A0F0", "G" = "#F09048",
                 "C" = "#F08080", "T" = "#00FF00",
                 "U" = "cornsilk", "J" = "cornsilk",
                 "B" = "black", "O" = "cornsilk",
                 "D" = "#C048C0", "E" = "#C048C0",
                 "N" = "#00FA00", "Q" = "#00FF00",
                 "I" = "#80A08C", "L" = "#80A0F0",
                 "M" = "#80A0F0", 
                 "F" = "#80A0F0", "W" = "#80A0F0",
                 "Y" = "#15A4A4", "V" = "#80A0F0",
                 "H" = "#15A4A4", "Z" = "black",
                 "K" = "#F01505", "R" = "#F01505",
                 "P" = "#FFFF00", "-" = null_color,
                 "S" = "#00FF00", "X" = "black")
# Defines Hydrophobicity palette
hydrophobicity_pal <- c("A" = "#AD0052", "G" = "#6A0095",
                 "C" = "#C2003D", "T" = "#61009E",
                 "U" = "cornsilk", "J" = "cornsilk",
                 "B" = "#0C00F3", "O" = "cornsilk",
                 "D" = "#0C00F3", "E" = "#0C00F3",
                 "N" = "#0C00F3", "Q" = "#0C00F3",
                 "I" = "#FF0000", "L" = "#EA0015",
                 "M" = "#B0004F", 
                 "F" = "#CB0034", "W" = "#5B00A4",
                 "Y" = "#4F00B0", "V" = "#F60009",
                 "H" = "#1500EA", "Z" = "#0C00F3",
                 "K" = "#0000FF", "R" = "#0000FF",
                 "P" = "#4600B9", "-" = null_color,
                 "S" = "#5E00A1", "X" = "#680097")
# filter_taxa_and_sites()
c("column", "Q45745BACTU277497", "C1GBBACTZ253449", 
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





