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





