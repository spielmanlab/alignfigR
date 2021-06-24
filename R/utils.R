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

#' Nucleotides
nucleotides <- c("A", "C", "G", "T", "U")

#' Needs a thing
nucleotide_pal <- c("mediumblue", "orangered1", "limegreen", "khaki1", "khaki1")

free_pal <- c("A" = "limegreen", "G" = "lightgreen",
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

ocean_pal <- c("A" = "turquoise", "G" = "lightgreen",
               "C" = "turquoise4", "T" = "tan4",
               "U" = "steelblue", "J" = "springgreen4",
               "B" = "seashell", "O" = "seashell4",
               "D" = "seagreen", "E" = "lightseagreen",
               "N" = "skyblue", "Q" = "skyblue4",
               "I" = "lightblue1", "L" = "lightcyan",
               "M" = "midnightblue", "V" = "powderblue",
               "F" = "blue", "W" = "dodgerblue",
               "Y" = "navajowhite", "V" = "moccasin",
               "H" = "navy", "Z" = "tan",
               "K" = "lavenderblush2", "R" = "lightgoldenrod",
               "P" = "deepskyblue4", "-" = null_color,
               "S" = "honeydew3", "X" = "aquamarine2")
forest_pal <- c("A" = "wheat4", "G" = "darkgreen",
                "C" = "saddlebrown", "T" = "tan4",
                "U" = "brown4", "J" = "springgreen4",
                "B" = "black", "O" = "goldenrod4",
                "D" = "seagreen", "E" = "chocolate4",
                "N" = "darkseagreen", "Q" = "bisque4",
                "I" = "burlywood4", "L" = "peru",
                "M" = "forestgreen", "V" = "rosybrown4",
                "F" = "olivedrab4", "W" = "green4",
                "Y" = "navajowhite4", "V" = "moccasin",
                "H" = "khaki4", "Z" = "tan",
                "K" = "sienna4", "R" = "beige",
                "P" = "gray20", "-" = null_color,
                "S" = "darkseagreen4", "X" = "peachpuff4")
fire_pal <- c("A" = "tomato", "G" = "orange",
              "C" = "yellow", "T" = "red",
              "U" = "brown", "J" = "tan",
              "B" = "saddlebrown", "O" = "goldenrod",
              "D" = "salmon", "E" = "chocolate",
              "N" = "gray45", "Q" = "paleturquoise",
              "I" = "burlywood4", "L" = "peru",
              "M" = "firebrick4", "V" = "sienna4",
              "F" = "sandybrown", "W" = "gray20",
              "Y" = "red4", "V" = "indianred",
              "H" = "khaki", "Z" = "moccasin",
              "K" = "sienna", "R" = "blanchedalmond",
              "P" = "lightgoldenrod", "-" = null_color,
              "S" = "wheat", "X" = "black")
floral_pal <- c("A" = "chartreuse2", "G" = "orange",
                "C" = "yellow", "T" = "red",
                "U" = "pink", "J" = "dodgerblue",
                "B" = "plum", "O" = "thistle",
                "D" = "salmon", "E" = "darkblue",
                "N" = "violet", "Q" = "paleturquoise",
                "I" = "steelblue", "L" = "peru",
                "M" = "purple", "V" = "purple4",
                "F" = "palevioletred", "W" = "midnightblue",
                "Y" = "palegreen", "V" = "white",
                "H" = "lavender", "Z" = "honeydew2",
                "K" = "darkorange3", "R" = "aquamarine",
                "P" = "mistyrose3", "-" = null_color,
                "S" = "wheat", "X" = "black")
















