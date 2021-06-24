# Color Function

null_color <- "grey85"
# Fill does not like when defining palette gives a palette with NAMES
# typemsa could be random, dna or rna, custom or free
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
# Example usage:
# Ask about if the ability to only enter ACSX would be helpful, or making it so they could enter dna instead of "dna"
define_palette("custom", c("A", "C", "S", "X"), c("dodgerblue", "cornsilk1", "snow", "red"))
define_palette("random", c("A", "C", "S", "X"))
define_palette("dna") 
define_palette("rna")
define_palette("random", uniques = c("-","T","S","A","N","L","Y","I","G","D","K","V","E","P","F","Q","R","H", "C","M","W"))










 


