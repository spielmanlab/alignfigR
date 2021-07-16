#' Assigns the color palette for plots, and allows you to select a custom palette
#'
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @return Returns a color palette
define_palette <- function(color_palette,
                           uniques = NA,
                           custom_colors = NA){
  if (tolower(color_palette) == "random") {
    # subcolors is equal to colors without null_color
    subcolors <- grDevices::colors()[grDevices::colors() != null_color]
    # palette is equal to a random sampling of subcolors, the same length as uniques
    palette <- sample(subcolors,
                      length(uniques))
    names(palette) <- uniques
    return(palette)
  } else if (tolower(color_palette) == "dna" || tolower(color_palette) == "rna"){
    return(nucleotide_pal)
  } else if (tolower(color_palette) == "custom") {
    # palette is defined as custom_colors
    palette <- custom_colors
    # names of the palette are defined as the bases
    names(palette) <- uniques
    return(palette)
  } else if (tolower(color_palette) == "basic") {
    # the palette is defined below
    return(basic_pal)
  } else if (tolower(color_palette) == "ocean") {
    # the palette is defined below
    return(ocean_pal)
  } else if (tolower(color_palette) == "forest") {
    # the palette is defined below
    return(forest_pal)
  } else if (tolower(color_palette) == "fire") {
    # the palette is defined below
    return(fire_pal)
  } else if (tolower(color_palette) == "floral") {
    # the palette is defined below
    return(floral_pal)
  } else if (tolower(color_palette) == "clustal") {
    # the palette is defined below
    return(clustal_pal)
  } else if (tolower(color_palette) == "zappo") {
    # the palette is defined below
    return(zappo_pal)
  } else if (tolower(color_palette) == "taylor") {
    # the palette is defined below
    return(taylor_pal)
  } else if (tolower(color_palette) == "hydrophobicity") {
    # the palette is defined below
    return(hydrophobicity_pal)
  } else if (tolower(color_palette) == "helixpro") {
    # the palette is defined below
    return(helix_propensity_pal)
  } else if (tolower(color_palette) == "strandpro") {
    # the palette is defined below
    return(strand_propensity_pal)
  } else if (tolower(color_palette) == "turnpro") {
    # the palette is defined below
    return(turn_propensity_pal)
  } else if (tolower(color_palette) == "buried index") {
    # the palette is defined below
    return(buried_index_pal)
  } else {
    stop("Not a valid palette.")
  }
}
