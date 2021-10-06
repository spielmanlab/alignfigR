#' Assigns the color palette for plots, and allows you to select a custom palette
#'
#' @param color_palette The palette you wish to use. Options are "random", "dna", "rna", "custom", "free", "ocean", "fire", "forest" and "floral".
#' @param uniques The protein/nucleotide identifiers used in your data
#' @param custom_colors A string of the colors you wish to have in the palette that contains the same amount of colors as unique protein/nucleotide identifiers you have in your data. The first identifier in uniques will be assigned the first color in custom_colors and so on.
#' @param type Type determined, or specified by user, in read_alignment() 
#' @return Returns a color palette
define_palette <- function(color_palette = "default",
                           uniques = NA,
                           custom_colors = NA,
                           type = NULL){
  if (tolower(color_palette) == "nucleotide") {
   if (type == "Nucleotide") {
   return(nucleotide_pal)
  } else {
    stop("Nucleotide palette assigned to a non-nucleotide alignment")
  }
  }
  else if (tolower(color_palette) == "custom") {
    # palette is defined as custom_colors
    palette <- custom_colors
    # names of the palette are defined as the bases
    names(palette) <- uniques
    return(palette)
  } else if (tolower(color_palette) == "default") {
    # the palette is defined below
    if (type == "Protein") {
      return(protein_default_pal)
    } else if (type == "Nucleotide") {
      return(nucleotide_default_pal)
    } else if (type == "Character") {
      return(character_default_pal)
    } else {
      stop("Clustal palette given to character alignment ")
    }
  } else if (tolower(color_palette) == "ocean") {
    # the palette is defined below
    return(protein_ocean_pal)
  } else if (tolower(color_palette) == "forest") {
    # the palette is defined below
    return(protein_forest_pal)
  } else if (tolower(color_palette) == "fire") {
    # the palette is defined below
    return(protein_fire_pal)
  } else if (tolower(color_palette) == "floral") {
    # the palette is defined below
    return(protein_floral_pal)
  } else if (tolower(color_palette) == "clustal") {
    # the palette is defined below
    if (type == "Protein") {
    return(protein_clustal_pal)
    } else if (type == "Nucleotide") {
      return(nucleotide_clustal_pal)
    } else {
      stop("Clustal palette given to character alignment ")
    }
  } else if (tolower(color_palette) == "zappo") {
    # the palette is defined below
    return(protein_zappo_pal)
  } else if (tolower(color_palette) == "taylor") {
    # the palette is defined below
    return(protein_taylor_pal)
  } else if (tolower(color_palette) == "hydrophobicity") {
    # the palette is defined below
    return(protein_hydrophobicity_pal)
  } else if (tolower(color_palette) == "helixpro") {
    # the palette is defined below
    return(protein_helix_propensity_pal)
  } else if (tolower(color_palette) == "strandpro") {
    # the palette is defined below
    return(protein_strand_propensity_pal)
  } else if (tolower(color_palette) == "turnpro") {
    # the palette is defined below
    return(protein_turn_propensity_pal)
  } else if (tolower(color_palette) == "buried index") {
    # the palette is defined below
    return(protein_buried_index_pal)
  } else if (tolower(color_palette) == "purpyr") {
    # the palette is defined below
    return(nucleotide_ry_pal)
  } else if (tolower(color_palette) == "cinema") {
    if (type == "Protein") {
      return(protein_cinema_pal)
    } else {
      stop("Protein palette assigned to a non-protein dataset")
    } 
    } else {
    stop("Not a valid palette.")
  } 
}

