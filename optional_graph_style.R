# New type of graph 
  extract_subalign <- function(alignment, tlist = c(), texcl = FALSE, clist = c(), cexcl = FALSE, stack = FALSE) {
    if (stack) {
      alignment -> data
    } else {
    # If tlist is empty, then alignment = data
    if (length(tlist) == 0){
      alignment -> data
      # If texcl is true, then if will remove all taxa in tlist from the data
    }else if (texcl){
      alignment %>%
        dplyr::select(-tlist) -> data
      # This will only include all mentioned taxa in tlist
    } else {
      alignment %>%
        dplyr::select(column, tlist) -> data
    } 
    }
    # Pipes data into Select
    data %>%
      # Removes column 'column'
      dplyr::select(-column) %>%
      # Pivots data longer, selects all columns
      tidyr::pivot_longer(cols = everything(), 
                          # Moves columns to 'Taxa'
                          names_to = "Taxa", 
                          # Moves values to 'seq' and sets it equal to data_longer
                          values_to = "seq") -> data_longer
    # Reorders the taxa to be alphabetical
    data_longer[order(data_longer$Taxa),] -> data_alphabetical
    # Counts the number of rows and saves it to number_of_rows
    as.integer(dplyr::count(data_alphabetical)) -> number_of_rows 
    # Determines the length of each individual taxon
    number_of_rows/as.integer(dplyr::count(unique(data_alphabetical[1]))) -> length_of_taxa
    if (stack) {
      data_alphabetical %>%
        dplyr::mutate(column = rep(1:length_of_taxa, number_of_rows/length_of_taxa)) -> d
    
    } else {
      # Pipes data_alphabetical into mutate
      data_alphabetical %>%
        # Creates a new column where is it repeating 1:length_of_taxa until it reaches the end
        dplyr::mutate(x1 = rep(1:length_of_taxa, number_of_rows/length_of_taxa), 
                      # Creates a new column where x2 is 1 greater than x1
                      x2 = x1 + 1, 
                      # Creates a new column where 1 is assigned to the first taxon, 2 to the 2nd and so on
                      y1 = c(t(replicate(length_of_taxa, 
                                         1:(number_of_rows/length_of_taxa)))), 
                      # Creates a new column where y2 is 1 greater than y1
                      y2 = y1 + 1) -> data_rect
      # if clist is empty
      if (length(clist)== 0){
        # data_rect is equal to d
        data_rect -> d
        # if clist is not empty
      } else {
        # replaces 'column' column so it can be used to filter the data
        data_rect %>%
          dplyr::mutate(column = rep(1:length_of_taxa, number_of_rows/length_of_taxa)) -> data_column_ready
        # if cexcl is true
        if (cexcl) {
          # filters the data for all columns that are not mentioned in clist
          data_column_ready %>%
            dplyr::filter(!column %in% clist) %>%
            # removes column and sets it equal to d
            dplyr::select(-column) -> d
          # if cexcl is false
        } else {
          # filters the data for all columns that are mentioned in clist
          data_column_ready %>%
            dplyr::filter(column %in% clist) %>%
            # removes column and sets it equal to d
            dplyr::select(-column) -> d
        }
      }
    }
    d
  }

  
  plot_alignment <- function(alignment, tlist = c(), texcl = FALSE, clist = c(), cexcl = FALSE, stack = FALSE, typemsa, uniques = NA, custom_colors = NA, taxon_labels = FALSE, graph_title = NA, legend_title = NA) {
    # runs extract_subalign_improved and defines it as plot_frame
    extract_subalign(alignment, tlist, texcl, clist, cexcl, stack) -> plot_frame
    # defines uniques as the uniques of the sequence in plot_frame
    unique(plot_frame$seq) -> uniques
    # runs define palette and sets it's output as pal
    define_palette(typemsa, uniques, custom_colors) -> pal
    if (stack) {
      plot_frame %>%
        ggplot2::ggplot() +
        ggplot2::aes(x = column, fill = seq) +
        ggplot2::geom_bar(position = "stack") +
        ggplot2::scale_fill_manual(values = pal) -> plot
      return(plot)
    }
    # if taxon_labels is equal to FALSE
    if (taxon_labels == FALSE){
      # defines plot as
      plot <- ggplot2::ggplot() +
        # geom_rect() using plot_frame from extract_subalign()
        geom_rect(plot_frame, mapping=ggplot2::aes(xmin=x1-1, xmax=x2-1, ymin=
                                            y1-1, ymax=y2-1, fill = seq), linetype=0) +       
        # sets the custom color palette as pal and the name of the legend
        scale_fill_manual(values=pal, 
                          name = legend_title) +
        # sets the graph title as graph_title
        labs(title = graph_title)
    } # if taxon_labels is equal to TRUE
    else {
      # defines plot as
      plot <- ggplot2::ggplot() + 
        # geom_rect() being run on plot_frame from extract_subalign()
        geom_rect(plot_frame, mapping=ggplot2::aes(xmin=x1, xmax=x2, ymin =
                                            y1, ymax=y2, fill = seq), linetype=0) +
        # defines the graph title as graph_title
        labs(title = graph_title) +
        # defiens the custom color palette and names the legend title as legend_title
        scale_fill_manual(values=pal, 
                          name = legend_title) +
        # places the taxon identifiers and column along the y axis
        scale_y_discrete(limits = names(alignment))
    }
    # returns the plot
    plot
  }  
plot_alignment(tibble_fasta, typemsa = "Forest", stack = TRUE)  
  
  
  
  
  
  
  
  
    