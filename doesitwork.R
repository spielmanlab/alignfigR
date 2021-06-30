# Use this script for playing around
load_all()
read_alignment("tests/testthat/Data/protein.fasta") -> tibble_fasta
plot_alignment(tibble_fasta, palette_msa = "forest")
define_palette("dna") -> test_palette
test_palette
data.class(1)
tibble_fasta

plot_alignment(tibble_fasta, taxon_labels = TRUE, palette_msa = "floral", graph_title = "Graph", legend_title = "Legend", clist = c(1:260), stack = TRUE) 






















