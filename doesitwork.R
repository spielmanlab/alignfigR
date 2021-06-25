# Use this script for playing around
load_all()
read_alignment("Data/protein.fasta") -> tibble_fasta
plot_alignment(tibble_fasta, typemsa = "free", taxon_labels = TRUE, graph_title = "New Graph", legend_title = "New Legend")
define_palette("dna") -> test_palette
test_palette
