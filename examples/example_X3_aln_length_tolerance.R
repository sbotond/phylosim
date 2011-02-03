# Example showing the 'alignment length tolerance' parameter to shorten plotted alignments by
# pruning away columns with the highest proportions of gaps.

library(phylosim)

pfam.aln <- "data/PF02171_seed.fasta"
pfam.tree <- "data/PF02171_seed.nh"

sim <- PhyloSim()
readTree(sim, pfam.tree)
readAlignment(sim, pfam.aln)

# Plot the alignment at a range of aln.length.tolerance parameter values.
p <- function(tolerance) {
  plot(sim, plot.chars=F, plot.nongap.bl=T, aln.length.tolerance=tolerance)
}
p(99)
p(1.2)
p(1.0)
p(0.9)
p(0.8)
