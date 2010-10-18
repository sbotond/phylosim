library(R.oo)
library(ape)
library(ggplot2)
source("../PhyloSimSource.R")

# Ensembl pax gene family.
pax.aln <- "data/ensembl_pax.fasta"
pax.tree <- "data/ensembl_pax.nh"

# Sub-tree of Ensembl pax gene family.
pax.sub.aln <- "data/ensembl_pax_sub.fasta"
pax.sub.tree <- "data/ensembl_pax_sub.nh"

# Indelible simulated tree & alignment.
slr.aln <- "data/slr_bigtree.fasta"
slr.tree <- "data/slr_bigtree.nh"

# Genomic alignment from Ensembl.
ensembl.genomic.aln <- "data/ensembl_genomic.fasta"

# Simulated alignment based on PAML's favorite b-globin tree.
slrsim.bglobin.aln <- "data/slrsim_bglobin_scores.fasta"
slrsim.bglobin.tree <- "data/slrsim_bglobin_scores.nh"

# Pfam example.
pfam.aln <- "data/PF02171_seed.fasta"
pfam.tree <- "data/PF02171_seed.nh"

###########################
# Let the plotting begin! #
###########################

pdf("example_plots.pdf",width=10,height=10)

# Plot just a tree.
sim <- PhyloSim()
readTree(sim,pax.sub.tree)
plot(sim)

# Plot just an alignment.
sim <- PhyloSim()
readAlignment(sim,pax.sub.aln)
plot(sim)

# Plot both.
sim <- PhyloSim()
readAlignment(sim,pax.sub.aln)
readTree(sim,pax.sub.tree)
plot(sim)

# Gives an error when the tree is larger than the alignment.
#sim <- PhyloSim()
#readAlignment(sim,pax.sub.aln)
#readTree(sim,pax.tree)

# Gives a warning when the alignment is larger than the tree
# (but still allow it since we might have ancestral seqs)
sim <- PhyloSim()
readAlignment(sim,pax.aln)
readTree(sim,pax.sub.tree)

# Plot the full PAX tree & alignment.
sim <- PhyloSim()
readAlignment(sim,pax.aln)
readTree(sim,pax.tree)
plot(sim,plot.chars=FALSE)

# Plot another one...
sim <- PhyloSim()
readAlignment(sim,slr.aln)
readTree(sim,slr.tree)
plot(sim,plot.chars=FALSE,plot.legend=T)

# Plot a number-encoded alignment.
sim <- PhyloSim()
readAlignment(sim,slrsim.bglobin.aln)
readTree(sim,slrsim.bglobin.tree)
plot(sim,plot.chars=FALSE,plot.legend=T)

# PFam example.
sim <- PhyloSim()
readAlignment(sim,pfam.aln)
readTree(sim,pfam.tree)
plot(sim,plot.chars=T,plot.legend=T)

# TODO: make a quick simulation and plot it too.

dev.off()
