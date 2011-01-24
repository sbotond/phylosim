##
## Extra example 1: plotting trees and alignments
##

library(phylosim)

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
#plot(sim)

# Plot just an alignment.
sim <- PhyloSim()
readAlignment(sim,pax.sub.aln)
plot(sim)

# Plot both.
sim <- PhyloSim()
readAlignment(sim,pax.sub.aln)
readTree(sim,pax.sub.tree)
plot(sim)

# Plot the tree and alignment with some tracks.
score.f <- function(n) {return(sin(2*pi*(1:(n))/n)/2 + 0.5)}
t1 <- data.frame(id='Simple track',layout='above',pos=1:300,score=score.f(300),color.gradient='blue,red',background='white')
print(head(t1))
t2 <- t1
t2$background <- 'lightblue'
t2$layout <- 'below'
t2$color.gradient <- 'white,pink'
t3 <- t1
t3$background <- 'lightgreen'
t3$layout <- 'below'
t3$color.gradient <- 'blue,red'
t4 <- data.frame(id='asdfasdf',layout='below',background='pink')
t5 <- data.frame(id='space!!!!',layout='above')
plot(sim,tracks=list(t5,t1,t2,t4,t3))

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
