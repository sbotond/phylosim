#!/usr/bin/env Rscript

##
## Example V2.1.1: Simulating substitutions under the HKY model
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# load PhyloSim
library("phylosim")

# Construct an HKY substitution process object:
p<-HKY(rate.params=list( "Alpha"=10,"Beta"=2),
                     base.freqs=c(4,3,2,1)/10
   )

# Get a plot of the instantaneous substitution matrix and equilibrium distribution of the p process (bubble plot):
plot(p,scale=0.5)

# Construct the root sequence, attach the substitution
# process to root sequence via the constructor and sample states:
root.len50.seq<-NucleotideSequence(length=50,processes=list(list(p)))
sampleStates(root.len50.seq)

# Print out root sequence:
print(root.len50.seq)

# Construct a PhyloSim object, set the phylo object and the root sequence:
sim<-PhyloSim(
        root.seq=root.len50.seq,
        phylo=read.tree("data/3taxa.nwk")
);

# Run simulation:
Simulate(sim)

# Save the resulting alignment (fasta format):
saveAlignment(sim,file="HKY_sim.fas")


