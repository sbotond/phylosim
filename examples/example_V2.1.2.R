#!/usr/bin/env Rscript

##
## Example V2.1.1: Simulating substitutions under the JC69 model - "unrolled" example
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# load PhyloSim
library("phylosim")

#Construct the root sequence object:
root.seq<-NucleotideSequence(length=50)

# Print out the root sequence:
print(root.seq)
# Note, that all states are still undefined.

# Construct a JC69 substitution process object:
p<-JC69()

# Attach the substitution process to root sequence:
attachProcess(root.seq,p)

# Sample the site states from the equilibrium distribution of the JC69
# substitution process:
sampleStates(root.seq)

#Print out the root sequence, now with the sampled states:
print(root.seq)

#Read in tree from file using APE:
tree<-read.tree("data/3taxa.nwk")

#Plot the tree:
plot(tree)

#Construct a PhyloSim object:
sim<-PhyloSim()

# Set the phylo object:
sim$phylo<-tree

# Set the root sequence:
sim$rootSeq<-root.seq

# Run the simulation:
Simulate(sim)

# Display the resulting alignment matrix:
sim$alignment

