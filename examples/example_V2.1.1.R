#!/usr/bin/env Rscript

##
## Example V2.1.1: Simulating substitutions under the JC69 model - compact example
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# load PhyloSim
library("phylosim")

# Run the simulation, display the alignment matrix: 
Simulate(PhyloSim(
        root.seq=sampleStates( NucleotideSequence(len=50,proc=list(list(JC69())) )),
        phylo=read.tree("data/3taxa.nwk")
))$alignment

