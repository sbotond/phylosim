#!/usr/bin/env Rscript

# load PhyloSim
library("phylosim")

##
## Example V2.3.1: Simulating under the discrete gamma (GTR+dG) model
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# Construct a GTR subsection process object:
         p<-GTR(
                     rate.params=list(
                             "a"=1, "b"=2, "c"=3,
                             "d"=1, "e"=2, "f"=3
                     ),
                     base.freqs=c(2,2,1,1)/6
             )

# Summary of object p:
summary(p)

# Construct the root sequence, attach substitution process:
root.seq<-NucleotideSequence(length=50,processes=list( list(p) ))

# Sample rate multipliers from a discrete gamma distribution with 4 categories and
# shape parameter 0.5:
plusGamma(root.seq,p,0.5)

# Get the sampled rate multipliers:
getRateMultipliers(root.seq,p);

# Construct the \psim\ object, sample states and set the phylo object:
sim<-PhyloSim(
        root.seq=sampleStates(root.seq),
        phylo=read.tree("data/3taxa.nwk")
)

# Run the simulation:
Simulate(sim)

# Plot the alignment alongside the tree, skip sequences at ancestral nodes:
plot(sim,num.pages=1,plot.ancestors=FALSE)

# Save the resulting alignment, omitting sequences at internal nodes:
saveAlignment(sim,file="Gamma_sim.fas",skip.internal=TRUE)

##
## Example V2.3.2: Simulating under the invariants and discrete gamma (GTR+I+dG) model
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# We will reuse the root sequence object and GTR object from the previous example. 
# The process is already attached, but we need to clear the states to have a new root sequence.

# Clear the states of the root sequence object and sample a set of new states:
clearStates(root.seq)
sampleStates(root.seq)

# Sample rate multipliers from a +I+d$\Gamma$ model:
plusInvGamma(root.seq,p,pinv=0.8,shape=0.5)

# Deal with the rest of the simulation (note that for variety we use a random coalescent tree 
# in this example):
sim<-Simulate(
        PhyloSim(
                root.seq=root.seq,
                phylo=rcoal(3)
        )
)
plot(sim,plot.ancestors=FALSE)

# Save the resulting alignment, skip sequences from internal nodes:
saveAlignment(sim,file="InvGamma_sim.fas",skip.internal=TRUE)

