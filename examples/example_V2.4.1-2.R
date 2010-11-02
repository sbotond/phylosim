#!/usr/bin/env Rscript

# load PhyloSim
library("phylosim")

##
## Example V2.4.1: Simulating indels - Insertion and deletions having the same length distribution
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# Set up the substitution process and the root sequence:
p<-JC69()
root.seq<-NucleotideSequence(len=50,processes=list(list(p)))
sampleStates(root.seq)

# Construct a deletion process proposing deletions with rate 0.25 according to a discrete length
# distribution:
 d<-DiscreteDeletor(
                     rate=0.25,
                     sizes=c(1,2),
                     probs=c(1/2,1/2)
   )

# Construct an insertion process proposing insertions with rate 0.25 according to a discrete length
# distribution:
 i<-DiscreteInsertor(
                     rate=0.25,
                     sizes=c(1,2),
                     probs=c(1/2,1/2)
   )

# Set the template sequence for the insertion process:
i$templateSeq<-NucleotideSequence(length=2,processes=list( list(p) ))

# The states of the template sequence are undefined.
# The actual states will be sampled from the equilibrium distribution of the attached substitution 
# process before performing the insertion.

# Attaching the indel processes:
attachProcess(root.seq,d)
attachProcess(root.seq,i)

# Construct the PhyloSim object, set the phylo object (random coalescent tree for three taxa) and run the simulation:
sim<-Simulate(PhyloSim(
        root.seq=root.seq,
        phylo=rcoal(3)
))

# Plot the alignment alongside the tree, skip sequences at ancestral nodes:
plot(sim,num.pages=1,plot.ancestors=FALSE)

# Save the resulting alignment, skip sequences from internal nodes:
saveAlignment(sim,file="indel1_sim.fas",skip.internal=TRUE)

##
## Example V2.4.2: Simulating indels - Insertion and deletions having different length distributions
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# The following code reuses the objects constructed in the previous example to simulate
# deletions and insertions with different length distributions.

# Construct a new insertion process proposing insertions of Cs with rate 0.5 according to a 
# discrete length distribution:
 i2<-DiscreteInsertor(
                     rate=0.25,
                     sizes=c(1,2,3,4),
                     probs=c(1,2,3,4)/10,
                     template.seq=NucleotideSequence(string="C")
   )

# Clear the states of the root sequence object:
clearStates(root.seq)

# Define a new set of processes (p - substitution, d - deletion, i2 -
# the new insertion process) for the root sequence using the processes virtual field:
root.seq$processes<-list(list(i2, d, p))

# Construct the PhyloSim object, sample states, set the phylo object and run the simulation:
sim<-Simulate(PhyloSim(
        root.seq=sampleStates(root.seq),
        phylo=read.tree("data/3taxa.nwk")
))

# Plot the alignment alongside the tree, skip sequences at ancestral nodes:
plot(sim,plot.ancestors=FALSE)

# Save the resulting alignment, skip sequences from internal nodes:
saveAlignment(sim,file="indel2_sim.fas",skip.internal=TRUE)

