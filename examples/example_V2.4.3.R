#!/usr/bin/env Rscript

# load PhyloSim
library("phylosim")

##
## Example V2.4.3: Simulating indels under selective constraints
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# Set up the substitution process and the root sequence:
p<-JC69()
root.seq<-NucleotideSequence(len=100,processes=list(list(p)))
sampleStates(root.seq)

# Construct a deletion process proposing deletions with rate 1 according to a discrete length
# distribution:
 d<-DiscreteDeletor(
                     rate=1,
                     sizes=c(1,2,3),
                     probs=c(3/6,2/6,1/6)
   )

# Construct an insertion process proposing insertions with rate 1 according to a discrete length
# distribution:
 i<-DiscreteInsertor(
                     rate=1,
                     sizes=c(1,2,3),
                     probs=c(3/6,2/6,1/6)
   )

# Set the template sequence for the insertion process:
i$templateSeq<-NucleotideSequence(length=2,processes=list( list(p) ))

# Attaching the indel processes:
attachProcess(root.seq,d)
attachProcess(root.seq,i)

# Set deletion tolerance values:
setDeletionTolerance(root.seq,d,0.08 + c(1/2:51,1/51:2))

# Plot deletion tolerance values:
plotParametersAtSites(root.seq,d,"deletion.tolerance")

# Set insertion tolerance values:
setInsertionTolerance(root.seq,i,0.08 + c(1/2:51,1/51:2))

# Construct the PhyloSim object, set the phylo object (random coalescent tree for four taxa) and run the simulation:
sim<-Simulate(PhyloSim(
        root.seq=root.seq,
        phylo=read.tree("data/4taxa.nwk")
))

# Plot the alignment alongside the tree, skip sequences at ancestral nodes:
plot(sim,plot.ancestors=FALSE)

# Save the resulting alignment, skip sequences from internal nodes:
saveAlignment(sim,file="indel1_sim.fas",skip.internal=TRUE)

