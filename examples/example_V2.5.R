#!/usr/bin/env Rscript

##
## Example V2.5: Simulating partitions
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# load PhyloSim
library("phylosim")

# The following example demonstrates how to use the processes and site- and process-specific
# parameters to simulate ``partitions'' with different properties.

# We will simulate four partitions:


#	* Partition 1: sites in range \code{1:25} evolving by JC+d$\Gamma$ with a shape parameter
#	  alpha=1
#
#	* Partition 2: sites in range \code{26:50} evolving by JC+d$\Gamma$ with a shape parameter 
#	  alpha=0.5
#
#	* Partition 3: sites in range \code{51:75} evolving by HKY+d$\Gamma$ with a shape parameter 
#	  alpha=1
#
#	* Partition 4: sites in range \code{76:100} evolving by HKY+d$\Gamma$ with a shape parameter 
#	  alpha=0.5

# First construct two substitution process objects:
jc69<-JC69()
hky<-HKY(rate.params=list( "Alpha"=5,"Beta"=2),
                     base.freqs=c(4,3,2,1)/10
      )

# Construct a root sequence object of length 100:
root.seq<-NucleotideSequence(length=100)

# Attach process jc69 to range 1:50:
attachProcess(root.seq,jc69,1:50)

# Attach process hky to range 51:100:
attachProcess(root.seq,hky,51:100)

# Sample rate multipliers in the four partitions:
plusGamma(root.seq,jc69,1,1:25)
plusGamma(root.seq,jc69,0.5,26:50)

plusGamma(root.seq,hky,1,51:75)
plusGamma(root.seq,hky,0.5,76:100)

# Construct the PhyloSim object, sample states, set root sequence, set the phylo object (random 
# coalescent tree for three taxa) and run the simulation:
sim<-Simulate(PhyloSim(
        root.seq=sampleStates(root.seq),
        phylo=rcoal(3)
))

# Plot the alignment alongside the tree, skip sequences at ancestral nodes:
plot(sim,plot.ancestors=FALSE)

