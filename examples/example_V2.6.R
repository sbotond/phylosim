#!/usr/bin/env Rscript

##
## Example V2.6: Simulating many replications
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# load PhyloSim
library("phylosim")

# Constructing Sequence objects with a large number of sites is expensive, so it is a good idea 
# to do that outside the cycle when simulating many replications with the same root sequence length.

# Reusing the root sequence object is easy, but do not forget to do the modifications needed to get 
# independent simulations (e.g. clearing the states of the root sequence, resampling the rate multipliers).

# The following code illustrates how to simulate many replications under the JC69+dG model.

# Construct the root sequence object and attach the substitution process:
p<-JC69();
root.seq<-NucleotideSequence(length=50)
attachProcess(root.seq,p)

# Read the required phylogeny from file (this will remain fixed in the simulated replications):
tree<-read.tree("data/3taxa.nwk");

# Simulate three replications. Note that the states are cleared and resampled; the rate multipliers 
# are resampled as well. The resulting aligments are stored in files aln_1.fas, aln_2.fas, aln_3.fas.
for(i in 1:3){
        cat(paste("\n\nSimulating replication ",i,"\n\n",sep=""))

        clearStates(root.seq)
        plusGamma(root.seq,p,0.25)
        sampleStates(root.seq)

        sim<-Simulate(PhyloSim(
                root.seq=root.seq,
                phylo=tree
        ))

        saveAlignment(sim,file=paste("aln_",i,".fas",sep=""))
}

