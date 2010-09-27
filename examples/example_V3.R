#!/usr/bin/env Rscript

##
## Example V3 - implementing an inverted duplication process
## 
##

# load the package
library(phylosim)

# enable fast & careless mode
PSIM_FAST<-TRUE;

# construct a DiscreteInsertor process
ivd<-DiscreteInsertor(rate=0.04,sizes=c(4,6),probs=c(2/3,1/3));

# set template sequence just to make the process object happy:
ivd$templateSeq<-NucleotideSequence(length=1);

# Replace the function object stored in the 
# generateBy virtual field. See the documentation of the 
# GeneralInsertor class.
ivd$generateBy<-function(process=NA,length=NA,target.seq=NA,event.pos=NA,insert.pos=NA){
	# get the target sequence length
	target.length<-target.seq$length;
	# construct a vector with the positions to copy:
	positions<-(insert.pos+1):(insert.pos + length)
	# discard illegal positions:
	positions<-positions[ positions > 0 & positions <= target.length];
	# copy subsequence
	insert<-copySubSequence(target.seq,positions,process);
	# reverse complement sequence,
	# take care, the class of this objects is "Sequence":
	revComp.NucleotideSequence(insert);
	# do not allow nested insertions:
	setRateMultipliers(insert,ivd,0);
	# return insert	
	return(insert);
}

# Now we have a process which perfroms inverted duplications.

# construct a JC69 process object
p<-JC69();

# construct root sequence object
s<-NucleotideSequence(length=50)

# attach processes via virtual field
s$processes<-list(list(p,ivd))

# sample states from the equilibrium
# distribution of the attached processes

sampleStates(s)
# detach the substitution process:
detachProcess(s,p)

# create among-sites rate variation for the inverted duplication
# process by sampling rate multipliers from an I+G model:
plusGamma(s,ivd,pinv=0.5,shape=0.5)

# construct simulation object
sim<-PhyloSim(root.seq=s, phylo=read.tree("smalldemotree.nwk"));

# run simulation
Simulate(sim)

# plot tree and alignment
plot(sim)
# save alingment
saveAlignment(sim,file="example_V3.fas");

# disable fast & careless mode
rm(PSIM_FAST)

