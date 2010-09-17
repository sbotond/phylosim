#!/usr/bin/env Rscript


##
## Example 3 - simulating rate variation,
## insertions and deletions.
##

#	# Construct a phylo object for the
#	# simulation, scale total tree length to 2:
#
tmp<-PhyloSim(phylo=rcoal(3))
scaleTree(tmp,2/tmp$treeLength)
tmp$treeLength
t<-tmp$phylo

# construct a GTR process object
gtr<-GTR(
		name="MyGTR",
        	rate.params=list(
	        	"a"=1, "b"=2, "c"=3,
                	"d"=1, "e"=2, "f"=3
        	),
        	base.freqs=c(2,2,1,1)/6
	)

# get object summary
summary(gtr)

# get a bubble plot
plot(gtr)

# construct root sequence object
s<-NucleotideSequence(length=30)

# attach process via virtual field
s$processes<-list(list(gtr))

# sample states from the equilibrium
# distribution of the attached processes

sampleStates(s)

# create among-sites rate variation by sampling
# the "rate.multiplier" site-process specific parameter
# from a discrete gamma distribution (F84+G).
plusGamma(s,gtr,shape=0.5)

# make the range 11:20 invariable
setRateMultipliers(s,gtr,0,11:20)

# get the rate multipliers for s and gtr
getRateMultipliers(s,gtr)

# construct deletion process object
# proposing length in range 1:3
d<-DiscreteDeletor(
	rate=1,
	name="MyDel",
	sizes=c(1:3),
	probs=c(3/6,2/6,1/6)
	)
# get object 
summary(d)

# plot deletion length distribution
plot(d)

# attach d to s
attachProcess(s,d)

# create a region rejecting all deletions
setDeletionTolerance(s,d,0,11:20)

# construct insertion process object
# proposing length in range 1:3
i<-DiscreteInsertor(
	rate=1,
	name="MyDel",
	sizes=c(1:2),
	probs=c(1/2,1/2),
	template.seq=NucleotideSequence(length=1,processes=list(list(JC69())))
	) 

# states will be sampled from the JC69 equilibrium distribution
# get object 
summary(i)

# plot insertion length distribution
plot(i)

# attach i to s
attachProcess(s,i)

# create a region rejecting all insertions
setInsertionTolerance(s,i,0,11:20)

# plot total site rates
plot(s)

# construct simulation object
sim<-PhyloSim(root.seq=s, phylo=t)

# get object summary
summary(sim)

# plot tree
plot(sim)

# run simulation
Simulate(sim)

# get the list of recorded per-branch event counts
getBranchEvents(sim)

# export the number of subtitions as a phylo object
subst<-exportStatTree(sim,"substitution")
subst

# plot the exported phylo object
plot(subst)

# plot tree and alignment
plot(sim)
# save and display alingment
file<-paste("PhyloSim_fasta_",Sys.getpid(),".fas",sep="");
saveAlignment(sim,file=file,paranoid=TRUE);
