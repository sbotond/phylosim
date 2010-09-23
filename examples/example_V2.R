#!/usr/bin/env Rscript

##
## Example V2: Evolving codon sequences
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

# Enable "fast & careless" mode:

PSIM_FAST<-TRUE;

# Construct a GY94 codon substitution model:
p<-GY94();

# Set the transition/transverion rate ratio:
p$kappa=2

# Sample codon frequencies from a normal distribution:
p$equDist<-abs(rnorm(61,mean=10,sd=3))

# Get object summary for p:
summary(p)

# Construct a discrete deletor process:
d<-DiscreteDeletor(
	rate=0.025,
	sizes=1:4,
	probs=c(4,3,2,1)	
);

# Construct a discrete insertor process inserting neutrally evolving sites:
i<-DiscreteInsertor(
	rate=0.04,
	sizes=1:4,
	probs=c(4,3,2,1),
	template.seq=CodonSequence(length=4,processes=list(list(p)))
);

# Construct root sequence and attach process p:

s<-CodonSequence(length=100,processes=list(list(p)))

# Sample omegas from a discrete model:
omegaVarM3(s,p,omegas=c(0,1,2),probs=c(2/4,1/4,1/4))

# Plot the omega values across sites:
plotParametersAtSites(s,p,"omega");

# Sample states:

sampleStates(s)

# Construct the simulation object:
sim<-PhyloSim(
	root.seq=s,
	phylo=read.tree("smalldemotree.nwk")
);

# Create a node hook function and attach to node 9:
node.hook<-function(seq){

	# Set all omegas to 1 (neutral):
	setOmegas(seq,p,1);
	# attach the deletion process:
	attachProcess(seq,d)
	# attach the insertion process:
	attachProcess(seq,i)

        return(seq);
}

attachHookToNode(
                sim,                    # PhyloSim object.
                node=9,                 # the node
                fun=node.hook           # the node hook function
);

# Run the simulation:
Simulate(sim)

# Plot the resulting alingment alongside the tree:
plot(sim);

# Save the resulting alignment:
saveAlignment(
                sim,                            # the phylo object      
                file="example_V2_aln.fas",      # filename for alignment
);


# Disable "fast & careless" mode:
rm(PSIM_FAST)

