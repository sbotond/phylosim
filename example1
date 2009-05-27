#!/usr/bin/env Rscript
library(R.oo)
library(ape)
source("./FullSource.R");
####################################################
#
# Example script file showing how to set up a very simple simulation:
#

#
# Setting up the processes:
#

wag<-WAG();	# Create a WAG substitution process.

summary(wag); # See some info about the subsitution process.

seq<-AminoAcidSequence(length=100); # Create a sequence of length 100.

attachProcess(seq,wag);

# Create rate variation among the sites of seq by the +I+G model.
plusInvGamma(
					seq,					# the sequence
					process=wag,  # the substitution process
					pinv=0.3,			# the proportion of invariant sites
					shape=0.5			# gamma shape parameter
				);

# Sample the states from the attached substitution process(es):
sampleStates(seq);

# Plot the "rate landscape":
plot(seq);

# Read in the tree using APE:
tree<-read.tree(
		file="smalldemotree.nwk"	# the path to the tree file
	);

# Create the simulation object:
sim<-PhyloSim(
			phylo=tree,		# the tree as an APE phylo object
			root.seq=seq	# the root sequence.
		);

plot(sim);	# Plot the tree.

print(sim$treeLength) # Print the tree length

summary(sim)	# Summary for the simulation object.

ll(sim)	# Methods in the PhyloSim class.

# Run the simulation:
Simulate(sim);

# And finally save the resulting alignment:
saveAlignment(
		sim,			# the phylo object	
		file="example1_aln.fas"		# filename for alignment
	);

