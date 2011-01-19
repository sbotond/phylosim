#!/usr/bin/env Rscript

##
## Example V3.1: Simulating amino acid sequences with "domains" and heterogeneous evolution.
## See also the package vignette (vignette("PhyloSim",package="phylosim")).
##

#
# Setting up the substitution processes:
#

# load PhyloSim
library("phylosim");

# Use the ll() method to list the methods and virtual fields implemented in the Sequence class:
ll(Sequence())

# Enable the "fast & careless mode":
PSIM_FAST <- TRUE

wag<-WAG();	# Create a WAG substitution process.
jtt<-JTT();	# Create a JTT substitution process.
lg<-LG();	# Create a LG substitution process.
pam<-PAM();	# Create a PAM substitution process.

# Get an object summary for wag:
summary(wag);

# Get a bubble plot of wag:
plot(wag);

# Create a continous deletor process:
cont.del<-ContinuousDeletor(
			rate=0.5,	# global rate for this deletion process
			max.length=10,	# the maximum allowed deletion length
			dist=expression(rnorm(1,mean=5,sd=3))	# length sampling expression
		);

# Creating the template sequence for the insertion process:
templ.seq.wag<-AminoAcidSequence(length=10); # this is just a sequence with length 10.

# Note that the template sequence state is undefined, so the states
# will be sampled from the equlibrium distribution of the substitution process(es). 

# Clone the template sequence:
templ.seq.lg<-clone(templ.seq.wag);

# Create a continous insertor process object:
cont.ins.wag<-ContinuousInsertor(
			rate=0.5,	# global rate for this insertion process
			max.length=10,	# the maximum allowed insertion length
			dist=expression(rnorm(1,mean=5,sd=3))	# length sampling expression
		);

# Create a continous insertor process object:
cont.ins.lg<-ContinuousInsertor(
			rate=0.005,	# global rate for this insertion process
			max.length=10,	# the maximum allowed insertion length
			dist=expression(rnorm(1,mean=5,sd=3)) # length sampling expression
		);

# Setting up the template sequences for the insertion processes:
templ.seq.wag$processes<-list(list(wag,cont.ins.wag,cont.del));
templ.seq.lg$processes<-list(list(lg,cont.ins.lg,cont.del));

# Disabling write protection for the insertion processes:
cont.ins.wag$writeProtected<-FALSE;
cont.ins.lg$writeProtected<-FALSE;

# Setting the template sequence for the insertion processes:
cont.ins.wag$templateSeq<-templ.seq.wag;
cont.ins.lg$templateSeq<-templ.seq.lg;

# Setting up the insert hook for the insertion processes: 
cont.ins.wag$insertHook<-function(seq,target.seq,event.pos,insert.pos){
	# Create rate variation among the sites of seq by the +I+G model.
	plusInvGamma(
			seq,		# the sequence object
			process=wag, 	# the substitution process
			pinv=0.4,    	# the proportion of invariant sites.
			shape=0.6	# gamma shape parameter
			);
	return(seq);
	
}

cont.ins.lg$insertHook<-function(seq,target.seq,event.pos,insert.pos){
	# Create rate variation among the sites of seq by the +I+G model.
	plusInvGamma(
			seq,		# the sequence
			process=lg, 	# the substitution process
			pinv=0.4,	# the proportion of invariant sites.
			shape=0.6	# gamma shape parameter
		);
	return(seq);
	
}

#
# Setting up the root sequence:
#

seq<-AminoAcidSequence(length=60); # Create a sequence of length 200.

# Create the process pattern:
process.pattern<-c(
		rep(list(list(wag,cont.del, cont.ins.wag)), times=20),	# Left linker model: WAG
		rep(list(list(jtt)), times=20),				# "Core" model: JTT
		rep(list(list(lg,cont.del, cont.ins.lg)), times=20)	# Right linker model: LG
);

# Apply the process pattern to the root sequence:
seq$processes<-process.pattern;

# Set up site specific rates:

# Iterate over sites:
for (i in 1:seq$length){
	# Set a low rate for the core sites:
	if(isAttached(seq$sites[[i]],jtt)){
		# Sample rate from a truncated normal distribution.
		while( (site.rate<-rnorm(1,mean=0.001,sd=0.01)) < 0 ){} 
		# Set the rate multiplier.
		setRateMultipliers(seq,jtt,site.rate,index=i); 
	}
	else if(isAttached(seq$sites[[i]],wag)){
		
  	plusInvGamma(
            seq,          	# the sequence
            process=wag,   	# the substitution process
            pinv=0.4,     	# the proportion of invariant sites.
            shape=0.6,     	# gamma shape parameter.
	    index=i		# index vector
          );

	}
	else if(isAttached(seq$sites[[i]],lg)){
		
  	plusInvGamma(
		seq,		# the sequence
		process=lg,	# the substitution process
		pinv=0.4,	# the proportion of invariant sites.
		shape=0.6,	# gamma shape parameter.
		index=i		# index vector
          );

	}
}

# Sample the states from the attached substitution process(es):
sampleStates(seq);

print(seq); # Print the actual sequence.

plot(seq);  # Plot the "rate landscape".

# Read in the tree using APE:
tree<-read.tree(
		file="data/smalldemotree.nwk"	# the path to the tree file
	);

# Create the simulation object:
sim<-PhyloSim(
			phylo=tree,	# the tree as an APE phylo object
			root.seq=seq	# the root sequence.
		);

# Create a node hook function:
node.hook<-function(seq){
	for (site in seq$sites){
		if(isAttached(site,jtt)){
			attachProcess(site,pam); # Attach the PAM process to the core sites. 
		}
	}
	return(seq);
}

# Attach the hook to node 8:
attachHookToNode(
		sim,		# PhyloSim object.
		node=8,		# the node
		fun=node.hook	# the node hook function
		);

# Run the simulation:
Simulate(sim)

# Plot the resulting alingment alongside the tree:
plot(sim)

# Save the resulting alignment, skip internal nodes:
saveAlignment(
		sim,				# the phylo object	
		file="example_V3.1_aln.fas",
		skip.internal=TRUE		# filename for alignment
);

# Disable fast mode:
rm(PSIM_FAST)
