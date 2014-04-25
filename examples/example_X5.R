#!/usr/bin/env Rscript
##
## Example 3 - simulating under the ToleranceSubstitution process.
##

# load the package
library(phylosim)

# construct a GTR process object, we will use this to fill in the rates
# for the ToleranceSubstitution process.
gtr         <- GTR(
		name="MyGTR",
        	rate.params=list(
	        	"a"=1, "b"=2, "c"=3,
                	"d"=1, "e"=2, "f"=3
        	),
        	base.freqs=c(2,2,1,1)/6
	)
rate.list.gtr   <- gtr$rateList

# Construct the ToleranceSubstitution process.
p   <- ToleranceSubstitution(
    name        = "MyTolSubst",
    alphabet    = NucleotideAlphabet(),
    rate.list   = rate.list.gtr,
)

plot(p)

# construct root sequence object
s<-NucleotideSequence(length=20)

# attach process 
attachProcess(s,p)

# sample states from the equilibrium
# distribution of the attached processes
sampleStates(s)

## Set the substitution tolerance parameters for some sites:
setParameterAtSites(s, p, "substitution.tolerance",c(0,0.05,0.1,0.2,0.5,0.6,0.8,1.0),1:8)

## Plot the substitution tolerance parameters across sites:
plotParametersAtSites(s,p,"substitution.tolerance")

# Construct simulation object:
sim <-PhyloSim(root.seq=s, phylo=rtree(100))

# Run simulation:
Simulate(sim)

# Plot alignment:
plot(sim)


