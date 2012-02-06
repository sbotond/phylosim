#!/usr/bin/env Rscript

# load PhyloSim
library("phylosim")

##
## Example X4: Simulating "nested transposon insertions" (suggested by Albert Vilella).
##
##

# Set up the substitution process and the root sequence:
p<-JC69()
root.seq<-NucleotideSequence(len=1000,processes=list(list(p)))
sampleStates(root.seq)

# Construct a deletion process proposing deletions with rate 0.25 according to a discrete length
# distribution:
 d<-DiscreteDeletor(
                     rate=0,
                     sizes=c(1,2),
                     probs=c(1/2,1/2)
   )

# Construct an insertion process proposing insertions with rate 0.25 according to a discrete length
# distribution:
 i<-DiscreteInsertor(
                     rate=0,
                     sizes=c(1,2),
                     probs=c(1/2,1/2)
   )

# Set the template sequence for the insertion process:
tmp<-NucleotideSequence(length=2,processes=list( list(p,d,i) ))
i$writeProtected<-FALSE
i$templateSeq<-tmp

# The states of the template sequence are undefined.
# The actual states will be sampled from the equilibrium distribution of the attached substitution 
# process before performing the insertion.

# Attaching the indel processes:
attachProcess(root.seq,d)
attachProcess(root.seq,i)

# Set up a special "transposon insertion" process:

# First, create a template sequence (transposon):

ts<-NucleotideSequence(length=50)

# Randomly fix a couple of sites:

setStates(
    ts,
    sample(c('A','T','G','C'), 15, replace=TRUE),
    sample(1:50, 15, replace=FALSE)
)

# Fix the start and the end:
setStates(ts,c('G','A','C','T','G','C'),1:6)
setStates(ts,c('G','C','A','G','T','C'),45:50)

# The rest of the sites have undefined states, they are sampled before insertion:
print(ts)

# Create a special insertion process:
tri<-DiscreteInsertor(rate=1,sizes=ts$length,probs=1);

# Define the "background" and "nested" rates: 
# Be careful with these rates! If they are too high
# the sequences will be plagued by transposons!
background.rate <- 0.001
nested.rate     <- 0.1

# Attach the processes to the template sequence:
ts$processes<-list(list(p,i,d,tri))

# Disable substitutions in inserted sequences:
setRateMultipliers(ts,p,0)

# Tweak the insertion rate for transposon insertion:
setRateMultipliers(ts, tri, nested.rate * c(1/25:1,1/1:25) )
# Now the nested insertions are more frequent around the middle:
plotParametersAtSites(ts,tri,"rate.multiplier")

# Disable write protection and set template sequence:
tri$writeProtected  <- FALSE
tri$templateSeq     <- ts

# Attach transposon insertion process to root sequence, set the
# "background" insertion rate.
attachProcess(root.seq, tri)
setRateMultipliers(root.seq, tri, background.rate)

# Construct the PhyloSim object, set the phylo object (random coalescent tree) and run the simulation:
PSIM_FAST<-TRUE

sim<-PhyloSim(
        root.seq=root.seq,
        phylo=rcoal(8)
)

Simulate(sim)

# Plot the alignment alongside the tree:
plot(sim,plot.ancestors=FALSE)

# Save the resulting alignment, skip sequences from internal nodes:
saveAlignment(sim,file="example_X4.fas",skip.internal=TRUE)


