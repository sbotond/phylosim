#!/usr/bin/env Rscript
##
## Example A1 - evolving a genomic region containing a small "gene"
## 

#
# The following code demonstrates how to simulate a genomic region
# containing a small "gene" with two exons and the flanking noncoding regions.

# Load the package:
library(phylosim)
# Enable fast mode:
PSIM_FAST<-TRUE

# Construct the alphabet objects:
nuc.alph    <- NucleotideAlphabet();
codon.alph  <- CodonAlphabet()

# Construct the root sequence object:
root.sequence<-Sequence(length=150)

# Define coding and noncoding regions:
coding.pos      <-c(31:50, 91:110)
noncoding.pos   <- (1:150)[ -coding.pos ]

# Set alphabets:
setAlphabets(root.sequence, list(nuc.alph), noncoding.pos)
setAlphabets(root.sequence, list(codon.alph), coding.pos)

# Construct the substitution processes:
k80     <-K80(rate.params=list("Alpha"=2,"Beta"=1), base.freqs=c(2, 1, 2, 1)/4)
gy94    <-GY94(kappa=2, omega.default=0.1, scale.nuc=TRUE)

# Set up indel length distribution:
id.dist<-exp(6:1)/sum(exp(6:1))

# Construct the deletion processes:
del.nc   <- DiscreteDeletor(rate=0.1, sizes=1:6, probs=id.dist )
del.c    <- DiscreteDeletor(rate=0.03, sizes=1:6, probs=id.dist  )

# Construct insertion processes:
ins.nc  <- DiscreteInsertor(rate=0.1, sizes=1:6, probs=id.dist )
ins.c   <- DiscreteInsertor(rate=0.03, sizes=1:6, probs=id.dist )

# Set the template sequences:
ins.nc$templateSeq  <- NucleotideSequence(length=1,processes=list(list( k80, del.nc, ins.nc ) ))
ins.c$templateSeq   <- CodonSequence(length=1,processes=list(list( gy94, del.c, ins.c ) ))

# Attach processes to root sequence:
setProcesses(root.sequence, list(list(k80, del.nc, ins.nc)), noncoding.pos)
setProcesses(root.sequence, list(list(gy94, del.c, ins.c)), coding.pos)

# Fix the stop codon:
start.pos   <- coding.pos[1]
setStates(root.sequence, "ATG", start.pos);                       # Set the state.
setRateMultipliers(root.sequence, gy94, 0, c(start.pos) )         # Make the site invariable.
setDeletionTolerance(root.sequence, del.c, 0, c(start.pos));      # Make the site reject deletions.
setInsertionTolerance(root.sequence,ins.c ,0, c(start.pos) );     # Make the site reject neighboring insertions.

# Construct a substitution process acting on stop codons only:
stop.alphabet   <- Alphabet(symbols=c("TAG", "TAA", "TGA"))
stop.subst      <-GeneralSubstitution(
        alphabet=stop.alphabet,
        rate.list=list("TAG->TAA"=1, 
                       "TAG->TGA"=2, 
                       "TAA->TAG"=3,
                       "TAA->TGA"=1, 
                       "TGA->TAG"=2,
                       "TGA->TAA"=3
        )
)
stop.pos    <- tail(coding.pos, 1)
root.sequence$sites[[stop.pos]]$alphabet    <- stop.alphabet    # Set alphabet for stop codon site.
setProcesses(root.sequence,list(list(stop.subst)), stop.pos)    # Set substitution process for stop codon site.

# Fix splicing sites:
splicing.sites<-c(51, 52, 89, 90);
setStates(root.sequence, c("G", "T", "A", "G"), splicing.sites) # Set site states.
setRateMultipliers(root.sequence, k80, 0, splicing.sites)       # Make sites invariable.
setDeletionTolerance(root.sequence, del.nc , 0, splicing.sites) # Make sites reject deletions.
setInsertionTolerance(root.sequence, ins.nc , 0, splicing.sites)# Make sites reject neighboring insertions.

# Sample site states:
sampleStates(root.sequence)

# Construct simulation object:
sim<-PhyloSim(
            phylo=read.tree("data/mammals.nwk"),
            root.seq=root.sequence
        )
# Run the simulation:
Simulate(sim)
# Save the resulting alignment:
saveAlignment(sim,file="example_A1.fas",skip.internal=TRUE)

