#!/usr/bin/env Rscript

##
## Example V4 - evolving a genomic region containing a "gene"
## 
##

# load the package
library(phylosim)

# The following code demonstrates how to simulate a genomic region
# containing a gene and noncoding regions.
#
# We will simulate the following structures:
#
#	* NC1 (length: 1000) - noncoding region 1, evolving under  a GTR substitution model
#	  deletions, and inverted duplications.
#
#	* E1 (length: 400) - exon 1, evolving under a nucleotide-scaled GY94 codon model and small indels
#
#	* I1-6 (length: 200) - introns 1-5, evoling under a F84 substitution model and indels
#
#	* E2-5 (length: 200) - exons 1-5, evolving under a nucleotide-scaled GY94 codon model
#
#	* E6 (length: 400)   - exon 6, evolving under a nucleotide-scaled GY94 codon model and small indels
#
#	* NC2 (length: 1200) - noncoding region 2, evolving under a K80 substitution model
#	
#	* Invariable start codon, stop codon and splice sites.
#	
#	The total sequence length is 5000. 
#	We will simulate the heterogeneity of nucleotide substitution rates, indel rates
# 	and omega ratios.
#	

# enable fast & careless mode
PSIM_FAST<-TRUE;

# Construct substitution process objects:
gtr<-GTR(
	rate.params=list(
		"a"=1, "b"=2, "c"=3,
		"d"=1, "e"=2, "f"=3
	),
	base.freqs=c(1.5,1,1.5,1)/5
)

f84<-F84();
f84$kappa<-1.8;

gy94.e1<-GY94(kappa=2);
gy94.e6<-clone(gy94.e1);
gy94.e6$kappa<-0.5
gy94.e2_5<-clone(gy94.e1);
gy94.e2_5$kappa<-1.5

# Construct deletion process objects:
del.nc1<-DiscreteDeletor(
        rate=0.08,
        sizes=c(4,6),
        probs=c(2/3,1/3)
);

del.e1<-DiscreteDeletor(
        rate=0.08,
	sizes=1:2,
        probs=c(2/3,1/3)
);
del.e6<-DiscreteDeletor(
        rate=0.08,
        sizes=1:4,
        probs=c(4,3,2,1)/10
);

del.introns<-DiscreteDeletor(
        rate=0.08,
	sizes=1:8,
        probs=8:1/36
);

# Construct insertion process objects:  
ins.nc1<-DiscreteInsertor(rate=0.08,sizes=c(4,6),probs=c(2/3,1/3),template.seq=NucleotideSequence(length=1));

# Replace the function object stored in the 
# generateBy virtual field. See the documentation of the 
# GeneralInsertor class.
ins.nc1$generateBy<-function(process=NA,length=NA,target.seq=NA,event.pos=NA,insert.pos=NA){
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

ins.e1<-DiscreteInsertor(
        rate=0.08,
        sizes=1:2,
        probs=c(2/3,1/3),
        template.seq=CodonSequence(length=2,processes=list(list(gy94.e1)))
);
ins.e6<-DiscreteInsertor(
        rate=0.08,
        sizes=1:2,
        probs=c(1/3,2/3),
        template.seq=CodonSequence(length=2,processes=list(list(gy94.e6)))
);
ins.introns<-DiscreteInsertor(
        rate=0.08,
        sizes=1:8,
        probs=8:1/36,
        template.seq=NucleotideSequence(length=8,processes=list(list(f84)))
);

#

# Construct root sequence object:
s<-Sequence(length=1000);

# Construct simulation object
sim<-PhyloSim(root.seq=s, phylo=read.tree("smalldemotree.nwk"));

# Run simulation
Simulate(sim)

# Plot tree and alignment
plot(sim)
# Save alingment
saveAlignment(sim,file="example_V3.fas");

# Disable fast & careless mode
rm(PSIM_FAST)

