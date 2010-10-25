#!/usr/bin/env Rscript

##
## Example V3.4 - evolving a genomic region containing a "gene"
## 
##

# load the package
library(phylosim)

#
# The following code demonstrates how to simulate a genomic region
# containing a gene and noncoding regions.
#
# We will simulate the following structures:
#
#	* NC1 (length: 1000) - noncoding region 1, evolving under  a GTR substitution model
#	  deletions and insertions of Cs.
#
#	* E1 (length: 400) - exon 1, evolving under a nucleotide-scaled GY94 codon model and small indels.
#
#	* I1-5 (length: 200) - introns 1-5, evoling under a F84 substitution model and indels.
#
#	* E2-5 (length: 200) - exons 1-5, evolving under a nucleotide-scaled GY94 codon model
#
#	* E6 (length: 400)   - exon 6, evolving under a nucleotide-scaled GY94 codon model and small indels.
#
#	* NC2 (length: 1400) - noncoding region 2, evolving under a K80 substitution model
#	
#	* Invariable start codon and splice sites.
#
#	* A special substitution process for stop codons.
#	
#	The total length of the Sequence object is 5000. 
#	We will simulate the heterogeneity of nucleotide substitution rates, indel rates
# 	and omega ratios.
#	

# enable fast & careless mode
PSIM_FAST<-TRUE;

# Create a list holding partition information:

part<-list();

# Partition NC1:
part$nc1<-list(
		"type"="noncoding",
		"len"=1000,
		"subst"=NA,
		"ins"=NA,
		"del"=NA,
		"gamma.shape"=0.5
	);

part$nc1$subst<-GTR(
        rate.params=list(
                "a"=1, "b"=2, "c"=3,
                "d"=1, "e"=2, "f"=3
        ),
        base.freqs=c(1.5,1,1.5,1)/5
)

part$nc1$inc<-DiscreteInsertor(
	rate=0.5,sizes=1:6,
	probs=6:1/21,
	template.seq=NucleotideSequence(string="CCCCCC")
);

part$nc1$del<-DiscreteDeletor(
        rate=0.5,
        sizes=1:6,
        probs=6:1/21
);


# Partition E1:
part$e1<-list(
		"type"="first.exon",
		"len"=400,
		"subst"=NA,
		"ins"=NA,
		"del"=NA,
		"omegas"=c(0,1,1.1),
		"omega.probs"=c(4,2,1)/5
	);

part$e1$subst<-GY94(kappa=2,scale.nuc=TRUE);

part$e1$ins<-DiscreteInsertor(
        rate=0.05,
        sizes=1:2,
        probs=c(2/3,1/3),
        template.seq=CodonSequence(length=2,processes=list(list(part$e1$subst)))
);


part$e1$del<-DiscreteDeletor(
        rate=0.05,
        sizes=1:2,
        probs=c(2/3,1/3)
);


f84<-F84();
f84$kappa<-1.8;

gy94.e2_5<-clone(part$e1$subst);
gy94.e2_5$kappa<-1.5

# Partitions E2-5 and I1-4:
del.introns<-DiscreteDeletor(
        rate=0.05,
	sizes=1:8,
        probs=8:1/36
);

ins.introns<-DiscreteInsertor(
        rate=0.05,
        sizes=1:8,
        probs=8:1/36,
        template.seq=NucleotideSequence(length=8,processes=list(list(f84)))
);

for(i in 1:4){
	part[[paste("i",i,sep="")]]<-list(
		"type"="intron",
		"len"=200,
		"subst"=f84,
		"ins"=ins.introns,
		"del"=del.introns,
		"gamma.shape"=abs(rnorm(1,mean=1,sd=0.2))
	)
	part[[paste("e",i+1,sep="")]]<-list(
		"type"="exon",
		"len"=200,
		"subst"=gy94.e2_5,
		"ins"=NA,
		"del"=NA,
		"omegas"=c(0,1),
		"omega.probs"=c(3/4,1/4)
	)

}

# Partition I5:
part$i5<-list(
		"type"="intron",
                "len"=200,
                "subst"=f84,
                "ins"=ins.introns,
                "del"=del.introns,
                "gamma.shape"=abs(rnorm(1,mean=1,sd=0.1))
)


rm(f84,gy94.e2_5,del.introns,ins.introns);

# Partition E6:
part$e6<-list(
		"type"="last.exon",
		"len"=400,
		"subst"=NA,
		"ins"=NA,
		"del"=NA,
		"omegas"=c(0,1,1.1),
		"omega.probs"=c(4,2,1)/5
);

tmp<-clone(part$e1$subst);
tmp$kappa<-0.5;
part$e6$subst<-tmp;

part$e6$del<-DiscreteDeletor(
        rate=0.5,
        sizes=1:4,
        probs=c(4,3,2,1)/10
);

part$e6$ins<-DiscreteInsertor(
        rate=0.5,
        sizes=1:2,
        probs=c(1/3,2/3),
        template.seq=CodonSequence(length=2,processes=list(list(part$e6$subst)))
);

# Partition NC2:
part$nc2<-list(
		"type"="noncoding",
		"len"=1400,
		"subst"=K80(rate.params=list(Alpha = 2, Beta = 1)),
		"ins"=NA,
		"del"=NA,
		"gamma.shape"=0.5
	);


# Construct root sequence object:
s<-Sequence(length=5000);

# Construct Alphabet objects:
nuc<-NucleotideAlphabet();
cod<-CodonAlphabet();

# Construct an Alphabet object containing the stop codons:
stop.alphabet<-Alphabet(symbols=c("TAG","TAA","TGA"));

# Construct a substitution process acting on stop codons only:
stop.subst<-GeneralSubstitution(
		alphabet=stop.alphabet,
		rate.list=list(
				"TAG->TAA"=1,
				"TAG->TGA"=2,
				"TAA->TAG"=3,
				"TAA->TGA"=1,
				"TGA->TAG"=2,
				"TGA->TAA"=3
			)
		);

# Get a bubble plot of stop.subst:
plot(stop.subst,scale=0.5);

# Iterate over partitions, set up processes:

pos<-0;
for(i in part){
	beg<-pos+1;	
	end<-pos+i$len;	
	range<-beg:end;


	if( (i$type=="noncoding") | (i$type=="intron") ){
		setAlphabets(s,list(nuc),range);
	}
	else {
		setAlphabets(s,list(cod),range);
	}
	
	if(!is.na(i$del)){
		attachProcess(s,i$del,range);
	}
	if(!is.na(i$ins)){
		attachProcess(s,i$ins,range);
	}

	if( (i$type=="noncoding") | (i$type=="intron")){
		attachProcess(s,i$subst,range);

		plusInvGamma(this=s,process=i$subst,pinv=0.6,shape=i$gamma.shape,range);
		if(i$type=="intron"){
			# fix splicing sites
			setStates(s,c("G","T","A","G"),c(beg,beg+1,end-1,end));
			setRateMultipliers(s,i$subst,0,c(beg,beg+1,end-1,end));
			setInsertionTolerance(s,i$ins,0,c(beg,beg+1,end-1,end));
			setDeletionTolerance(s,i$del,0,c(beg,beg+1,end-1,end));
		}
	} 
	else if(i$type=="exon"){
		attachProcess(s,i$subst,range);

		omegaVarM3.CodonSequence(s,i$subst,i$omegas,i$omega.probs,range);
	}
	if(i$type=="first.exon"){
		attachProcess(s,i$subst,range);
		# Fix start codon:
		setStates(s,"ATG",beg);
		setRateMultipliers(s,i$subst,0,beg)
		setInsertionTolerance(s,i$ins,0,beg);
		setDeletionTolerance(s,i$del,0,beg);
	}
	if(i$type=="last.exon"){
		attachProcess(s,i$subst,range);
		# Detach GY94 from last site:
		detachProcess(s,i$subst,end);
		# Replace alphabet:
		setAlphabets(s,list(stop.alphabet),end);
		# Attach stop codon process:
		attachProcess(s,stop.subst,end);
		# Sample stop codon:
		sampleStates(s,end);
		# Protect against indels:
		setInsertionTolerance(s,i$ins,0,end);
		setDeletionTolerance(s,i$del,0,end);
	}
	
	pos<-end;
}

# Sample remaining site states:
sampleStates(s);

# Construct simulation object
sim<-PhyloSim(root.seq=s, phylo=rcoal(3));

# Scale tree length to 0.2:
scaleTree(sim,0.2/sim$treeLength);

# Run simulation
Simulate(sim)

# Plot tree and alignment
plot(sim)
# Save alingment
saveAlignment(sim,file="example_V4.fas");

# Disable fast & careless mode
rm(PSIM_FAST)

