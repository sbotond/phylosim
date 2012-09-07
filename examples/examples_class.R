#!/usr/bin/env Rscript

##
## Full set of examples shipped before with the documentation of the
## PhyloSim constructor.
## insertions and deletions.
##

# load the package
library(phylosim)

# The following examples demonstrate
	## the typical use of the framework.
	## See the package vignette and
	## \url{http://github.com/sbotond/phylosim/tree/master/examples/}

	## The ll() method gives information about the methods defined
	## in the immediate class of an object.
	## Useful when exploring the framework.

	s<-Sequence()
	ll(s)
	ll(PhyloSim())
	ll(GTR())

	## Example 1 - A short simulation:
	## simulate nucleotide seqeunces and display 
	## the resulting alignment matrix.

	Simulate(
		PhyloSim(phy=rcoal(3),root=NucleotideSequence(string="ATGC", proc=list(list(JC69())) ) )
	)$alignment

	# Construct a phylo object for the following
	# simulations, scale total tree length to 1:

	tmp<-PhyloSim(phylo=rcoal(3))
	scaleTree(tmp,1/tmp$treeLength)
	tmp$treeLength
	t<-tmp$phylo

	## Example 2 - An "unrolled" example:
	## simulate sequences of binary characters.

	# construct root sequence object
	s<-BinarySequence(string="000000")
	# construct a substitution process object
	p<-BinarySubst(rate.list=list("0->1"=1,"1->0"=0.5))
	# display a bubble plot
	plot(p)
	# attach process to sequence
	attachProcess(s,p)
	# construct simulation object
	sim<-PhyloSim(root.seq=s, phylo=t)
	# run simulation
	Simulate(sim)
	# display alignment matrix
	sim$alignment
	# plot tree and alignment
	plot(sim)

	## Check the consistency of object p
	print(checkConsistency(p))

	## Check the consistency of all PhyloSim
	## related objects.
	PSRoot$globalConsistencyCheck();

	## Example 3 - simulating rate variation,
	## insertions and deletions.
	## See the examples/example_3_clean.R file
	## in the phylosim GitHub repository.

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
	# display a bubble plot
	plot(gtr)

	# construct root sequence object
	s<-NucleotideSequence(length=40)
	# attach process via virtual field
	s$processes<-list(list(gtr))
	# sample states from the equilibrium
	# distribution of the attached processes
	sampleStates(s)
	# create among-site rate variation by sampling
	# the "rate.multiplier" site-process-specific parameter
	# from a discrete gamma distribution (GTR+G).
	plusGamma(s,gtr,shape=0.1)
	# make the range 11:20 invariable
	setRateMultipliers(s,gtr,0,11:20)
	# get the rate multipliers for s and gtr
	getRateMultipliers(s,gtr)

	# construct a deletion process object
	# proposing lengths in the range 1:3
	d<-DiscreteDeletor(
		rate=0.1,
		name="MyDel",
		sizes=c(1:3),
		probs=c(3/6,2/6,1/6)
	)
	# get object 
	summary(d)
	# plot deletion length distribution
	plot(d)
	# attach deletion process d to sequence s
	attachProcess(s,d)
 	# create a region rejecting all deletions
       setDeletionTolerance(s,d,0,11:20)

	# construct an insertion process object
	# proposing lengths in the range 1:3
	i<-DiscreteInsertor(
		rate=0.1,
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
	# attach insertion process i to sequence s
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
	# export the number of substitutions as a phylo object
	subst<-exportStatTree(sim,"substitution")
	# plot the exported phylo object
	plot(subst)
	# plot tree and alignment
	plot(sim)
	# save and display alingment
	file<-paste("PhyloSim_dummy_fasta_",Sys.getpid(),".fas",sep="");
	saveAlignment(sim,file=file,paranoid=TRUE);
	# print out the Fasta file
	cat(paste(scan(file=file,what=character(),sep="\n"),collapse="\n"));cat("\n");
	# delete Fasta file
	unlink(file);

	## Example 4 - simulating amino acid sequences
	## and exploring more facilities.

	# enable fast & careless mode
	# WARNING: do not do this only
	# if you are sure, that your simulation
	# settings are 100% flawless!

	PSIM_FAST<-TRUE;
	
	# construct substitution model objects
	wag<-WAG()
	lg<-LG()
	# display a bubble plot of wag
	plot(wag)
	# construct root sequence
	s<-AminoAcidSequence(length=50)
	# attach process wag to the range 1:10
	attachProcess(s,wag,1:10)
	# attach process lg to the range 31:50
	attachProcess(s,lg,31:50)
	# create a pattern of processes in the range 11:30
	setProcesses(s,list(list(wag),list(lg),list(wag,lg)),11:30)
	# set rate multipliers to reduce 
	# the rate to half at every third site
	setRateMultipliers(s,wag,0.5,seq(from=13,to=30,by=3))
	setRateMultipliers(s,lg,0.5,seq(from=13,to=30,by=3))
	# Now every third site in the range 11:30 evolves
	# according to a combination of amino acid substitution models!

	# sample states
	sampleStates(s)
	# construct simulation object
	sim<-PhyloSim(root.seq=s, phylo=t)
	# set a log file
	sim$logFile<-paste("PhyloSim_dummy_log_",Sys.getpid(),sep="")
	# set log level to debug
	sim$logLevel<-1
	# run simulation
	Simulate(sim)
	# get a sequence object 
	rs<-sim$sequences[[4]]
	# print sequence string
	rs$string
	# show alignment matrix
	sim$alignment
	# plot tree and alignment, omitting ancestral sequences
	plot(sim,plot.ancestors=FALSE)
	# display the head of the log file
	cat(paste(scan(nmax=20,file=sim$logFile,what=character(),sep="\n"),collapse="\n"));cat("\n");
	# delete log file
	unlink(sim$logFile);

	# Reading alignments:

	# get a safe file name  
       fname<-paste("PhyloSim_dummy_fas_",Sys.getpid(),sep="")
       # write out a fasta alignment
       cat("> t3\nGTCTTT-CG-\n",file=fname);
       cat("> t4\nG--TC-TCGG\n",file=fname,append=TRUE);
       cat("> t2\nG--TC-TCGG\n",file=fname,append=TRUE);
       cat("> t1\nGTC-G-TCGG",file=fname,append=TRUE);
       # construct a PhyloSim object,
       # set the phylo object
       sim<-PhyloSim(phylo=rcoal(4))
       # read the alignment
       readAlignment(sim,fname)
       # remove alignment file
       unlink(fname)
       # plot the tree & alignment
       plot(sim)


	# disable fast & careless mode
	rm(PSIM_FAST)

	## See the package vignette and 
	## the GitHub repository for more examples.
