##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass PhyloSim
# \alias{phylosim}
# 
# @title "The PhyloSim class"
# 
# \description{ 
#
#	PhyloSim is an extensible object-oriented framework for the Monte Carlo simulation 
#	of sequence evolution written in 100 percent \code{R}.
#	It is built on the top of the \code{\link{R.oo}} and \code{\link{ape}} packages and uses 
#	Gillespie's direct method to simulate substitutions, insertions and deletions.
#
#	Key features offered by the framework:	
#	\itemize{
#	\item  Simulation of the evolution of a set of discrete characters with arbitrary states evolving 
#		by a continuous-time Markov process with an arbitrary rate matrix.
#	\item Explicit implementations of the most popular substitution models (for nucleotides, amino acids and codons).
# 	\item Simulation under the popular models of among-sites rate variation, like the gamma (+G) and invariants plus gamma (+I+G) models.
#	\item The possibility to simulate with arbitrarily complex patterns of among-sites rate variation by setting the site specific rates according to any \code{R} expression.
#	\item Simulation with one or more separate insertion and/or deletion processes acting on the sequences, which sample the insertion/deletion length from an arbitrary discrete distribution or an \code{R} expression (so all the probability distributions implemented in \code{R} are readily available for this purpose).
#	\item Simulation of the effects of variable functional constraints over the sites by site-process-specific insertion and deletion tolerance parameters, which determine the rejection probability of a proposed insertion/deletion.
#	\item The possibility of having a different set of processes and site-process-specific parameters for every site, which allow for an arbitrary number of partitions in the simulated data.
#	\item Simulation of heterotachy and other cases of non-homogeneous evolution by allowing the user to set "node hook" functions altering the site properties at internal nodes of the phylogeny.
#	\item The possibility to export the counts of various events ("branch statistics") as phylo objects (see \code{\link{exportStatTree.PhyloSim}}).
#	}
#
#	General notes:
#	\itemize{
#	\item The \code{Sequence} objects have no "immortal links". The simulation
#	is aborted if the sequence length shrinks to zero. It is up to the user 
#	to choose sensible indel rates and sequence lengths to prevent that.
#	\item The sites near the beginning and end of the sequences have less sites proposing
#	insertion and deletion events around the so the insertion and deletion processes 
#	have an "edge effect". The user can simulate
#	realistic flanking sequences to alleviate the edge effect in the simulation settings where
#	it may be an issue.
# }
#
#	Notes on performance: 
#	\itemize{
#	\item The pure \code{R} implementation offers felxibility, but also comes
#	with a slower simulation speed. If the \code{PSIM_FAST} object is present in the environment, a "fast & careless"
#	mode is enabled. In this mode most of the error checking is skipped, increasing the speed.
#	It is recomended that simulations are only run in fast mode if you are sure that the simulation
#	settings are free from errors. It is probably a good practice to set up the simulations in normal mode
#	with short sequences and enable fast mode when running the actual simulation with long sequences.
#	\item Please note, that no "branch statistics" are saved in fast mode.
#	\item Logging also has a negative impact on performance, so it's not a good idea to run
#	large simulations with the logging enabled.
#	\item The time needed to run a simulation depends not only on the number of the sites, 
#	but also on the length of the tree.
#	\item Constructing \code{Sequence} objects with large number of sites is expensive. Avoid doing
#	that inside a cycle.
#	\item In the case of \code{Sequence} objects with a large number of sites (more than 10 000) the 
#	amount of available memory can be limiting as well.
#  }
#
#	The examples below demonstrate only some more common simulation settings,
#	the framework offers much more flexibility. See the package
#	vignette (\code{vignette("PhyloSim",package="phylosim")}) and the
#	examples directory (\url{http://github.com/sbotond/phylosim/tree/master/examples/}) 
#	for additional examples.
#	
#	@classhierarchy
# }
#
#
# \references{
#	Gillespie, DT (1977) Exact stochastic simulation of coupled chemical reactions - 
#	J. Phys. Chem. 81 (25):2340-2361 \url{http://dx.doi.org/10.1021/j100540a008}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{phylo}{A rooted phylo object, constructed by the APE package.}
# 	\item{root.seq}{A valid Sequence object with Process objects attached. Used as the starting sequence during simulation.}
# 	\item{name}{The name of the object (a character vector of length one).}
# 	\item{log.file}{Name of the file used for logging.}
# 	\item{log.level}{An integer specifying the verbosity of logging (see \code{\link{setLogLevel.PhyloSim}}).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#   set.seed(1)
#	## The following examples demonstrate
#	## the typical use of the framework.
#	## See the package vignette and
#	## \url{http://github.com/sbotond/phylosim/tree/master/examples/}
#
#	## The ll() method gives information about the methods defined
#	## in the immediate class of an object.
#	## Useful when exploring the framework.
#
#	s<-Sequence()
#	ll(s)
#	ll(PhyloSim())
#	ll(GTR())
#
#	## Example 1 - A short simulation:
#	## simulate nucleotide seqeunces and display 
#	## the resulting alignment matrix.
#
#	Simulate(
#		PhyloSim(phy=rcoal(3),
#       root=NucleotideSequence(string="ATGC", proc=list(list(JC69())) ) )
#	)$alignment
#
#	# Construct a phylo object for the following
#	# simulations, scale total tree length to 1:
#
#	tmp<-PhyloSim(phylo=rcoal(3))
#	scaleTree(tmp,1/tmp$treeLength)
#	tmp$treeLength
#	t<-tmp$phylo
#
#	## Example 3 - simulating rate variation,
#	## insertions and deletions.
#	## See the examples/example_3_clean.R file
#	## in the phylosim GitHub repository.
#
#	# construct a GTR process object
#       gtr<-GTR(
#		name="MyGTR",
#               rate.params=list(
#                       "a"=1, "b"=2, "c"=3,
#                       "d"=1, "e"=2, "f"=3
#               ),
#               base.freqs=c(2,2,1,1)/6
#       )
#	# get object summary
#	summary(gtr)
#	# display a bubble plot
#	plot(gtr)
#
#	# construct root sequence object
#	s<-NucleotideSequence(length=40)
#	# attach process via virtual field
#	s$processes<-list(list(gtr))
#	# sample states from the equilibrium
#	# distribution of the attached processes
#	sampleStates(s)
#	# create among-site rate variation by sampling
#	# the "rate.multiplier" site-process-specific parameter
#	# from a discrete gamma distribution (GTR+G).
#	plusGamma(s,gtr,shape=0.1)
#	# make the range 11:20 invariable
#	setRateMultipliers(s,gtr,0,11:20)
#	# get the rate multipliers for s and gtr
#	getRateMultipliers(s,gtr)
#
	# construct a deletion process object
#	# proposing lengths in the range 1:3
#	d<-DiscreteDeletor(
#		rate=0.1,
#		name="MyDel",
#		sizes=c(1:3),
#		probs=c(3/6,2/6,1/6)
#	)
#	# get object 
#	summary(d)
#	# plot deletion length distribution
#	plot(d)
#	# attach deletion process d to sequence s
#	attachProcess(s,d)
# 	# create a region rejecting all deletions
#       setDeletionTolerance(s,d,0,11:20)
#
#	# construct an insertion process object
#	# proposing lengths in the range 1:3
#	i<-DiscreteInsertor(
#		rate=0.1,
#		name="MyDel",
#		sizes=c(1:2),
#		probs=c(1/2,1/2),
#		template.seq=NucleotideSequence(length=1,processes=list(list(JC69())))
#	) 
# 	# states will be sampled from the JC69 equilibrium distribution
#	# get object 
#	summary(i)
#	# plot insertion length distribution
#	plot(i)
#	# attach insertion process i to sequence s
#	attachProcess(s,i)
# 	# create a region rejecting all insertions
#       setInsertionTolerance(s,i,0,11:20)
#
#	# plot total site rates
#	plot(s)
#	# construct simulation object
#	sim<-PhyloSim(root.seq=s, phylo=t)
#	# get object summary
#	summary(sim)
#	# plot tree
#	plot(sim)
#	# run simulation
#	Simulate(sim)
# 	# get the list of recorded per-branch event counts
# 	getBranchEvents(sim)
#	# export the number of substitutions as a phylo object
#	subst<-exportStatTree(sim,"substitution")
#	# plot the exported phylo object
#	plot(subst)
#	# plot tree and alignment
#	plot(sim)
#	# save and display alingment
#	file<-paste("PhyloSim_dummy_fasta_",Sys.getpid(),".fas",sep="");
#	saveAlignment(sim,file=file,paranoid=TRUE);
#	# print out the Fasta file
#	cat(paste(scan(file=file,what=character(),sep="\n"),collapse="\n"));cat("\n");
#	# delete Fasta file
#	unlink(file);
#
#   # See \url{http://github.com/sbotond/phylosim/tree/master/examples/examples_class.R}
#   # for the full list of PhyloSim constructor examples.
#   
#	## See the package vignette and 
#	## the GitHub repository for even more examples.
# }
# 
# @author
#
# \seealso{ 
#	\code{\link{PSRoot} \link{Alphabet} \link{AminoAcidAlphabet} 
#	\link{AminoAcidSequence} \link{AminoAcidSubst}
#	\link{AnyAlphabet} \link{BinaryAlphabet} \link{BinarySequence} 
#	\link{BinarySubst} \link{BrownianInsertor} \link{CodonAlphabet}
#	\link{CodonSequence} \link{CodonUNREST} \link{ContinuousDeletor}
#	\link{ContinuousInsertor} \link{cpREV} \link{DiscreteDeletor}
#	\link{DiscreteInsertor} \link{Event} \link{F81} \link{F84}
#	\link{FastFieldDeletor} \link{GeneralDeletor}
#	\link{GeneralInDel} \link{GeneralInsertor} \link{GeneralSubstitution} 
#	\link{GTR} \link{GY94} \link{HKY} \link{JC69} \link{JTT} \link{JTT.dcmut}
#	\link{K80} \link{K81} \link{LG} \link{mtArt} \link{mtMam} \link{mtREV24}
#	\link{MtZoa} \link{NucleotideAlphabet} \link{NucleotideSequence} \link{PAM} 
#	\link{PAM.dcmut} \link{PhyloSim} \link{Process} \link{QMatrix} \link{Sequence}
#	\link{Site} \link{T92} \link{TN93} \link{UNREST} \link{WAG}}
# }
# 
#*/###########################################################################
setConstructorS3(
"PhyloSim",
  function(
		phylo=NA,
		root.seq=NA,
		name=NA,
		log.file=NA,
		log.level=-1, # no loggin is performed by default
		... 
		)	{

		this<-PSRoot();	
		this<-extend(this,
			"PhyloSim",
			.name="Anonymous",
			.phylo=NA,
			.root.sequence=NA, 
			.sequences=list(),		# references to the sequence objects
			.node.hooks=list(),		# references to the node hook functions.
			.branch.stats=list(), # branch statistics.
			.alignment=NA,				# the resulting alignment in fasat format.
			.log.file=NA, 				# the name of the log file.
			.log.connection=NA,		# connection for the log file.
			.log.level=NA					# log level
		);

		if(!all(is.na(phylo))){
			this$phylo<-phylo;
		}

		if(!all(is.na(root.seq))){
			this$rootSeq<-root.seq;
		}
	
		if(!missing(name)){
			this$name<-name;
		}

		if(!missing(log.file)){
			this$logFile<-log.file;
		} else {	
			# Setting default log file:
			tmp<-this$id;
			tmp<-gsub(":","_",tmp);
			this$logFile<-paste(tmp,".log",sep="");
		}

		# Setting log level:
		this$logLevel<-log.level;

		return(this);

  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
###########################################################################/**
#
# @RdocMethod	checkConsistency
# 
# @title "Check object consistency"
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
#
# \arguments{ 
#       \item{this}{An object.} 
#       \item{...}{Not used.} 
# } 
# 
# 
# \value{ 
#		Returns an invisible TRUE if no inconsistencies found in the object, throws 
#		an error otherwise. 
# } 
# 
# @author 
# 
# \seealso{ 
#	@seeclass
# } 
# 
#*/###########################################################################
setMethodS3(
	"checkConsistency", 
	class="PhyloSim", 
	function(
		this,
		...
	){

      may.fail<-function(this) {
					
				# Checking the name:	
				this$name<-this$name;
				# Checking the phylo object:
				if (!any(is.na(this$.phylo)) & !is.phylo(this$.phylo) ){
					throw("The phylo object is invalid!\n");
				}
				# Checking the log level:
				if(!is.numeric(this$.log.level) | (length(this$.log.level) != 1) ){
					throw("The log level must be numeric vector of length 1!\n");
				}
				# Checking lof file:
				if(!is.character(this$.log.file) | (length(this$.log.level) != 1) ){
					throw("The log file must be charcter vector of length 1!\n");
				}
				# Checking the sequences:
				for (seq in this$.sequences){
					if(is.Sequence(seq)){
						checkConsistency(seq);
					}
				}
				# Checking node hooks:
				for (hook in this$.node.hooks){
					if(!is.null(hook) & !is.function(hook)){
						throw("Invalid node hook found!\n");
					}
				}
				# Checking the alignment:
				if(!any(is.na(this$.alignment))){
					.checkAlignmentConsistency(this, this$.alignment);
				}
		
      }
      tryCatch(may.fail(this));
			return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: is.phylo.default
##	
###########################################################################/**
#
# @RdocDefault is.phylo
# 
# @title "Check if an object is an instance of the phylo class" 
# 
# \description{ 
#	@get "title".
#	Phylo objects are created by the \pkg{APE} package. This method just return the value of \code{inherits(this,"phylo")}.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	# load APE
#	library(ape);
#	# create some objects
#	o1<-Object();
#	o2<-rcoal(3);
#	# check if they are phylo objects
#	is.phylo(o1);
#	is.phylo(o2);			
#
# } 
# 
# @author 
# 
# \seealso{ 
# 	The \pkg{ape} package.
# } 
# 
#*/###########################################################################
setMethodS3(
	"is.phylo", 
	class="default", 
	function(
		this,
		...
	){

		inherits(this,"phylo");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: setPhylo
##	
###########################################################################/**
#
# @RdocMethod setPhylo
# 
# @title "Set the phylo object for a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The internal structure of the provided phylo object is reordered in a cladeweise fashion.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{value}{A phylo object created by the \pkg{ape} package.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A phylo object or FALSE.
# } 
# 
# \examples{
#	#create a PhyloSim object
#	sim<-PhyloSim();
#	# creat a phylo object
#	tree<-rcoal(3);
#	# get/set phylo object
#	setPhylo(sim,tree);
#	getPhylo(sim,tree);
#	# get/set phylo object via virtual field
#	sim$tree<-rcoal(5);
#	sim$tree;
# } 
# 
# @author 
# 
# \seealso{ 
# 	The PhyloSim class, the \pkg{ape} package.
# } 
# 
#*/###########################################################################
setMethodS3(
	"setPhylo", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No object provided!\n");
		}
		else if(!is.phylo(value)){
			throw("The new value must be a \"phylo\" object!\n");
		}
		else if(!is.rooted(value)){
			throw("The new value must be a rooted \"phylo\" object!\n");
		}
		else {

			.checkTipLabels(value);
			this$.phylo<-value;
			this$.phylo<-reorder(this$.phylo, order="cladewise");
			for (i in this$nodes){
				this$.sequences[[i]]<-NA;
			}
			return(this$.phylo);

		}
		return(FALSE);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkTipLabels
##	
setMethodS3(
	".checkTipLabels", 
	class="phylo", 
	function(
		this,
		...
	){

		for(label in this$tip.label){
			if(length(grep("^Node \\d+$",label,perl=TRUE,value=FALSE)) > 0){
					throw("Sorry, but the node labels matching \"Node \\d+\" are reserved for internal nodes! Blaming label: ",label,".\n");	
			}
			else if(length(grep("^Root node \\d+$",label,perl=TRUE,value=FALSE)) > 0){
					throw("Sorry, but the node labels matching \"Root node \\d+\" are reserved for the root node! Blaming label: ",label,".\n");	
			}
			
		}

	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: getPhylo
##	
###########################################################################/**
#
# @RdocMethod getPhylo
# 
# @title "Get the phylo object aggregated in a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A phylo object or NA.
# } 
# 
# \examples{
#	#create a PhyloSim object
#	sim<-PhyloSim();
#	# creat a phylo object
#	tree<-rcoal(3);
#	# get/set phylo object
#	setPhylo(sim,tree);
#	getPhylo(sim,tree);
#	# get/set phylo object via virtual field
#	sim$tree<-rcoal(5);
#	sim$tree;
# } 
# 
# @author 
# 
# \seealso{ 
# 	The PhyloSim class, the \pkg{ape} package.
# } 
# 
#*/###########################################################################
setMethodS3(
	"getPhylo", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.phylo;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: setRootSeq
##	
###########################################################################/**
#
# @RdocMethod setRootSeq
# 
# @title "Set the root sequence for a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The root sequence will be used as a starting point for the simulation. The phylo object must be set before
#	trying to set the root sequence object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{value}{A valid Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The root Sequence object if succesfull, FALSE otherwise.
# } 
# 
# \examples{
#	# create some objects
#	sim<-PhyloSim(phylo=rcoal(3));
#	seq<-NucleotideSequence(string="ATGCC");
#	# set/get root sequence
#	setRootSeq(sim, seq);
#	getRootSeq(sim, seq);
#	# set/get root sequence via virtual field
#	sim$rootSeq<-BinarySequence(string="111000111000");
#	sim$rootSeq;
#
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass Sequence Process
# } 
# 
#*/###########################################################################
setMethodS3(
	"setRootSeq", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No object provided!\n");
		}
		else if(!is.Sequence(value)){
			throw("The new value must be a sequence object!\n");
		}
		else {

			this$.root.sequence<-clone(value);
			this$.root.sequence$name<-paste("Root node",this$rootNode);
        
            # Call garbage collection:
            gc();
            gc();

			return(this$.root.sequence);

		}
		return(FALSE);


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: getRootSeq
##	
###########################################################################/**
#
# @RdocMethod getRootSeq
# 
# @title "Get the root sequence aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The root Sequence object or NA.
# } 
# 
# \examples{
#	# create some objects
#	sim<-PhyloSim(phylo=rcoal(3));
#	seq<-NucleotideSequence(string="ATGCC");
#	# set/get root sequence
#	setRootSeq(sim, seq);
#	getRootSeq(sim, seq);
#	# set/get root sequence via virtual field
#	sim$rootSeq<-BinarySequence(string="111000111000");
#	sim$rootSeq;
#
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass Sequence Process
# } 
# 
#*/###########################################################################
setMethodS3(
	"getRootSeq", 
	class="PhyloSim", 
	function(
		this,
		...
	){

			this$.root.sequence;


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: as.character.PhyloSim
##	
###########################################################################/**
#
# @RdocMethod as.character
# 
# @title "Return the character representation of a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The character representation is the identifier of the PhyloSim object as returned by the \code{getId} method.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	o<-PhyloSim(name="MySim");
#	# get character representation
#	as.character(o)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"as.character", 
	class="PhyloSim", 
	function(
		x,
		...
	){

		return(getId(x));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##
## Method: getName
##
###########################################################################/**
#
# @RdocMethod getName
# 
# @title "Get the name of a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	o<-PhyloSim();
#	# set/get name
#	setName(o,"MySim");
#	getName(o,"MySim");
#	# set/get name via virtual field
#	o$name<-"George";
#	o$name
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getName",
  class="PhyloSim",
  function(
    this,
    ...
  ){

    this$.name;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setName
##
###########################################################################/**
#
# @RdocMethod setName
# 
# @title "Set the name of a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{new.name}{A character vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new name.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	o<-PhyloSim();
#	# set/get name
#	setName(o,"MySim");
#	getName(o,"MySim");
#	# set/get name via virtual field
#	o$name<-"George";
#	o$name
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "setName",
  class="PhyloSim",
  function(
    this,
    new.name,
    ...
  ){

    this$.name<-as.character(new.name);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getId
##
###########################################################################/**
#
# @RdocMethod getId
# 
# @title "Get the unique identifier of a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#	The unique identifier is the concatenation of the class, the object name as returned by getName() and the object hash 
#       as returned by hashCode().
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	o<-PhyloSim(name="MySim");
#	# get id
#	getId(o);
#	# get id via virtual field
#	o$id;
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getId",
  class="PhyloSim",
  function(
    this,
    ...
  ){

  	this.class<-class(this)[1];
	id<-paste(this.class,this$.name,hashCode(this),sep=":");
	return(id);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setId
##
###########################################################################/**
#
# @RdocMethod setId
#
# @title "Forbidden action: setting the unique identifier of a PhyloSim object"
#
# \description{
#       @get "title".
#
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setId",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

  throw("Id is generated automatically and it cannot be set!\n");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: Simulate
##
###########################################################################/**
#
# @RdocMethod Simulate
# 
# @title "Run a simulation according to a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The phylo object and the root sequence must be set before attempting to run a simulation.
#	Also the bigRate of the root sequence must not be NA or zero, so at least one sane
#	Process object must be attached to the root sequence object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{quiet}{TRUE or FALSE (default).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATGC",processes=list(list(JC69())))
#	);
#	# Run the simulation
#	Simulate(sim);
#	# Print the resulting sequences
#	sim$sequences
#	# Print the resulting alignment
#	sim$alignment
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "Simulate",
  class="PhyloSim",
  function(
    		this,
		quiet=FALSE,
    ...
  ){


		if(!is.phylo(this$.phylo)){
			throw("Cannot simulate because the phylo object is not set or it is invalid!\n");
		}
		# Check for the root sequence:
		else if(!is.Sequence(this$.root.sequence)){
			throw("Cannot simulate because the root sequence is not set or it is invalid!\n");
		}
		# Check bigRate validity:
		else if(is.na(this$.root.sequence$bigRate)){
			throw("Cannot simulate because the bigRate of the root sequence is NA!\n");
		}
		else{

			# Warn for zero bigRate:
			if(this$.root.sequence$bigRate == 0){
				warning("The bigRate of the root sequence is zero! You are running a pointless simulation!\n");
			}

			if(exists(x="PSIM_FAST")){
				if(!quiet){
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
cat("!! WARNING: fast & careless mode is on, most of the error checking is omitted!  !!\n");
cat("!!    Please note that this also disables the saving of branch statistics.      !!\n");
cat("!!       You can go back to normal mode by deleting the PSIM_FAST object.       !!\n");
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
				}
			Log(this,"WARNING: fast & careless mode is on, most of the error checking is omitted!");
			}
		
			# Attach root sequence to root node:
			Log(this,paste("Attaching root sequence ",this$.root.sequence$id,sep=""));
			attachSeqToNode(this, node=getRootNode(this),seq=this$.root.sequence);

			# Write protecting the root sequence:
			Log(this,paste("Write protecting root sequence ",this$.root.sequence$id,sep=""));
			this$.root.sequence$writeProtected<-TRUE;

			# Traverse the tree and simulate:
			Log(this,paste("Starting simulation on the object",this$id));	
			edge.counter<-1;
			n.edges<-this$nedges;
			for(edge in 1:n.edges){
				if(!quiet){ cat("Simulating edge",edge,"of", n.edges,"\n");}
				Log(this,paste("Starting to simulate edge",edge,"of",n.edges));	
				.simulateEdge(this,number=edge);
				edge.counter<-edge.counter+1;
			}
		}
		Log(this, "Simulation finished, building alignment!\n");
		this$.alignment<-.recoverAlignment(this);
		# Flush the log connection:
		if(!is.na(this$.log.connection)){
				close(this$.log.connection);
		}

        # Call the garbage collector:
        gc();
        gc();

		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .simulateEdge
##
setMethodS3(
  ".simulateEdge",
  class="PhyloSim",
  function(
    this,
		number=NA,
    ...
  ){

		# Get edge:
		edge<-getEdge(this, number);
		# Get parent node:
		start.seq<-getSeqFromNode(this, edge[[1,"from"]]);
		# Evolve sequence:
		new.seq<-.evolveBranch(this, start.seq=start.seq, branch.length=edge[1,"length"], old.node=edge[[1,"from"]],new.node=edge[[1,"to"]], branch.number=number);
		# Write protect the sequence:
		new.seq$writeProtected<-TRUE;
		# Attach sequence to children node:
		attachSeqToNode(this, node=edge[1,"to"], seq=new.seq);

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .evolveBranch
##
setMethodS3(
  ".evolveBranch",
  class="PhyloSim",
  function(
    this,
    start.seq=NA,
    branch.length=NA,
		old.node=NA,
		new.node=NA,
		branch.number=NA,
    ...
  ){

	if(!exists(x="PSIM_FAST")){

		if(missing(start.seq)){
			throw("No starting sequence provided!\n");
		}
		else if(missing(branch.length)){
			throw("No branch length provided!\n");
		}
		else if(!is.numeric(branch.length)){
			throw("The branch length must be numeric!\n");
		}
	}
		if(.checkSeq(this, start.seq) ){
		
			# Cloning the starting sequence:
			seq<-clone(start.seq);
			
			# Set the name of the sequence object:
			if(is.tip(this, new.node)){
				seq$name<-this$tipLabels[[new.node]];
			}
			else {
				seq$name<-paste("Node",new.node);
			}
			.GillespieDirect(this, seq=seq, branch.length=branch.length, branch.number=branch.number);
			
			# Call the node hook if exists:
			hook<-this$.node.hooks[[as.character(new.node)]];
			if(!is.null(hook) & is.function(hook)){
				Log(this,paste("Calling node hook for node",new.node));
				seq<-hook(seq=seq);	
				if(!is.Sequence(seq)){
					throw("Node hook returned an invalid sequence object!\n");
				}
				else if(is.na(seq$bigRate)){
					throw("Node hook returned sequence with NA bigRate!\n");
				}
				else if(seq$bigRate == 0.0){
					throw("Node hook returned sequence with zero bigRate!\n");
				}
				else{
				 checkConsistency(seq, omit.sites=TRUE);
				}
			}

			# Return the resulting sequence object:
			return(seq);
		
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .GillespieDirect
##
setMethodS3(
  ".GillespieDirect",
  class="PhyloSim",
  function(
    this,
    seq=NA,
    branch.length=NA,
		branch.number=NA,
    ...
  ){

		Debug(this, paste("Branch length is",branch.length));
		
		# Initialize time:

		time<-0.0;
		
		# Sample the next waiting time until
		# the branch length is consumed:	

		while( (time<-time + rexp(n=1, rate=(big.rate<-getBigRate(seq)))) <= branch.length){

			# Generate a random number between zero and the bigRate:

			E<-runif(n=1,min=0,max=big.rate);

			# Identify the target site:

			site.number<-which(.getCumulativeRatesFast(seq) >= E)[[1]];

			# Get the events from the target site:

			site<-seq$.sites[[site.number]];
			site$.position<-site.number;
			events<-getEvents(site);
			site$.position<-NULL;
			
			# Get the rates:
			rates<-double();
			for(e in events){
				rates<-c(rates,e$.rate);
			}
		
			# Calculate the corresponding cumulative rates:	
			if(site.number > 1){
				rates<-cumsum(c(seq$.cumulative.rates[[site.number - 1]], rates));
			}
			else {
				rates<-cumsum(c(0.0, rates));
			}

			# Pick the event:

			event.number<-which(rates >= E)[[1]] - 1;
			event<-events[[event.number]];

			# Log the event:

			Log(this,paste("Performing event [",event$.name,"] at position",event$.position,"generated by the process",event$.process$.id));

			# Perform the event:

			event.details<-Perform(event);
			Debug(this,paste("Remaining branch length is",(branch.length-time) ));
			
			# Log event details:
	
					# Log deletion event details:
						if(event$.name == "Deletion"){
						Log(this,paste("The process",event$.process,"proposed to delete range",paste(event.details$range,collapse="--"),". Accepted:",event.details$accepted));
					}
					# Log insertion event details:
					else if(event$.name == "Insertion"){
						message<-paste("The process ",event$.process," proposed insertion at position ",event.details$position,". Accepted: ",event.details$accepted,sep="");
						if(event.details$accepted == TRUE){
						message<-paste(message,"."," Insert length was ",event.details$length,sep="");
						}
						Log(this, message);
					}

			# Update branch statistics if not in fast mode:
			if(!exists(x="PSIM_FAST")){
				.UpdateBranchStats(this,event,event.details, branch.number);
			}

			# Abort if sequence length shrunk to zero:

			if(seq$.length == 0){
				message<-paste("Terminating the simulation because the length of the sequence ",seq$name," shrunk to zero! Please be more careful when tuning the indel rates!\n");
				Log(this, message);
				throw(message);
			}	

		} #/while

        # Calling garbage collection:
        gc();
        gc();
		return(seq);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: attachSeqToNode
##
###########################################################################/**
#
# @RdocMethod	attachSeqToNode
# 
# @title "Assotiate a Sequence object with a given node of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	This method is mainly used internally.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{node}{Node identifier.}
#	\item{seq}{A Sequence object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "attachSeqToNode",
  class="PhyloSim",
  function(
		this,
		node=NA,
		seq=NA,
    ...
  ){

		if(!is.phylo(this$.phylo)){
			throw("The phylo object is not set, sequence to node is not possible!\n");
		}
		if(missing(node)){
			throw("No node specified!\n");
		}
		else if(missing(seq)){
			throw("No sequence object given");
		}
		else if(.checkNode(this,node) & .checkSeq(this, seq)){
			
			if(is.Sequence(this$.sequences[[node]])){
				throw("The node has already an attached sequence. Detach that before trying to attach a new one!\n");
			}
			else {
				this$.sequences[[as.numeric(node)]]<-seq;
				return(invisible(this));
			}

		}
	
		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: attachHookToNode
##
###########################################################################/**
#
# @RdocMethod attachHookToNode
# 
# @title "Attach a callback function to a given node of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	A "node hook" is a function which accepts a Sequence object through the named argument "seq" and returns a 
#	Sequence object. The node hook function must accept any object which inherits from the \code{Sequence} class!
#
#	After simulating the branch leading to the node, the resulting Sequence object is passed
#	to the node hook and the returned object is used to simulate the downstream branches.
#
#	By using node hooks the attached processes can be replaced during simulation, hence enabling the simulation of 
#	non-homogeneous sequence evolution.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{node}{Node identifier.} 
# 	\item{fun}{A function (see above).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATGC",processes=list(list(JC69())))
#	);
#	# create a node hook function
#	hook<-function(seq=NA){
#		# replace the substitution process with F84
#		if(inherits(seq,"NucleotideSequence")){
#			cat("Replacing JC69 with F84.\n");
#			seq$processes<-list(list(F84(rate.params=list("Kappa" = 2)))); 
#		}
#		return(seq);
#	}
#	# attach hook function to node 5
#	attachHookToNode(sim,5,hook);
#	# Run the simulation
#	Simulate(sim);
#	# Check if the processes have been truly replaced
#	lapply(sim$sequences, getUniqueProcesses.Sequence)
#	# Print the resulting alignment
#	sim$alignment
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "attachHookToNode",
  class="PhyloSim",
  function(
    		this,
		node=NA,
		fun=NA,
    ...
  ){

		if(!is.phylo(this$.phylo)){
			throw("The phylo object is not set, attaching node hook is not possible!\n");
		}
		if(missing(node)){
			throw("No node specified!\n");
		}
		else if(missing(fun)){
			throw("No function given!");
		}
		else if(!is.function(fun)){
			throw("The argument \"fun\" must be a function!\n");
		}
		else if( length(intersect(names(formals(fun)), "seq")) == 0 ){
			throw("The function argument must have a an argument named \"seq\"");
		}
		else if(!is.Sequence(fun(Sequence(length=1)))){
      throw("The insert hook function must return a Sequence object!\n");
		}
		else if( .checkNode(this,node) ){
			if(is.function(this$.node.hooks[[as.character(node)]])){
				throw("The node has already an attached node hook. Detach that before trying to attach a new one!\n");
			}
			else {
				this$.node.hooks[[as.character(node)]]<-fun;
				return(invisible(this));
			}

		}
	
		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: .checkNode
##
setMethodS3(
  ".checkNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		} else if( length(intersect(node, getNodes(this))) != 1){
			throw("The specified node is invalid!\n");	
		}
		else {
			return(TRUE);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkSeq
##
setMethodS3(
  ".checkSeq",
  class="PhyloSim",
  function(
    this,
		seq=NA,
    ...
  ){

		if(missing(seq)){
			throw("No sequence specified!\n");
		} else if(!is.Sequence(seq)){
			throw("The sequence object is invalid!\n");	
		}
		else {
			return(TRUE);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: detachSeqFromNode
##
###########################################################################/**
#
# @RdocMethod	detachSeqFromNode
# 
# @title "Detach a Sequence object from a given node of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	This method is mainly used internally.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{node}{Node identifier.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "detachSeqFromNode",
  class="PhyloSim",
  function(
    		this,
		node=NA,
    		...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				this$.sequences[[as.numeric(node)]]<-NA;
		}
	
		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: detachHookFromNode
##
###########################################################################/**
#
# @RdocMethod	detachHookFromNode
# 
# @title "Detach a node hook function from a given node of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{node}{Node identifier.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATGC",processes=list(list(JC69())))
#	);
#	# create a node hook function
#	hook<-function(seq=NA){
#		# replace the substitution process with F84
#		if(inherits(seq,"NucleotideSequence")){
#			cat("Replacing JC69 with F84.\n");
#			seq$processes<-list(list(F84(rate.params=list("Kappa" = 2)))); 
#		}
#		return(seq);
#	}
#	# attach hook function to node 5
#	attachHookToNode(sim,5,hook);
#	# detach hook from node 5
#	detachHookFromNode(sim,5);
#	# Run the simulation again
#	Simulate(sim);	# You should not see the message printed out by the "hook" function.
#
# } 
# 
# @author 
# 
# \seealso{ 
# 	attachHookToNode PhyloSim Simulate.PhyloSim
# } 
# 
#*/###########################################################################
setMethodS3(
  "detachHookFromNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				this$.node.hooks[[as.character(node)]]<-NA;
		}
	
		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: getSeqFromNode
##
###########################################################################/**
#
# @RdocMethod getSeqFromNode
# 
# @title "Get the Sequence object associated with a given node of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{node}{Node identifier.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object.
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
#	);
#	# get the sequence associated with node 5
#	getSeqFromNode(sim,5)	# Should be NA
#	# Run the simulation
#	Simulate(sim)
#	# try again
#	getSeqFromNode(sim,5)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getSeqFromNode",
  class="PhyloSim",
  function(
    		this,
		node=NA,
    		...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				return(this$.sequences[[as.numeric(node)]]);
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getSequences
##
###########################################################################/**
#
# @RdocMethod getSequences
# 
# @title "Gets all the Sequence objects associated with the nodes of a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The order of the Sequence objects in the returned list reflects the identifiers of the associated nodes.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of sequence objects.
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
#	);
#	# run the simulation
#	Simulate(sim)
#	# get all the associated sequence objects
#	getSequences(sim)
#	# get the sequence associated with node 3
#	# via virtual field
#	sim$sequences[[3]]
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getSequences",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		slist<-list();
		for (node in getNodes(this)){
			slist[[node]]<-getSeqFromNode(this, node=node);
		}
		return(slist);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setSequences
##
###########################################################################/**
#
# @RdocMethod setSequences
#
# @title "Forbidden action: setting the Sequence objects associated with the nodes of a phylo object aggregated by a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setSequences",
  class="PhyloSim",
  function(
    		this,
		value,
    		...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getAlignment
##
###########################################################################/**
#
# @RdocMethod	getAlignment
# 
# @title "Get the alignment stored in a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The alignment as a matrix. Gap are represented by strings composed of dashes.
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
#	);
#	# run the simulation
#	Simulate(sim)
#	# get the resulting aligment
#	getAlignment(sim)
#	# via virtual fileld:
#	sim$alignment
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getAlignment",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		this$.alignment;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setAlignment
##
###########################################################################/**
#
# @RdocMethod setAlignment
#
# @title "Forbidden action: setting the alignment stored in a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setAlignment",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

               this$alignment <- value;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .recoverAlignment
##
setMethodS3(
  ".recoverAlignment",
  class="PhyloSim",
  function(
    		this,
		paranoid=FALSE,
    		...
  ){

		# Refuse to build alignment if at least one of the sequences is NA:
		for (seq in this$.sequences){
			if(!is.Sequence(seq)){
				throw("Cannot build alignment because the simulation is incomplete!\n");
			}
		}

		# The list holding all the partial alignment matrices:
		aln.mat<-list();
		
		# Assigning NA-s here to prevent creation of these variables in the global
		# environment.
		row.names<-NA;
		from.node<-NA;
		to.node<-NA;
		from.seq<-NA;
		to.seq<-NA;
		edge<-NA;
		from.name<-NA;
		to.name<-NA;
		from.mat<-NA;
		to.mat<-NA;

		# Initialize the variables:

		init.vars<-function(){

			# Getting the edge:
			edge<<-getEdge(this, edge.number);
			
			# Getting the nodes:
			from.node<<-edge[[1,"from"]];
			to.node<<-edge[[1,"to"]];

			# Getting the sequence objects:
			from.seq<<-getSeqFromNode(this, from.node)
			to.seq<<-getSeqFromNode(this, to.node)
	
			# Getting sequence names:	
			from.name<<-from.seq$name;
			to.name<<-to.seq$name;

		}

		# Initialize the aligment matrices:
		init.aln.mats<-function(){

			# Initialize "from" element in aln.mat if necessary:	
			if( is.null(aln.mat[[from.name]] )){
					# Create a row of the states:
					tmp<-rbind(as.character(lapply(from.seq$.sites, getState)));
					# Label the columns by the site position:
					colnames(tmp)<-seq(along.with=from.seq$.sites);
					# Label the row with the sequence name:
					rownames(tmp)<-from.name;
					# Set the corresponding list element in aln.mat:
					aln.mat[[ from.name ]]<-tmp;
			}
			# Set from.mat
			from.mat<<-aln.mat[[ from.name ]];
			
			# Initialize "to" element int aln.mat if necessary
			if( is.null(aln.mat[[to.name]]) ){
				# Create a new entry if we are dealing with a tip:
				if(is.tip(this, to.node)){
					# Create a vector of states:
					tmp<-rbind(as.character(lapply(to.seq$.sites, getState)));
					# Label columns by position:
					colnames(tmp)<-seq(along.with=to.seq$.sites);
					# Label row by sequence name:
					rownames(tmp)<-to.name;
					aln.mat[[ to.name ]]<-tmp;
				}
				else {
					# A "to" element can be null only if its a tip:
					throw("aln.mat inconsistency!\n");
				}
			}
			# Set to.mat:
			to.mat<<-aln.mat[[ to.name ]];

			# Save row names:
			# The order is important! First "from", than "to"!
			row.names<<-c(rownames(from.mat), rownames(to.mat));					

		}

		# Get the sequence position of a given alignment column from
		# the column labels:
		get.seq.pos<-function(mat=NA, col=NA){
						# Column number cannot be larger than length:
						if(col > dim(mat)[[2]]){
							throw("Invalid column number!\n");
						}
						# Get the corresponding column name:
						tmp<-colnames(mat)[[col]];		
						# Return if NA:
						if(is.na(tmp)){
							return(NA);
						}
						else{
							return(as.numeric(tmp));	
						}
	  }
	
		# Check if two positions from the two *sequences* are homologous.	
		is.homologous<-function(from.pos=NA, to.pos=NA){
				# Check position validity:
				if(to.pos > to.seq$length){
					throw("to.pos too big ",to.pos);
				}
				if(from.pos > from.seq$length){
					throw("from.pos too big ",from.pos);
				
				}
				# Check if the ancestral from to.seq/to.pos is from.seq/from.pos:
				return(equals(to.seq$.sites[[ to.pos ]]$.ancestral, from.seq$.sites[[ from.pos ]]));
		}

		# Get the symbol length from "to.seq" at position to.pos:
		get.to.symlen<-function(pos=NA){
				len<-stringLength(to.mat[to.name , pos]);
				if( is.na(len) | (len < 1) ){
					throw("Trouble in getting to.symlen!");
				} else {
					return(len);
				}
		}

		# Get the symbol length from "from.seq" at position from.pos:
		get.from.symlen<-function(pos=NA){
				len<-stringLength(from.mat[from.name , pos]);
				if( is.na(len) | (len < 1) ){
					throw("Trouble in getting from.symlen!");
				} else {
					return(len);
				}
		}

		make.gap.in.from<-function(label=NA,symlen=NA){
				# Create the gap symbol:
				gap<-paste(rep("-",times=symlen),collapse="");
				# Create the vector with gaps:
 				gaps<-cbind(rep(gap, times=dim(from.mat)[[1]] ));
				# Label the column:
        colnames(gaps)<-c(label);
				# Bind the gaps with the corresponding column from to.mat,
				# and than bind with res.mat:
        res.mat<<-cbind(res.mat, rbind( gaps, cbind(to.mat[,j])  ) );
				# Restore rownames:	
        rownames(res.mat)<<-row.names;
				# Increment counter for to.mat:
        j<<-j+1;
		}

		make.gap.in.to<-function(label=NA,symlen=NA){
			# See above.
			gap<-paste(rep("-",times=symlen),collapse="");
			gaps<-cbind(rep(gap, times=dim(to.mat)[[1]] ));
			colnames(gaps)<-c(label);
			res.mat<<-cbind(res.mat, rbind(  cbind(from.mat[,i]), gaps  ) );
			rownames(res.mat)<<-row.names;
			i<<-i+1;

		}

		emmit.homologous<-function(){
			# Bind the two columns into one column:
			tmp<-cbind(rbind( cbind(from.mat[,i]), cbind(to.mat[,j]) ) );
			# Label the column by from.pos:	
			colnames(tmp)<-c(from.pos);
			# Set res.mat
			res.mat<<-cbind(res.mat, tmp );
			# resotre rownames:
			rownames(res.mat)<<-row.names;
			
			i<<-i+1;
			j<<-j+1;
		}

		# Iterate over the reverse of the edge matrix:	
		for (edge.number in rev(seq(from=1, to=this$nedges))){

			# Call variable initialization:
			init.vars();

			# Initialize partial alignment matrices:
			init.aln.mats();
		
			# The matrix holding the resulting partial alignment:	
			res.mat<-c();

			# Column counter for from.mat:
			i<-1;
			# Column counter for to.mat:
			j<-1;
			
			while(i <=dim(from.mat)[[2]] | j <=dim(to.mat)[[2]] ){

					# First of all, check for counter overflow:
					if(i > dim(from.mat)[[2]]){
							# If i is greater than the length of from.mat, but we 
							# are still iterating, that means that we have parts left from
							# to.mat, so we have to create gaps in from.mat, and increment j.
							make.gap.in.from(symlen=get.to.symlen(pos=j));
							next();
					}
					else if (j > dim(to.mat)[[2]]){
							# If j is greater than the length of to.mat and we still iterating,
							# that means that we have still some columns in from.mat, so we create
							# gaps in to.mat and increment i. We label the new column with from.pos.
							from.pos<-get.seq.pos(mat=from.mat, col=i);
							make.gap.in.to(label=from.pos,get.from.symlen(pos=i));
							next();
					}

					# Now figure out the positions:
					from.pos<-get.seq.pos(mat=from.mat, col=i);
					to.pos<-get.seq.pos(mat=to.mat, col=j);

					# Now check for the gaps wich have been introduced before:

					if(is.na(from.pos)){
						# If we have a gap in from.mat,
						# than emmit the columnt with a gap in "to":
						make.gap.in.to(symlen=get.from.symlen(pos=i));
						next();
					}

					if(is.na(to.pos)){
						# Existent gap in to.mat:
						make.gap.in.from(symlen=get.to.symlen(pos=j));
						next();
					}

					# Now we have some real alignment to do here:

					if(is.homologous(from.pos=from.pos, to.pos=to.pos)){
						# We have to homologous columns, bind them, and emmit:
						emmit.homologous();
						next();
					}
					else if(is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
						# The two columns are not homologous. The column in "to"
						# was inserted by a process. Make gap in from:
						make.gap.in.from(symlen=get.from.symlen(pos=i));
						next();

					} 
					else {
						# The only possibility left is a deletion in the child sequence.
						# Make gaps in "to", label new column by from.pos:
						make.gap.in.to(label=from.pos,symlen=get.from.symlen(pos=i));
						next();
					}

		 } # while i | j
			
			# Replace the "from" element in aln.mat with the resulting partial
			# alignment matrix: 
			aln.mat [[ from.name ]]<-res.mat;

		} # for edge.number
		alignment <-aln.mat[[ this$rootSeq$name ]];
		# Check the correcteness of the alignment if paranoid:
		if(paranoid){
			.checkAlignmentConsistency(this, alignment);
		}

        # Call garbage collection:
        gc();
        gc();

		# The whole alignment is associated with the root node:
		return(alignment);
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkAlignmentConsistency
##
setMethodS3(
  ".checkAlignmentConsistency",
  class="PhyloSim",
  function(
    this,
    aln,
    ...
  ){
	
	# First check if the sequences are intact:
	for(node in this$nodes){
		seq.node<-getSeqFromNode(this, node);
		seq.aln<-aln[seq.node$name, ];
		seq.aln<-seq.aln[ grep("^[^-]+$",seq.aln) ];
		seq.aln<-paste(seq.aln, collapse="");
		if(seq.aln != seq.node$string){
			throw("The alignment is inconsistent with the sequence objects!\n Blaming ",seq.node$name, ".\n");
		}
	}

	for(edge.number in rev(seq(from=1, to=this$nedges))){

			# Getting the edge:
			edge<-getEdge(this, edge.number);
			
			# Getting the nodes:
			from.node<-edge[[1,"from"]];
			to.node<-edge[[1,"to"]];

			# Getting the sequence objects:
			from.seq<-getSeqFromNode(this, from.node)
			to.seq<-getSeqFromNode(this, to.node)
	
			# Getting sequence names:	
			from.name<-from.seq$name;
			to.name<-to.seq$name;

			# Initializing positions:
			from.pos<-1;
			to.pos<-1;

			is.gap<-function(string){
			
					res<-length(grep("^-+$",string));
					if( res == 1 ){
							return(TRUE);
					} 
					else if (res > 1){
							throw("is.gap: argument vector too long!\n");
					}
					else {
							return(FALSE);
					}
	
			}
			
			# Iterate over edges:
			for (i in 1:dim(aln)[[2]]){

					# Overflow in "from" counter,
					if(from.pos > from.seq$length){

						to.char<-aln[to.name,i];			
						if(!is.gap(to.char)){
							# we have a final insertion:
							if(!is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
								throw("Alignment insertion inconsistency!\n");
							}
							to.pos<-to.pos+1;
						} 
							next();
					}				
					
					# Overflow in "to" counter (final deletion):
					if(to.pos > to.seq$length){
						break();
					}				
					
					# Get the symbols from alignment:	
					from.char<-aln[from.name,i];			
					to.char<-aln[to.name,i];			

					is.gap.to<-is.gap(to.char);
					is.gap.from<-is.gap(from.char);
	
					# Skip if we have to gap symbols:	
					if( is.gap.from & is.gap.to ){
						next();
					}
					# Deletion in to.seq:
					else if(is.gap.to & !is.gap.from ){
						from.pos<-(from.pos+1);
					}
					# Insertion in to.seq:
					else if(!is.gap.to & is.gap.from ){
							# Check ancestral pointer for inserted sites:
							if(!is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
								throw("Alignment insertion inconsistency!\n");
							}
							to.pos<-(to.pos+1);
					} else {
							 # We must have a homology here:						
							 if(!equals(to.seq$.sites[[ to.pos ]]$.ancestral, from.seq$.sites[[ from.pos ]])){
									throw("Non-homologous sites aligned! Alignment is inconsistent!\n");	
							}
							 from.pos<-(from.pos+1);
							 to.pos<-(to.pos+1);
					}
	
			}

	}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: saveAlignment
##
###########################################################################/**
#
# @RdocMethod saveAlignment
# 
# @title "Save the alignment stored in a PhyloSim object in a Fasta file" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{file}{The name of the output file.} 
# 	\item{skip.internal}{Do not save sequences corresponding to internal nodes.} 
# 	\item{paranoid}{Check the consistency of the alignment.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
#	);
#	# run the simulation
#	Simulate(sim)
#	# save the alignment
#	file<-paste("PhyloSim_dummy_fasta_",Sys.getpid(),".fas",sep="");
#	saveAlignment(sim,file=file,paranoid=TRUE);
#	# print out the Fasta file
#	cat(paste(scan(file=file,what=character(),sep="\n"),collapse="\n"));cat("\n");
#	# delete Fasta file
#	unlink(file);
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "saveAlignment",
  class="PhyloSim",
  function(
    		this,
    		file="phylosim.fas",
    		skip.internal=FALSE,
		paranoid=FALSE,
    		...
  ){

		if(any(is.na(this$.alignment))){
			warning("Alignment is undefined, nothin to save!\n");
			return();
		}
		else {
			if(paranoid){
				.checkAlignmentConsistency(this, this$.alignment);
			}
			sink(file);
			if(!skip.internal){
				for(i in 1:dim(this$.alignment)[[1]]){
					cat(">",rownames(this$.alignment)[[i]],"\n");
					cat(paste(this$.alignment[i,],collapse=""),"\n");
				}
			} else {
				for(i in 1:dim(this$.alignment)[[1]]){

					name<-rownames(this$.alignment)[[i]];

					if(!any((length(grep("^Node \\d+$",name,perl=TRUE,value=FALSE)) > 0),(length(grep("^Root node \\d+$",name,perl=TRUE,value=FALSE)) > 0))){
						cat(">",name,"\n");
						cat(paste(this$.alignment[i,],collapse=""),"\n");
					}

				}
			}
			sink(NULL);
		}
		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: plot.PhyloSim
##
###########################################################################/**
#
# @RdocMethod plot
# 
# @title "Plot a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#       This method plots the aggregated alignment alongside the tree used for simulation. Various options
#       allow for control over the plot style.
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A PhyloSim object.}
# 	\item{plot.tree}{Whether to plot the tree alongside the alignment. TRUE or FALSE; defaults to TRUE.}
# 	\item{plot.ancestors}{Whether to plot the ancestral sequences. TRUE or FALSE; defaults to TRUE.}
# 	\item{plot.chars}{Whether to plot the actual text of the characters.}
# 	\item{plot.legend}{Whether to plot the legend showing the character-to-color mapping.}
# 	\item{plot.labels}{Whether to plot the sequence labels along the y-axis}
# 	\item{aspect.ratio}{(Experimental; when set, this option forces the num.pages value to 1) Constrains the alignment residues to have a certain aspect ratio; values above 1 cause vertically-stretched blocks. FALSE disables aspect ratio control, numerical values set the aspect ratio; defaults to FALSE.}
# 	\item{num.pages}{Optionally split the alignment over a number of vertically-stacked pages. This is useful for long alignments. 'auto' chooses a sensible number of pages, numerical values specify a number; defaults to 'auto'.}
# 	\item{char.text.size}{Text size for the aligned characters. This may require tweaking depending on the DPI and output format. Defaults to 'auto'.}
# 	\item{axis.text.size}{Text size for the sequence labels along the y-axis. This may require tweaking depending on the DPI and output format. Defaults to 'auto'.}
#       \item{color.scheme}{Color scheme to use ("auto", "binary", "dna", "protein", "codon", "combined", "combined_codon"). Defaults to 'auto'. When set to 'auto', the function will choose an appropriate coloring scheme based on the alignment content.}
#       \item{color.branches}{The event count used to color the branches ("substitutions" by default). See \code{\link{getBranchEvents.PhyloSim}}.}
#       \item{tree.xlim}{The x-axis limits of the tree panel.}
#       \item{aln.xlim}{The x-axis limits of the alignment panel (in alignment column coordinates).}
#       \item{tracks}{Tracks to display above or below the alignment as colored blocks.
#
# The input format for tracks is a list of data frames with the following possible fields, all of which are optional and can be omitted:
# \itemize{
# \item pos - the sequence position (starting with 1) of the feature. Defaults to NULL.
# \item score - the score (between 0 and 1) of the feature. Scores above 1 or below zero will be truncated.
# Defaults to 1.
# \item y_lo - the lower Y offset (between 0 and 1) of the feature. Defaults to 0. Use a y_lo and y_hi
# value for each row in the track data frame to create a wiggle plot like effect.
# \item y_hi - the upper Y offset (between 0 and 1) of the feature. Defaults to 1. Use just a y_hi value
# for each row in the track data frame to create a bar plot like effect.
# \item [the fields below are considered unique per track; the values from the first row in the track
# data frame are used.]
# \item id - the display ID for the track. Defaults to 'Track'.
# \item layout - set to 'above' to put the track above the alignment, 'below' for below.
# \item height - the number of alignment rows for the track to span in height. Defaults to 3.
# \item color.gradient - a comma-separated list of colors to interpolate between when coloring
# the blocks. Examples: 'white,red' 'blue,gray,red' '#FF00FF,#FFFFFF'. Defaults to 'white,black'.
# \item color - a single color to use when coloring the blocks. Mutually exclusive with color.gradient,
# and if a color.gradient value exists then this value will be ignored. Defaults to black.
# \item background - a color for the background of the track. Defaults to white.
# }}
#       \item{aln.length.tolerance}{The desired alignment/sequence length ratio (A/S ratio) to trim the alignment to.
#       The A/S ratio is defined as the ratio between the alignment length and the mean ungapped sequence length, and
#       the alignment trimming procedure will remove blocks of indel-containing columns (in a sensible order) until 
#       either (a) the desired indel tolerance is reached, or (b) no more columns can be removed without yielding an empty
#       alignment. A track is added below the alignment to indicate how many indels each resulting alignment column used
#       used to harbor, and black squares are overlaid onto the alignment where extant sequence data has been trimmed.
#       Defaults to NULL (no trimming); values in the range of 0.9 to 1.3 tend to work well at improving the
#       legibility of very gappy alignments.}
#       \item{plot.nongap.bl}{If set to TRUE, plots the non-gap branch length (defined as the branch length of the subtree of non-gapped sequences) as a track below the alignment. Defaults to FALSE.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATGCTAGCTAGG",processes=list(list(JC69())))
#	);
#       # plot the aggregated phylo object
#       plot(sim)
#	# run simulation
#	Simulate(sim)
#       # Plot the alignment without the tree or ancestral sequences.
#       plot(sim, plot.ancestors=FALSE, plot.tree=FALSE)
#       # Force a DNA-based color scheme 
#       # (default is 'auto' to auto-detect based on the sequence composition)
#       plot(sim, color.scheme='dna', plot.legend=TRUE)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "plot",
  class="PhyloSim",
  function(
    x,
    plot.tree,
    plot.ancestors,
    plot.chars,
    plot.legend,
    plot.labels,
    aspect.ratio,
    num.pages,
    char.text.size,
    axis.text.size,
    color.scheme,
    color.branches,
    tree.xlim,
    aln.xlim,
    tracks,
    aln.length.tolerance,
    plot.nongap.bl,
    ...
  ){
		
		if(missing(char.text.size)){
   		        char.text.size <- 'auto'
		}
		if(missing(axis.text.size)){
			axis.text.size <- 'auto'
		}
		if(missing(color.scheme)){
			color.scheme <- 'auto'
		}
		if(missing(color.branches)){
			color.branches <- 'substitutions'
		}
		if(missing(plot.tree)){
			plot.tree <- TRUE
		}
		if(missing(plot.ancestors)){
			plot.ancestors <- TRUE
		}
		if(missing(plot.chars)){
			plot.chars <- TRUE
		}
		if(missing(plot.legend)){
			plot.legend <- FALSE
		}
		if(missing(plot.labels)){
			plot.labels <- TRUE
		}
		if(missing(aspect.ratio)){
			aspect.ratio <- FALSE
		}
		if(missing(num.pages)){
			num.pages='auto'
		}
                if(missing(color.scheme)){
                        color.scheme='auto'
                }
                if(missing(tree.xlim)){
                  tree.xlim=NULL
                }
                if(missing(aln.xlim)){
                  aln.xlim=NULL
                }
                if(missing(tracks)){
                  tracks=NULL
                }
                if(any(is.na(x$.phylo))) {
                  plot.tree <- FALSE
                }
                if(missing(aln.length.tolerance)){
                  aln.length.tolerance=NULL
                }
                if(missing(plot.nongap.bl)){
                  plot.nongap.bl=FALSE
                }
                
		if(all(!is.na(x$.alignment), is.matrix(x$.alignment))){
			.plotWithAlignment(x,
				plot.tree=plot.tree,
				plot.ancestors=plot.ancestors,
				plot.chars=plot.chars,
                                plot.legend=plot.legend,
                                plot.labels=plot.labels,
				aspect.ratio=aspect.ratio,
				num.pages=num.pages,
                                char.text.size=char.text.size,
                                axis.text.size=axis.text.size,
                                color.scheme=color.scheme,
                                color.branches=color.branches,
                                tree.xlim=tree.xlim,
                                aln.xlim=aln.xlim,
                                tracks=tracks,
                                aln.length.tolerance=aln.length.tolerance,
                                plot.nongap.bl=plot.nongap.bl
			);
			return(invisible(x));
		}

		plot(x$.phylo);
		nodelabels();

		return(invisible(x));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: .plotWithAlignment
##
setMethodS3(
  ".plotWithAlignment",
  class="PhyloSim",
  function(
    x,
    plot.tree,
    plot.ancestors,
    plot.chars,
    plot.legend,
    plot.labels,
    aspect.ratio,
    num.pages,
    char.text.size,
    axis.text.size,
    color.scheme,
    color.branches,
    tree.xlim,
    aln.xlim,
    tracks,
    aln.length.tolerance,
    plot.nongap.bl,
    ...
  ){

   # ugly empirical fix of some R CMD check warnings:
   id           <-NA;
   pos          <-NA;
   char         <-NA;
   xend         <-NA;
   yend         <-NA;
   y            <-NA;
   substitutions<-NA;
   event.count  <-NA;
   type         <-NA;
   track_index  <-NA;
   xx           <-NA;
   yy           <-NA;
   xmin         <-NA;
   xmax         <-NA;
   ymin         <-NA;
   ymax         <-NA;

    ### First, we need to define a bunch of sparsely-documented utility functions. ###

   # Re-orders the alignment rows by matching the tree's tips.
   sort.aln.by.tree = function(aln,tree) {
     names <- dimnames(aln)[[1]]
     if (length(names) > length(tree$tip.label)) {
       return(aln)
     }
     newPositions <- rev(match(tree$tip.label,names))
     aln[newPositions,] <- aln
     dimnames(aln) <- list(names[newPositions])
     return(aln)
   }
   
    # Extracts a list of child node IDs for the given node. Returns (-1,-1) if the node is a leaf.
    child.nodes <- function(phylo,node) {
      edge.indices <- which(phylo$edge[,1]==node)
      nodes <- phylo$edge[edge.indices,2]
      if (length(nodes)==0) {
        nodes <- list(c(-1,-1))
      } else {
        nodes <- list(nodes)
      }
      return(list(nodes))
    }

    # Extracts the parent node ID for the given node. Returns -1 if the node is root.
    parent.node <- function(phylo,node) {
      edge.index <- which(phylo$edge[,2]==node)
      node <- phylo$edge[edge.index,1]
      if (length(node)==0) {
        node <- -1
      }
      return(node)
    }

    generic.count <- function(sim,node,type) {
      if (type == 'insertions' || type == 'ins') {
        type <- 'insertion'
      }
      if (type == 'deletions' || type == 'del') {
        type <- 'deletion'
      }
      if (type == 'substitutions' || type == 'subst' || type == 'sub') {
        type <- 'substitution'
      }
      if (type == 'syn') {
        type <- 'synonymous'
      }
      if (type == 'nsyn') {
        type <- 'non-synonymous'
      }
                                        # Default value of 0.
      cnt <- 0
      if(type=='none' || type == '') {
        return(as.numeric(cnt))
      }
                                        # Find the edge which points to the given node.
      edge.index <- which(phylo$edge[,2]==node)
      if (length(edge.index) > 0) {
        bs <- sim$.branch.stats
        cnt <- bs[[paste(edge.index)]][[type]]
        if (is.null(cnt) || is.na(cnt)) {
          cnt <- 0
        }
      }
      return(as.numeric(cnt))
    }
   
    subst.count <- function(sim,node) {
      return(generic.count(sim,node,'substitution'))
    }
    ins.count <- function(sim,node) {
      return(generic.count(sim,node,'insertion'))
    }
    del.count <- function(sim,node) {
      return(generic.count(sim,node,'deletion'))
    }
    syn.count <- function(sim,node) {
      return(generic.count(sim,node,'synonymous'))
    }
    nsyn.count <- function(sim,node) {
      return(generic.count(sim,node,'non-synonymous'))
    }

    # Finds the node with a given label.
    node.with.label <- function(tree,label) {
      return(which(tree$tip.label %in% label))
    }
   
    # Extracts the length of the branch above the given node. Returns 0 if the node is root.
    branch.length <- function(phylo,node) {
      edge.index <- which(phylo$edge[,2]==node)
      bl <- phylo$edge.length[edge.index]
      if (length(bl)==0) {
        bl <- 0
      }
      return(bl)
    }

    # The maximum root-to-tip length in the tree.
    max.length.to.root <- function(phylo) {
      max.length <- 0
      for (i in 1:length(phylo$tip.label)) {
        cur.length <- length.to.root(phylo,i)
        max.length <- max(max.length,cur.length)
      }
      return(max.length)
    }

    # The length from the root to the given node. Can be given either as a node ID or a tip label.
    length.to.root <- function(phylo,node) {
      tip.index <- node
      if (is.character(node)) {
        tip.index <- which(phylo$tip.label==node)
      }
      
      cur.node.b <- tip.index

      p.edges <- phylo$edge
      p.lengths <- phylo$edge.length
      
      length <- 0
      while(length(which(p.edges[,2]==cur.node.b)) > 0) {
        cur.edge.index <- which(p.edges[,2]==cur.node.b)
        cur.edge.length <- p.lengths[cur.edge.index]
        length <- length + cur.edge.length
        cur.node.a <- p.edges[cur.edge.index,1]
        cur.node.b <- cur.node.a # Move up to the next edge
      }
      return(length)
    }

    # Returns a data frame defining segments to draw the phylogenetic tree.
    phylo.layout.df <- function(phylo,layout.ancestors=FALSE,align.seq.names=NULL) {
      # Number of nodes and leaves.
      n.nodes <- length(phylo$tip.label)+phylo$Nnode
      n.leaves <- length(phylo$tip.label)

      # Create the skeleton data frame.
      df <- data.frame(
                       node=c(1:n.nodes),                                            # Nodes with IDs 1 to N.
                       x=0,                                                          # These will contain the x and y coordinates after the layout procedure below.
                       y=0,
                       label=c(phylo$tip.label,((n.leaves+1):n.nodes)),            # The first n.leaves nodes are the labeled tips.
                       is.leaf=c(rep(TRUE,n.leaves),rep(FALSE,n.nodes-n.leaves)),    # Just for convenience, store a boolean whether it's a leaf or not.
                       parent=0,                                                     # Will contain the ID of the current node's parent
                       children=0,                                                   # Will contain a list of IDs of the current node's children
                       branch.length=0                                               # Will contain the branch lengths
                       )

      # Collect the parents, children, and branch lengths for each node
      parent <- c()
      bl <- list()
      children <- list()
      event.count <- list()
      for (i in 1:nrow(df)) {
        node <- df[i,]$node
        parent <- c(parent,parent.node(phylo,node))
        bl <- c(bl,branch.length(phylo,node))
        children <- c(children,child.nodes(phylo,node))
        event.count <- c(event.count,generic.count(x,node,color.branches))
      }
      df$parent <- parent
      df$children <- children
      df$branch.length <- bl
      df$event.count <- as.numeric(event.count)

      # Start the layout procedure by equally spacing the leaves in the y-dimension.
      df[df$is.leaf==TRUE,]$y = c(1:n.leaves)

      found.any.internal.node.sequences <- FALSE

      # For each leaf: travel up towards the root, laying out each internal node along the way.
      for (i in 1:n.leaves) {
        cur.node <- i
        while (length(cur.node) > 0 && cur.node != -1) {
          # We always use branch lengths: x-position is simply the length to the root.
          df[cur.node,]$x <- length.to.root(phylo,cur.node)

          # The y-position for internal nodes is the mean of the y-position of the two children.
          children <- unlist(df[cur.node,]$children)
          if (length(children) > 0 && children[1] != -1) {
            child.a <- children[1]
            child.b <- children[2]
            child.a.y <- df[child.a,]$y
            child.b.y <- df[child.b,]$y
            df[cur.node,]$y <- (child.a.y+child.b.y)/2
          }

          # Try to find the index of this node in the alignment names.
          if (!is.null(align.seq.names)) {
            lbl <- df[cur.node,]$label
            index.in.names <- which(align.seq.names == lbl | align.seq.names %in% c(paste('Node',lbl),paste('Root node',lbl)))
            if (length(index.in.names)>0) {
              df[cur.node,]$y <- index.in.names
              if (!df[cur.node,]$is.leaf) {
                found.any.internal.node.sequences <- TRUE
              }
            }
          }
          
          cur.node <- unlist(df[cur.node,]$parent)
        }
      }
      
      # We have a data frame with each node positioned.
      # Now we go through and make two line segments for each node (for a 'square corner' type tree plot).
      line.df <- data.frame()
      for (i in 1:nrow(df)) {
        row <- df[i,]            # Data frame row for the current node.
        if (row$parent == -1) {
          next; # Root node!
        }
        p.row <- df[row$parent,] # Data frame row for the parent node.
        if (layout.ancestors && found.any.internal.node.sequences) {
          horiz.line <- data.frame(
                                   x=row$x,
                                   xend=p.row$x,
                                   y=row$y,
                                   yend=p.row$y,
                                   lbl=row$label,
                                   event.count=row$event.count
                                   )
          line.df <- rbind(line.df,horiz.line)
        } else {
          horiz.line <- data.frame(
                                   x=row$x,
                                   xend=p.row$x,
                                   y=row$y,
                                   yend=row$y,
                                   lbl=row$label,
                                   event.count=row$event.count
                                   )    # First a line from row.x to parent.
          vert.line <- data.frame(
                                  x=p.row$x,
                                  xend=p.row$x,
                                  y=row$y,
                                  yend=p.row$y,
                                  lbl=row$label,
                                  event.count=row$event.count
                                  ) # Now a line from row.y to parent.y
          
          #horiz.line <- data.frame(x=row$x,xend=(p.row$x+row$x)/2,y=row$y,yend=row$y,lbl=row$label)    # First a line from row.x to parent.
          #vert.line <- data.frame(x=(p.row$x+row$x)/2,xend=p.row$x,y=row$y,yend=p.row$y,lbl=row$label) # Now a line from row.y to parent.y
          line.df <- rbind(line.df,horiz.line,vert.line)
        }
      }
      return(line.df)
    }

    # Call this to put a ggplot panel into a specified layout position [for example: print(p,vp=subplot(1,2)) ]
    subplot <- function(x, y) viewport(layout.pos.col=x, layout.pos.row=y)

    # Call this to create a layout with x and y rows and columns, respectively
    vplayout <- function(x, y) {
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(y,x)))
    }

    # Creates a color aesthetic for alignments
    alignment.colors <- function(scheme,darken=F) {
      scheme <- tolower(scheme)
      if (scheme == 'binary') {
        cols <- c(
                  '0' = "#000000",
                  '1' = '#FFFFFF'
                  )
      } else if (scheme == 'dna') {
        cols <- c(
                  'G' = "#FFFF00",
                  'C' = "#00FF00",
                  'T' = "#FF0000",
                  'A' = "#0000FF"
                  )
      } else if (scheme == 'numeric') {
        #cols <- heat.colors(10)      
        cols <- colorRampPalette(c("red","yellow","green"))(10)
        cols <- c(
                  '0' = cols[1],
                  '1' = cols[2],
                  '2' = cols[3],
                  '3' = cols[4],
                  '4' = cols[5],
                  '5' = cols[6],
                  '6' = cols[7],
                  '7' = cols[8],
                  '8' = cols[9],
                  '9' = cols[10]
        )
      } else if (scheme == 'taylor' || scheme == 'protein') {
        cols <- c(
                  'A' = "#CCFF00",       'a' = "#CCFF00",
                  'C' = "#FFFF00",       'c' = "#FFFF00",
                  'D' = "#FF0000",       'd' = "#FF0000",
                  'E' = "#FF0066",       'e' = "#FF0066",
                  'F' = "#00FF66",       'f' = "#00FF66",
                  'G' = "#FF9900",       'g' = "#FF9900",
                  'H' = "#0066FF",       'h' = "#0066FF",
                  'I' = "#66FF00",       'i' = "#66FF00",
                  'K' = "#6600FF",       'k' = "#6600FF",
                  'L' = "#33FF00",       'l' = "#33FF00",
                  'M' = "#00FF00",       'm' = "#00FF00",
                  'N' = "#CC00FF",       'n' = "#CC00FF",
                  'P' = "#FFCC00",       'p' = "#FFCC00",
                  'Q' = "#FF00CC",       'q' = "#FF00CC",
                  'R' = "#0000FF",       'r' = "#0000FF",
                  'S' = "#FF3300",       's' = "#FF3300",
                  'T' = "#FF6600",       't' = "#FF6600",
                  'V' = "#99FF00",       'v' = "#99FF00",
                  'W' = "#00CCFF",       'w' = "#00CCFF",
                  'Y' = "#00FFCC",       'y' = "#00FFCC",
                  '2' = "#888888",       '2' = "#888888",
                  'O' = "#424242",       'o' = "#424242",
                  'B' = "#7D7D7D",       'b' = "#7D7D7D",
                  'Z' = "#EEEEEE",       'z' = "#EEEEEE",
                  'X' = "#000000",       'x' = "#000000"
                  )
      } else if (scheme == 'codon') {
                                        # Get the protein colors.
        protein.colors <- alignment.colors('protein')
                                        # Create a list of codons.
        ca <- CodonAlphabet()
        nucs <- c('G','A','C','T')
        codons <- expand.grid(nucs,nucs,nucs,stringsAsFactors=F)
                                        # For each codon give the protein color.
        codon.colors <- c()
        for (i in 1:nrow(codons)) {
          codon = paste(codons[i,],collapse='')
          aa <- translateCodon(ca,codon)
          codon.colors[codon] = protein.colors[aa]
        }
        cols <- codon.colors
      } else if (scheme == 'combined') {
        dna.colors <- alignment.colors('dna')
        binary.colors <- alignment.colors('binary')
        protein.colors <- alignment.colors('protein')
        cols <- c(dna.colors,protein.colors,binary.colors)
      } else if (scheme == 'combined_codon') {
        dna.colors <- alignment.colors('dna')
                                        # Make the DNA stand out here by being a little darker
        for (i in 1:length(dna.colors)) {
          color <- dna.colors[i]
          darker.color <- darker(color)
          dna.colors[i] <- darker.color
        }
        binary.colors <- alignment.colors('binary')
        protein.colors <- alignment.colors('protein')
        codon.colors <- alignment.colors('codon')
                                        # Put them all together. (One remaining issue: the protein G,A,T,C will be colored as DNA!)
        cols <- c(dna.colors,protein.colors,binary.colors,codon.colors)
      }

      if (darken) {
        for (i in 1:length(cols)) {
          color <- cols[i]
          darker.color <- darker(color,0.85)
          cols[i] <- darker.color
        }
      }
      
      return(cols)
    }

    darker <- function(color,factor=0.7) {
      x <- col2rgb(color)
      x <- round(x * factor)
      y <- rgb(x[1],x[2],x[3],maxColorValue=255)
      return(y)
    }
    lighter <- function(color) {
      x <- col2rgb(color)
      x <- round(x * 1.2)
      y <- rgb(min(x[1],255),min(x[2],255),min(x[3],255),maxColorValue=255)
      return(y)
    }

    # Scores each column according to the branch length of the subtree created by
    # non-gap residues (we use nongap.bl as the var name), and stores the 'nongap.str'
    # which is the pasted list of labels for non-gapped sequences at this site.
    # This information is used by the 'remove.gaps' function to remove columns to
    # reach a certain alignment length threshold.
    score.aln.columns <- function(tree,aln) {
      aln.length <- length(aln[1,])

      score.df <- data.frame()
      for (i in 1:aln.length) {
        aln.column <- aln[,i]

        nongap.seqs <- names(aln.column[aln.column != '-'])
        gap.seqs <- names(aln.column[aln.column == '-'])

        # Get the non-gap branch length.
        if (length(nongap.seqs) == 1) {
          nongap.node <- node.with.label(tree,nongap.seqs[1])
          nongap.bl <- branch.length(tree,nongap.node)
        } else {
          nongap.tree <- drop.tip(tree,gap.seqs)
          nongap.bl <- sum(nongap.tree$edge.length)
        }

        nongap.str <- paste(nongap.seqs,collapse=';')
        cur.df <- data.frame(
          pos=i,
          score=nongap.bl,
          nongap.str=nongap.str,
          stringsAsFactors=F
        )
        score.df <- rbind(score.df,cur.df)
      }
      score.df <- score.df[order(score.df$score,score.df$pos),]
      return(score.df) 
    }

    # Goes through the alignment of the sim object and removes columns until either the desired
    # tolerance is reached or there are no more low-scoring columns to remove (whichever comes
    # first). Tolerance is defined as the ratio of the alignment length to the mean sequence
    # length. So, an alignment with no indels at all is exactly 1; an alignment with lots
    # of deletions (but no insertion) is less than 1; an alignment with lots of insertion is
    # greater than 1.
    #
    # Values of 0.9 - 1.3 tend to give good results in a variety of situations.
    #
    remove.gaps <- function(sim,tolerance) {
      aln <- sim$.alignment
      tree <- sim$.phylo
      col.scores <- score.aln.columns(tree,aln)

      # Store the deletion markers in a separate data frame.
      deletion.df <- NULL

      # Get the mean sequence length
      lengths <- apply(aln,1,function(x) {
        x <- x[x != '-']
        return(stringLength(paste(x,collapse='')))
      })
      mean.seq.length <- mean(lengths)

      repeat {
        aln.length <- length(aln[1,])  
        ratio <- aln.length / mean.seq.length
        if (ratio < tolerance) {
          break;
        }

        # Take the next site from the sorted scores
        lowest.scores <- col.scores[1,]
        col.scores <- col.scores[-c(1),]
        cur.pos <- lowest.scores$pos # Current position of lowest-scoring column.
        cur.score <- lowest.scores$score # The current column score.
        cur.str <- lowest.scores$nongap.str # The current column's nongap pattern.

        # Grab the entire 'current chunk' of alignment which has the same
        # score and non-gap string.
        repeat {
          first.pos <- col.scores[1,'pos']
          first.score <- col.scores[1,'score']
          first.str <- col.scores[1,'nongap.str']
          if (cur.score == max(col.scores$score)) {
            lowest.scores <- NULL
            cur.ratio <- length(aln[1,]) / mean.seq.length
            print(sprintf("Nothing left to remove at ratio %.2f!",cur.ratio))
            break;
          }
          if (first.pos == cur.pos + 1 && first.score == cur.score && first.str == cur.str) {
            cur.pos <- col.scores[1,]$pos
            lowest.scores <- rbind(lowest.scores,col.scores[1,])
            col.scores <- col.scores[-1,]
          } else {
            #print("Done!")
            break;
          }
        }

        if (is.null(lowest.scores)) {
          break;
        }

        # remove.us should be a contiguous vector of integers,
        # representing the set of columns to remove.
        remove.us <- lowest.scores$pos

        if (any(diff(remove.us) > 1)) {
          print("ERROR: Removing a non-consecutive set of columns!")
        }

        #print(paste("Removing at ",paste(remove.us[1])))

        # Go through columns from right to left, making sure to update
        # the new positions of columns on the right side of the splice.
        rev.pos <- rev(remove.us)
        for (i in 1:length(rev.pos)) {
          cur.pos <- rev.pos[i]
          aln <- splice.column(aln, cur.pos)

          # Update new positions of column scores
          above <- which(col.scores$pos > cur.pos)
          col.scores[above,]$pos <- col.scores[above,]$pos - 1

          # Update new positions of deletion locations.
          if (!is.null(deletion.df) && nrow(deletion.df) > 0) {
            above <- which(deletion.df$pos > cur.pos)
            deletion.df[above,]$pos <- deletion.df[above,]$pos - 1
          }
        }


        # Add a single entry to the data frame of deletions, to be used
        # by the plot function to indicate deletion points.
        cur.deletion <- data.frame(
          pos = cur.pos, # The first position AFTER the deletion splice.
          length = length(rev.pos), # The length of the block removed.
          nongap.str = as.character(cur.str), # nongap sequence IDs for this deletion
          stringsAsFactors=F
        )
        #print(cur.deletion)
        deletion.df <- rbind(deletion.df,cur.deletion)
      }

      # Create a new PhyloSim object, assign the tree & aln, and return.
      sim.temp <- PhyloSim();
      sim.temp$.alignment <- aln
      sim.temp$.phylo <- tree
      sim.temp$.indels <- deletion.df
      return(sim.temp)
    }

    splice.column <- function(aln,pos) {
      return(aln[,-pos])
    }
    
    ####################################
    ### Let the real plotting begin! ###
    ####################################
    
    # Apply the deletion tolerance if needed.
    if (!is.null(aln.length.tolerance)) {
      x <- remove.gaps(x, tolerance=aln.length.tolerance)
      indels <- x$.indels
    } else {
      indels <- NULL
    }

    df <- data.frame()
    aln <- x$.alignment
    phylo <- x$.phylo

                                        # Do some reordering of alignment & tree.
    if (!any(is.na(phylo))) {
      x$.phylo <- reorder(x$.phylo, order="cladewise");
      aln <- sort.aln.by.tree(aln,phylo)
    }

    names <- dimnames(aln)[[1]]
   
    #print(paste("Aln length:",length(aln[1,])))
    #print(paste("Num seqs:",length(names)))

   # Create a factor of all the characters in the alignment.
   char.levels <- sort(unique(as.vector(aln)))
   names.levels <- names
   
    for (i in 1:length(names)) {
      char.list <- aln[i,]
      name <- names[i]
                                        # Store the indices of where the gaps are -- we won't plot the gaps.
      gaps <- char.list == '-' 
      seq.pos <- seq(1,length(char.list))
                                        # Get the position and character of each non-gap residue.
      pos.nogaps <- seq.pos[gaps==FALSE]
      char.nogaps <- as.character(char.list[gaps==FALSE])
                                        # Create a data frame with 1 row per residue to plot.
      df <- rbind(df,data.frame(
                                id=factor(x=rep(name,length(pos.nogaps)),levels=names.levels),      # Sequence ID to which this residue belongs
                                seq_index=rep(i,length(pos.nogaps)),  # Index of the containing sequence
                                pos=pos.nogaps,                       # Alignment position
                                char=factor(x=char.nogaps,levels=char.levels)                      # Character of the residue
                                ))
    }
    
                                        # Turn the IDs into a factor to plot along the y axis.
    
    if (!plot.ancestors) {
                                        # Remove the ancestral nodes from the plot.
      tip.name.indices <- grep("node",names,ignore.case=TRUE,invert=TRUE)
      names <- names[tip.name.indices]
      df$id <- factor(df$id,levels=names)
      df <- subset(df,id %in% names)
    }

   df$type <- 'aln'
   
   ### Add indels to the data frame.
   if (!is.null(indels)) {
     # For each non-gap sequence of each chunk deleted, add a row
     # to the data frame.
     del.df <- data.frame()

     # For each position, store the count of indels in a track.
     max.pos <- max(c(df$pos,indels$pos))
     indel.histogram <- data.frame(
       pos=1:max.pos - 0.5,
       count=0,
       length=0
     )
     for (i in 1:nrow(indels)) {
       row <- indels[i,]
       seqs <- strsplit(row$nongap.str,";")[[1]]
       if (length(seqs) == 0) {
         # Edge case: an indel row came from a column with no aligned sequence,
         # so the 'nongap.str' is empty. Just continue on...
         next()
       }

       cur.df <- data.frame(
         id=seqs,
         pos=row$pos,
         length=row$length,
         type='indel'
       )
       del.df <- rbind(del.df,cur.df)

       # Tick up the histogram.
       indel.histogram[row$pos,]$count <- indel.histogram[row$pos,]$count + 1
       indel.histogram[row$pos,]$length <- indel.histogram[row$pos,]$length + row$length
     }

     # Sync the two data frame's columns, fill empty stuff with NAs.
     columns.from.df <- colnames(df)[!(colnames(df) %in% colnames(del.df))]
     columns.from.del <- colnames(del.df)[!(colnames(del.df) %in% colnames(df))]
     del.df[,columns.from.df] <- NA
     df[,columns.from.del] <- NA

     df <- rbind(df,del.df)

     # Transform the histogram and add it to our tracks.
     max.count <- max(indel.histogram$count)
     max.length <- max(indel.histogram$length)
     indel.histogram$y_lo <- 0
     indel.histogram$score <- indel.histogram$count / (max.count+1)
     indel.histogram$y_hi <- indel.histogram$score
     indel.histogram$id <- 'Hidden Indel Count'
     indel.histogram$height <- 5
     indel.histogram$layout <- 'below'
     indel.histogram$color.gradient <- 'darkblue,darkblue'
     indel.histogram$type <- 'track'

     #indel.len <- indel.histogram
     #indel.len$id <- 'Hidden Indel Length'
     #indel.len$score <- indel.len$length / (max.length+1)
     #indel.len$y_hi <- indel.len$score
     #indel.len$color.gradient <- 'darkgreen,darkgreen'
     
     if (!is.null(tracks)) {
       tracks <- c(tracks,list(indel.histogram))
     } else {
       tracks <- list(indel.histogram)
     }     
   }


   if (plot.nongap.bl && is.phylo(phylo)) {
     score.df <- score.aln.columns(phylo,aln)
     max.bl <- max(score.df$score)
     bl.track <- data.frame(
       id = 'Non-gap Branch Length',
       layout = 'below',
       pos = score.df$pos,
       score = score.df$score / max.bl * 0.9,
       y_hi = score.df$score / max.bl * 0.9,
       color.gradient = 'red,black,black'
     )
     if (!is.null(tracks)) {
       tracks <- c(tracks,list(bl.track))
     } else {
       tracks <- list(bl.track)
     }
   }
   
   # Track input format is a list of data frames with one row per feature to be
   # displayed, with the following columns. The only mandatory column is 'pos';
   # all others have sensible default values.
   # ---
   # [fields below are unique per row]
   # pos:     The sequence position (starting with 1) of the feature
   # score:   The score (between 0 and 1) of the feature. Scores above 1 or below
   #          zero will be truncated.
   # y_lo:    The lower Y offset (between 0 and 1) of the feature block.
   # y_hi:    The upper Y offset (between 0 and 1) of the feature block. These two
   #          values allow bars to be positioned along the y-axis.
   # [fields below are unique per track; the value from the first row is used.]
   # id:      The display ID for the track.
   # layout:  Set to 'above' to put the track above the alignment, 'below' for below.
   # height:  The number of alignment rows for the track to span in height.
   # color.gradient: A comma-separated list of colors to interpolate between when coloring
   #          the blocks. Examples: 'white,red' 'blue,gray,red' '#FF00FF,#FFFFFF'
   # color:   A single color to use for the track, no matter what the scores. Overridden
   #          color.gradient if both exist.
   # 
   # ---
   #
   
   # What we do is add the track data to the alignment data frame (so it gets
   # paged and scaled properly along with the alignment data) and then separate
   # it out before plotting, so it can be plotted separately from the alignment.
   df$track_index <- -1

   if (!is.null(tracks)) {
     df$score <- NA
     i <- 0
     for (track in tracks) {
       i <- i + 1
       track$track_index <- i
       track$type <- 'track'
       
       # Add default values.
       if (is.null(track$background)) {
         track$background <- 'white'
       }
       if (length(track$pos) == 0) {
         # If no positions are included in the data frame, set the first position
         # to 1 and put the foreground color same as the background.
         # This has the effect of creating a 'spacer' row.
         track$pos <- 1
         track$color <- track[1,]$background
       }
       if (is.null(track$layout)) {
         track$layout <- 'above'
       }
       if (is.null(track$color.gradient)) {
         if (!is.null(track$color)) {
           # No color gradient exists, but we have 'color' instead. Use that.
           track$color.gradient <- paste(track[1,]$color, track[1,]$color, sep=',')
         } else {
           # No color.gradient OR color value exists, so use the default white-to-black.
           track$color.gradient <- 'white,black'
         }
       }
       if (is.null(track$score)) {
         track$score <- 1
       }
       if (is.null(track$y_lo)) {
         track$y_lo = 0
       }
       if (is.null(track$y_hi)) {
         track$y_hi = 1
       }
       if (is.null(track$id)) {
         track$id <- paste('Track',i)
       }
       if (is.null(track$height) || is.na(track$height)) {
         track$height <- 4
       }

       # Ensure that we don't have positions at zero or below.
       track[track$pos <= 0,'pos'] <- 1

       # Limit score range
       track$score <- pmin(track$score,1)
       track$score <- pmax(track$score,0)
       
       # Sync the two data frame's columns, fill empty stuff with NAs.
       columns.from.aln <- colnames(df)[!(colnames(df) %in% colnames(track))]
       columns.from.track <- colnames(track)[!(colnames(track) %in% colnames(df))]
       track[,columns.from.aln] <- NA
       df[,columns.from.track] <- NA

       track$type <- 'track'
       df <- rbind(df,track)
     }
   }

   if (num.pages != 'auto') {
     num.pages <- as.numeric(num.pages)
   }

    aln.length <- max(df$pos)
    chars.per.page <- aln.length
    num.seqs <- length(names)
   
    if (tolower(num.pages) == 'auto' || num.pages > 1) {
                                        # Plotting a multi-page alignment.
      if (tolower(num.pages) == 'auto') {

        # If we have tracks, add the total track height to the 'num.seqs' variable.
        track.rows <- subset(df,type == 'track')
        if (nrow(track.rows) > 0) {
          track.indices <- sort(unique(track.rows$track_index))
          for (track.index in track.indices) {
            sub.track <- subset(track.rows,track_index==track.index)
            if (nrow(sub.track) > 0) {
              num.seqs <- num.seqs + sub.track[1,]$height 
            }
          }
        }
                                        # Formula to get a square-ish total plot.
        num.pages <- sqrt(aln.length/num.seqs)
        num.pages <- ceiling(num.pages)+1
                                        # One-page rule for short alignments.
        if (aln.length < 30) {num.pages <- 1}
      }
                                        # Add a 'page' factor to the data frame and use facet_grid.
      chars.per.page <- ceiling(aln.length / num.pages)
      df$page <- floor((df$pos-1) / chars.per.page) + 1
      if (nrow(subset(df,page <= 0)) > 0) {
        df[df$page <= 0,]$page <- 1  # Fix errors where pos=0.5 goes to page 0
      }
      df$pos <- df$pos - (chars.per.page*(df$page-1))
      page.labels <- paste((0:(num.pages-1))*chars.per.page+1,(1:num.pages)*chars.per.page,sep="-")
      page.numbers <- sort(unique(df$page))
      page.labels <- page.labels[page.numbers]
      df$page <- factor(df$page,levels=page.numbers,labels=page.labels)

      num.pages <- length(page.labels)
      #print(paste("Num pages:",num.pages))

    } else {
      # We've only got one page. Create a factor...
      df$page <- 1
      df$page <- factor(df$page,levels=c(1),labels=c('1'))

      # Store some values which will be used later.
      aln.length <- max(df$pos)
      chars.per.page <- aln.length
    }

    if (color.scheme == 'auto') {
      all.chars <- unlist(aln)
      all.chars <- all.chars[all.chars != '-']
      n.chars <- length(unique(toupper(all.chars)))

      dna <- c('a','t','g','c')
      dna <- c(dna,toupper(dna))

      protein <- letters
      protein <- protein[!(protein %in% c('b','j','o','u','x','z'))]
      protein <- c(protein,toupper(protein))

      ca <- CodonAlphabet()
      nucs <- c('G','A','C','T')
      codon.grid <- expand.grid(nucs,nucs,nucs,stringsAsFactors=F)
      codons <- c()
      for (i in 1:nrow(codon.grid)) {
        codons <- c(codons,paste(codon.grid[i,],collapse=''))
      }
      if(any(all.chars %in% codons)) {
                                        # If we see any codon alphabets, use the combined_codon
        color.scheme <- 'combined_codon'
      } else {      
                                        # Else just use the combined color scheme. It's good enough!
        color.scheme <- 'combined'
      }
    }

    legend.title <- color.scheme

    # Remove any tracks from the main aln data frame.
   tracks <- subset(df,type=='track')
   indels <- subset(df,type=='indel')
   df <- subset(df,type=='aln')

   # Set positional values in the aln data frame.
   df$xx <- df$pos
   df$yy <- as.numeric(df$id)
   df$xmin <- df$xx - .5
   df$xmax <- df$xx + .5
   df$ymin <- df$yy - .5
   df$ymax <- df$yy + .5

   darken.colors <- FALSE
   if (nrow(indels) > 0) {
     darken.colors <- TRUE
   }
   
   color.map <- alignment.colors(color.scheme,darken=darken.colors)
   df$colors <- color.map[as.character(df$char)]
   
   # Set the base for y-axis configurations.
   y.lim <- c(0,length(names)+1)
   y.breaks <- 1:length(names)
   y.labels <- names
   
   # Create the ggplot panel.
   p <- ggplot(df,aes(x=xx,y=yy,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))
   p <- p + geom_rect(aes(fill=colors))

   if (!is.null(aln.xlim)) {
     aln.limits <- aln.xlim
   } else {
     aln.limits <- c(0,max(df$pos)+1)
   }
   
   if (nrow(tracks) > 0) {
     # Handle tracks.

     # These variables hold the current above and below offsets.
     cur.y.above <- length(names) + 1 - .5
     y.lim[2] <- length(names) + 1 - .5
     cur.y.below <- 0.5

     bg.out <- data.frame()
     track.out <- data.frame()
     track.indices <- sort(unique(tracks$track_index))
     for (track.index in track.indices) {
       sub.track <- subset(tracks,track_index==track.index)

       # Collect the track-wide features.
       track.id <- sub.track[1,]$id
       track.height <- sub.track[1,]$height
       track.layout <- sub.track[1,]$layout
       color.gradient <- strsplit(sub.track[1,]$color.gradient,',')[[1]]
       track.bg <- sub.track[1,]$background
       track.ramp <- colorRamp(colors=color.gradient)
       sub.track$colors <- rgb(track.ramp(sub.track$score),maxColorValue=255)

       sub.track$xx <- sub.track$pos
       sub.track$xmin <- sub.track$pos-.5
       sub.track$xmax <- sub.track$pos+.5

       if (track.layout == 'below') {
         sub.track$yy <- cur.y.below
         sub.track$ymin <- cur.y.below - track.height
         sub.track$ymax <- cur.y.below

         y.lim[1] <- cur.y.below - track.height
         y.breaks <- c(y.lim[1] + track.height/2,y.breaks)
         y.labels <- c(as.character(track.id),y.labels)

         # Temporarily store the current min and max for y.
         cur.y.min <- cur.y.below - track.height
         cur.y.max <- cur.y.below

         # Add our track height to the state variable.
         cur.y.below <- cur.y.below - track.height
       } else {
         sub.track$yy <- cur.y.above
         sub.track$ymin <- cur.y.above
         sub.track$ymax <- cur.y.above + track.height
         
         y.lim[2] <- cur.y.above + track.height
         y.breaks <- c(y.breaks,y.lim[2] - track.height/2)
         y.labels <- c(y.labels,as.character(track.id))

         # Temporarily store the current min and max for y.
         cur.y.min <- cur.y.above
         cur.y.max <- cur.y.above + track.height

         # Add our track height to the state variable.
         cur.y.above <- cur.y.above + track.height
       }

       # Adjust bar position if we have y_lo and y_hi values.
       if(length(sub.track$y_lo) > 0) {
         sub.track$ymin <- cur.y.min + track.height*sub.track$y_lo
       }
       if (length(sub.track$y_hi) > 0) {
         sub.track$ymax <- cur.y.min + track.height*sub.track$y_hi
       }

       track.out <- rbind(track.out,sub.track)

       # Create a background rectangle for each page, to place behind the current track.
       pages <- sort(unique(df$page))
       page.indices <- sort(unique(as.integer(df$page)))
       for (page.index in 1:length(pages)) {
         pg <- pages[page.index]
         current.track.bg <- data.frame(
           page=pg,
           colors=track.bg,
           xx=0.5,
           yy=cur.y.min,
           xmin=0.5,
           xmax=chars.per.page + 0.5,
           ymin=cur.y.min,
           ymax=cur.y.max
         )
         bg.out <- rbind(bg.out,current.track.bg)
       }
     }

     # Add layers for the backgrounds and tracks.
     p <- p + geom_rect(aes(fill=colors),data=bg.out)
     p <- p + geom_rect(aes(fill=colors),data=track.out)
   }

   if (nrow(indels) > 0) {
     # Plot indels as small bars on top of characters.
     indel.width <- 0.25
     indels$pos <- indels$pos - .5 # Position indel on seq boundary.
     indels$xx <- indels$pos
     indels$yy <- as.numeric(indels$id)
     indels$xmin <- indels$xx - indel.width
     indels$xmax <- indels$xx + indel.width
     indels$ymin <- indels$yy - .5
     indels$ymax <- indels$yy + .5
     indels$colors <- 'black'

     p <- p + geom_rect(aes(fill=colors),data=indels)
   }
   
   #color.map <- alignment.colors(color.scheme)
   p <- p + scale_fill_identity()

   p <- p + scale_x_continuous(limits=aln.limits,expand=c(0,0))
   
   if (plot.labels) {
     p <- p + scale_y_continuous(limits=y.lim,breaks=y.breaks,labels=y.labels,expand=c(0,0))
   } else {
     p <- p + scale_y_continuous(limits=y.lim,breaks=y.breaks,labels=rep('',length(y.labels)),expand=c(0,0))
   }

   if (aspect.ratio) {
      axis.text.size <- 5
      char.text.size <- 2
   }
   
    if (num.pages > 1) {
      p <- p + facet_grid(page ~ .)
    }

    if (char.text.size == 'auto') {
      char.text.size <- 125 / (num.pages * (num.seqs+1))
      char.text.size <- min(char.text.size,10)
      char.text.size <- max(char.text.size,1)
    }
   
    if (plot.chars) {
      p <- p + geom_text(aes(label=char),colour='black',size=char.text.size)
    }    
    if (aspect.ratio) {
      p <- p + coord_equal(ratio=aspect.ratio)
    }

    if (axis.text.size == 'auto') {
      axis.text.size <- 500 / (num.pages * (num.seqs+1))
      axis.text.size <- min(axis.text.size,10)
      axis.text.size <- max(axis.text.size,1)
    }
   
    plot.theme <- theme(
		  axis.text.y = element_text(size=axis.text.size,hjust=1),
		  axis.title.x = element_blank(),
		  axis.title.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                      
                  plot.margin = unit(c(0,0,0,0),'npc')
                  )
    p <- p + plot.theme

    if (!plot.legend) {
       p <- p + theme(legend.position='none')
    }
    
    if (plot.tree) {      
      if (plot.ancestors) {
        tree.df <- phylo.layout.df(phylo,layout.ancestors=TRUE,align.seq.names=names)
      } else {
        tree.df <- phylo.layout.df(phylo,align.seq.names=names)
      }

      if (num.pages > 1) {
        tree.df$page <- 1
        df.copy <- tree.df
        for (i in 2 : num.pages) {
          df.copy$page <- i
          tree.df <- rbind(tree.df,df.copy)
        }
      }

      max.length <- max.length.to.root(phylo)
      aln.length <- length(aln[1,])
      n.leaves <- length(names)
      
      if (max(tree.df$event.count) > 0) {
        #print(paste("Coloring tree by",color.branches))
        q <- ggplot(tree.df,aes(colour=event.count))
        q <- q + scale_colour_gradient()
      } else {
        q <- ggplot(tree.df)
      }

      q <- q + geom_segment(aes(x=x,y=y,xend=xend,yend=yend))
      if (plot.labels) {
        q <- q + scale_y_continuous(limits=y.lim,breaks=y.breaks,labels=y.labels,expand=c(0,0))
      } else {
        q <- q + scale_y_continuous(limits=y.lim,breaks=y.breaks,labels=rep('',length(y.labels)),expand=c(0,0))
      }
      if (!is.null(tree.xlim)) {
        tree.limits <- tree.xlim
      } else {
        tree.limits <- c(0,max.length)
      }
      q <- q + scale_x_continuous(limits=tree.limits,expand=c(0.05,0))
      q <- q + plot.theme
#      q <- q + theme(plot.margin = unit(c(0,0,0,0),'npc'))
      if (num.pages > 1) {
        q <- q + facet_grid(page ~ .)
        q <- q + theme(strip.text.y=element_blank())
      }

      if (!plot.legend) {
        q <- q + theme(legend.position='none')
      }

      if (aspect.ratio) {
        warning("Aspect ratio set while plotting tree -- the alignment and tree won't line up!")
      }
      
      vplayout(4,1)
      print(p,vp=subplot(2:4,1))
      print(q,vp=subplot(1,1))
    } else {
      p <- p + plot.theme
      print(p)
    }
    
    # this method has no meaningful return value
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: summary.PhyloSim
##
###########################################################################/**
#
# @RdocMethod summary
#
# @title "Summarize the properties of an object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{object}{An object}
#       \item{...}{Not used.}
# }
#
# \value{
#  Returns a PSRootSummary object.
# }
#
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#	sim<-PhyloSim(
#		name="TinySim",
#		phylo=rcoal(3),
#		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
#	);
#       # get a summary
#       summary(sim)
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "summary",
  class="PhyloSim",
  function(
    object,
    ...
  ){

     this<-object;
     this$.summary$"Name"<-this$name;
     this$.summary$"Id"<-this$id;

		 if(is.Sequence(this$rootSeq)){
			root.seq<-this$rootSeq$id;
		 } else {
		 	this$.summary$"Root Sequence"<-"undefined";
		 }
		 if(is.Sequence(this$rootSeq)){
		 	this$.summary$"Root Sequence big rate"<-this$rootSeq$bigRate;
		 }
		 if(is.phylo(this$.phylo)){

     	this$.summary$"Tree length"<-this$treeLength;
		 	phylo.details<-grep(pattern="[[:alnum:]]+",x=capture.output(print(this$.phylo)),perl=TRUE,value=TRUE);
		 	phylo.details<-paste("\n",phylo.details,collapse="",sep="\t");
			this$.summary$"Phylo object details"<-phylo.details;

		 } else {
			this$.summary$"Phylo object details"<-"undefined";
		 }

		 aln<-"undefined";
		 if(is.matrix(this$alignment)){
				aln<-"defined";	
		 }
     this$.summary$"Alignment"<-aln;
     this$.summary$"Log file"<-this$.log.file;
     this$.summary$"Log level"<-this$.log.level;

     NextMethod();


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##### Logging Methods #####

##	
## Method: getLogFile
##	
###########################################################################/**
#
# @RdocMethod getLogFile
# 
# @title "Get the name of the file used for logging" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# Create a PhyloSim object
#	sim<-PhyloSim();
#	# get the name of the log file		
#	getLogFile(sim)	
#	# modify log file name
#	setLogFile(sim,"OldLog.txt")
#	# get/set log file name via virtual field
#	sim$logFile
#	sim$logFile<-"NewLog"
#	sim$logFile
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"getLogFile", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.log.file;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLogFile
##	
###########################################################################/**
#
# @RdocMethod setLogFile
# 
# @title "Set the name of the file used for logging" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{value}{The name of the file used for logging.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new logfile.
# } 
# 
# \examples{
#	# Create a PhyloSim object
#	sim<-PhyloSim();
#	# get the name of the log file		
#	getLogFile(sim)	
#	# modify log file name
#	setLogFile(sim,"OldLog.txt")
#	# get/set log file name via virtual field
#	sim$logFile
#	sim$logFile<-"NewLog"
#	sim$logFile
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"setLogFile", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			if(missing(value)){
				throw("No value provided!\n");
			}
			value<-as.character(value);
			if( length(value) != 1 ){
				throw("The new value must be a character vector of length 1!\n");
			}
			else{ 
				if( file.access(value,mode=0) == c(0) ){
					warning("The specified file already exists and it will be overwritten during simulation!\n");
				}
				this$.log.file<-value;

			}
			return(this$.log.file);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getLogLevel
##	
###########################################################################/**
#
# @RdocMethod getLogLevel
# 
# @title "Get the log level from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The log level as an integer vector of length one.
# } 
# 
# \examples{
#	# Create a PhyloSim object
#	sim<-PhyloSim();
#	# get/set log level
#	getLogLevel(sim)
#	setLogLevel(sim,0)
#	# set/get log level via virtual field
#	sim$logLevel<- -1
#	sim$logLevel
#	# clean up
#	unlink(sim$logFile)
# } 
# 
# @author 
# 
# \seealso{ 
# 	setLogLevel PhyloSim 
# } 
# 
#*/###########################################################################
setMethodS3(
	"getLogLevel", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.log.level;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLogLevel
##	
###########################################################################/**
#
# @RdocMethod setLogLevel
# 
# @title "Set the log level for a given PhyloSim object" 
# 
# \description{ 
#	@get "title".
#	
#	No logging is performed if the log level is negative. If the log level is zero, the messages passed to 
#	the \code{Log} method will be writen in the log file. If the log level is positive, the messages passed to
#	the \code{Debug} method are saved as well.
#
#	The default log level is -1. The specified file will be truncated in the case it already exists.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{value}{The new log level as an integer.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new level as an integer vector of length one.
# } 
# 
# \examples{
#	# Create a PhyloSim object
#	sim<-PhyloSim();
#	# get/set log level
#	getLogLevel(sim)
#	setLogLevel(sim,0)
#	# set/get log level via virtual field
#	sim$logLevel<- -1
#	sim$logLevel
# } 
# 
# @author 
# 
# \seealso{ 
# 	getLogLevel PhyloSim 
# } 
# 
#*/###########################################################################
setMethodS3(
	"setLogLevel", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			if(missing(value)){
				throw("No value provided!\n");
			}
			if((!is.numeric(value)) | length(value) != 1 ){
				throw("The new value must be a numeric vector of length 1!\n");
			}
			else{ 
    			# Create/wipe out log file.
				if(value >= 0 ){
						if(file.access(this$.log.file,mode=0) == c(0)){
							warning("The log file already existed and it was wiped out!\n");
						}
						# Creating the associated connection:
						this$.log.connection<-file(paste(this$.log.file),"w+");
				}
				this$.log.level<-value;
			}
			return(this$.log.level);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .getMessageTemplate
##	
setMethodS3(
	".getMessageTemplate", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		template<-list(
			time=paste("[",Sys.time(),"]",sep=""),
			level="Info",
			event=""
		);

		return(template);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .logMessage
##	
setMethodS3(
	".logMessage", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){

			if(missing(message)){
				throw("No message given!\n");
			}
			else if (!is.list(message)){
				throw("The message should be a list");
			}
			else if( length(intersect(names(message),c("time","level","event"))) != 3){
				throw("The \"time\", \"level\" and \"event\" elements are mandatory in the message list!\n");
			}
			else {
				writeLines(paste(message[["time"]]," "),con=this$.log.connection,sep="");
				message[["time"]]<-NULL;
				writeLines(paste(message[["level"]]," "),con=this$.log.connection,sep="");
				message[["level"]]<-NULL;
				writeLines(paste(message[["event"]]," "),con=this$.log.connection,sep="");
				message[["event"]]<-NULL;
				writeLines(paste(message,collapse=", "),con=this$.log.connection,sep="");
				writeLines("\n",con=this$.log.connection,sep="");
				return(TRUE);
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: Log
##	
###########################################################################/**
#
# @RdocMethod Log
# 
# @title "Save a message in the PhyloSim log file" 
# 
# \description{ 
#	@get "title".
#
#	The message is written to the log file only if the log level is non-negative. You can use this method for logging
#	in the case you write classes for PhyloSim.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{message}{A character vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The message (invisible).
# } 
# 
# \examples{
#	# create a PhyloSim object,
#	# with logLevel set to zero	
#	sim<-PhyloSim(log.level=0);
#	# log a message
#	Log(sim,"Hiya there!");
#	# close log connection
#	close(sim$.log.connection)
#	# print out the log file
#	cat(paste(scan(file=sim$LogFile,what=character(),sep="\n"),collapse="\n"));cat("\n");
#	# clean up
#	unlink(sim$logFile)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"Log", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){
	
			if(this$.log.level < 0){
				return(invisible(FALSE))
			}
			if(missing(message)){
				throw("No message given!\n");
			} else {
				template<-.getMessageTemplate(this);
				template$level<-"Info";
				message<-c(template,as.list(message));
				.logMessage(this, message);
				return(invisible(message));
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: Debug
##	
###########################################################################/**
#
# @RdocMethod Debug
# 
# @title "Save a debug message in the PhyloSim log file" 
# 
# \description{ 
#	@get "title".
#
#	The debug message is written to the log file only if the log level is non-negative. You can use this method for logging
#	debug messages in the case you write classes for PhyloSim.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{message}{A character vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The message (invisible).
# } 
# 
# \examples{
#	# create a PhyloSim object,
#	# with logLevel set to zero	
#	sim<-PhyloSim(log.level=0);
#	# log a debug message
#	Debug(sim,"Some useful detail...");
#	# close log connection
#	close(sim$.log.connection)
#	# print out the log file
#	cat(paste(scan(file=sim$LogFile,what=character(),sep="\n"),collapse="\n"));cat("\n");
#	# clean up
#	unlink(sim$logFile)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"Debug", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){
		
			if(missing(message)){
				throw("No message given!\n");
			} 
			else if( this$.log.level <= 0){
				return(invisible(FALSE))
			}
			else {
				template<-.getMessageTemplate(this);
				template$level<-"DEBUG";
				message<-c(template,as.list(message));
				.logMessage(this, message);
				return(invisible(message));
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .UpdateBranchStats
##	
setMethodS3(
	".UpdateBranchStats", 
	class="PhyloSim", 
	function(
		this,
		event,
		details,
		branch.number,
		...
	){

		if(details$type == "substitution"){
			
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$substitution)){
				this$.branch.stats[[as.character(branch.number)]]$substitution<-1;
			} else {
			this$.branch.stats[[as.character(branch.number)]]$substitution<-(this$.branch.stats[[as.character(branch.number)]]$substitution + 1);
			}
			name<-event$name;
	
			if(is.null(this$.branch.stats[[as.character(branch.number)]][[name]])){
				this$.branch.stats[[as.character(branch.number)]][[name]]<-1;
			}
			else {
				this$.branch.stats[[as.character(branch.number)]][[name]]<-(this$.branch.stats[[as.character(branch.number)]][[name]] + 1);
			}
		
			# Special stuff for the GY94 codon model:	
			if(is.GY94(event$.process)){
				# Increment synonymous counter:
				if(event$.type == "synonymous"){
					if(is.null(this$.branch.stats[[as.character(branch.number)]][["nr.syn.subst"]])){
						# First event of this type on this branch, initialize the list element to 1.
						this$.branch.stats[[as.character(branch.number)]][["nr.syn.subst"]]<-1;
					}
					else {
					this$.branch.stats[[as.character(branch.number)]][["nr.syn.subst"]]<-(this$.branch.stats[[as.character(branch.number)]][["nr.syn.subst"]] + 1);
					}
				}
				# Increment non-synonymous counter:
				else if(event$.type == "non-synonymous"){
					if(is.null(this$.branch.stats[[as.character(branch.number)]][["nr.nsyn.subst"]])){
						# First event of this type on this branch, initialize the list element to 1.
						this$.branch.stats[[as.character(branch.number)]][["nr.nsyn.subst"]]<-1;
					}
					else {
						this$.branch.stats[[as.character(branch.number)]][["nr.nsyn.subst"]]<-(this$.branch.stats[[as.character(branch.number)]][["nr.nsyn.subst"]] + 1);
					}
				} else {
					throw("The event generated by the GY94 has no type!\n");
				}

			}

		}
		else if(details$type == "deletion"){
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$deletion)){
				this$.branch.stats[[as.character(branch.number)]]$deletion<-1;
			}
			else {
				this$.branch.stats[[as.character(branch.number)]]$deletion<-(this$.branch.stats[[as.character(branch.number)]]$deletion + 1);
			}
		}
		else if(details$type == "insertion"){
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$insertion)){
			this$.branch.stats[[as.character(branch.number)]]$insertion<-1;					
			}
			else {
			this$.branch.stats[[as.character(branch.number)]]$insertion<-(this$.branch.stats[[as.character(branch.number)]]$insertion + 1);					

			}
		}
		else {
			throw("Invalid event type!\n");
		}
			

	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBranchEvents
##	
###########################################################################/**
#
# @RdocMethod getBranchEvents
# 
# @title "Get the list of events having per-branch statistics recorded" 
# 
# \description{ 
#	@get "title".
#
#	During simulation the number of events performed on every branch is recorded. The recorded events can be "basic"
#	events, like "insertion", "deletion" and "A->T" or events which are sums of basic events, like "substituion". The 
#	\code{getBranchEvents} method returns a character vector with the names of the events having per-branch 
#	statistics recorded. The method should be called after the simulation is finished.
#
#	The per-branch statistics can be exported as phylo objects by using the \code{exportStatTree} method.
#	The branch lengths of the exported phylo objects are set to the value of the respective per-branch event count.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector.
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#
#	# NOTE: this will be a little bit slow
#	sim<-PhyloSim(
#		phylo=rcoal(3),
#		root.seq=CodonSequence(
#                               string="ATGATTATT",
#                               processes=list(list(GY94(kappa=2,omega.default=0.5))))
#	);
#	# make the tree longer to have more events
#	scaleTree(sim,5)
#	# plot the tree
#	plot(sim)
#	# run simulation
#	Simulate(sim)
#	# get the list of recorded per-branch event counts
#	getBranchEvents(sim)
#	# export the number of subtitions as a phylo object
#	subst<-exportStatTree(sim,"substitution")
#	# plot the exported phylo object
#	plot(subst)
#	#export the number of synonymous substitutions as a phylo object
#	subst<-exportStatTree(sim,"nr.syn.subst")
#	# plot the exported phylo object
#	plot(subst)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"getBranchEvents", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		tmp<-character();
		for(branch in this$.branch.stats){
			tmp<-c(tmp,names(branch));
		}
		return(unique(sort(tmp)));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBranchEvents
##	
###########################################################################/**
#
# @RdocMethod setBranchEvents
#
# @title "Forbidden action: setting the list of events having per-branch statistics recorded"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
	"setBranchEvents", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			virtualAssignmentForbidden(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: exportStatTree
##	
###########################################################################/**
#
# @RdocMethod exportStatTree
# 
# @title "Export the per-branch counts of an event as a phylo object" 
# 
# \description{ 
#	@get "title".
#
#	During simulation the number of events performed on every branch is recorded. The recorded events can be "basic"
#	events, like "insertion", "deletion" and "A->T" or events which are sums of basic events, like "substituion". The 
#	\code{getBranchEvents} method returns a character vector with the names of the events having per-branch 
#	statistics recorded. The method should be called after the simulation is finished.
#
#	The per-branch statistics can be exported as phylo objects by using the \code{exportStatTree} method.
#	The branch lengths of the exported phylo objects are set to the value of the respective per-branch event count.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{event}{The name of the event as returned by the \code{getBranchEvents} method.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A phylo object.
# } 
# 
# \examples{
#	# Create a PhyloSim object.
#	# Provide the phylo object 
#	# and the root sequence.
#
#	# NOTE: this will be a little bit slow
#	sim<-PhyloSim(
#		phylo=rcoal(3),
#		root.seq=CodonSequence(
#                               string="ATGATTATT",
#                               processes=list(list(GY94(kappa=2,omega.default=0.5)))
#                               )
#	);
#	# make the tree longer to have more events
#	scaleTree(sim,5)
#	# plot the tree
#	plot(sim)
#	# run simulation
#	Simulate(sim)
#	# get the list of recorded per-branch event counts
#	getBranchEvents(sim)
#	# export the number of substitutions as a phylo object
#	subst<-exportStatTree(sim,"substitution")
#	# plot the exported phylo object
#	plot(subst)
#	#export the number of synonymous substitutions as a phylo object
#	subst<-exportStatTree(sim,"nr.syn.subst")
#	# plot the exported phylo object
#	plot(subst)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"exportStatTree", 
	class="PhyloSim", 
	function(
		this,
		event,
		...
	){

 		if(!is.matrix(this$.alignment)){
      			throw("Simulation is not complete, cannot export statistics!\n");
    		}
		else if(missing(event)){
			throw("No event name specified!\n");
		}
		else if(length(intersect(event, this$branchEvents)) != 1 ){
			throw("Invalid event name!");
		}
		else {

			phylo.copy<-this$phylo;
			phylo.copy$edge.length<-.getStatBrlen(this, event);
			return(phylo.copy);		
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .getStatBrlen
##	
setMethodS3(
	".getStatBrlen", 
	class="PhyloSim", 
	function(
		this,
		event,
		...
	){

		tmp<-numeric();
		for(i in dimnames(this$edges)[[1]]){
				if(is.null(this$.branch.stats[[i]][[event]])){
					tmp[[as.numeric(i)]]<-0;
				}
				else {
					tmp[[as.numeric(i)]]<-this$.branch.stats[[i]][[event]];
				}
		}
		return(tmp);


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##### Phylo object interface methods  #####

##
## Method: getEdges
##
###########################################################################/**
#
# @RdocMethod	getEdges
# 
# @title "Get the edge matrix from a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	The rows of the edge matrix contain the nodes connected by the edge and the edge length.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the edge matrix
#	getEdges(sim)
#	# get the edge matrix via virtual field
#	sim$edges
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getEdges",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
			   if(length(this$.phylo$edge.length) > 2){
				if(attr(this$.phylo, "order") != "cladewise"){
					throw("The order of the phylo object is not cladewise! Someone must have been messing with that!\n");
				}
			   }
				tmp<-cbind(this$.phylo$edge,this$.phylo$edge.length);
				colnames(tmp)<-c("from","to","length");
				rownames(tmp)<-1:dim(tmp)[[1]];
				return(tmp);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setEdges
##
###########################################################################/**
#
# @RdocMethod setEdges
#
# @title "Forbidden action: setting the edge matrix for a phylo object aggregated by a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setEdges",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNtips
##
###########################################################################/**
#
# @RdocMethod	getNtips
# 
# @title "Get the number of the tips form a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the number of tips
#	getNtips(sim)
#	# get the number of tips via virtual field
#	sim$ntips
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getNtips",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
								return(length(this$.phylo$tip.label));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNtips
##
###########################################################################/**
#
# @RdocMethod setNtips
#
# @title "Forbidden action: setting the number of the tips for a phylo object aggregated by a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setNtips",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTipLabels
##
###########################################################################/**
#
# @RdocMethod getTipLabels
# 
# @title "Get the tip labels from a phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the tip labels.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the tip labels
#	getTipLabels(sim)
#	# get the tip lables via virtual field
#	sim$tipLabels
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getTipLabels",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					tmp<-rbind(this$.phylo$tip.label);
					rownames(tmp)<-c("Labels:");
					colnames(tmp)<-c(1:length(tmp));
					return(tmp);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTipLabels
##
###########################################################################/**
#
# @RdocMethod setTipLabels
#
# @title "Forbidden action: setting the tip labels for a phylo object aggregated by a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setTipLabels",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNodes
##
###########################################################################/**
#
# @RdocMethod getNodes
# 
# @title "Get the node identifiers from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the node IDs
#	getNodes(sim)
#	# get the node IDs via virtual field
#	sim$nodes
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getNodes",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# This is dumb but safe:
					return(sort(unique(as.vector(this$.phylo$edge))));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNedges
##
###########################################################################/**
#
# @RdocMethod	getNedges
# 
# @title "Get the number of edges from phylo object aggregated by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the number of the edges
#	getNedges(sim)
#	# get the number of the edges via virtual field
#	sim$nedges
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getNedges",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					return(dim(this$.phylo$edge)[[1]]);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNedges
##
###########################################################################/**
#
# @RdocMethod setNedges
#
# @title "Forbidden action: setting the number of edges for phylo object aggregated by a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setNedges",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNodes
##
###########################################################################/**
#
# @RdocMethod setNodes
#
# @title "Forbidden action: setting the node identifiers for a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setNodes",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTips
##
###########################################################################/**
#
# @RdocMethod	getTips
# 
# @title "Get the node identifiers of the tip nodes from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the tip IDs
#	getTips(sim)
#	# get the tip IDs via virtual field
#	sim$tips
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getTips",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# This is dumb but safe:
					#return(sort(unique(as.vector(this$.phylo$edge))));
					return(1:(getNtips(this)));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTips
##
###########################################################################/**
#
# @RdocMethod setTips
#
# @title "Forbidden action: setting the node identifiers of the tip nodes for a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setTips",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getRootNode
##
###########################################################################/**
#
# @RdocMethod getRootNode
# 
# @title "Get the identifier of the root node from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the root node ID
#	getRootNode(sim)
#	# get the root node ID via virtual field
#	sim$rootNode
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getRootNode",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# Relying on cladewise order:
					return(this$.phylo$edge[1,1]);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setRootNode
##
###########################################################################/**
#
# @RdocMethod setRootNode
#
# @title "Forbidden action: setting the identifier of the root node for a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setRootNode",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: is.tip
##
###########################################################################/**
#
# @RdocMethod is.tip
# 
# @title "Check if a node is a tip" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{node}{A node identifier (integer vector of length one).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# check if node 4 is a tip
#	is.tip(sim,4)
#	# check if node 6 is a tip
#	is.tip(sim,6)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "is.tip",
  class="PhyloSim",
  function(
    		this,
		node=NA,
    		...
  ){

		if(missing(node)){
			throw("No node number specified!\n");
		}
		else if(!is.numeric(node)){
			throw("The node number must be numeric!\n");
		}
		else {
			return(round(node) <= this$ntips);
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getEdge
##
###########################################################################/**
#
# @RdocMethod getEdge
# 
# @title "Get and edge from the edge matrix" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{number}{The edge number.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The edge as a matrix with a single row.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get edge number 3
#	getEdge(sim,3)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getEdge",
  class="PhyloSim",
  function(
    this,
    number=NA,
    ...
  ){

    if(missing(number)){
      throw("No object provided!\n");
    }
    else if(!is.numeric(number)){
      throw("The edge number must be numeric!\n");
    }
    else {
				number<-round(number);
				tmp<-rbind(c(this$.phylo$edge[number,],this$.phylo$edge.length[number]));
				colnames(tmp)<-c("from","to","length");
				rownames(tmp)<-c("Edge:");
				return(tmp);

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTreeLength
##
###########################################################################/**
#
# @RdocMethod getTreeLength
# 
# @title "Get the tree length from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	This method retruns the sum of the edge lengths stored in the aggregated phylo object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the tree length
#	getTreeLength(sim)
#	# get tree length via virtual field
#	sim$treeLength
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getTreeLength",
  class="PhyloSim",
  function(
    this,
    ...
  ){
		
		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
				return(sum(this$.phylo$edge.length));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTreeLength
##
###########################################################################/**
#
# @RdocMethod setTreeLength
#
# @title "Forbidden action: setting the tree length for a PhyloSim object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	Throws an error.
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "setTreeLength",
  class="PhyloSim",
  function(
    this,
		value,
    ...
  ){
		
		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: scaleTree
##
###########################################################################/**
#
# @RdocMethod scaleTree
# 
# @title "Scale the branch lengths of a phylo object aggragted by a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#	This method multiples all the edge lengths by the specified factor.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{factor}{A numeric vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# create a PhyloSim object
#	sim<-PhyloSim(phylo=rcoal(5));
#	# get the tree length
#	sim$treeLength
#	# scale tree
#	scaleTree(sim,10)
#	# get the scaled tree length
#	sim$treeLength
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "scaleTree",
  class="PhyloSim",
  function(
    		this,
		factor,
    		...
  ){

		if(missing(factor)){
			throw("No branch length scaling factor specified!\n");
		} else if((!is.numeric(factor)) | (length(factor) != 1)){
			throw("The scaling factor must be a numeric vector of length 1!\n");	
		} else if(!is.phylo(this$.phylo)){
			throw("The phylo object is not set or it is invalid!\n");
		} else {

			this$.phylo$edge.length<-(this$.phylo$edge.length * factor);
			return(invisible(this));
		
		}
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################/**
#
# @RdocMethod readAlignment
# 
# @title "Read alignment from file" 
# 
# \description{ 
#	@get "title".
#
#	This method reads an alignment by using the \code{read.dna} function from the \code{\link{ape}}
#	package and stores in the \code{PhyloSim} object. If a tree is already attached to the \code{PhyloSim}
#       object, the alignment must at least contain the sequences corresponding to tip nodes (but it
#       may also contain additional ancestral sequences).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{file}{A file name specified by either a variable of mode character, or a double-quoted string.}
#	\item{format}{a character string specifying the format of the DNA sequences. Four choices are possible: "interleaved", "sequential", "clustal", or "fasta", or any unambiguous abbreviation of these.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# get a safe file name	
#	fname<-paste("PhyloSim_dummy_fas_",Sys.getpid(),sep="")
#	# write out a fasta alignment
#	cat("> t3\nGTCTTT-CG-\n",file=fname);
#	cat("> t4\nG--TC-TCGG\n",file=fname,append=TRUE);
#	cat("> t2\nG--TC-TCGG\n",file=fname,append=TRUE);
#	cat("> t1\nGTC-G-TCGG",file=fname,append=TRUE);
#	# construct a PhyloSim object,
#	# set the phylo object
#	sim<-PhyloSim(phylo=rcoal(4))
#	# read the alignment
#	readAlignment(sim,fname)
#	# remove alignment file
#	unlink(fname)
#	# plot the tree & alignment
#	plot(sim)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "readAlignment",
  class="PhyloSim",
  function(
    		this,
		file,
		format="fasta",
    		...
  ){

	aln<-toupper(read.dna(file=file,format=format,as.matrix=TRUE,as.character=TRUE));
	aln.names<-dimnames(aln)[[1]];

        if (!all(is.na(this$.phylo))) {
          tip.labels<-this$tipLabels;
          length.overlap <- length(intersect(tip.labels,aln.names))
          if(length.overlap != length(tip.labels)){
            throw("The alignment must contain all sequences corresponding to tip nodes!");
          }
          if (length(aln.names) > length(tip.labels)) {
            warning("Alignment has more sequences than the tree's leaf count -- either it contains ancestral sequences or something is wrong!")
          }
        }

	this$.alignment<-aln;

	return(invisible(this));
  
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getAlignmentLength
##
###########################################################################/**
#
# @RdocMethod getAlignmentLength
# 
# @title "Get the alignment length from a PhyloSim object" 
# 
# \description{ 
#	@get "title".
#
#	This method retruns the number of columns in the alignment stored in the PhyloSim object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a PhyloSim object and run a simulation:
#	sim<-Simulate(
#                     PhyloSim(phy=rcoal(3),
#                     root=NucleotideSequence(string="ATGC", proc=list(list(JC69())) ) )
#             )
#	# get the alignment length
#	getAlignmentLength(sim)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "getAlignmentLength",
  class="PhyloSim",
  function(
    this,
    ...
  ){
		
		if(!all(is.na(this$.alignment))){
                  return(dim(this$.alignment)[2]);
		}
		else{
			throw("The alignment object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################/**
#
# @RdocMethod readTree
# 
# @title "Read tree from file" 
# 
# \description{ 
#	@get "title".
#
#	This method reads a tree by using the \code{read.tree} function from the \code{\link{ape}}
#	package and stores in the \code{PhyloSim} object. If an alignment is already attached
#       to the \code{PhyloSim} object, it must contain all sequences corresponding to tip nodes.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PhyloSim object.} 
#	\item{file}{A file name specified by either a variable of mode character, or a double-quoted string.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	The PhyloSim object (invisible).
# } 
# 
# \examples{
#	# get a safe file name	
#	fname<-paste("PhyloSim_dummy_fas_",Sys.getpid(),sep="")
#	# write out a fasta alignment
#	cat("(a,(b,c));",file=fname);
#	# construct a PhyloSim object:
#	sim<-PhyloSim()
#	# read the alignment
#	readTree(sim,fname)
#	# remove alignment file
#	unlink(fname)
#	# plot the tree
#	plot(sim)
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "readTree",
  class="PhyloSim",
  function(
    		this,
		file,
    		...
  ){
        tree <- read.tree(file)

        if (!any(is.na(this$.alignment))) {
          # Check for overlap between leaves and seqs.
          aln <- this$.alignment;
          aln.names <- dimnames(aln)[[1]];
          tip.labels <- tree$tip.label;
          aln.tree.overlap <- length(intersect(tip.labels,aln.names))
          if(aln.tree.overlap != length(tip.labels)){
            throw("The alignment must contain all sequences corresponding to tip nodes!");
          }
          if (length(aln.names) > length(tip.labels)) {
            warning("Alignment has more sequences than the tree's leaf count -- either it contains ancestral sequences or something is wrong!")
          }
        }
        
        this$.phylo <- tree
	return(invisible(this));  
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################/**
#
# @RdocMethod Undocumented
# \alias{Undocumented}
# \alias{newMatrix}
# \alias{setEquDist.CodonSubst}
# \alias{BrownianPath}
# \alias{buildFromPAML}
# \alias{checkConsistency}
# \alias{clearStates}
# \alias{areSynonymous}
# \alias{attachHookToNode}
# \alias{attachProcess}
# \alias{attachSeqToNode}
# \alias{copySubSequence}
# \alias{Debug}
# \alias{deleteSubSequence}
# \alias{detachHookFromNode}
# \alias{detachProcess}
# \alias{detachSeqFromNode}
# \alias{enableVirtual}
# \alias{exportStatTree}
# \alias{flagTotalRate}
# \alias{generateInsert}
# \alias{getAcceptBy}
# \alias{getAcceptWin}
# \alias{getAlignment}
# \alias{getAlphabet}
# \alias{getAlphabets}
# \alias{getAlphabet}
# \alias{getAncestral}
# \alias{getAncestral}
# \alias{getBaseFreqs}
# \alias{getBigRate}
# \alias{getBranchEvents}
# \alias{getCodonFreqs}
# \alias{getComments}
# \alias{getCumulativeRates}
# \alias{getCumulativeRatesFromRange}
# \alias{getDeletionTolerance}
# \alias{getDist}
# \alias{getEdge}
# \alias{getEdges}
# \alias{getEquDist}
# \alias{getEventRate}
# \alias{getEventRateAtSite}
# \alias{getEvents}
# \alias{getEventsAtSite}
# \alias{getGenerateBy}
# \alias{getHandler}
# \alias{getId}
# \alias{getInsertHook}
# \alias{getInsertionTolerance}
# \alias{getInsertionTolerance}
# \alias{getKappa}
# \alias{getLength}
# \alias{getLengthParam1}
# \alias{getLengthParam2}
# \alias{getLogFile}
# \alias{getLogLevel}
# \alias{getMatrix}
# \alias{getMaxLength}
# \alias{getMethodsList}
# \alias{getNedges}
# \alias{getNodes}
# \alias{getNtips}
# \alias{getOmegas}
# \alias{getParameterAtSite}
# \alias{getParameterAtSites}
# \alias{getPhylo}
# \alias{getProbs}
# \alias{getProcess}
# \alias{getProcesses}
# \alias{getProposeBy}
# \alias{getQMatrix}
# \alias{getRate}
# \alias{getRateList}
# \alias{getRateMultipliers}
# \alias{getRateParam}
# \alias{getRateParamList}
# \alias{getRootNode}
# \alias{getRootSeq}
# \alias{getScale}
# \alias{getScaledMatrix}
# \alias{getSeqFromNode}
# \alias{getSequence}
# \alias{getSequences}
# \alias{getSite}
# \alias{getSites}
# \alias{getSiteSpecificParamIds}
# \alias{getSiteSpecificParamList}
# \alias{getSize}
# \alias{getSizes}
# \alias{getState}
# \alias{getStates}
# \alias{getString}
# \alias{getSymbolFreqs}
# \alias{getSymbolLength}
# \alias{getSymbols}
# \alias{getTableId}
# \alias{getTargetState}
# \alias{getTemplateSeq}
# \alias{getTheta}
# \alias{getTipLabels}
# \alias{getTips}
# \alias{getToleranceMargin}
# \alias{getTotalRate}
# \alias{getTotalRates}
# \alias{getTotalRatesFromRange}
# \alias{getTransTable}
# \alias{getTreeLength}
# \alias{getType}
# \alias{getUniqueAlphabets}
# \alias{getUniqueProcesses}
# \alias{getWriteProtected}
# \alias{globalConsistencyCheck}
# \alias{hasSiteSpecificParameter}
# \alias{hasSymbols}
# \alias{hasUndefinedRate}
# \alias{insertSequence}
# \alias{intersect}
# \alias{list}
# \alias{isAttached}
# \alias{isEmpty}
# \alias{is}
# \alias{default}
# \alias{isStartCodon}
# \alias{isStopCodon}
# \alias{is}
# \alias{tip}
# \alias{Log}
# \alias{my}
# \alias{all}
# \alias{equal}
# \alias{newAAMatrix}
# \alias{omegaHist}
# \alias{omegaVarM0}
# \alias{omegaVarM1}
# \alias{omegaVarM10Cont}
# \alias{omegaVarM10Cont}
# \alias{omegaVarM2}
# \alias{omegaVarM3}
# \alias{omegaVarM4}
# \alias{Perform}
# \alias{plotParametersAtSites}
# \alias{plot}
# \alias{plusGamma}
# \alias{plusInvGamma}
# \alias{proposeLength}
# \alias{rescaleQMatrix}
# \alias{sampleState}
# \alias{sampleStates}
# \alias{saveAlignment}
# \alias{Scale}
# \alias{scaleTree}
# \alias{setAcceptBy}
# \alias{setAcceptWin}
# \alias{setAlignment}
# \alias{setAlphabet}
# \alias{setAlphabets}
# \alias{setAncestral}
# \alias{setBaseFreqs}
# \alias{setBigRate}
# \alias{setBranchEvents}
# \alias{setCodonFreqs}
# \alias{setCodonFreqs}
# \alias{setComments}
# \alias{setCumulativeRates}
# \alias{setDeletionTolerance}
# \alias{setDist}
# \alias{setEdges}
# \alias{setEquDist}
# \alias{setEquDist}
# \alias{setEvents}
# \alias{setGenerateBy}
# \alias{setHandler}
# \alias{setId}
# \alias{setInsertHook}
# \alias{setInsertionTolerance}
# \alias{setInsertionTolerance}
# \alias{setKappa}
# \alias{setKappa}
# \alias{setLength}
# \alias{setLengthParam1}
# \alias{setLengthParam2}
# \alias{setLogFile}
# \alias{setLogLevel}
# \alias{setMatrix}
# \alias{setMaxLength}
# \alias{setMethodsList}
# \alias{setName}
# \alias{setNedges}
# \alias{setNodes}
# \alias{setNtips}
# \alias{setOmegas}
# \alias{setParameterAtSite}
# \alias{setParameterAtSites}
# \alias{setPhylo}
# \alias{setPosition}
# \alias{setProbs}
# \alias{setProcess}
# \alias{setProcesses}
# \alias{setProposeBy}
# \alias{setQMatrix}
# \alias{setRate}
# \alias{setRateList}
# \alias{setRateMultipliers}
# \alias{setRateParam}
# \alias{setRateParamList}
# \alias{setRootNode}
# \alias{setRootSeq}
# \alias{setScale}
# \alias{setScaledMatrix}
# \alias{setSequence}
# \alias{setSequences}
# \alias{setSite}
# \alias{setSiteSpecificParamIds}
# \alias{setSiteSpecificParamList}
# \alias{setSize}
# \alias{setSizes}
# \alias{setState}
# \alias{setStates}
# \alias{setString}
# \alias{setSymbolLength}
# \alias{setSymbols}
# \alias{setTableId}
# \alias{setTargetState}
# \alias{setTemplateSeq}
# \alias{setTheta}
# \alias{setTipLabels}
# \alias{setTips}
# \alias{setToleranceMargin}
# \alias{setTotalRate}
# \alias{setTotalRates}
# \alias{setTransTable}
# \alias{setTreeLength}
# \alias{setType}
# \alias{setUniqueAlphabets}
# \alias{setUniqueProcesses}
# \alias{setWriteProtected}
# \alias{Simulate}
# \alias{Translate}
# \alias{translateCodon}
# \alias{virtualAssignmentForbidden}
# \alias{intersect.list}
# \alias{is.tip}
# \alias{my.all.equal}
# \alias{plot.PSRoot}
# \alias{revComp}
# \alias{readAlignment}
# \alias{readTree}
# \alias{getOmegaScalingFactor}
# \alias{saveLoadReference}
# \alias{getAlignmentLength}
# 
# @title "Undocumented object (PhyloSim package)" 
#
# \description{ 
#	@get "title".
#
#	See the corresponding specific methods if applicable.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{...}{Not used.} 
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "Undocumented",
  class="PhyloSim",
  function(
    		...
  ){
		cat("This method has no documentation!\n");	
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
