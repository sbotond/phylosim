##	
##########################################################################/** 
#
# @RdocClass PhyloSim
# 
# @title "The PhyloSim class"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{phylo}{A rooted phylo object, constructed by the APE package.}
# 	\item{root.seq}{A valid Sequence object with Process objects attached. Used as the starting sequence during simulation.}
# 	\item{name}{The name of the object (a character vector of length one).}
# 	\item{log.file}{Name of the file used for logging.}
# 	\item{log.level}{An integere specifying the verbosity of logging (see \code{\link{setLogLevel}}).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#
# }
# 
# @author
#
# \seealso{ 
# 	Sequence Site Process Event
# }
# 
#*/###########################################################################
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
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
# \value{ 
#		Returns an invisible TRUE if no inconsistencies found in the object, throws 
#		an error otherwise. 
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
					# FIXME - check seq argument.
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
			else if(length(grep("^Root Node \\d+$",label,perl=TRUE,value=FALSE)) > 0){
					throw("Sorry, but the node labels matching \"Root Node \\d+\" are reserved for the root node! Blaming label: ",label,".\n");	
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
#	The root sequence will be used as a starting point for the simulation.
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
# 	\item{value}{A valid Sequence object.} 
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
setMethodS3(
  "Simulate",
  class="PhyloSim",
  function(
    this,
		quiet=FALSE,
    ...
  ){


		# Check for the phylo object:	- FIXME: discrimintae NA-s
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
				simulateEdge(this,number=edge);
				edge.counter<-edge.counter+1;
			}
		}
		Log(this, "Simulation finished, building alignment!\n");
		this$.alignment<-.recoverAlignment(this);
		# Flush the log connection:
		if(!is.na(this$.log.connection)){
				close(this$.log.connection);
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
## Method: simulateEdge
##
setMethodS3(
  "simulateEdge",
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
		new.seq<-evolveBranch(this, start.seq=start.seq, branch.length=edge[1,"length"], old.node=edge[[1,"from"]],new.node=edge[[1,"to"]], branch.number=number);
		# Write protect the sequence:
		new.seq$writeProtected<-TRUE;
		# Attach sequence to children node:
		attachSeqToNode(this, node=edge[1,"to"], seq=new.seq);

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
setMethodS3(
  "setAlignment",
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
## Method: .recoverAlignment
##
setMethodS3(
  ".recoverAlignment",
  class="PhyloSim",
  function(
    this,
		paranoid=PhyloSim$DEBUG,
    ...
  ){
		
		if(is.null(paranoid)){
			paranoid<-FALSE;
		}

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
					colnames(tmp)<-seq(along=from.seq$.sites);
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
					colnames(tmp)<-seq(along=to.seq$.sites);
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
					throw("Trouble in getting to.symlen!");
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

		# FIXME - extend to work with tupples!

		# Iterate over the reverse of the edege matrix:	
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

					# Now check fot the gaps wich were introduced before:

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
		# FIXME - disable checking by default!
		if(paranoid){
			.checkAlignmentConsistency(this, alignment);
		}

		# The whole alignment is assotiated with the root node:
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
setMethodS3(
  "saveAlignment",
  class="PhyloSim",
  function(
    this,
    file="phylosim.fas",
		paranoid=FALSE,
    ...
  ){

		if(any(is.na(this$.alignment))){
			warning("Alignment is undefined, so nothing was saved!\n");
			return();
		}
		else {
			if(paranoid){
				.checkAlignmentConsistency(this, this$.alignment);
			}
			sink(file);
			for(i in 1:dim(this$.alignment)[[1]]){
				cat(">",rownames(this$.alignment)[[i]],"\n");
				cat(paste(this$.alignment[i,],collapse=""),"\n");
			}
			sink(NULL);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: plot.Phylosim
##
setMethodS3(
  "plot",
  class="PhyloSim",
  function(
    x,
    ...
  ){

		plot(x$.phylo);
		nodelabels();
		#add.scale.bar();

		return(invisible(x));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: summary
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
#
#       # create an object
#       a<-NucleotideAlphabet()
#       # get a summary
#       summary(a)
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

