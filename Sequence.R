##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	
##########################################################################/** 
#
# @RdocClass Sequence
# 
# @title "The Sequence class"
# 
# \description{ 
#	
#	This is the class representing a sequence. The backbone of the Sequence objects are
#	lists aggregating Site objects. The class has fields for keeping track of cumulative
#	site rates, the sum of all active event rates and methods for performing actions 
#	on a collection of sites (positions).
#
#	The Sequence objects have a field specifying an ancestral object, which can be a Sequence
#	object (when the object is obtained through clone() ) or the "Root insertion process" object 
#	(for newly created objects).
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
#	\item{name}{The name of the Sequence object.}
# 	\item{string}{A string containing symbols belonging to the associated Alphabet object. 
#	It can be used to set the initial states of the aggregated Site objects. It also specifies the length of the sequence}
# 	\item{length}{The length of the sequence. It cannot be used when 'string' is specified.}
# 	\item{alphabets}{A list of Alphabet objects to be associated with the Site objects. 
#	The list is recycled in the case it is shorter than the sequence length.}
# 	\item{processes}{A list of lists of Process objects to be attached 
#	(recycled if shorter than sequence length). }
# 	\item{ancestral.obj}{The ancestral object (Sequence or Process).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a sequence object by
#	# providng alphabets, processes and states
#	s.one<-Sequence(
#		name="Seq",
#		string="AATTGGCCTTAAGGCCTTAA",
#		alphabets=list(NucleotideAlphabet()),
#		processes=list(list(JC69()))
#	)
#	s.one
#	# check if inherits from Sequence
#	is.Sequence(s.one)
#	# get object summary
#	summary(s.one)
#	# create a sequence object,
#	# specifying length, alphabets
#	# and ancestral object
#	s<-Sequence(
#		name="Seq",
#		length=20,
#		ancestral.obj=s.one
#	)
#	# get sequence string
#	s$string
#	# get the list of site objects
#	s$sites
#	# get object id
#	s$id
#	# set and get name
#	s$name<-"SeqSeq"
#	s$seq
#	# get length
#	s$length
#	# get and set ancestral object
#	s$ancestral
#	s$ancestral<-Sequence();
#	# set alphabets
#	setAlphabets(s,list(NucleotideAlphabet()))
#	# set states
#	# "A"-s in the range 1:10
#	setStates(s,"A",1:10)
#	# a pattern of "ATGC"-s in the range 11:20
#	setStates(s,c("A","T","G","C"),11:20)
#	# get states from range 10:12
#	getStates(s,10:12)
#	# attach a JC69 process to range 1:10
#	jc69<-JC69()
#	attachProcess(s,jc69,1:10)
#	# set the rate multiplier site-process specific parameter for jc69
#	setParameterAtSites(s,jc69,"rate.multiplier",2,1:10)
#	# get "rate.multiplier" for jc69 from range 1:2
#	getParameterAtSites(s, jc69, "rate.multiplier",1:2)
#	# attach a GTR process to range 11:20
#	gtr<-GTR()
#	attachProcess(s,gtr,11:20)
#	# set and get rate multipliers for gtr
#	setRateMultipliers(s, gtr, c(0,5,1), 11:20)
#	getRateMultipliers(s, gtr, 11:20)
#	# get processes from range 1:5
#	getProcesses(s,1:5)
#	# replace the processes with a (GTR, JC69), JC69 pattern
#	s$processes<-list(list(GTR(), JC69()), list(JC69()))
#	# get processes from range 1:2
#	getProcesses(s,1:2)
#	# get unique processes
#	s$uniqueProcesses
#	# get unique alphabets
#	s$uniqueAlphabets
#	# get symbol frequencies
#	getSymbolFreqs(s)
#	# get the big rate
#	s$bigRate
#	# get a vector of total rates from range 1:4
#	getTotalRatesFromRange(s,1:4)
#	# get a vector of cumulative rates from range 1:4
#	getCumulativeRatesFromRange(s,1:4)
#	# reset all site states	
#	clearStates(s)
#	s
#	# sample states from the equilibrium distributions
#	# of the attached substitution processes
#	sampleStates(s)
#	s	
#	# clone s
#	s.clone<-clone(s)
#	# insert a sequence in s.clone after position 2
#	insertSequence(s.clone,NucleotideSequence(string="AAAAAA"),2)
#	s.clone
#	# delete positions 1,2,3 and 10
#	deleteSubSequence(s.clone, c(1:3,10))
#	s.clone
#	# copy positions 7:10 into a new sequence object
#	sc<-copySubSequence(s.clone, 7:10)
#	sc
#	# get events from sc in the range 1:2
#	getEvents(sc,1:2)
# }
# 
# @author
#
# \seealso{ 
# 	Alphabet Site Process Event
# }
# 
#*/###########################################################################
setConstructorS3(
	"Sequence",
	function(
		name=NA,
		string=NA,
		length=NA,
		alphabets=NA,
		processes=NA,
		ancestral.obj=NA,
		...
	){

		# Marking the instance as static by default:	
		STATIC<-TRUE;	

		# Extending the PSRoot class:
		this<-extend(
			PSRoot(),
			"Sequence",
			.name="Anonymous",
			.length=NA,
			.sites=list(),
			.ancestral.obj=NA,
			.cumulative.rates=NA,
			.total.rates=NA,
			.cumulative.rate.flag=TRUE,
			.flagged.sites=integer(0),
			.write.protected=FALSE,
			.is.sequence=TRUE,
			.root.ins=NA
		);

		# Initializing the variables for length and states:
		len<-0;
		str<-list();

		# Optional argument: name
		if(!missing(name)) {
			this$name<-name;
			STATIC<-FALSE;
		}
		
		# The user can specify a sequence
		# or the sequence length, but not both.		
		if (!missing(string) & !missing(length)) {
			throw("You can specify the sequence, or the sequence length, but not both!\n");}
		else if (!missing(string)){
			STATIC<-FALSE;
			# An alphabet list must be specified along the sequence! 
			if(missing(alphabets)) {throw("A list of valid alphabets must be specified when a string is given!\n");}
		}

		# Deal with the string or length argument:
		if (!missing(length)) {
			STATIC<-FALSE;
			len<-length;
		}
		else if( !missing(string) ) {
					str<-strsplit(string,split="",fixed=TRUE)[[1]];
					len<-length(str);
		}
		this$.length<-len;

		root.ins<-NA;
		if (!is.Process(Sequence$.root.ins)) {
			# Dummy proces used as ancestral object for sites.
			this$.root.ins<-Process(name="Root insertion process");
			this$.root.ins$comments<-"This is just a dummy process object serving as ancestral for newly created site and sequence objects.";
			root.ins<-this$.root.ins;
		} else {
			root.ins<-Sequence$.root.ins
		}

		# Site template object:
			 site.template<-Site(
            			ancestral=root.ins,
            			sequence=this
          	);	
	

		# Clone and store the site objects:
		if(!STATIC) {
			if ( len > 0 ) {
				for(position in 1:len) {
					 this$.sites[[position]]<-clone.Object(site.template);
				}
			}

		}	
		
		# Optional argument: ancestral object
		if (!missing(ancestral.obj)) {
			STATIC<-FALSE;
			this$ancestral<-ancestral.obj;
		} else {
			this$ancestral<-root.ins;
		}

		# Setting the alphabets:
		if(!missing(alphabets)) {
			STATIC<-FALSE;
			# setAlphabets will check the arguments
			setAlphabets(this, alphabets);
		}
	
		# Setting the processes:
		if (!missing(processes)) {
			STATIC<-FALSE;
			# setProcesses will take care about the arguments
			# and deal with alphabet mismatch.
			setProcesses(this,processes)	
		}

			# Initializing these vectors properly is
			# importtant for the insertion method!
			this$.total.rates<-double(len);
			this$.cumulative.rates<-double(len);

		if(!STATIC){

			# Now we are prepared to set the states:	
			if (!missing(string)) {
				setStates(this, str);
			}

			# Calculate cumulative rates for the first time, but only if
			# states are defined. This is expensive, as total rates are calculated.
			if (!missing(string) & (length(str) > 0) ) {
				.recalculateCumulativeRates(this);
			}

		}

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.Sequence
##	
###########################################################################/**
#
# @RdocDefault is.Sequence
# 
# @title "Check whether an object inherits from the Sequence class" 
# 
# \description{ 
#	@get "title".
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
# 	TRUE or FALSE
# } 
# 
# \examples{
#	# create some objects
#	seq<-Sequence(length=10)
#	a<-Alphabet()
#	# check if they inherit from Sequence
#	is.Sequence(seq)
#	is.Sequence(a)
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
	"is.Sequence", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.sequence)){return(TRUE)}
    if ( inherits(this, "Sequence")) {
      this$.is.sequence<-TRUE;
      return(TRUE);
    } else {
      return(FALSE)
    }

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
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
#       \item{omit.sites}{Do not check aggregated site objects.} 
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
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"checkConsistency", 
	class="Sequence", 
	function(
		this,
		omit.sites=FALSE,
		...
	){

		if(this$length != length(this$.sites)) {
			throw("Sequence length inconsistency detected");
		} else if (length(this$.cumulative.rates) != this$.length) {
			throw("Cumulative rates vector length mismatch!\n");
		} else if (length(this$.total.rates) != this$.length) {
			throw("Total rates vector length mismatch!\n");
		} else if (!identical(this$.cumulative.rates, cumsum(this$.total.rates))) {
			throw("Cumulative rates vector is not consistent with total rates vector!\n");
		}

		if(!is.numeric(this$.flagged.sites)) {
			throw("Flagged sites vector is not numeric!\n");
		} else if (length(this$.flagged.sites) > 0) {
			if ( (min(this$.flagged.sites) < 1) | ( max(this$.flagged.sites) > this$.length) ) {
				throw("Inconsistency in the flagged sites vector!\n");
			}	
		}

		if(!is.character(this$.name)) {
			throw("Sequence name is invalid!\n");
		} else if(stringLength(this$.name) == 0) {
			throw("Sequence name is of length zero!\n");
		}

		if(!is.Sequence(this$.ancestral.obj) & !is.Process(this$.ancestral.obj)) {
			throw("The ancestral object is invalid!\n");
		}
		if(!is.logical(this$.cumulative.rate.flag)) {
			throw("Cumulative rate flag is not logical!\n");
		}

		# Calling consistency check on sites.
		# This will be painfully slow!
		if(!omit.sites){
			for(site in this$.sites){
				checkConsistency(site);
			}
		}
		return(invisible(TRUE));

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
# @title "Get the unique identifier of a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The identifier is the concatenation of the object name and the object hash code as returned
#	by hashCode().
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A charcter vector of length one.
# } 
# 
# \examples{
#	# create a Sequence object.
#	s<-Sequence(length=5)
#	# get id
#	getId(s)
#	# get id via virtual field
#	s$id
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
  class="Sequence",
  function(
    this,
    ...
  ){

  this.class<-class(this)[1];
  id<-paste(this.class,this$.name,hashCode(this),sep=":");

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
# @title "Forbidden action: setting the unique identifier of a Sequence object"
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
	class="Sequence", 
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
## Method: getName
##	
###########################################################################/**
#
# @RdocMethod getName
# 
# @title "Get the name of a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create object
#	s<-Sequence(length=10);
#	# get object name
#	getName(s)
#	# get name via virtual field
#	s$name
#
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
	class="Sequence", 
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
# @title "Set the name of a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{new.name}{A character vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new object name (invisible).
# } 
# 
# \examples{
#	# create a Sequence object
#	s<-Sequence(name="MySeq",length=4)
#	# get sequence name
#	s$name
#	# rename object
#	setName(s,"SO")
#	s$name
#	# rename via virtual field
#	s$name<-"SeqSeq"
#	s$name
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
	class="Sequence", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getString
##	
###########################################################################/**
#
# @RdocMethod getString
# 
# @title "Get the string representation of a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The string representation is the concatenation of the states of the 
#	aggregated Site object. Undefined states (NA-s) are represented by question marks.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create object
#	s<-Sequence(length=10)
#	# get character representation
#	getString(s)	# a bunch of '?'-s
#	# get string reperesentation via virtual field
#	s$string
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
	"getString", 
	class="Sequence", 
	function(
		this,
		...
	){
			str<-character();
			for(site in this$.sites){
					if(is.na(site$.state)){
							if(is.Alphabet(site$.alphabet)){
								str<-c(str,rep("?",site$.alphabet$.symbolLength));
							}
							else {
								str<-c(str,"?");
							}
					}
					else {
						str<-c(str,site$.state);
					}
			}
			return(paste(str,collapse=""));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setString
##	
###########################################################################/**
#
# @RdocMethod setString
#
# @title "Forbidden action: setting the string representation of a Sequence object"
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
	"setString", 
	class="Sequence", 
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
## Method: getLength
##	
###########################################################################/**
#
# @RdocMethod getLength
# 
# @title "Get the number of Site objects aggregated in a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An integer vector of length one.
# } 
# 
# \examples{
#	# create a Sequence object
#	s<-Sequence(length=5)
#	# get sequence length
#	getLength(s)
#	# get length via virtual field
#	s$length
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
	"getLength", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$.length;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLength
##	
###########################################################################/**
#
# @RdocMethod setLength
#
# @title "Forbidden action: setting the length of a Sequence object"
#
# \description{
#       @get "title".
#	The length of the Sequence objects can be specified when the object is constructed,
#	or modified later by the "insertSequence" and "deleteSubSequence" methods. 
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
	"setLength", 
	class="Sequence", 
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
## Method: getSites
##	
###########################################################################/**
#
# @RdocMethod getSites
# 
# @title "Get the list of the Site object aggregated in a Sequence object" 
# 
# \description{ 
#	@get "title".
#	Warning: there is no setSites method!
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Site objects.
# } 
# 
# \examples{
#	# create a sequence object
#	s<-Sequence(alphabets=list(NucleotideAlphabet()),string="AATTGCCC")
#	# get the list of aggregated Site objects
#	getSites(s)
#	# get Site objects via virtual field
#	s$sites
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
	"getSites", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		this$.sites;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getStates
##	
###########################################################################/**
#
# @RdocMethod getStates
# 
# @title "Get the states of a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{index}{An integer vector specifying a set of positions. 
#	It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector.
# } 
# 
# \examples{
#	# create a sequence object
#	s<-Sequence(alphabets=list(NucleotideAlphabet()),string="AATTGCCCCCTTGG")
#	# get all Site states
#	getStates(s)
#	# get the states for a collection of sites
#	getStates(s,c(1:3,5,8))
#	# get states via virtual field
#	s$states
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
	"getStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		 if (missing(index)) {
      index<-seq(along.with=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$.state;
      }
    );

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setStates
##	
###########################################################################/**
#
# @RdocMethod setStates
# 
# @title "Set the states for a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The value vector is recycled, which is useful when creating repeated patterns.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{value}{A character vector containg the states (recycled if shorter than the index vector). The states must be compatible with the corresponding Alphabet object.}
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a sequence object of length 10
#	s<-Sequence(alphabets=list(NucleotideAlphabet()),length=10)
#	# set the states in some ranges
#	setStates(s,c("A","T","A"),index=1:5)
#	setStates(s,c("G","C"),index=6:10)
#	# display sequence
#	s
#	# set the states for the whole Sequence object
#	setStates(s,c("A","T","T","A"))
#	s
#	# set states via virtual field
#	s$states<-c("A","T")
#	s
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
	"setStates", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
	  else if (missing(index)) {
      index<-seq(along.with=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
		# Recycling value vector by using rep().
    if (length(value) < length(this$.sites) ) {
        value<-rep(as.character(value),length.out=length(index))
    }
		
    for (i in 1:length(index)) {
        this$.sites[[ index[[i]] ]]$state<-value[i];
    }
		# Flagging the changed sites:
		this$.cumulative.rate.flag<-TRUE;
		this$.flagged.sites<-c(this$.flagged.sites, index);
		.recalculateCumulativeRates(this);

    invisible(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabets
##	
###########################################################################/**
#
# @RdocMethod getAlphabets
# 
# @title "Get the list of the Alphabet objects attached to the Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Alphabet objects.
# } 
# 
# \examples{
#	# create a Sequence object with NucleotideAlphabet
#	#and BinaryAlphabet objects attached
#	s<-Sequence(alphabets=list(NucleotideAlphabet(),BinaryAlphabet()),length=5)	
#	setStates(s,c("A","0"))
#	# get the list of attached Alphabet objects
#	getAlphabets(s)
#	# get Alphabets from a range
#	getAlphabets(s,c(2:3,5))
#	# get alphabets via virtual field
#	s$alphabets
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
	"getAlphabets", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 		if (missing(index)) {
      index<-seq(along.with=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$alphabet;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabets
##	
###########################################################################/**
#
# @RdocMethod setAlphabets
# 
# @title "Assotiate Alphabet objects to a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{value}{A list of Alphabet objects, recycled if shorter than the index vector.} 
# 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a Sequence object
#	s<-Sequence(length=10)	
#	# set the alphabets for range 1:5
#	setAlphabets(s,list(NucleotideAlphabet(),BinaryAlphabet()),1:5)
#	# set the alphabets for range 6:10	
#	setAlphabets(s,list(AminoAcidAlphabet()),6:10)
#	# get the list of attached Alphabet objects
#	getAlphabets(s)
#	# get Alphabets from a range
#	getAlphabets(s,c(2:3,5))
#	# set alphabets via virtual field
#	s$alphabets<-list(BinaryAlphabet(),NucleotideAlphabet())
#	# get alphabets via virtual field
#	s$alphabets
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
	"setAlphabets", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
				for(a in value) {
					if(!is.Alphabet(a)) {
						throw("The value parameter must be a list of valid alphabet objects!\n");
					}
				}
		}

	  if (missing(index)) {
      index<-seq(along.with=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!

    value.counter<-1;
    for (i in index)  {
        if(value.counter > length(value)) {
          value.counter<-1;
        }
	setAlphabet(this$.sites[[i]], value[[value.counter]]);

        value.counter<-(value.counter + 1);
    }
    invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueAlphabets
##	
###########################################################################/**
#
# @RdocMethod getUniqueAlphabets
# 
# @title "Get the list of unique Alphabet objects associated to Site objects aggaregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The returned list contains unique instances of the Alphabet class. The symbol sets are not compared, so
#	two instances of the same class are considered to be different.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Alphabet objects.
# } 
# 
# \examples{
#	# create a Sequence object with some Alphabet objects attached
#	s<-Sequence(
#               alphabets=list(NucleotideAlphabet(),
#               BinaryAlphabet(),
#               NucleotideAlphabet()),
#               length=10
#               )	
#	# get the list of attached alphabets
#	s$alphabets
#	# get the unique list of attahced Alphabet objects
#	getUniqueAlphabets(s)
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
	"getUniqueAlphabets",
	class="Sequence", 
	function(
		this,
		...
	){
	
	tmp<-list();			
    	lapply(
      	this$.sites,
      	function(site) {
		tmp<<-c(tmp,list(site$.alphabet))
      	}
	);
	return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueAlphabets
##	
###########################################################################/**
#
# @RdocMethod setUniqueAlphabets
#
# @title "Forbidden action: setting the list of unique Alphabet objects attached to the Site object aggregated by a Sequence object"
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
	"setUniqueAlphabets", 
	class="Sequence", 
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
## Method: attachProcess
##	
###########################################################################/**
#
# @RdocMethod attachProcess
# 
# @title "Attach a Process object to a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.}
# 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a Sequence object of length 6
#	s<-Sequence(length=10,alphabets=list(NucleotideAlphabet()))
#	# attach a JC69 substitution process 
#	attachProcess(s,JC69())
#	# get the list of attached processes 
#	s$processes
#	# attach the GTR substitution process to range 3:6
#	attachProcess(s,GTR(),3:6)
#	# get the list of attached processes 
#	s$processes
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
	"attachProcess", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
	 if(!exists(x="PSIM_FAST")){
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
	 }
 		if (missing(index)) {
      		index<-seq(along.with=this$.sites);
    		} else {
			index<-.checkIndexSanity(this, index);	
		}

		for(i in index){
				attachProcess(this$.sites[[i]],process);
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
## Method: detachProcess
##	
###########################################################################/**
#
# @RdocMethod detachProcess
# 
# @title "Detach a Process object from a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a sequence object with two processes attached
#	p<-JC69()
#	s<-Sequence(length=4,alphabets=list(NucleotideAlphabet()),processes=list(list(p,K80())))
#	# get the list of attached processes
#	s$processes
#	# detach JC69 from range c(1,4)
#	detachProcess(s,p,c(1,4))
#	s$processes
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
	"detachProcess", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
	 if(!exists(x="PSIM_FAST")){
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
	}
 		if (missing(index)) {
      		index<-seq(along.with=this$.sites);
    		} else {
			index<-.checkIndexSanity(this, index);
		}
		lapply(
      		this$.sites[index],
      		function(site) {
        	detachProcess(site,process);
      }
    );	
		return(invisible(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcesses
##	
###########################################################################/**
#
# @RdocMethod getProcesses
# 
# @title "Get the Process objects attached to the Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of lists of Process objects.
# } 
# 
# \examples{
#	# create a sequence object with some processes attached
#	s<-Sequence(
#               length=4,
#               alphabets=list(NucleotideAlphabet()),
#               processes=list(list(JC69(),K80()),list(GTR()))
#               )
#	# get the list of lists of attached processes from positions 1 and 3
#	getProcesses(s,c(1,3))
#	# get processes via virtual field
#	s$processes
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
	"getProcesses", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 	if (missing(index)) {
      	index<-seq(along.with=this$.sites);
    	} else {
			index<-.checkIndexSanity(this, index);
		}

    	lapply(
      	this$.sites[index],
      	function(site) {
        site$processes;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueProcesses
##	
###########################################################################/**
#
# @RdocMethod getUniqueProcesses
# 
# @title "Get the list of unique Process instances attached to the Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Process objects.
# } 
# 
# \examples{
#	# create a sequence object and attach processes
#	p<-JC69()
#	s<-Sequence(
#               length=4,
#               alphabets=list(NucleotideAlphabet()),
#               processes=list(list(p,K80()),list(p))
#               )
#	# get the unique list of attached Process instances
#	getUniqueProcesses(s)
#	# via virtual field
#	s$uniqueProcesses
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
	"getUniqueProcesses", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		tmp<-list();			
      lapply(
      this$.sites,
      function(site) {
				tmp<<-c(tmp,site$processes)
      }
		);
		return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueProcesses
##	
###########################################################################/**
#
# @RdocMethod setUniqueProcesses
#
# @title "Forbidden action: setting the list of unique Process instances attached to the sites of a Sequence object"
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
	"setUniqueProcesses", 
	class="Sequence", 
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
## Method: setProcesses
##	
###########################################################################/**
#
# @RdocMethod setProcesses
# 
# @title "Specify a set of Process objects to be attached to a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The Process objects in a given inner list correspond to the set of processes to be attached to one Site object. 
#	Process objects already attached to a given Site are skipped. Attached processes which are not memebers of the list
#	are detached, so specifying an empty list will detach all processes.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{value}{A list of list of Process objects, recycled if shorter than the index vector.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a sequence of length 5
#	s<-Sequence(length=5,alphabets=list(NucleotideAlphabet()));
#	# set a pattern of processes
#	setProcesses(s,list(list(JC69(),K81())))
#	# get attached processes
#	s$processes
#	# detach all processes from range 1:3
#	setProcesses(s,list(list()),1:3)
#	s$processes
#	# detach all processes via virtual field
#	s$processes<-list(list())
#	# create a process pattern in the full sequence via virtual field
#	s$processes<-list(list(JC69()),list(GTR(),K80()))
#	s$processes
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
	"setProcesses", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		...
	){

		.checkWriteProtection(this);	

		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
			lapply(
					value,
					function(element) {
						if(!is.list(element)){
							throw("The value parameter must be a list of lists containing process objects!\n");
						}
					}
			);
		}

	  	if (missing(index)) {
      		index<-seq(along.with=this$.sites);
    		} else {
			index<-.checkIndexSanity(this, index);	
		}

    		value.counter<-1;

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!
    		for (i in index)  {
        	if(value.counter > length(value)) {
          		value.counter<-1;
        	}
		setProcesses(this$.sites[[i]], value[[value.counter]]);
	
        	value.counter<-(value.counter + 1);
    	}
    	invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setParameterAtSites
##	
###########################################################################/**
#
# @RdocMethod setParameterAtSites
# 
# @title "Set the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A valid Process object.} 
# 	\item{id}{The identifier of the site-process specific parameter.} 
#	\item{value}{A vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector. It should be consistent with the type of the parameter.}
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a sequence, attach a process
#	p<-K80()
#	s<-Sequence(length=6,alphabets=list(NucleotideAlphabet()),processes=list(list(p)))
#	# set a new pattern of rate multipliers in the range 1:3,
#   # the default value is 1.0 by the way
#	setParameterAtSites(s,p,"rate.multiplier",c(2,3),1:3)
#	# get rate multipliers
#	getParameterAtSites(s,p,"rate.multiplier")
#	# set a new value for the whole sequence
#	setParameterAtSites(s,p,"rate.multiplier",0.5)
#	# get rate multipliers
#	getParameterAtSites(s,p,"rate.multiplier")
# } 
# 
# @author 
# 
# \seealso{ 
# 	Site Process @seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"setParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		value,
		index,
		...
	){

		.checkWriteProtection(this);

	 if(!exists(x="PSIM_FAST")){

		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		} else if (missing(value)){
			throw("No new value given!\n");
		}
	}

		if (missing(index)) {
			index<-seq(along.with=this$.sites);
		} else {
			index<-.checkIndexSanity(this, index);
		}
	
		if(length(value) == 1) {
			lapply(
				this$.sites[index],
				function(site){
					setParameterAtSite(process,site,id,value);
				}
			);
		} else {
			
			counter<-1;
			lapply(
				this$.sites[index],
				function(site){
					if( counter > length(value) ){
						counter<<-1;
					}	
					setParameterAtSite(process,site,id,value[[counter]]);
					counter<<-(counter+1);

				}
			);
				
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
## Method: setRateMultipliers
##	
###########################################################################/**
#
# @RdocMethod setRateMultipliers
# 
# @title "Set the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method just calls \code{setParameterAtSites(this=this,process=process,id="rate.multiplier",value=value,index=index)} See setParameterAtSites.Sequence for details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A valid Process object.} 
#	\item{value}{A numeric vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector.}
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	setParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"setRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

	
		if(missing(process)){
			throw("No process given!\n");
		}
		else if(missing(value)){
			throw("No value provided!\n");
		}
		#else if(!is.GeneralSubstitution(process)){
		#	throw("The specified process is not a substitution process!\n");
		#}
		setParameterAtSites(this=this,process=process,id="rate.multiplier",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateMultipliers
##	
###########################################################################/**
#
# @RdocMethod getRateMultipliers
# 
# @title "Get the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method just calls \code{getParameterAtSites(this=this,process=process,id="rate.multiplier",index=index)} See getParameterAtSites.Sequence for details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A valid Process object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector with the current values of the rate multiplier in the specified range.
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	setParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"getRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		
		rm<-getParameterAtSites(this=this,process=process,id="rate.multiplier",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getParameterAtSites
##	
###########################################################################/**
#
# @RdocMethod getParameterAtSites
# 
# @title "Get the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A valid Process object.} 
# 	\item{id}{The identifier of the site-process specific parameter.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of site specific paramters. A site specific paramter is a list storing the id, the name, the value 
#	and the type of the parameter.
# } 
# 
# \examples{
#	# create a sequence, attach a process
#	p<-K80()
#	s<-Sequence(length=6,alphabets=list(NucleotideAlphabet()),processes=list(list(p)))
#	# set a new pattern of rate multipliers in the 
#   # range 1:3, the default value is 1.0 by the way
#	setParameterAtSites(s,p,"rate.multiplier",c(2,3),1:3)
#	# get rate multipliers
#	getParameterAtSites(s,p,"rate.multiplier")
#	# set a new value for the whole sequence
#	setParameterAtSites(s,p,"rate.multiplier",0.5)
#	# get rate multipliers
#	getParameterAtSites(s,p,"rate.multiplier")
# } 
# 
# @author 
# 
# \seealso{ 
# 	Site Process @seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"getParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index,
		...
	){

	 if(!exists(x="PSIM_FAST")){

		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		}
	}

		if (missing(index)) {
			index<-seq(along.with=this$.sites);
		} else {
				index<-.checkIndexSanity(this, index);
		}	
		
		lapply(
			this$.sites[index],
			function(site){
				getParameterAtSite(process,site,id);
			}
		);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEvents
##	
###########################################################################/**
#
# @RdocMethod getEvents
# 
# @title "Get the list of active Event objects for a set of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Event objects.
# } 
# 
# \examples{
#	# create a sequence with a process attached
#	s<-Sequence(
#               string="ATGC",
#               alphabets=list(NucleotideAlphabet()),
#               processes=list(list(JC69()))
#               )
#	# get the active events from range 1:3
#	getEvents(s,1:3)
#	# get all active events via virtual field
#	s$events
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
	"getEvents", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
		
 	if (missing(index)) {
      		index<-seq(along.with=this$.sites);
	} else {
        	index<-.checkIndexSanity(this, index);
    	}

	tmp<-list();
    	for (i in index){
		# Setting the .positions field for then Events.
		this$.sites[[i]]$.position<-i;
        	tmp<-c(tmp, getEvents(this$.sites[[i]]));
		# Deleting the .position field;
		this$.sites[[i]]$.position<-NULL;
    	}
	tmp;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRatesFromRange
##	
###########################################################################/**
#
# @RdocMethod getTotalRatesFromRange
# 
# @title "Get the vector of total site rates for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a sequence with some processes attached
#	s<-Sequence(
#               string="ATGC",
#               alphabets=list(NucleotideAlphabet()),
#               processes=list(list(JC69()),list(JC69(),GTR()))
#               )
#	# get total rates for positions 1 and 3
#	getTotalRatesFromRange(s,c(1,3))
#	# get all total rates via virtual field
#	s$totalRates	# via the "getTotalRates.Sequence" method
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
	"getTotalRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){


	if (missing(index)) {
      	index<-seq(along.with=this$.sites);
    	} else {
        	index<-.checkIndexSanity(this, index);
    	}

	
		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.total.rates[index];
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRates
##	
###########################################################################/**
#
# @RdocMethod getTotalRates
# 
# @title "Get the total site rates from a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method simply calls \code{getTotalRatesFromRange(this)}.
#	See \code{getTotalRatesFromRange.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector containing the total site rates.
# } 
# 
# @author 
# 
# \seealso{ 
# 	getTotalRatesFromRange.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"getTotalRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getTotalRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTotalRates
##	
###########################################################################/**
#
# @RdocMethod setTotalRates
#
# @title "Forbidden action: setting the list of total site rates for a Sequence object"
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
	"setTotalRates", 
	class="Sequence", 
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
## Method: getAncestral
##	
###########################################################################/**
#
# @RdocMethod getAncestral
# 
# @title "Get the ancestral object of a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object or a Process object.
# } 
# 
# \examples{
#	# create a sequence object
#	s<-Sequence(length=4)
#	# get ancestral object
#	getAncestral(s)	# newly created sequences have the "Root insertion process" as ancestral
#	# clone sequence
#	cs<-clone(s)
#	# get ancestral object id via virtual field
#	cs$ancestral$id
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
	"getAncestral", 
	class="Sequence", 
	function(
		this,
		...
	){

	this$.ancestral.obj;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRatesFromRange
##	
###########################################################################/**
#
# @RdocMethod getCumulativeRatesFromRange
# 
# @title "Get the cumulative site rates for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a sequence with some processes attached
#	s<-Sequence(
#               string="ATGC",
#               alphabets=list(NucleotideAlphabet()),
#               processes=list(list(JC69()),list(JC69(),GTR()))
#               )
#	# get cumulative rates for positions 1 and 3
#	getCumulativeRatesFromRange(s,c(1,3))
#	# get all cumulative rates via virtual field
#	s$cumulativeRates	# via the "getCumulativeRates.Sequence" method
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
	"getCumulativeRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		if (missing(index)) {
      			index<-seq(along.with=this$.sites);
    		} else {
        		index<-.checkIndexSanity(this, index);
   		}


		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.cumulative.rates[index];


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

setMethodS3(
	".getCumulativeRatesFast", 
	class="Sequence", 
	function(
		this,
		...
	){

		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.cumulative.rates;

	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .recalculateCumulativeRates
##	
setMethodS3(
	".recalculateCumulativeRates", 
	class="Sequence", 
	function(
		this,
		target.site,
		...
	){
		
		length<-this$.length;
		if(length ==  0){
			return();
		}

		total.rates<-this$.total.rates;
		sites<-this$.sites;
		flagged.sites<-this$.flagged.sites;

		if( length(flagged.sites) == 0 ) {
			# Fresh start:
			total.rates<-as.numeric(lapply(sites,getTotalRate));

		} else {
			# We have some flagged sites, recalculate just their total rates:
			total.rates[flagged.sites]<-as.numeric(lapply(sites[flagged.sites],getTotalRate));
		}
		
		this$.total.rates<-total.rates;
		this$.cumulative.rates<-cumsum(total.rates);	
		this$.flagged.sites<-integer(0);
		this$.cumulative.rate.flag<-FALSE;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRates
##	
###########################################################################/**
#
# @RdocMethod getCumulativeRates
# 
# @title "Get the total site rates from a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method simply calls \code{getCumulativeRatesFromRange(this)}.
#	See \code{getCumulativeRates.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector containing the total site rates.
# } 
# 
# @author 
# 
# \seealso{ 
# 	getCumulativeRatesFromRange.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"getCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getCumulativeRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setCumulativeRates
##	
###########################################################################/**
#
# @RdocMethod setCumulativeRates
#
# @title "Forbidden action: setting the cumulative rates for the sites aggregated by a Sequence object"
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
	"setCumulativeRates", 
	class="Sequence", 
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
## Method: getBigRate
##	
###########################################################################/**
#
# @RdocMethod getBigRate
# 
# @title "Get the sum of all active event rates from a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The sum of active event rates depends on all Site object states and on the attached Process objects. 
#	It basically returns the last element of the cumulative site rates vector.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a nucleotide sequence attach a process
#	s<-NucleotideSequence(length=5);
#	s$processes<-list(list(JC69()))
#	# get the sum of active event rates
#	getBigRate(s)	# returns NA because site states are undefined
#	# set site states
#	s$states<-c("A","T")
#	# get big rate via virtual field
#	s$bigRate
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
	"getBigRate", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		if (length(this$.sites) > 0) {
			if(this$.cumulative.rate.flag)	{
				.recalculateCumulativeRates(this);	
			}
			return(this$.cumulative.rates[this$.length]);
		} else {
			return(NA);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBigRate
##	
###########################################################################/**
#
# @RdocMethod setBigRate
#
# @title "Forbidden action: setting the sum of total active event rates for a Sequence object"
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
	"setBigRate", 
	class="Sequence", 
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
## Method: setAncestral
##	
###########################################################################/**
#
# @RdocMethod setAncestral
# 
# @title "Set the ancestral object of a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{value}{A Sequence or a Process object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new ancestral object (invisible).
# } 
# 
# \examples{
#	# create a nucleotide sequence and a process object
#	s<-NucleotideSequence(string="AGCT")
#	p<-Process(name="MyProcess")
#	# set the p as the ancestral of s
#	setAncestral(s,p)
#	s$ancestral
#	# clone s
#	cs<-clone(s)
#	# set cs as ancestral of s via virtual field
#	s$ancestral<-cs
#	# get ancestral ids
#	s$ancestral$id
#	cs$ancestral$id
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
	"setAncestral", 
	class="Sequence", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);	
		if (!is.Sequence(value) & ! is.Process(value)) {
			throw("Ancestral object must be a sequence or a process!\n");
		}	else {
			this$.ancestral.obj<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: clone.Sequence
##	
###########################################################################/**
#
# @RdocMethod clone
# 
# @title "Clone a Sequence object" 
# 
# \description{ 
#	@get "title".
#
#	The cloning of Sequence objects involves the cloning of all aggregated Site objects. Because of that the 
#	cloning of long sequences is quite expensive.
#	The cloned Site objects have the orginal Site objects as ancestral.
#	The new Sequence objects has the original object as ancestral.	
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object.
# } 
# 
# \examples{
#	# cretate a nucleotide sequence
#	s<-NucleotideSequence(string="ATG")
#	# clone the sequence
#	cs<-clone(s)
#	# get some properties
#	equals(s,s)
#	equals(s,cs)
#	cs$ancestral
#	cs$sites[[1]]$ancestral
# } 
# 
# @author 
# 
# \seealso{ 
# 	Sequence clone.Object
# } 
# 
#*/###########################################################################
setMethodS3(
	"clone", 
	class="Sequence", 
	function(
		this,
		...
	){

		# Cloning the whole sequence object:
		that<-clone.Object(this)
		# Disabling write protection:
		if(that$writeProtected) {
			that$writeProtected<-FALSE
		}
		# Setting the ancestral sequence:
		that$.ancestral.obj<-this
		# Resetting comments:
		that$.comments<-list()

		# Cloning sites:
		clone.sites<-that$.sites
        if(this$.length > 0) {
            for (i in 1:this$.length) {
                site<-this$.sites[[i]]
                clone<-clone.Object(site)
                clone$.ancestral<-site
                clone$.sequence<-that
                clone.sites[[i]]<-clone
            }
        }
		that$.sites<-clone.sites
		
		# Setting the name:
		that$name<-paste("clone of",this$.name)
		return(that)
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: getWriteProtected
##
###########################################################################/**
#
# @RdocMethod getWriteProtected
#  
# @title "Check if the object is write protected" 
# 
# \description{ 
#	@get "title".
#	Write protected objects cannot be modified through get/set methods and virtual fields.
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
# 	TRUE or FALSE
# } 
# 
# \examples{
#
#       # create an object
#       o<-Sequence()
#       # toggle write protection
#       o$writeProtected<-TRUE
#       # check if it's write protected
#       getWriteProtected(o)
#       # check write protection via virtual field
#       o$writeProtected
#	
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
  "getWriteProtected",
  class="Sequence",
  function(
    this,
    ...
  ){

    this$.write.protected;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkIndexSanity
##
setMethodS3(
  ".checkIndexSanity",
  class="Sequence",
  function(
    this,
    index,
    ...
  ){

		if (length(index) == 0 ) {
				return(c());
		}
           if(!exists(x="PSIM_FAST")){
	  
	   if( length(index) == 1 ) {
			if(is.na(index)) {
				warning("Index vector is NA! Coercing to empty vector!\n");
				return(c());
			} 
			if (is.nan(index)) {
				warning("Index vector is NaN! Coercing to empty vector!\n");
				return(c());
			}

		}

		if(min(index) < 1 ) {
			throw("Index vector element ",min(index)," too small!\n");
		}
		if( max(index) > this$.length ) {
			throw("Index vector element ",max(index)," too big!\n");
		}
	     }
		return(index);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
##
## Method: setWriteProtected
##
###########################################################################/**
#
# @RdocMethod setWriteProtected
#  
# @title "Set the write protection field for an object" 
# 
# \description{ 
#	@get "title".
#	Write protected objects cannot be modified through get/set methods and virtual fields.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{value}{A logical vector of size one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE or FALSE.
# } 
# 
# \examples{
#
#	# create an object
#	o<-Sequence()
#	# toggle write protection
#	setWriteProtected(o,TRUE)
#	# check write protection
#	o$writeProtected
#	# set write protection via virtual field
#	o$writeProtected<-FALSE
#	o$writeProtected
#	
#	
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
  "setWriteProtected",
  class="Sequence",
  function(
    this,
    value,
    ...
  ){

    if(!is.logical(value)) {throw("The new value must be logical!\n")}
    else {
      this$.write.protected<-value;
    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkWriteProtection
##
setMethodS3(
  ".checkWriteProtection",
  class="Sequence",
  function(
    this,
    value,
    ...
  ){

    if(getWriteProtected(this)) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Sequence
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
#       a<-Sequence()
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
	class="Sequence", 
	function(
		object,
		...
	){

		 this<-object;
		 this$.summary$"Name"<-this$name;
		 this$.summary$"Id"<-this$id;
		 this$.summary$"Length"<-this$length;
		 this$.summary$"Big rate"<-this$bigRate;
		 this$.summary$"Ancestral object"<-this$ancestral$id;

		 if(this$.cumulative.rate.flag) {
		 	this$.summary$"Cumulative rate flag"<-TRUE;
		 }
		 if(length(this$.flagged.sites) > 0 ) {
		 	this$.summary$"Flagged sites"<-paste(this$.flagged.sites,collapse=" ");
		 }
		 if(this$writeProtected) {
		 	this$.summary$"Write protected"<-TRUE;
		 }
		


			NextMethod();
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: as.character.Sequence
##	
###########################################################################/**
#
# @RdocMethod as.character
# 
# @title "Get the string representation of a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The string representation is the concatenation of the states of the 
#	aggregated Site object. Undefined states (NA-s) are represented by question marks.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create object
#	s<-Sequence(length=10)
#	# get character representation
#	as.character(s)
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
	class="Sequence", 
	function(
		x,
		...
	){

		getString(x);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
###########################################################################/**
#
# @RdocMethod plot
# 
# @title "Plot the total site rates for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE or FALSE.
# } 
# 
# \examples{
#	# create a nucleotide sequence with a process attached
#	s<-NucleotideSequence(string="ATGGCCA",processes=list(list(JC69())))
#	# plot total rates in range 1:4
#	plot(s,1:4)
#	# plot all total rates
#	plot(s)
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
	class="Sequence", 
	function(
		x,
		index=NA,
		...
	){
  
    this<-x;
    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
		if( length(unique(this$totalRates)) == 1 & any(is.na(unique(this$totalRates))) ){
      warning("The total rates are undefined, nothing to plot here!\n");
      return(invisible(FALSE));
		}
    else {
      if(missing(index)) {
        index<-seq(along.with=1:this$length,by=1);
      }
      if(this$.cumulative.rate.flag){
        .recalculateCumulativeRates(this);
      }
      what<-this$.total.rates[c(index)]

      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Total rate plot for sequence", this$id),
        xlab="Position",
        ylab="Total rate",
				ylim=c(0,max(what)),
        xlim=c(min(index),max(index)),
				xaxt="n"
      );
	axis(side=1, at=index, labels=index);
	
	return(invisible(TRUE));
    	}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plotParameterAtSites
##	
###########################################################################/**
#
# @RdocMethod plotParametersAtSites
# 
# @title "Plot the value of a site-process specifc paramter for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	The type of the paramter must be numeric. The Process object must be attached to all positions specified 
#	in the index vector. 
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{id}{The identifier of the site-process specific parameter.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE or FALSE.	
# } 
# 
# \examples{
#	# create a nucleotide sequence with a process attached
#	p<-JC69()
#	s<-NucleotideSequence(string="ATGGCCA",processes=list(list(p)))
#	# plot rate multipliers in range 1:4
#	plotParametersAtSites(s,p,"rate.multiplier",1:4)
#	# plot rate multiplier for the full sequence
#	plotParametersAtSites(s,p,"rate.multiplier")
# } 
# 
# @author 
# 
# \seealso{ 
# 	Site Process Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"plotParametersAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index,
		...
	){


    	if(this$length == 0) {
      	warning("The sequence leght is zero, nothing to plot here!\n");
      	return(invisible(FALSE));
    	}
    	if(missing(index)) {
      		index<-seq(along.with=1:this$.length,by=1);
    	}
		what<-apply(as.array(index),1,
			function(pos){
				tmp<-getParameterAtSites(this,process,id,pos)[[1]]$value;
				if(!is.numeric(tmp)){
					throw("Plot method failed becuase encountered non-numeric parameter value!\n");
				}
				return(tmp);
			}
		);
      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Plot of parameter",id,"for process",process$id),
        xlab="Position",
        ylab="Value",
        xlim=c(min(index),max(index)),
				ylim=c(0,max(what)),
				xaxt="n"
      );
	axis(side=1, at=index, labels=index);
	invisible(TRUE);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setDeletionTolerance
##	
###########################################################################/**
#
# @RdocMethod setDeletionTolerance
# 
# @title "Set the deletion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method does some error checking and the calls \code{setParameterAtSites(this=this,process=process,id="deletion.tolerance",value=value,index=index)}.
#	See \code{setParameterAtSites.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{value}{A numeric vector, recycled if shorter than the index vector.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	setParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"setDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

	 if(!exists(x="PSIM_FAST")){
		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
	}
		setParameterAtSites(this=this,process=process,id="deletion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getDeletionTolerance
##	
###########################################################################/**
#
# @RdocMethod getDeletionTolerance
# 
# @title "Get the deletion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method does some error checking and the calls \code{getParameterAtSites(this=this,process=process,id="deletion.tolerance",index=index)}.
#	See \code{getParameterAtSites.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	getParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"getDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

	 if(!exists(x="PSIM_FAST")){
		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
	}
		rm<-getParameterAtSites(this=this,process=process,id="deletion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertionTolerance
##	
###########################################################################/**
#
# @RdocMethod setInsertionTolerance
# 
# @title "Set the insertion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method does some error checking and the calls \code{setParameterAtSites(this=this,process=process,id="insertion.tolerance",value=value,index=index)}.
#	See \code{setParameterAtSites.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{value}{A numeric vector, recycled if shorter than the index vector.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	setParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"setInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){
	
	    if(!exists(x="PSIM_FAST")){
		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
            }
		setParameterAtSites(this=this,process=process,id="insertion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertionTolerance
##	
###########################################################################/**
#
# @RdocMethod getInsertionTolerance
# 
# @title "Get the insertion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method does some error checking and the calls \code{getParameterAtSites(this=this,process=process,id="insertion.tolerance",index=index)}.
#	See \code{getParameterAtSites.Sequence} for more details.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	getParameterAtSites.Sequence
# } 
# 
#*/###########################################################################
setMethodS3(
	"getInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

	 if(!exists(x="PSIM_FAST")){
		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
	}
		rm<-getParameterAtSites(this=this,process=process,id="insertion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: sampleStates
##	
###########################################################################/**
#
# @RdocMethod sampleStates
# 
# @title "Sample the states for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#	This method samples new states from the equlibrium distribution of the attched process(es) for sites
#	having undefined states (NA). 
#	If a site has more than one substitution process attached, then the method samples the new state from the 
#	mixture of equlibrium distributions. The weight of each equlibrium distribution is proportional to the 
#	site-process specific rate multiplier of the corresponding process at the given site. 
#
#	Sites having defined states are not touched. All sites with undefined states must have at least one 
#	substitution process (object inheriting from GeneralSubstitution) attached.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a nucleotide sequence
#	s<-NucleotideSequence(length=80)
#	# create some processes
#	jc69<-JC69(); 				# Jukes-Cantor
#	hky<-HKY(base.freqs=c(0.1,0.4,0.1,0.4)) # "GC-rich" HKY
#	# attach the processes
#	s$processes<-list(list(jc69)) # jc is attached to all sites
#	attachProcess(s,hky,60:80)  # hky is attached to range 60:80
#	# tweak rate multiplier for hky
#	setRateMultipliers(s,hky,10,60:80)
#	# set states in range 1:20
#	setStates(s,"A",1:20)
#	# sample remaining states
#	sampleStates(s)
#	# print sequence
#	s
# } 
# 
# @author 
# 
# \seealso{ 
# 	sampleState.GeneralSubstitution GeneralSubstitution
# } 
# 
#*/###########################################################################
setMethodS3(
	"sampleStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		if(!missing(index)){
			index<-.checkIndexSanity(this, index);
		}
		else {
			index<-seq(along.with=this$.sites);
		}
	
    for(site in this$.sites[index]){
    # Sample states from the equlibrium distributions if
    # the state is NA:
      if(is.na(site$state)){
        # Assemble the list of substitution processes:
        subst.proc<-list();
        for (proc in site$processes){
          if(is.GeneralSubstitution(proc)){
              subst.proc<-c(subst.proc, list(proc));
            }
        }

        # Complain if we have no substitution processes to sample from:
        if(length(subst.proc) == 0){
          throw("Site state is NA and no substitution processes are attached. Cannot sample state!\n");
        }

        site.rates<-as.numeric(lapply(
            subst.proc,
            function(proc){
              return(getParameterAtSite(proc, site,"rate.multiplier")$value);
            }
        ));

        # Normalizing site rates:
        site.rates<-site.rates/sum(site.rates);

        # Single subst process:
        if(length(subst.proc) == 1){
          site$state<-sampleState(subst.proc[[1]]);
        }
        else {
          # Sample a substitution process according to the rate multipliers:
          nproc<-sample(x=c(1:length(subst.proc)),size=1, replace=FALSE, prob=site.rates);
          # Sample the state from the winner process:
          site$state<-sampleState(subst.proc[[nproc]]);
        }
      } # if is.na...

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
## Method: clearStates
##	
###########################################################################/**
#
# @RdocMethod clearStates
# 
# @title "Set the states of a collection of Site objects aggregated by a Sequence object to undefined (NA)" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a nucleotide sequence
#	s<-NucleotideSequence(string="ATGC")
#	s
#	# set states to NA in the range 2:3	
#	clearStates(s,2:3)
#	s
#	# set all states to NA
#	clearStates(s)
#	s
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
	"clearStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		if(!missing(index)){
			index<-.checkIndexSanity(this, index);
		}
		else {
			index<-seq(along.with=this$.sites);
		}
	
    		for(site in this$.sites[index]){
			site$.state<-NA;
			site$.total.rate<-NA;
    		}
		
		this$.cumulative.rate.flag<-TRUE;

		return(invisible(this));
	
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setOmegas
##	
setMethodS3(
	"setOmegas", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
		# dummy method to force the creation of the generic function
		setOmegas.CodonSequence(this,process,index,...);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getOmegas
##	
setMethodS3(
	"getOmegas", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
		# dummy method to force the creation of the generic function
		getOmegas.CodonSequence(this,process,index,...);

		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSymbolFreqs
##	
###########################################################################/**
#
# @RdocMethod getSymbolFreqs
# 
# @title "Get a table with the frequencies of the states of a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A table.
# } 
# 
# \examples{
#	# create a nucleotide sequence
#	s<-NucleotideSequence(length=30,processes=list(list(JC69())))
#	# sample states
#	sampleStates(s)
#	# get state frequencies from ranges 1:10 and 20:30
#	getSymbolFreqs(s,c(1:10,20:30))
#	# get symbol frequencies for the full sequence
#	getSymbolFreqs(s)
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
	"getSymbolFreqs", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
		
			if(!missing(index)){
				index<-.checkIndexSanity(this, index);
			} else {
				index<-seq(along.with=this$.sites);
			}

			prop.table(table(as.character(lapply(this$.sites[index],getState))));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .discretizeGamma
##	
setMethodS3(
	".discretizeGamma", 
	class="Sequence", 
	function(
		this,
		shape,
		ncat,
		...
	){
		# figure out cutting points	
		cut<-apply(rbind(1:(ncat-1)/ncat),1, function(x){qgamma(x,shape=shape,rate=shape)})	
		cut<-c(0,cut,Inf);

		# incomplete gamma function
		Igamma<-function(x,a){
			pgamma(x,shape=a, scale=1)
		}

		# function to calculate a category mean
		cm<-function(a,b,shape,ncat){
			( Igamma(b * shape, shape+1) - Igamma(a * shape,shape+1)) * ncat;
		}
		
		# calculate category means
		means<-c();
		for (i in 1:(length(cut)-1)){
			means<-c(means,cm(cut[i], cut[i+1], shape, ncat));
		}
		
		# return a vector with the means
		return(means);

	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plusGamma
##	
###########################################################################/**
#
# @RdocMethod plusGamma
# 
# @title "Sample the rate multiplier parameters of a Process from a Gamma distribution for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#
#	The gamma distribution is discretized by calculating the means of the portions corresponding 
#	to the categories having equal probabilities. If the \code{ncat} argument is not numeric, the 
#	rates are sampled from the continuous gamma distribution.
# } 
#
# \references{
#	Yang, Z. (1994) Maximum likelihood phylogenetic estimation from DNA sequences with variable 
#	rates over sites: approximate methods - Journal of Molecular Evolution 39:306-314 \url{http://dx.doi.org/10.1007/BF00160154}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{shape}{The shape parameter of the gamma distribution.} 
# 	\item{index}{A vector of positions.} 
# 	\item{ncat}{Numer of categories in the discretized gamma distribution (4 by default).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible);
# } 
# 
# \examples{
#	# create a sequence
#	s<-NucleotideSequence(length=20)
#	# attach a process
#	p<-JC69()
#	attachProcess(s,p)
#	# get rate multipliers
#	getRateMultipliers(s,p)	# the default value is 1.0
#	# sample rate multipliers in range 1:5 from a discrete 
#	#gamma distribution with shape parameter 0.5
#	plusGamma(s,p,0.5,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)	# the default value is 1.0
#	# sample rates from a continuous gamma distribution 
#	# with shape parameter 0.5
#	plusGamma(s,p,0.5,ncat="cont")
#	# get rate multipliers
#	getRateMultipliers(s,p)
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
	"plusGamma", 
	class="Sequence", 
	function(
		this,
		process,
		shape,	
		index,
		ncat=4,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		
			if(missing(index)){
				index<-seq(along.with=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}
		
			if(!is.numeric(ncat)){
				# continuous gamma	
				setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(length(index),shape=shape,rate=shape),index=index);
			}
			else{
				setParameterAtSites(this, process=process, id="rate.multiplier",sample(.discretizeGamma(this,shape,ncat),size=length(index),replace=TRUE),index=index);
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
## Method: plusInvGamma
##	
###########################################################################/**
#
# @RdocMethod plusInvGamma
# 
# @title "Sample the rate multiplier parameters of a Process from an I+Gamma distribution for a collection of Site objects aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
#
#	The gamma distribution is discretized by calculating the means of the portions corresponding 
#	to the categories having equal probabilities. If the \code{ncat} argument is not numeric, the 
#	rates are sampled from the continuous gamma distribution.
# } 
#
# \references{
#	Gu X, Fu, YX, Li, WH (1995) Maximum likelihood estimation of the heterogeneity of substitution 
#	rate among nucleotide sites - Mol. Biol. Evol. 12(4):546-57 \url{http://bit.ly/aE6xF0}
#
#	Yang, Z (1994) Maximum likelihood phylogenetic estimation from DNA sequences with variable 
#	rates over sites: approximate methods - Journal of Molecular Evolution 39:306-314 \url{http://dx.doi.org/10.1007/BF00160154}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
# 	\item{process}{A Process object.} 
# 	\item{pinv}{The proportion of invariant sites.} 
# 	\item{shape}{The shape parameter of the gamma distribution.} 
# 	\item{index}{A vector of positions.} 
# 	\item{ncat}{Numer of categories in the discretized gamma distribution (4 by default).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible);
# } 
# 
# \examples{
#	# create a sequence
#	s<-NucleotideSequence(length=20)
#	# attach a process
#	p<-JC69()
#	attachProcess(s,p)
#	# get rate multipliers
#	getRateMultipliers(s,p)	# the default value is 1.0
#	# sample rate multipliers in range 1:5 from I+discrete Gamma
#	plusInvGamma(s,p,pinv=0.5,shape=0.5,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)	# the default value is 1.0
#	# sample rates from an I+continuos Gamma model
#	plusInvGamma(s,p,pinv=0.5,shape=0.5,1:5,ncat="cont")
#	# get rate multipliers
#	getRateMultipliers(s,p)
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
	"plusInvGamma", 
	class="Sequence", 
	function(
		this,
		process,
		pinv,
		shape,	
		index,
		ncat=4,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		else if(missing(pinv)){
			throw("No proportion of invariant sites given!\n");
		}
		else if(!all(is.numeric(pinv)) | length(pinv) != 1){
			throw("The pinv parameter must be a numeric vector of lenght 1!\n");	
		}
		else if(pinv > 1){
			throw("Tpe proportion of invariant sites cannot be larger than 1.!");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		else {
		
			if(missing(index)){
				index<-seq(along.with=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}

			# discretize gamma distribution
			dg<-c()
			if(is.numeric(ncat)){
				dg<-.discretizeGamma(this,shape,ncat);
			}

			# Iterating over the sites specified by the index vector:
			
			for(site in index){

				# Choose between invariant and gamma:
				type<-sample(c("INV","GAMMA"),size=1, replace=FALSE, prob=c( pinv, (1-pinv) ) );

				if(type == "INV"){
					setParameterAtSites(this, process=process, id="rate.multiplier",value=0,index=c(site));
				}
				else {
					if(!is.numeric(ncat)){
						# continuous gamma	
						setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(1,shape=shape,rate=shape),index=c(site));
					}
					else{
						setParameterAtSites(this, process=process, id="rate.multiplier",sample(dg,size=1),index=c(site));
					}
				}
		
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
## Method: insertSequence
##
###########################################################################/**
#
# @RdocMethod insertSequence
# 
# @title "Insert a Sequence object into another Sequence object after a specified position" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{The target Sequence object.} 
# 	\item{insert}{The Sequence object to be inserted.} 
# 	\item{position}{The position after the Sequence object will be inserted.} 
# 	\item{process}{The Process object performing the insertion (optional).} 
# 	\item{paranoid}{If TRUE, then the consistency of teh target objects is checked more rigurously after insertion.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create some sequence objects.
#	target<-NucleotideSequence(string="AAAAAAAAAAA")
#	insert<-NucleotideSequence(string="GGGGGGGGGGG")
#	# insert after position 5
#	insertSequence(target,insert,5)
#	# print the target sequence
#	target
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
  "insertSequence",
  class="Sequence",
  function(
    		this,
		insert,
		position,
		process=NA,
		paranoid=FALSE,
    ...
  ){

		.checkWriteProtection(this);
		 if(!exists(x="PSIM_FAST")){

		if(missing(insert)) {
			throw("Insert sequence object is missing!\n");
		}
		else if (missing(position)) {
			throw("Insertion position is missing!\n");
		}

		if(!is.Sequence(insert)) {
			throw("Insert object not valid!\n");
		}
		else if (this$length == 0 & position != 0 ) {
			throw("Acceptor sequence length is zero! The only valid insertion position is 0!\n");	
		}
		else if ( !( position >= 0 & position <=(this$.length + 1))) {
			throw("Insertion position ",position," is invalid!\n");
		}
	        
	        }

	 # Just return if insert has zero length:	
	 if(insert$length == 0){
			warning("The length of the sequence to be inserted is zero! Nothing to do here!\n");
			return(invisible(FALSE));	
	 } 
	 # Clone insert object:
	 insert<-clone(insert);

	 # Set the generator process:
	 if(!missing(process)) {
			if( (length(process) == 0) | !is.Process(process)){
				throw("Process object invalid!\n");
			}
	 } else {
			process<-Sequence$.root.ins;
	 }
	 for(site in insert$.sites) {
				site$.ancestral<-process;
				site$.sequence<-this;
	 }

	 # Recalculate cumulative rates if the flag is on:
	 if(this$.cumulative.rate.flag) {
			.recalculateCumulativeRates(this);
	 }

	 # Flagging cumulative rates:
		this$.cumulative.rate.flag<-TRUE;

		# Inserting new site objects:

		if ( position == this$.length) {
			# Insertion at the end of the sequence;
			this$.sites<-c(this$.sites,insert$.sites);
			this$.total.rates<-c(this$.total.rates,rep(c(NA),times=insert$.length) );
			this$.cumulative.rates<-c(this$.cumulative.rates,rep(NA,times=insert$.length) );

		} else if (position == 0) {
			# Insertion in the sequence
			this$.sites<-c(insert$.sites, this$.sites);
			this$.total.rates<-c(rep(NA,times=insert$.length),this$.total.rates);
      			this$.cumulative.rates<-c(rep(NA,times=insert$.length),this$.cumulative.rates);

		} else {
			# Insertion at position 0
			this$.sites<-c(this$.sites[1:position],insert$.sites,this$.sites[(position+1):this$.length]);
			this$.total.rates<-c(this$.total.rates[1:position],rep(NA,times=insert$.length),this$.total.rates[(position+1):this$.length]);
			this$.cumulative.rates<-c(this$.cumulative.rates[1:position],rep(NA,times=insert$.length),this$.cumulative.rates[(position+1):this$.length]);

		}

	 # Checking if lengths are consistent:
	  if(!exists(x="PSIM_FAST")){	

		if(length(this$.sites) != (this$.length + insert$.length)) {
			throw("Length inconsistency after insertion!\n");
		}
	  }

	# Setting new length:	
	this$.length<-(this$.length + insert$.length);
	# Flagging the inserted sites:
		this$.flagged.sites<-c(this$.flagged.sites,(position+1):(position+insert$.length));

	 if(!exists(x="PSIM_FAST")){
		if(length(this$.total.rates) != this$.length) {
			throw("Total rates vector inconsistency after insertion!\n");
		}
		if(length(this$.cumulative.rates) != this$.length) {
			throw("Cumulative rates vector inconsistency after insertion!\n");
		}
	}

	# Recalculating cumulative rates:
		.recalculateCumulativeRates(this);

	# Paranoid check of total rates:

	if(paranoid) {	
		for (i in 1:this$.length) {
				if(this$.sites[[i]]$totalRate != this$.total.rates[[i]]) {
					throw("Object total rates inconsistent with total rates vector!\n");
				}
		}
	}

	# Deleting the insert:
	rm(insert);

	return(invisible(this));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: deleteSubSequence
##
###########################################################################/**
#
# @RdocMethod deleteSubSequence
# 
# @title "Delete a collection of sites aggregated by a Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An index vector specifying a collection of sites to be deleted. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE or errror.
# } 
# 
# \examples{
#	# create a nucleotide sequence
#	s<-NucleotideSequence(string="ATATATATATATATAT")
#	# delete sites 2, 4 and 6
#	deleteSubSequence(s,c(2,4,6))
#	s
#	# delete sites in the range 3:6
#	deleteSubSequence(s,3:6)
#	s
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
  "deleteSubSequence",
  class="Sequence",
  function(
    		this,
		index,
    		...
  ){
		
		if(length(index) == 0) {
			return(FALSE);
		}

		.checkWriteProtection(this);
	 	if(!exists(x="PSIM_FAST")){

		if(missing(index)) {
			 throw("No index vector specified!\n");
		}


		if(length(index) == 0) {
			return(FALSE);
		}
		
		}
			index<-.checkIndexSanity(this, index);

			# Avoid deletion on dirty sequence as
			# that will cause havoc.
			if(this$.cumulative.rate.flag){
			 .recalculateCumulativeRates(this);	
			}
			# Flagging cumulative rates:	
			this$.cumulative.rate.flag<-TRUE;
			min.index<-min(index);
			# Deleting site objects:	
			this$.sites[index]<-NULL;
			# Updating rate vectors:
			this$.total.rates<-this$.total.rates[-index];
			this$.cumulative.rates<-this$.cumulative.rates[-index];
			
			# Flag the site before the deletion to
			# to force cumulative rate recalculation:
			if (min.index > 2 ) {
				this$.flagged.sites<-c(this$.flagged.sites,(min.index - 1));
			}

		 if(!exists(x="PSIM_FAST")){
			if( length(this$.sites) != (this$.length - length(index) ) ) {
				throw("Inconsistency after deleting sites!\n");
			} 
		 }
	
			this$.length<-length(this$.sites);
			.recalculateCumulativeRates(this);	
				return(invisible(TRUE));
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: copySubSequence
##
###########################################################################/**
#
# @RdocMethod copySubSequence
# 
# @title "Copy a collection of Site objects aggregated by a Sequence object into a new Sequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Sequence object.} 
#	\item{index}{An index vector specifying a collection of sites to be copied. It is set to 1:seq$length if omitted.}
# 	\item{process}{The Process object performing the copy (optional).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A new Sequence object.
# } 
# 
# \examples{
#	# create a nucleotide sequence
#	s<-NucleotideSequence(string="ATATATATATATATATA")
#	# copy sites in the range 3:8 in a new object
#	s2<-copySubSequence(s,3:8)
#	s2
#	# copy sites 1,3 and 5 from s2
#	s3<-copySubSequence(s2,c(1,3,5))
#	s3
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
  "copySubSequence",
  class="Sequence",
  function(
    		this,
		index,
		process=NA,
    		...
  ){

		if(missing(index)) {
				index<-seq(along.with=this$.sites);
		}
		else {
			index<-.checkIndexSanity(this, index);
		}

	 if(!exists(x="PSIM_FAST")){
		if(!is.na(process) & !is.Process(process)) {
			throw("Process object invalid!\n");		
		} 
	}

			# Avoid copying from dirty sequence:
			if(this$.cumulative.rate.flag){
			 .recalculateCumulativeRates(this);	
			}

			length<-length(index);

			# Create an empty sequence object:
			copy<-Sequence();

			# Flag copy cumulative rates:
			copy$.cumulative.rate.flag<-TRUE;

			if(is.na(process)){
				# Getting the root insertion process:
				process<-Sequence$.root.ins;
			}
			# Setting the ancestral to sequence:
			copy$.ancestral.obj<-process;
			
			# Setting copy name:
			copy$name<-paste("Copied from",this$name);

			# Setting length:
			copy$.length<-length;

			# Clone the sites:
			copy$.sites<-lapply(this$.sites[index],
				function(site){
					site.copy<-clone(site);
					site.copy$.ancestral<-process;
					return(site.copy);
				}
			);

			# Copy total rates:	
			copy$.total.rates<-this$.total.rates[index];

			# Create cumulative rates vector:
			copy$.cumulative.rates<-cumsum(copy$.total.rates);

			copy$.cumulative.rate.flag<-FALSE;

		 if(!exists(x="PSIM_FAST")){
			if(length(copy$.sites) != length){
				throw("Sites list length mismatch!\n")
			}
			else if(length(copy$.total.rates) != length){
				throw("Total rates vector length mismatch!\n")
			}
			else if(length(copy$.cumulative.rates) != length){
				throw("Cumulative rates vector length mismatch!\n")
			}
		}

			return(copy);
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
