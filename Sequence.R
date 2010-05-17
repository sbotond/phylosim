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
#	lists aggregating Site objects. The class has fileds for keeping track of cumulative
#	site rates, the sum of all active event rates and methods for performing actions 
#	on a collection of sites (positions).
#
#	The Sequence objects have a field specifying an ancestral object, which can be a Sequence
#	object (when the object is obtained through clone() ) or a Process object 
#	(for newly created objects).
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
#	\item{name}{The name of the Sequence object.}
# 	\item{string}{A string containing symbols belonging to the assotiated Alphabet object. 
#	It can be used to set the initial states of the aggregated Site objects. It also specifies th length of the sequence}
# 	\item{length}{The length of the sequence. It cannot be used when 'string' is specified.}
# 	\item{alphabets}{A list of Alphabet objects to be assotiated with the Site objects. 
#	The list is recycled in the case it is shorter than the sequence length.}
# 	\item{processes}{A list of Process objects to be attached 
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
#
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
					str<-strsplit(string,split="")[[1]];
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
					 this$.sites[[position]]<-clone(site.template);
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
## Method: is.Site
##	
###########################################################################/**
#
# @RdocMethod is
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
		ommit.sites=FALSE,
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
		if(!ommit.sites){
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
								str<-paste(str,paste(rep("?",site$.alphabet$symbolLength),collapse=""),sep="");
							}
							else {
								str<-paste(str,"?",sep="");
							}
					}
					else {
						str<-paste(str,site$.state,sep="");
					}
			}
			return(str);

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
# 	\item{index}{A numeric vector specifying a set of positions. 
#	It is set to 1:seq$length if ommited.} 
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
      index<-seq(along=this$.sites);
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
#	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.}
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
      index<-seq(along=this$.sites);
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
		.flagCumulativeRates(this);
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
## Method: getStatesList
##	
###########################################################################/**
#
# @RdocMethod getStatesList
# 
# @title "Get an object representing the list of the states from a Sequence object" 
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
# 	A StatesList object.
# } 
# 
# \examples{
#	# create a Sequence object
#	s<-Sequence(alphabets=list(NucleotideAlphabet()),string="ATGGC")
#	# get a StatesList object
#	sl<-getStatesList(s)
#	print(sl)
#	# get the StatesList object via virtual field
#	s$statesList
# } 
# 
# @author 
# 
# \seealso{ 
# 	StatesList Sequence 
# } 
# 
#*/###########################################################################
setMethodS3(
	"getStatesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		StatesList(seq=this);	

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
# 	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.} 
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
      index<-seq(along=this$.sites);
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
# 	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.} 
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
		sloppy=FALSE,
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
      index<-seq(along=this$.sites);
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
				# Arguments were verified before, using the sloppy method:
				if(sloppy) {
					.setAlphabetSloppy(this$.sites[[i]], value[[value.counter]]);
				} else {
					setAlphabet(this$.sites[[i]], value[[value.counter]]);
				}

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
## Method: getAlphabetsList
##	
###########################################################################/**
#
# @RdocMethod getAlphabetsList
# 
# @title "Get an object representing a list of Alphabet objects assotiated to the Site objects aggregated by a Sequence object" 
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
# 	An AlphabetsList object.
# } 
# 
# \examples{
#	# create a Sequence object with NucleotideAlphabet
#	#and BinaryAlphabet objects attached
#	s<-Sequence(alphabets=list(NucleotideAlphabet(),BinaryAlphabet()),length=5)	
#	# get an AlphabetsList object
#	getAlphabetsList(s)
#	# get alphabets list via virtual field
#	s$alphabetsList
# } 
# 
# @author 
# 
# \seealso{ 
# 	AlphabetsList Alphabet Sequence Site
# } 
# 
#*/###########################################################################
setMethodS3(
	"getAlphabetsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$alphabetsList<-NULL;
		AlphabetsList(seq=this);	

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
# @title "Get the list of unique Alphabet objects assotiated to Site objects aggaregated by a Sequence object" 
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
#	s<-Sequence(alphabets=list(NucleotideAlphabet(),BinaryAlphabet(),NucleotideAlphabet()),length=10)	
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
# 	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.} 
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
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      		index<-seq(along=this$.sites);
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
# 	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.} 
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
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      		index<-seq(along=this$.sites);
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
# 	\item{index}{A numeric vector specifying a set of positions. It is set to 1:seq$length if ommited.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of lists of Process objects.
# } 
# 
# \examples{
#	# create a sequence object with some processes attached
#	s<-Sequence(length=4,alphabets=list(NucleotideAlphabet()),processes=list(list(JC69(),K80()),list(GTR())))
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
      	index<-seq(along=this$.sites);
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
#	s<-Sequence(length=4,alphabets=list(NucleotideAlphabet()),processes=list(list(p,K80()),list(p)))
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
# @title "" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{}{} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 
# } 
# 
# \examples{
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
	"setProcesses", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		sloppy=FALSE,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}

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
      		index<-seq(along=this$.sites);
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
		if (sloppy == FALSE) {
				setProcesses(this$.sites[[i]], value[[value.counter]]);
		} else {
			.setProcessesSloppy(this$.sites[[i]], value[[value.counter]]);
		}
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

		if (missing(index)) {
			index<-seq(along=this$.sites);
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
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
		}
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
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
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

		if (missing(index)) {
			index<-seq(along=this$.sites);
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
## Method: getProcessesList
##	
setMethodS3(
	"getProcessesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		ProcessesList(seq=this);

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
## Method: getEventsList
##	
setMethodS3(
	"getEventsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		getEvents(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setEventsList
##	
setMethodS3(
	"setEventsList", 
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
## Method: getTotalRates
##	
setMethodS3(
	"getTotalRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){


 		if (missing(index)) {
      index<-seq(along=this$.sites);
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
## Method: setTotalRatesList
##	
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
setMethodS3(
	"getCumulativeRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

 		if (missing(index)) {
      index<-seq(along=this$.sites);
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
		
		# No flagged sites, we have a fresh start:	

		if( length(this$.flagged.sites) == 0 ) {
			if( this$.length > 0 ) {
				# Calculate total rates:
				for(i in 1:this$.length) {
					this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
				}
				this$.cumulative.rates<-cumsum(this$.total.rates);	
				this$.flagged.sites<-integer(0);

			}
	
		} else {
			if( this$.length > 0 ) {

				# We have some flagged sites, recalculate just their total rates:
        for(i in this$.flagged.sites) {
          this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
        }

				# The site before the first flagged site: 
				min.index<-(min(this$.flagged.sites) - 1);
				
				if(min.index > 1){
					
					# Do the cumsum only on the dirty part:
					new.cumrates<-this$.cumulative.rates[1:(min.index-1)];
					new.cumrates<-c(new.cumrates, cumsum( c(this$.cumulative.rates[min.index], this$.total.rates[(min.index+1):length(this$.total.rates) ] ) ) );
					this$.cumulative.rates<-new.cumrates;

				} else {
        	this$.cumulative.rates<-cumsum(this$.total.rates);
				}
				
				# Cleaning out flagged sites.
				this$.flagged.sites<-integer(0);
				
      } #/else
	
		}
	
		this$.cumulative.rate.flag<-FALSE;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .flagCumulativeRates
##	
setMethodS3(
	".flagCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){
		
		this$.cumulative.rate.flag<-TRUE;

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
			#getCumulativeRatesFromRange(this, this$.length);
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
setMethodS3(
	"clone", 
	class="Sequence", 
	function(
		this,
		...
	){

		# Cloning the whole sequence object:
		that<-clone.Object(this);

		# Disabling write protection:

		if(that$writeProtected) {
			that$writeProtected<-FALSE;
		}

		# Setting the ancestral sequence:
		that$.ancestral.obj<-this;

		# Resetting comments:
		that$.comments<-list();

		# Cloning sites;
		if(this$length > 0) {
					for (i in 1:this$.length) {
						site<-this$.sites[[i]];
						clone<-clone(site);
						clone$.ancestral<-site;
						clone$.sequence<-that;
						that$.sites[[i]]<-clone;
			}
		}

		# Setting the name:
		that$name<-paste("clone of",this$.name);
	
		return(that);

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
        index<-seq(along=1:this$length,by=1);
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
setMethodS3(
	"plotParametersAtSite", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index=NA,
		...
	){


    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
    if(missing(index)) {
      index<-seq(along=3:this$length,by=1);
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

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
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
setMethodS3(
	"getDeletionTolerance", 
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
		if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
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

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
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
setMethodS3(
	"getInsertionTolerance", 
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
		if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
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
			index<-seq(along=this$.sites);
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
			index<-seq(along=this$.sites);
		}
	
    for(site in this$.sites[index]){
			site$.state<-NA;
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
		
		# FIXME	- dummy method to force the creation of the generic function
		warning("This is just a dummy method!");

		
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
		
		# FIXME	- dummy method to force the creation of the generic function
		warning("This is just a dummy method!");

		
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
setMethodS3(
	"getSymbolFreqs", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
			if(!missing(index)){
				index<-.checkIndexSanity(this, index);
			} else {
				index<-seq(along=this$.sites);
			}

			prop.table(table(as.character(lapply(this$.sites[index],getState))));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

