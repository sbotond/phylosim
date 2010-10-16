##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
# @RdocClass Site
# 
# @title "The Site class"
# 
# \description{ 
#
#	This is the class representing a site. Site objects can have one associated Alphabet object and one or 
#	more Process objects that act on their states.
#	The associated Process and Site objects must have associated Alphabet objects with the same symbols set, or
#	at least one of the Alphabet objects should inherit from the class AnyAlphabet.
#
#	Site objects store the site-process-specific parameters of the attached Process objects. 
#	A site-process-specific parameter is a list containing: the identifier, the name, the value and type of the parameter.
#	For example the ubiquitous rate multiplier site-process-specific parameter looks like 
#	\code{list(id="rate.multiplier",name="Rate multiplier",value=1,type="numeric")}.
#
#	Templates for site-process-specific parameters and their default values are stored in the Process objects and 
#	copied into the Site object when the process is attached.
#
#	Site objects have fields for associated ancestral Site objects and Sequence objects.
#
#  	@classhierarchy
# 
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{state}{A symbol belonging to the specified alphabet.}
# 	\item{alphabet}{An alphabet object.}
# 	\item{ancestral}{The ancestral Site object.}
# 	\item{processes}{A list of Process objects.}
# 	\item{sequence}{The Sequence object to which the Site object belongs.}
#	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
#               @allmethods
# }
# 
# \examples{ 
#		# create a site object
#		site<-Site();
#		# print the character representation (state)
#		print(site);
#		# get a summary
#		summary(site);
#		# create a site with a nucleotide alphabet
#		site<-Site(state="A",alphabet=NucleotideAlphabet());
#		# set site state
#		site$state<-"G"
#		# manipulate the associated Sequence object
#		site$sequence<-Sequence()
#		site$sequence
#		# attach a substitution process
#		site$processes<-list(JC69())
#		# add one more substitution process
#		attachProcess(site,K80())
#		# get a list of active Event objects
#		site$events
#		# get a summary of the Site object
#		summary(site);
# }
# 
# @author
#
#
# \seealso{ 
#		Alphabet, Process, Event
# }
# 
#*/###########################################################################
setConstructorS3(
	"Site",
	function(
		state=NA,	#
		alphabet=NA,
		ancestral=NA,
		processes=NA,
		sequence=NA,
		...
	){
	

		# Extend the PSRoot Class:
		this<-extend(
			PSRoot(),
			"Site",
			.state=NA,
			.ancestral=NA,
			.alphabet=NA,
			.processes=list(),
			.total.rate=NA,
			.sequence=NA,
			.is.site=TRUE
		);
		
		# The instance is static by default:
		STATIC<-TRUE;

		# Set alphabet if present:	
		if(!missing(alphabet)){
			this$alphabet<-alphabet;
			STATIC<-FALSE;
		}

		# Alphabet is mandatory if 
		# ancestral is present:

		if (!missing(ancestral) & missing(alphabet) & !is.Process(ancestral)) {
				throw("Ancestral object sepcified, but no alphabet is given!\n");
		}

		# Set ancestral pointer if present:	
		if(!missing(ancestral)){
			# The ancestral is a site or a process:
			if( !is.Process(ancestral) & !is.Site(ancestral)) {
					throw("The ancestral object must be a site or a process!\n");	
			} else {
				this$.ancestral<-ancestral;
				STATIC<-FALSE;
			}
		}

		# Set state if present,
		# complain if no alphabet is specified:
		if (!missing(state)) {
				STATIC<-FALSE;
				if(!missing(alphabet)){
					this$state<-state;
				} else { throw("The state is specified, but no alphabet is given!\n"); }
		}
	
		# Set the processes:		
		if(!missing(processes)){	
			this$processes<-processes;
		}
		

		# Set the parent sequence if present:		
		if(!missing(sequence)){	
			this$sequence<-sequence;
		}

		# Calculate total rate given the state
		# and the processes:
		if(!STATIC){ 
			if(!is.na(this$.state)) {
				.recalculateTotalRate(this);
			}
			.checkConsistency(this);
		}

		# Return the Site object:
		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.Site
##	
###########################################################################/**
#
# @RdocDefault is.Site
# 
# @title "Check if an object is an instance of the Site class" 
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
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	
#	# create an object
#	s<-Site();
#	# check whether is a Site object
#	is.Site(s)
#	# the same with an Event object
#	is.Site(Event());
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
	"is.Site", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
		if(!is.null(this$.is.site)){return(TRUE)}
    if ( inherits(this, "Site")) {
			this$.is.site<-TRUE;
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
## Method: .checkConsistency
##	
setMethodS3(
	".checkConsistency", 
	class="Site", 
	function(
		this,
		...
	){
			message<-"Site state is inconsistent! ";		

			# If the ancestral object is a site:	
			if (is.Site(this$.ancestral)) {
				
					#Check if the alphabets match:		
					# Warning: using the '!='.Alphabet here!
					if( this$alphabet != this$ancestral$alphabet ) {
						throw(message, "The ancestral alphabet and the site alphabet is different!\n");
					}
			} else if (is.Process(this$.ancestral)) { 
					# Hook for checking the process object;
					# print(this$.ancestral)			
			} else if (!is.na(this$.ancestral)){
					throw("Ancestral object is invalid!\n");
			}
			
			# Check if the total rate is numeric or NA:
			if(is.null(this$.total.rate)) {
				throw("The total rate is NULL!\n");
			}
			if (!is.numeric(this$.total.rate) && !is.na(this$.total.rate)) {throw(message,"The total rate is not numeric!\n")}
				
			return(invisible(TRUE));
				

	},
	private=TRUE,
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
	class="Site", 
	function(
		this,
		...
	){

			#cat("Checking site consistency ...\n");
			# Reassigning the values by virtual fields.
			# The methods should complain if something is wrong.
			# Slow but maybe elegant.

			if(is.null(this$.alphabet)) {
				throw("Site alphabet is NULL!\n");
			}
			else if(!is.na(this$.alphabet)) {
				this$.alphabet<-this$.alphabet;
			}

			
			if(is.null(this$.ancestral)) {
				throw("Ancestral object is NULL!\n");
			}
			
			if(is.null(this$.processes)) {
				throw("Process list is NULL!\n");
			}
			else {
				this$processes<-this$processes;
			}
			
			.checkConsistency(this);
			
			lapply(
				this$.processes,
				function(p) {
						# Even more paranoid check is possible here!
						.checkSiteSpecificParamList(p$object,plist=p$site.params);
				}
			);
			return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);



##	
## Method: getState
##	
###########################################################################/**
#
# @RdocMethod getState
# 
# @title "Get the current state of a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one contaning the state (a symbol belonging to the attached Alphabet object).
# } 
# 
# \examples{
#	
#	# create a Site object with an Alphabet object attached
#	s<-Site(alphabet=Alphabet(symbols=c(0,1)), state=1);
#	# get current state
#	getState(s)
#	# get state via virtual field
#	s$state
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
	"getState", 
	class="Site", 
	function(
		this,
		...
	){
	
		this$.state;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setState
##	
###########################################################################/**
#
# @RdocMethod setState
# 
# @title "Set the state of a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{new.state}{A character vector of length one, containing a symbol belonging to the attached Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Returns the new state (invisible).	
# } 
# 
# \examples{
#
#       # create a Site object with an Alphabet object attached
#       s<-Site(alphabet=Alphabet(symbols=c(0,1)), state=1);
#       # set a new state 
#       setState(s,"0")
#       # get state via virtual field
#       s$state
#       # set a new state via virtual field
#       s$state<-1
#	s$state
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
	"setState", 
	class="Site", 
	function(
		this,
		new.state,
		...
	){
		
		new.state<-as.character(new.state);	

		 if(!exists(x="PSIM_FAST")){

		# Check if new.state is scalar:
		if (length(new.state) != 1 ){throw("The state must be a vector of length 1!\n")}
		# Check if the site has an alphabet attached:
		else if(is.na(this$alphabet)) {throw("Cannot set state because the site has no alphabet attached!\n")}
		# Check if symbol is in the site alphabet:	
		else if( !hasSymbols(this$.alphabet,new.state)) {throw("Symbol not in site alphabet!\n")}

		}

		this$.total.rate<-NA;
		
		if(!is.na(this$.sequence)){
		this$.sequence$.cumulative.rate.flag<-TRUE;
		}
		this$.state<-new.state;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
###########################################################################/**
#
# @RdocMethod getAlphabet
# 
# @title "Get the Alphabet object attached to a Site object" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An Alphabet object.
# } 
# 
# \examples{
#	
#	# create a site object
#	s<-Site()
#	# create an Alphabet object
#	a<-Alphabet(c("A","T","G"))
#	# attach alphabet to site object
#	setAlphabet(s,a)
#	# get attached alphabet
#	getAlphabet(s)
#	# get attached alphabet via virtual field
#	s$alphabet
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
	"getAlphabet", 
	class="Site", 
	function(
		this,
		...
	){
	
		this$.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
###########################################################################/**
#
# @RdocMethod setAlphabet
# 
# @title "Attach an Alphabet object to a Site object" 
# 
# \description{ 
#	@get "title".
#	If the ancestral site is not NA, then the symbol set of the ancestral Alphabet object and the new Alphabet 
#	object must be the same. The current state must be in the symbol set of the new Alphabet object, unless it is NA.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{new.alphabet}{A valid Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Returns the new Alphabet object (invisible).
# } 
# 
# \examples{
#	
#	# create a site object
#	s<-Site()
#	# create an Alphabet object
#	a<-Alphabet(c("A","T","G"))
#	# attach alphabet to site object
#	setAlphabet(s,a)
#	# set site state
#	s$state<-"A"
#	# clone the alphabet object
#	b<-clone(a)
#	# modify symbol set in b
#	b$symbols<-c(b$symbols,"C")
#	# attach b to s via virtual field
#	s$alphabet<-b
#	s$alphabet
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
	"setAlphabet", 
	class="Site", 
	function(
		this,
		new.alphabet,
		...
	){
	
	 if(!exists(x="PSIM_FAST")){	

		if(!is.Alphabet(new.alphabet)){
				throw("The supplied alphabet object is not valid!\n");
		} else if (is.Site(this$.ancestral)) {
				if (this$.ancestral$alphabet != new.alphabet) {
							throw("The alphabet is not equivalent with the ancestral alphabet!\n");
					}
		} 
		else if(!is.na(this$.state) & !hasSymbols(new.alphabet, this$.state)){
			throw("The current state is not part of the new alphabet!\n");
		}

		}
		
		this$.total.rate<-NA;
		if(!is.na(this$.sequence)){
		this$.sequence$.cumulative.rate.flag<-TRUE;
		}
		this$.alphabet<-new.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .setAlphabetSloppy
##	
setMethodS3(
	".setAlphabetSloppy", 
	class="Site", 
	function(
		this,
		new.alphabet,
		...
	){
	
		this$.alphabet<-new.alphabet;
		return(invisible(new.alphabet));

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
# @title "Get the ancestral object of a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Site object, a Process object or NA.
# } 
# 
# \examples{
#	a<-Alphabet();
#	# create some site object
#	s1<-Site(alphabet=a)
#	s2<-Site(ancestral=s1, alphabet=a)
#	# get ancestral objects
#	getAncestral(s1)
#	s2$ancestral
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
	class="Site", 
	function(
		this,
		...
	){
		
		this$.ancestral;
		
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
# @title "Forbidden action: setting the ancestral object for a Site object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{A Site object.}
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
	"setAncestral", 
	class="Site", 
	function(
		this,
		value,
		...
	){
		
		throw("You should never try to modify directly the ancestral attribute!\n");		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .recalculateTotalRate
##	
setMethodS3(
	".recalculateTotalRate", 
	class="Site", 
	function(
		this,
		...
	){

if(!is.na(this$.state)){
  total.rate<-0;	
  proc<-this$.processes;
  for (p in lapply(names(proc),function(id){proc[[id]][["object"]]})) {
                                for(e in getEventsAtSite(p, this)){
					total.rate<- total.rate + e$.rate;
                                }
  }
			
  this$.total.rate<-total.rate	

}
	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRate
##	
###########################################################################/**
#
# @RdocMethod getTotalRate
# 
# @title "Get the total active event rate" 
# 
# \description{ 
#	@get "title".
#	The total rate is the sum of the rates of all active events given the current state of the Site object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#
#	# create a nucleotide site with a JC69 substitution process attached
#       s<-Site(state="A",alphabet=NucleotideAlphabet(),processes=list(JC69()))
#       # get the total rate
#	getTotalRate(s)
#       # add a new process
#       attachProcess(s,K80(rate.params=list("Alpha"=1,"Beta"=0.5)))
#       # get the total rate via virtual field
#       s$totalRate
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
	"getTotalRate", 
	class="Site", 
	function(
		this,
		...
	){

		tr<-this$.total.rate;		
		if(is.na(tr)) {
			.recalculateTotalRate(this);
			return(this$.total.rate);
		} else {
			return(tr);
		}

		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTotalRate
##	
###########################################################################/**
#
# @RdocMethod setTotalRate
#
# @title "Forbidden action: setting the total active event rate for a Site object"
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
	"setTotalRate", 
	class="Site", 
	function(
		this,
	  	value,	
		...
	){
		
		throw("You should never try to set the totalRate directly!\n");	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: flagTotalRate
##	
###########################################################################/**
#
# @RdocMethod flagTotalRate
# 
# @title "Flag the total event rate" 
# 
# \description{ 
#	@get "title".
#	This method sets the cached total active event rate to NA, which will trigger its
#	recalculation when next accessed via getTotalRate.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible NA.
# } 
# 
# \examples{
#	# create a site object:
#	p<-K80(rate.params=list("Alpha"=2,"Beta"=0.5))
#	s<-Site(alphabet=NucleotideAlphabet(), state="G", processes=list(p))
#	# get site rate
#	s$totalRate
#	# modifying site object in a dangerous way (do not do this under any circumstances!)
#	s$.processes = list()	# site object is now inconsistent!
#	# get cached rate
#	s$totalRate		# incorrect value 
#	# flag total rate
#	flagTotalRate(s)
#	# get site rate
#	s$totalRate		# correct value
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
	"flagTotalRate", 
	class="Site", 
	function(
		this,
		...
	){
		
		# Setting .total.rate to NA,
		# this will force recalculation
		# when next accessed.	
		this$.total.rate<-NA;

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
# @title "Get the list of active event objects given the current state of the Site object" 
# 
# \description{ 
#	@get "title".
#	The list of active event object might change according to the state of the Site object.	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of event objects.
# } 
# 
# \examples{
#	# create a site object with a JC69 substitution process attached
#	s<-Site(alphabet=NucleotideAlphabet(), state="A",processes=list(JC69()))
#	# get the list of active event objects
#	getEvents(s)
#	# modify site state
#	s$state<-"T"
#	# get the list of active event objects via virtual field
#	s$events
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
	"getEvents", 
	class="Site", 
	function(
		this,
		...
	){

		procs<-lapply(names(this$.processes),function(id){this$.processes[[id]][["object"]]});

		tmp<-list();
		for (p in procs) {
				tmp<-c(tmp,getEventsAtSite(p, this));
		}
		return(tmp);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

setMethodS3(
	".getEventRates", 
	class="Site", 
	function(
		this,
		...
	){

	tmp<-double();
	for (p in lapply(names(this$.processes),function(id){this$.processes[[id]][["object"]]})) {
				for(e in getEventsAtSite(p, this)){
					tmp<-c(tmp,e$.rate);
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

##	
## Method: setEvents
##	
###########################################################################/**
#
# @RdocMethod setEvents
#
# @title "Forbidden action: setting the list of active events for a Site object"
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
	"setEvents", 
	class="Site", 
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
## Method: getSequence
##	
###########################################################################/**
#
# @RdocMethod getSequence
# 
# @title "Get the Sequence object associated with a given Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object or NA.
# } 
# 
# \examples{
#	# create a site object
#	s<-Site(sequence=Sequence())
#	# get the associated Sequence object
#	getSequence(s)
#	# get the associated Sequence object via virtual field	
#	s$sequence
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
	"getSequence", 
	class="Site", 
	function(
		this,
		...
	){

		this$.sequence;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSequence
##	
###########################################################################/**
#
# @RdocMethod setSequence
# 
# @title "Assotiate a Sequence object with a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{new.seq}{A valid Sequence object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object (invisible).
# } 
# 
# \examples{
#	# create a site object
#	s<-Site()
#	# get associated Sequence object
#	s$sequence
#	# set associated Sequence object
#	setSequence(s,Sequence())
#	s$sequence
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
	"setSequence", 
	class="Site", 
	function(
		this,
		new.seq,
		...
	){

	 if(!exists(x="PSIM_FAST")){
		if(!is.Sequence(new.seq)) {
			throw("Sequence object invalid!\n");
		} 
	}
		
		this$.sequence<-new.seq;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
##	
## Method: as.character
##	
###########################################################################/**
#
# @RdocMethod as.character
# 
# @title "Get the character representation of a Site object" 
# 
# \description{ 
#	@get "title".
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one containing the current state.
# } 
# 
# \examples{
#	# create site object
#	s<-Site(alphabet=NucleotideAlphabet(),state="A")
#	# get character represenation
#	x<-as.character(s)
#	x
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
	"as.character", 
	class="Site", 
	function(
		x,
		...
	){

		x$.state;		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Site
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
#       o<-Site()
#       # get a summary
#       summary(o)
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
	class="Site", 
	function(
		object,
		...
	){
			this<-object;
			this$.summary$"State"=this$state;
			if(!is.na(this$alphabet)) {
			alphabet_symbols = paste(this$alphabet$symbols,collapse=" ");
			this$.summary$"Alphabet"=paste("\n","  Type: ",this$alphabet$type,"\n","  Symbols: ", alphabet_symbols,sep="");
			} else {
				this$.summary$"Alphabet"=NA
			}
			
			attached_processes<-this$processes;
			header<-paste("Attached processes (",length(attached_processes),")",sep="");
			tmp<-character(0);

			for (p in attached_processes) {
				tmp<-paste(tmp,"\n ",p$id)
			}		

			this$.total.rate<-NA;
			if(!is.na(this$.sequence)){
		 	this$.sequence$.cumulative.rate.flag<-TRUE;
			}

			this$.summary[[header]]<-tmp;
			
			tmp<-character(0);
			for (e in this$events) {
				tmp<-paste(tmp,"\n ");
				tmp<-paste(tmp,"Name:",e$name);
				tmp<-paste(tmp," Rate:",e$rate);
				tmp<-paste(tmp," Process:",e$process$id);
			}
			this$.summary$"Active events"<-tmp;

			this$.summary$"Total rate"<-getTotalRate(this);
	
			if(!is.na(this$sequence)){
				this$.summary$"Part of sequence"<-this$sequence$id;
			}

			if(is.Process(this$ancestral)) {
					this$.summary$"Directly inserted by"<-this$ancestral$id;
			} else if (is.Site(this$ancestral)) {
					this$.summary$"Ancestral state"<-this$ancestral$state;
			} else if (!is.na(this$ancestral)){
					throw("summary.Site detected inconsistent state!\n");

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
## Method: isAttached
##
###########################################################################/**
#
# @RdocMethod isAttached
# 
# @title "Check whether a Process object is attached to a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{process}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	# create a Site object
#	s<-Site(alphabet=NucleotideAlphabet())
#	# create a Process object
#	p<-JC69()
#	# check if p is attached to s
#	isAttached(s,p)
#	# attach p to s
#	s$processes<-list(p)
#	isAttached(s,p)
# } 
# 
# @author 
# 
# \seealso{ 
# 	Site Process attachProcess detachProcess getProcesses setProcesses
# } 
# 
#*/###########################################################################
setMethodS3(
  "isAttached",
  class="Site",
  function(
    this,
    process,
    ...
  ){
			

	 if(!exists(x="PSIM_FAST")){
		if (!is.Process(process)) {throw("Process object invalid!\n")}	
	 }
		attached_processes<-getProcesses(this);
		if (length(attached_processes) == 0 ){ return(FALSE)}
			
		tmp<-lapply(
			attached_processes,
			function(proc) { equals(proc, process)}
		);	
			tmp<-unique(tmp);
			
			if(length(tmp) == 1 ) {
					# If we have only one process attached,
					# than simply return the result of the equals() function.
					return(tmp[[1]]);
			} else {
					
					# Additional check to make sure that the .process entry is here.	
					#	if (length (intersect(class(this$.processes[[getId(process)]]),"list")) == 0) {
					#		throw("Something evil is happening! The process is attached, but the .process entry is invalid!\n");
					#	}

					# If length(tmp) > 1, than one of its elements must be TRUE,
					# so returning TRUE.
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
## Method: attachProcess
##
###########################################################################/**
#
# @RdocMethod attachProcess
# 
# @title "Attach a Process object to a Site object" 
# 
# \description{ 
#	@get "title".
#
#	The Alphabet objects associated with the Site and Process objects must have the same symbol set, or at least one
#	of them should inherit from the class AnyAlphabet. 
#
#	During the attachment, the site-process specific parameter templates are copied from the Process object and 
#	stored in the Site object.
#	The Process objects are marked as write protected if the attachment was succesful. 
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{process}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	The Site object (invisible).
# } 
# 
# \examples{
#	# create a Site object and the associated NucleotideAlphabet object
#	s<-Site(alphabet=NucleotideAlphabet())
#	# create a K80 substitution process
#	p<-K80()
#	# attach p to s
#	attachProcess(s,p)
#	# get the list of attached processes
#	s$processes
#	# check write protection for p
#	p$writeProtected
# } 
# 
# @author 
# 
# \seealso{ 
# 	Site Process detachProcess setProcesses getProcesses isAttached
# } 
# 
#*/###########################################################################
setMethodS3(
  "attachProcess",
  class="Site",
  function(
    this,
    process,
    ...
  ){

		if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); 
		}
	 if(!exists(x="PSIM_FAST")){
	
		if(!is.Process(process)) {
			throw("Process object is not valid!\n"); }
		else if( is.na(process$alphabet) ){
				throw("The process has no alphabet attached!\n"); }
		else if( is.na(this$alphabet) ){
				throw("The site has no alphabet attached!\n"); }
		else if (this$alphabet != process$alphabet) {
				throw("The site and process alphabets are incompatible!\n"); 
		}
		else if( hasUndefinedRate(process) ){
				warning("The process",process$id," has undefined rates!\n");
		}

		}
		
		this$.processes[[process$id]]<-list (
				object	= process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params = process$siteSpecificParamList	
			);
		this$.total.rate<-NA;
		
		if(!is.na(this$.sequence)){
		this$.sequence$.cumulative.rate.flag<-TRUE;
		}
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .attachProcessSloppy
##
setMethodS3(
  ".attachProcessSloppy",
  class="Site",
  function(
    this,
    process,
    ...
  ){
	
		if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); }
		else {
			this$.processes[[process$id]]<-list (
				object 				= 	process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params		=		process$siteSpecificParamList	
			);
		}
		this$.total.rate<-NA;
		if(!is.na(this$.sequence)){
		this$.sequence$.cumulative.rate.flag<-TRUE;
		}
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);

  },
  private=TRUE,
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
# @title "Site" 
# 
# \description{ 
#	@get "title".
#	The site-process specific parameters stored in the Site object and belonging to the detached Process objects will be destroyed.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object} 
# 	\item{process}{A Process object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Site object (invisible).
# } 
# 
# \examples{
#	# create a Site objects and attach some processes
#	s<-Site()
#	s$alphabet<-NucleotideAlphabet()
#	p1<-JC69(); p2<-K80()
#	attachProcess(s,p1)
#	attachProcess(s,p2)
#	# get the list of attached processes
#	s$processes
#	# detach p1
#	detachProcess(s,p1)
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
  class="Site",
  function(
    this,
    process,
    ...
  ){
			
	 if(!exists(x="PSIM_FAST")){
		if(!is.Process(process)) {
			throw("Process object is not valid!\n");
		}
	}
		else if (!isAttached(this,process)) {
				warning("Process is not attached, doing nothing!\n");
		}
		
		# Setting the list entry to NULL,
		# so it will wanish from the list.
		this$.processes[[process$id]]<-NULL;
	
		this$.total.rate<-NA;
		if(!is.na(this$.sequence)){
		this$.sequence$.cumulative.rate.flag<-TRUE;
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
## Method: setProcesses
##
###########################################################################/**
#
# @RdocMethod getProcesses
# 
# @title "Get the list of Process objects attached to a Site object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Process objects.
# } 
# 
# \examples{
#	# create a Site object with some processes attached
#	s<-Site(alphabet=NucleotideAlphabet(),processes=list(K80(),JC69()))
#	# get list of attached Process objects
#	getProcesses(s)
#	# get list of attached Process objects via virtual field
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
  class="Site",
  function(
    this,
    ...
  ){
  
		lapply(names(this$.processes),function(id){this$.processes[[id]][["object"]]});
		
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
# @title "Specify the list of Process objects attached to a Site object" 
# 
# \description{ 
#	@get "title".
#	The Process objects in the "value" list correspond to the set of processes to be attached to the Site object.
#       Process objects already attached to a given Site are skipped. Attached processes which are not memebers of the list
#       are detached, so specifying an empty list will detach all processes.
#
#	This method is an alternative to \code{attachProcess.Site} and \code{detachProcess.Site}, working with 
#	more than one process object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Site object.} 
#	\item{value}{A list of valid Process objects.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Site object (invisible).
# } 
# 
# \examples{
#	# create a Site object
#	s<-Site(alphabet=NucleotideAlphabet())
#	# create some processes
#	p1<-JC69(); p2<-K80(); p3<-DiscreteInsertor(rate=1);	
#	# attach the processes
#	setProcesses(s,list(p1,p2,p3))
#	# attach one more process via virtual field
#	s$processes<-c(s$processes,list(GTR()))
#	# get the list of attached processes
#	s$processes
#	# detach all processes via virtual field
#	s$processes<-list()
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
  class="Site",
  function(
    this,
    value,
    ...
  ){

		if(missing(value)) {throw("No new value given!\n")}
		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		for(i in value) {
			if(!is.Process(i)){
					throw("The accepted argument is a list of processes!\nVectors and lists are not euivalent, take care!\n");
			}
		}
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcess(this,process)});
		lapply(to.attach, function(process) {attachProcess(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .setProcessesSloppy
##
setMethodS3(
  ".setProcessesSloppy",
  class="Site",
  function(
    this,
		value,
    ...
  ){

		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcess(this,process)});
		lapply(to.attach, function(process) {.attachProcessSloppy(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );
