##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass GeneralInDel
# 
# @title "The GeneralInDel class"
# 
# \description{ 
#
#	This is a class implementing the methods which are needed by both the 
#	GeneralInsertor and GeneralDeletor process.
#	
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{rate}{The general rate of the object.}
#	\item{propose.by}{A function used to propose events.}
#	\item{accept.by}{A function used to accept/reject events.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a GeneralInDel object
#	# rejecting half of the events
#	# and proposing sizes in the range 1:10
#	o<-GeneralInDel(
#                   rate=1,
#                   propose.by=function(process){sample(1:10,1)},
#                   accept.by=function(){sample(c(TRUE,FALSE),1)}
#                   );
#	# check if inherits from GeneralInDel
#	is.GeneralInDel(o)
#	# check if it has undefined rates
#	hasUndefinedRate(o)
#	# get object summary
#	summary(o)
#	# set/get proposeBy function via virtual field
#	o$proposeBy<-function(process){return(3)}	# fixed event length
#	o$proposeBy
#	# set/get acceptBy function via virtual field
#	o$acceptBy<-function(){return(TRUE)}		# accept all events
#	o$acceptBy
#	# get/set general rate
#	o$rate
#	o$rate<-2	# double the rate
#	# propose event length
#	proposeLength(o)
# }
# 
# @author
#
# \seealso{ 
# 	Process GeneralInsertor GeneralDeletor GeneralSubstitution 
# }
# 
#*/###########################################################################
setConstructorS3(
  "GeneralInDel",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		any.alphabet<-AnyAlphabet();
		this<-Process(
			alphabet=any.alphabet
		);
    this<-extend(
      			this,
      			"GeneralInDel",
			.rate=rate,
			.propose.by=NA,
			.accept.by=NA,
			.is.general.indel=TRUE
    );

		# Using virtual field to clear Id cache:
		this$name<-name;
		# setting propose.by
		if(!missing(propose.by) && is.function(propose.by)){
			this$proposeBy<-propose.by;
		}
		# setting accept.by
		if(!missing(accept.by) && is.function(accept.by)){
			this$acceptBy<-accept.by;
		}

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
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"checkConsistency", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$rate)) {
          this$rate<-this$rate;
        }

				if(!is.function(this$proposeBy)){
					if(!is.na(this$proposeBy)){
						throw("proposeBy is invalid!\n");
					}
				}
				
				if(!is.function(this$acceptBy)){
					if(!is.na(this$acceptBy)){
						throw("acceptBy is invalid!\n");
					}
				}

      }
      tryCatch(may.fail(this),finally=this$writeProtected<-wp);
			NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRate
##	
###########################################################################/**
#
# @RdocMethod getRate
# 
# @title "Get the general rate" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	o<-GeneralInDel(rate=0.5)
#	# get/set general rate
#	getRate(o)
#	setRate(o, 1.5)
#	# get/set rate via virtual field
#	o$rate
#	o$rate<-0.3
#	o$rate
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
	"getRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.rate;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasUndefinedRate
##	
###########################################################################/**
#
# @RdocMethod hasUndefinedRate
# 
# @title "Check whether the general rate of a GeneralInDel object is undefined" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	o<-GeneralInDel()
#	# check if the general rate is undefined
#	hasUndefinedRate(o)
#	# set general rate
#	o$rate<-1
#	# check rate again
#	hasUndefinedRate(o)
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
	"hasUndefinedRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		return(is.na(this$.rate));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
###########################################################################/**
#
# @RdocMethod setRate
# 
# @title "Set the general rate" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
#	\item{value}{The new general rate (a numeric vector of length one).}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new general rate.
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	o<-GeneralInDel(rate=0.5)
#	# get/set general rate
#	getRate(o)
#	setRate(o, 1.5)
#	# get/set rate via virtual field
#	o$rate
#	o$rate<-0.3
#	o$rate
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
	"setRate", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)) {
			throw("No new value provided!\n");}
		else if(!is.numeric(value)) {
			throw("Rate must be numeric!\n");
		} 
	}	
		this$.rate<-value;
		return(this$.rate);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProposeBy
##	
###########################################################################/**
#
# @RdocMethod getProposeBy
# \alias{getProposeBy.GeneralInsertor}
# 
# @title "Get the function used for proposing indel lengths" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A function object.
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	# proposing events with a constant length of 5
#	o<-GeneralInDel(rate=1, propose.by=function(process){return(5)});
#	# set/get the proposeBy function
#	setProposeBy(o,value=function(process){return(6)})
#	getProposeBy(o)
#	# set/get proposeBy function via virtual field
#	o$proposeBy<-function(process){return(3)}
#	o$proposeBy
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
	"getProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.propose.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProposeBy
##	
###########################################################################/**
#
# @RdocMethod setProposeBy
# \alias{setProposeBy.GeneralInsertor}
# 
# @title "Set the function used for proposing indel lengths" 
# 
# \description{ 
#	@get "title".
#
#	The function must return a numeric vector of length one. The function must have an
#	argument named "process" which will hold the calling process object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
#	\item{value}{A function object returning a numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The function object (invisible).
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	# proposing events with a constant length of 5
#	o<-GeneralInDel(rate=1, propose.by=function(process){return(5)});
#	# set/get the proposeBy function
#	setProposeBy(o,value=function(process){return(6)})
#	getProposeBy(o)
#	# set/get proposeBy function via virtual field
#	o$proposeBy<-function(process){return(3)}
#	o$proposeBy
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
	"setProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of proposeBy must be a function.!\n");	
		}
	}
		this$.propose.by<-value;
		return(invisible(this$.propose.by));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptBy
##	
###########################################################################/**
#
# @RdocMethod getAcceptBy
# \alias{getAcceptBy.GeneralInsertor}
# 
# @title "Get the function used for accepting/rejecting indel events" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A function object.
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	# rejecting half of the events
#	o<-GeneralInDel(
#                   rate=1,
#                   propose.by=function(process){return(5)},
#                   accept.by=function( ){sample(c(TRUE,FALSE),1)}
#                   );
#	# set/get the acceptBy function
#	setAcceptBy(o,value=function(){return(FALSE)})	# reject all events
#	getAcceptBy(o)
#	# set/get acceptBy function via virtual field
#	o$acceptBy<-function(){return(TRUE)}		# accept all events
#	o$acceptBy
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
	"getAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.accept.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptBy
##	
###########################################################################/**
#
# @RdocMethod setAcceptBy
# \alias{setAcceptBy.GeneralInsertor}
# 
# @title "Set the function used for accepting/rejecting indel events" 
# 
# \description{ 
#	@get "title".
#
#	The function object must have the following arguments: process (the caller process), sequence (the target sequence), 
#	window (a vector containing the positions affecting acceptance).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
#	\item{value}{A function object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The function object (invisible).
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	# rejecting half of the events
#	o<-GeneralInDel(
#                   rate=1, 
#                   propose.by=function(process){return(5)},
#                   accept.by=function( ){sample(c(TRUE,FALSE),1)}
#                   );
#	# set/get the acceptBy function
#	setAcceptBy(o,value=function( ){return(FALSE)})	# reject all events
#	getAcceptBy(o)
#	# set/get acceptBy function via virtual field
#	o$acceptBy<-function(){return(TRUE)}		# accept all events
#	o$acceptBy
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
	"setAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of acceptBy must be a function.!\n");	
		}
	}
		this$.accept.by<-value;
		return(invisible(this$.accept.by));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: proposeLength
##	
###########################################################################/**
#
# @RdocMethod proposeLength
# 
# @title "Propose indel length" 
# 
# \description{ 
#	@get "title".
#
#	This method simply calls the function returned by the \code{getProposeBy} method.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInDel object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one (the indel length).
# } 
# 
# \examples{
#	# create a GeneralInDel object
#	# proposing event lengths in the range 1:10
#	o<-GeneralInDel(rate=1, propose.by=function(process){sample(c(1:10),1)});
#	# propose indel length
#	proposeLength(o)
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
	"proposeLength", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		return( this$.propose.by(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: is.GeneralIndel
##	
###########################################################################/**
# @RdocDefault is.GeneralInDel
# 
# @title "Check if an object inherits from the GeneralInDel class" 
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#		\item{this}{An object.}
#		\item{...}{Not used.}
#
# } 
# 
# \value{ 
#	TRUE or FALSE.
# } 
#
# \examples{
#	# create some objects
#	o<-GeneralInDel(rate=1, propose.by=function(process){sample(c(1:10),1)});
#	x<-GTR()
#	# check if they inherit from GeneralInDel
#	is.GeneralInDel(o)
#	is.GeneralInDel(x)
# } 
# 
# 
# @author 
# 
#*/###########################################################################
setMethodS3(
	"is.GeneralInDel", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.indel)){return(TRUE)}
    if ( inherits(this, "GeneralInDel")) {
      this$.is.general.indel<-TRUE;
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
#       a<-GeneralInDel(rate=1,propose.by=function(process){sample(c(1,2,3),1)})
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
	class="GeneralInDel", 
	function(
		object,
		...
	){
	
		.addSummaryNameId(object);
		object$.summary$"General rate"<-object$rate;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################
# Class:GeneralInsertor
##########################################################################/** 
#
# @RdocClass GeneralInsertor
# 
# @title "The GeneralInsertor class"
# 
# \description{ 
#
#	This is a class implementing a process generating insertion events.
#	The rate of each event is calculated as the product of the general rate of the process 
#       and the "rate.multiplier" site-process specific parameter.
#	The simulation code calls the \code{Perform} method on the selected insertion event objects,
#	which call their insertion event handler to perform the insertion.
#
#	The insert lengths are proposed by the function stored in the \code{proposeBy} 
#	virtual field. The function must have the following arguments:
#	process (the insertion process object).
#
#	The insertion events are accepted or rejected by the function stored in the \code{acceptBy} virtual field.
#	The function must have the following arguments: process (the insertion process object), sequence (the target sequence object), 
#	window (a vector of positions affecting acceptance).
#	The probability of accepting an insertion is calculated as the product of the site-process-specific 
#	"insertion.tolerance" parameters of the sites neighboring the insertion. 
#	The number of sites considered is determined by the \code{acceptWin} virtual field.
#
#	The insert is generated by the \code{generateInsert} method by calling the function stored in the \code{generateBy} virtual field.
#	The default generator function truncates/duplicates the sequence object stored in the \code{templateSeq} virtual field to get a sequence
#	having the sampled length. After constructing the Sequence object, it  calls the \code{sampleStates.Sequence} method on the resulting object. 
#	That means that if we start with a template sequence which has NA states, but has a substitution process attached, then the resulting sequence
#       will be different every time.
#
#	Before inserting the sequence returned by \code{generateInsert}, the handler function will pass the object through the function stored in the
#	\code{insertHook} virtual field. This allows to perform arbitrary modifications on the inserted Sequence object.
#
#	The sequence is inserted randomly on the left or the right of the target position.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{rate}{The general rate of the object (no default).}
#       \item{propose.by}{A function used to propose events (no default).}
#       \item{accept.by}{A function used to accept/reject events (no default).}
#	\item{template.seq}{A Sequence object used as a template for generating insertions (no default).}
#	\item{insert.hook}{A function object, see \code{setInsertHook} (no default).}
#	\item{accept.win}{A window of sites affecting the acceptance of insert events.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		name="GIN",
#		rate=1,
#		propose.by=function(process){4}, # fixed insert length
#		acceptBy=function(process,sequence,window){TRUE},# always accept insertions
#		template.seq=NucleotideSequence(string="A"),# a boring template sequence
#		insert.hook=function(seq){ return(seq)},# a boring insert hook
#		accept.win=2 #4 sites affecting acceptance
#	)
#	i
#	# check if object inherits from GeneralInsertor
#	is.GeneralInsertor(i)
#	# get object summary
#	summary(i)
#	# set/get general rate
#	i$rate<-0.5
#	i$rate
#	# set/get name
#	i$name<-"Ins"
#	i$name
#	# set/get proposeBy
#	# sample insertion length between 1 and 10
#	i$proposeBy<-function(process){sample(1:10,1)}
#	i$proposeBy
#	# set/get acceptBy
#	# reject half of the insertions
#	i$acceptBy<-function(process, sequence, window){ sample(c(TRUE,FALSE), 1) }
#	i$acceptBy
#	# get generateBy
#	i$generateBy
#	# set/get acceptWin
#	i$acceptWin<-1;
#	# set/get insert hook
#	i$insertHook<-function(
#                           seq, 
#                           target.seq,
#                           event.pos,
#                           insert.pos
#                           ){ attachProcess(seq, GTR() );seq} 
#	i$insertHook
#	# set/get template sequence
#	i$templateSeq<-NucleotideSequence(
#                                       length=5,
#                                       processes=list(list(JC69()))
#                                   ) # length: 5, states: NA
#	i$templateSeq
#	# generate an insert sequence
#	generateInsert(i)
#	# create a sequence object and attach the process i
#	s<-NucleotideSequence(string="AAAAA",processes=list(list(i)))
#       # set rate multiplier
#       setRateMultipliers(s,i,2)
#       # get the list of active events from site 2
#       events<-getEventsAtSite(i,s$sites[[2]])
#       events
#       # set postition for event
#       e<-events[[1]]
#       e$.position<-2  
#       # print sequence
#       s
#       # perform event
#       Perform(e)
#       # check sequence again
#       s
# }
# 
# @author
#
# \seealso{ 
# 	GeneralInDel DiscreteInsertor ContinuousInsertor BrownianInsertor
# }
# 
#*/###########################################################################
setConstructorS3(
  "GeneralInsertor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		template.seq=NA,
		insert.hook=NA,
		accept.win=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralInsertor",
			.generate.by=NA,
			.handler.template=NA,
			.template.seq=NA,
			.insert.hook=NA,
			.accept.win=1,
			.is.general.insertor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="insertion.tolerance",
      name="Insertion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );
	
		if(!missing(template.seq)){
			this$templateSeq<-template.seq;
		}

		this$acceptBy<-function(process=NA,sequence=NA,range=NA){
			
				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Discard the site if the process is not attached to it:
						if(!isAttached(site, process)){
							next();
						}
						else {
							accept.prob<-c(accept.prob, getParameterAtSite(process, site, "insertion.tolerance")$value);
						}
				}
				accept.prob<-prod(as.numeric(accept.prob));


			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}

	###	

	 this$generateBy<-function(process=NA,length=NA,target.seq=NA,event.pos=NA,insert.pos=NA){

		if(!exists(x="PSIM_FAST")){	
			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	
			else if(is.na(process$.template.seq)){
				throw("Cannot generate insert without template sequence!\n");
			}
		}

			times<-( ceiling( length/this$.template.seq$.length) );
			to.delete<-( ( (this$.template.seq$.length) * times) - length);

			tmp<-clone(this$.template.seq);
		
			if( (times-1) > 0){
				for(i in 1:(times-1)){
					insertSequence(tmp,process$.template.seq,tmp$length);
				}
			}

			if(to.delete > 0){
				deleteSubSequence(tmp,(tmp$length - to.delete + 1):tmp$length);
			}
			return(tmp);
				
	 }

	if(!missing(insert.hook)){
		this$insertHook<-insert.hook;
	}

	###	
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 WINDOW.SIZE<-this$.accept.win;
					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 process<-event$.process;
					 sequence<-event$.site$.sequence;
					 details<-list();
					 details$type<-"insertion";

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Set insertion tolerance window:
					 window<-integer();
					 insert.pos<-position;
					 if(direction == "LEFT") {
							window<-(position-WINDOW.SIZE):position;
					 		insert.pos<-(position-1);
					 }
					 else if (direction == "RIGHT"){
							window<-position:(position+WINDOW.SIZE);
					 }
					 else {
						throw("You should never see this message!\n");
					}

					details$position<-insert.pos;
					details$accepted<-FALSE;

					# Discard illegal positions:
					window<-window[ window > 0 & window <= sequence$.length];
				  if(process$.accept.by(process=process,sequence,window)){
							details$accepted<-TRUE;
							insert<-generateInsert(process,target.seq=sequence,event.pos=position,insert.pos=insert.pos);
							details$length<-insert$length;
							# Call the insert hook:
							if(is.function(this$.insert.hook)){
								insert<-this$.insert.hook(seq=insert,target.seq=sequence,event.pos=position,insert.pos=insert.pos);
							}
							insertSequence(sequence,insert, insert.pos,process=process);
					}
					return(details);
					
				}
		 }
		###

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralInsertor
##	
###########################################################################/**
#
# @RdocDefault is.GeneralInsertor
# 
# @title "Check whether an object inherits from GeneralInsertor" 
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#		\item{this}{An object.}
#		\item{...}{Not used.}
#
# } 
# 
# \value{ 
#	TRUE or FALSE.
# } 
#
# \examples{
#	# create some objects
#	d<-GeneralDeletor()
#	i<-GeneralInsertor()
#	# check if they inherit from GeneralInsertor
#	is.GeneralInsertor(i)
#	is.GeneralInsertor(d)
# }
# 
# @author 
# 
#*/###########################################################################
setMethodS3(
	"is.GeneralInsertor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.insertor)){return(TRUE)}
    if ( inherits(this, "GeneralInsertor")) {
      this$.is.general.insertor<-TRUE;
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
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$templateSeq)) {
          this$templateSeq<-this$templateSeq;
        }

        if(!is.function(this$generateBy)){
          if(!is.na(this$generateBy)){
            throw("generateBy is invalid!\n");
          }
        }

      }
      tryCatch(may.fail(this),finally=this$writeProtected<-wp);
      NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventsAtSite
##	
###########################################################################/**
#
# @RdocMethod	getEventsAtSite
# 
# @title "Generate insertion event object given the state of the target site" 
# 
# \description{ 
#	@get "title".
#	
#	This method generates a list with one insertion event. The rate of the 
#	event is calculated as the product of the general rate of the process 
#	and the "rate.multiplier" site-process specific parameter. An empty list is
#	returned if the rate is zero or NA.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{target.site}{A Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Event objects.
# } 
# 
# \examples{
#	# create a sequence object
#	s<-NucleotideSequence(string="AAAA")
#	# create a GeneralInsertor process, provide template sequence.
#	# propsed insert lengths:3, always accept.
#	i<-GeneralInsertor(
#		rate=0.5,
#		template.seq=NucleotideSequence(string="GGG"),
#		propose.by=function(process){3},
#		accept.by=function(process,sequence,window){TRUE}
#	)
#	# attach process to site
#	s$processes<-list(list(i));
#	# set rate multiplier
#	setRateMultipliers(s,i,2)
#	# get the list of active events from site 2
#	events<-getEventsAtSite(i,s$sites[[2]])
#	events
#	# set postition for event
#	e<-events[[1]]
#	e$.position<-2	
#	# print sequence
#	s
#	# perform event
#	Perform(e)
#	# check sequence again
#	s
# } 
# 
# @author 
# 
# \seealso{ 
# 	GeneralInsertor GeneralInDel Process Event
# } 
# 
#*/###########################################################################
setMethodS3(
	"getEventsAtSite", 
	class="GeneralInsertor", 
	function(
		this,
		target.site,
		...
	){

	if(!exists(x="PSIM_FAST")){
		if(missing(target.site)) {
			throw("No target site provided!\n");
		}
		if(!is.Site(target.site)) {
			throw("Target site invalid!\n");
		}
		else if(!is.function(this$.propose.by)) {
			throw("proposeBy is not set, cannot propose insertion!\n");
		} 
		else if (!is.function(this$.accept.by)){
			throw("acceptBy is not set, cannot generate insertion event!\n");
		}
	}

		# Just return an empty list if the rate is undefined or zero:
		if( is.na(this$.rate) | this$.rate == 0) {
			return(list());
		}

		 # Clone the event template object:
		 insertion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 insertion.event$.position<-target.site$.position;
		 # Set the target site:
		 insertion.event$.site<-target.site;
		 # Set event name:
		 insertion.event$.name<-"Insertion";
		 # Set the generator process:
		 insertion.event$.process<-this;
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
		 rate.multiplier<-target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value;
		 if(rate.multiplier == 0 ) {
			return(list());
		 }
		 insertion.event$.rate<-(this$.rate * rate.multiplier );

		 # Set the handler for the insertion event:
		 insertion.event$.handler<-this$.handler.template;

		# Return the event object in a list:
		list(insertion.event);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: generateInsert
##	
###########################################################################/**
#
# @RdocMethod generateInsert
# 
# @title "Generate an insert" 
# 
# \description{ 
#	@get "title".
#
#	This method uses the function stgored in the \code{proposeBy} virtual field to
#	sample the insert length and then calls the function stored in the \code{generateBy}
#	field to generate the insert. 	
#
#	The default \code{generateBy} function set by the GeneralInsertor constructor truncates/repeats
#	the template sequence stored in the \code{templateSeq} field to have the sequence with the right size
#	and then calls the \code{sampleStates} method on the resulting object. That means that if we start with a
#	template sequence which has NA states, but has a substitution process attached, then the resulting sequence
#	will be different every time the \code{generateInsert} method is called.
#
#	The \code{generateBy}, \code{proposeBy} and \code{templateSeq} fields must be set in order to use this method.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
#	\item{length}{Generate an insert with the specified length if this argument is present.}
#	\item{target.seq}{The Sequence object targeted by the insertion (optional). This argument is passed to the \code{generateBy} method.}
#	\item{event.pos}{The position of the site proposing the insertion (optional). This argument is passed to the \code{generateBy} method.}
#	\item{insert.pos}{The position of the insertion in the target sequence (optional). This argument is passed to the \code{generateBy} method.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object.
# } 
# 
# \examples{
#	# build the template sequence
#	ts<-NucleotideSequence(length = 10,processes=list(list(JC69())));
#	# fix some site states
#	setStates(ts,"A",c(1,2));
#	setStates(ts,"T",c(5,6));
#	setStates(ts,"C",c(9,10));
#	# print template sequence
#	ts
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		template.seq=ts,
#		propose.by=function(process){sample(c(5:50),1)}, # inserts between 5 and 50
#	)
#	# generate some inserts
#	generateInsert(i)
#	generateInsert(i)
#	generateInsert(i)
#	generateInsert(i)
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
	"generateInsert", 
	class="GeneralInsertor", 
	function(
		this,
		length=NA,
		target.seq=NA,
		event.pos=NA,
		insert.pos=NA,
		...
	){

		if(missing(length)){
			length<-this$.propose.by(process=this);
		}
		insert<-this$.generate.by(process=this,length=length,target.seq=target.seq,event.pos=event.pos,insert.pos=insert.pos);
		sampleStates(insert);	
		return(insert);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getGenerateBy
##	
###########################################################################/**
#
# @RdocMethod getGenerateBy
# 
# @title "Get the function object used for generating inserts" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A function object.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(string="AAAAAAA")
#	)
#
#	# save insert generator
#	old.gen<-getGenerateBy(i)
#	# set a new insert generator
#	i$generateBy<-function(
#                           process,
#                           length, 
#                           target.seq,
#                           event.pos,
#                           insert.pos
#                        ){ 
#                           return(NucleotideSequence(string="AATTGGCC"))
#                           }
#	# get the generator function
#	i$generateBy
#	# generate insert
#	generateInsert(i)
#	# restore old generator
#	i$generateBy<-old.gen
#	# generate insert
#	generateInsert(i)
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
	"getGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.generate.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setGenerateBy
##
###########################################################################/**
#
# @RdocMethod setGenerateBy
# 
# @title "Set the function object used for generating inserts" 
# 
# \description{ 
#	@get "title".
#	The provided function must return a Sequence object whne called and must have the
#	following arguments: process, length, target.seq, event.pos, insert.pos (see \code{generateInsert.GeneralInsertor}).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
#	\item{value}{A function object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The function object.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(string="AAAAAAA")
#	)
#
#	# save insert generator
#	old.gen<-getGenerateBy(i)
#	# set a new insert generator
#	i$generateBy<-function(
#                           process,
#                           length,
#                           target.seq,
#                           event.pos,
#                           insert.pos){ 
#                   return(NucleotideSequence(string="AATTGGCC"))
#                   }
#	# get the generator function
#	i$generateBy
#	# generate insert
#	generateInsert(i)
#	# restore old generator
#	i$generateBy<-old.gen
#	# generate insert
#	generateInsert(i)
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
	"setGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of generateBy must be a function.!\n");	
		}
	}
		this$.generate.by<-value;
		return(this$.generate.by);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTemplateSeq
##	
###########################################################################/**
#
# @RdocMethod getTemplateSeq
# 
# @title "Get the template sequence object" 
# 
# \description{ 
#	@get "title".
#	The template sequence object is used by the default \code{generateBy} function
#	to generate insert sequences.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Sequence object or NA.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(string="AAAAAAA")
#	)
#	# get template sequence
#	getTemplateSeq(i)
#	# get template sequence via virtual field
#	i$templateSeq
#	# set template sequence
#	setTemplateSeq(i, NucleotideSequence(string="C"));
#	# generate insert
#	generateInsert(i)
#	# set template sequence via virtual field
#	i$templateSeq<-NucleotideSequence(string="G")
#	# generate insert
#	generateInsert(i)
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
	"getTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.template.seq;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTemplateSeq
##	
###########################################################################/**
#
# @RdocMethod setTemplateSeq
# 
# @title "Set the template sequence object" 
# 
# \description{ 
#	@get "title".
#	The template sequence object is used by the default \code{generateBy} function
#	to generate insert sequences.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
#	\item{value}{A Sequence object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Sequence object.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(string="AAAAAAA")
#	)
#	# get template sequence
#	getTemplateSeq(i)
#	# get template sequence via virtual field
#	i$templateSeq
#	# set template sequence
#	setTemplateSeq(i, NucleotideSequence(string="C"));
#	# generate insert
#	generateInsert(i)
#	# set template sequence via virtual field
#	i$templateSeq<-NucleotideSequence(string="G")
#	# generate insert
#	generateInsert(i)
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
	"setTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)) {
				throw("No new template sequence provided!\n");	
		}
		else if(!is.Sequence(value)){
			 throw("Sequence object is invalid!\n");	
		}
		else if(value$length == 0) {
			throw("Cannot set template sequence of length zero!\n");
		}
	}
		this$.template.seq<-clone(value);
		for (site in this$.template.seq$.sites){
			site$.ancestral<-this;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptWin
##	
###########################################################################/**
#
# @RdocMethod getAcceptWin
# 
# @title "Get the size of the acceptance window" 
# 
# \description{ 
#	@get "title"
#
#	This parameter determines the number of sites neighbouring the position (from left and right) of the insertion considered when accepting/rejecting 
#	a proposed insertion. The "insertion.tolerance" parameter is retrived from sites falling in the window specified by this parameter.
#	The default value is 1, so the two neighbouring sites are considered by default.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(rate=0.5);
#	# get acceptance window size
#	getAcceptWin(i)
#	# get acceptance window size via virtual field
#	i$acceptWin
#	# set acceptance window size
#	setAcceptWin(i,2)
#	# set acceptance window size via virtual field
#	i$acceptWin<-3
#	i$acceptWin
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
	"getAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.accept.win;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptWin
##	
###########################################################################/**
#
# @RdocMethod setAcceptWin
# 
# @title "Set the size of the acceptance window" 
# 
# \description{ 
#	@get "title"
#
#	This parameter determines the number of sites neighbouring the position (from left and right) of the insertion considered when accepting/rejecting 
#	a proposed insertion. The "insertion.tolerance" parameter is retrived from sites falling in the window specified by this parameter.
#	The default value is 1, so the two neighbouring sites are considered by default.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
#	\item{value}{An integer vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(rate=0.5);
#	# get acceptance window size
#	getAcceptWin(i)
#	# get acceptance window size via virtual field
#	i$acceptWin
#	# set acceptance window size
#	setAcceptWin(i,2)
#	# set acceptance window size via virtual field
#	i$acceptWin<-3
#	i$acceptWin
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
	"setAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)){
			throw("No new value provided");
		}
		else if(!all(is.numeric(value)) | (length(value) != 1)){
			throw("The new value must be a numeric vector of length one.");
		}
	}
		this$.accept.win<-floor(value);
		return(this$.accept.win);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertHook
##	
###########################################################################/**
#
# @RdocMethod getInsertHook
# 
# @title "Get the insert hook function" 
# 
# \description{ 
#	@get "title".
#
#	The insert hook allows to make various modifications on the insert before performing the insertion.
#
#	The insert hook function is called by the insertion event handler function. The insert hook takes the 
#	sequence generated by the \code{generateInsert} method throught the "seq" argument. The function
#	must return a Sequnece object, which will be inserted in the target sequence.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A function object.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(length=5,processes=list(list(JC69())))
#	)
#	# set a dummy insert hook
#	setInsertHook(i,function(seq){return(seq)})
#	# set a new insert hook via virtual field
#	i$insertHook<-function(seq){
#		seq$processes<-list(list(GTR())) # replace the subsitution process
#		return(seq)
#	}
#	# get the insert hook via virtual field
#	i$insertHook
#	# get the insert hook
#	getInsertHook(i)
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
	"getInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.insert.hook;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertHook
##	
###########################################################################/**
#
# @RdocMethod setInsertHook
# 
# @title "Set the insert hook function" 
# 
# \description{ 
#	@get "title".
#
#	The insert hook allows to make various modifications on the insert before performing the insertion.
#	The function must have the following arguments: seq (the sequence object to insert), target.seq (the target Sequence object), 
#	event.pos (the position of the site which generated the insertion event), insert.pos (the position of the insertion).
#
#	The insert hook function is called by the insertion event handler function. The insert hook takes the 
#	sequence generated by the \code{generateInsert} method throught the "seq" argument. The function
#	must return a Sequnece object, which will be inserted in the target sequence.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralInsertor object.} 
# 	\item{value}{A function object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The function object.
# } 
# 
# \examples{
#	# create a GeneralInsertor object
#	i<-GeneralInsertor(
#		rate=0.5,
#		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
#		template.seq=NucleotideSequence(length=5,processes=list(list(JC69())))
#	)
#	# set a dummy insert hook
#	setInsertHook(i,function(seq){return(seq)})
#	# set a new insert hook via virtual field
#	i$insertHook<-function(seq){
#		seq$processes<-list(list(GTR())) # replace the subsitution process
#		return(seq)
#	}
#	# get the insert hook via virtual field
#	i$insertHook
#	# get the insert hook
#	getInsertHook(i)
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
	"setInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.Sequence(this$.template.seq)){
			throw("Cannot set insert hook because the template sequence is not defined!\n");
		}
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The insert hook must be a function.!\n");	
		}
		else if( length(intersect(names(formals(value)), "seq")) == 0 ){
      throw("The insert hook function must have a an argument named \"seq\"");
		}
		else if(!is.Sequence(value(generateInsert(this,length=1)))){
			throw("The insert hook function must return a Sequence object!\n");	
		} else {
			this$.insert.hook<-value;
		}
		return(this$.insert.hook);

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
#       a<-GeneralInsertor(rate=1)
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
	class="GeneralInsertor", 
	function(
		object,
		...
	){

		.addSummaryNameId(object);
		object$.summary$"Accept window size"<-object$.accept.win;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###########################################################################
# Class:GeneralDeletor
##########################################################################/** 
#
# @RdocClass GeneralDeletor
# 
# @title "The GeneralDeletor class"
# 
# \description{ 
#	This is the class implementing a process generating deletion events.
#	The rates of the deletion events are calculated as the product of the general rate
#	of the process and the "rate.multiplier" site-process-specific parameter.
#
#	The simulation code calls the \code{Perform} method on the selected 
#	deletion event objects,	which call their handler function to perform the deletion.
#
#	The deletion lengths are proposed by the function stored in the \code{proposeBy} virtual field.
#	The function must have the following arguments: process (the process object), sequence (the target sequence), 
#	position (the position of the site which generated the event).
#
#	The deletion randomly affects the sites from the left or from the right of the target position (but never both).
#	Positions which are out of range are discarded.
#
#	The proposed deletions are accepted or rejected by the function stored in the \code{acceptBy} virtual field.
#	The function must have the following arguments: process (the deletion prcoess), sequence (the target sequence), range (a vector of positions
#	affected by the deletion).
#
#	The probability of accepting a deletion is calculated as the product of the "deletion.tolerance" site-process-specific 
#	parameters from the sites affected by the deletion event.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{rate}{The general rate of the object.}
#	\item{propose.by}{A function used to propose events.}
#	\item{accept.by}{A function used to accept/reject events.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
# 	# create a GeneralDeletor object
#       # proposed deletion length: 4, always accept
#       d<-GeneralDeletor(
#                       name = "DEL",
#                       rate = 1,
#                       propose.by=function(process, sequence, position){ 4 },
#                       accept.by=function(process, sequence, range){ TRUE }
#       )
#	d
#	# check if object inherits from GeneralDeletor
#	is.GeneralDeletor(d)
#	# get object summary
#	summary(d)
#	# set/get name
#	d$name<-"Del Bosque"
#	d$name
#	# set/get rate
#	d$rate<-0.5
#	d$rate
#	# set/get proposeBy
#	# propose deletion lengths between 3 and 6
#	d$proposeBy<-function(process, sequence, position){ sample(3:6,1) }
#	d$proposeBy
#	# set/get acceptBy
#   # reject half of the events
#	d$acceptBy<-function(process, sequence, range){ sample(c(TRUE, FALSE), 1)} 
#	d$acceptBy
#	# create a sequence object, attach process
#	s<-NucleotideSequence(string="AATTGGCCCCGGTTAA", processes=list(list(d)))
#	# set the rate multiplier
#       setRateMultipliers(s,d,2)
#       # get the list of active events at site 6
#       events<-getEventsAtSite(d,s$sites[[6]])
#       events;
#       # print sequence 
#       s
#       # set the position for the event object
#       e<-events[[1]];
#       e$.position<-6;
#       # perform the deletion event
#       Perform(e)
#       # check the results
#       s
# }
# 
# @author
#
# \seealso{ 
# 	GeneralInDel DiscreteDeletor ContinuousDeletor FastFieldDeletor
# }
# 
#*/###########################################################################
setConstructorS3(
  "GeneralDeletor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralDeletor",
			.handler.template=NA,
			.is.general.deletor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="deletion.tolerance",
      name="Deletion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );

		this$acceptBy<-function(process=NA,sequence=NA,range=NA){

				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Reject if the range contains a site which is not attached to 
						# the process:
						if(!isAttached(site, process)){
							return(FALSE);
						}
						accept.prob<-c(accept.prob, getParameterAtSite(process, site, "deletion.tolerance")$value);
				}

				# Calculate the product of the per-site 
				# acceptance probabilities.
				accept.prob<-prod(as.numeric(accept.prob));

			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}
		
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 process<-event$.process;
					 sequence<-event$.site$.sequence;
					 details<-list();
					 details$type<-"deletion";
					 details$accepted<-FALSE;

					 # Propose a sequence length:
					 length<-process$.propose.by(process=process,seq=sequence, pos=position);

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Calculate the sites to delete:
					 range<-numeric();	
					 if(direction == "RIGHT") {
							range<-position:(position+length-1);
					 } else if(direction == "LEFT") {
						  range<-(position-length+1):position;
					 } else {
							throw("You should never see this message!\n");
					 }

					 # Discard potential negative values and values larger than the sequence length:
					 range<-range[ range > 0 & range <= sequence$.length];
					 details$range<-c(min(range),max(range));
					 
					 # Perform the deletion if it is accepted:
					 if (process$.accept.by(process=process,sequence=sequence,range=range) == TRUE) {
						details$accepted<-TRUE;
					 	deleteSubSequence(sequence,range);
					}					
			
					# Return event details:	
					return(details);

				}
		 }

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralDeletor
##	
###########################################################################/**
#
# @RdocDefault is.GeneralDeletor
# 
# @title "Check whether an object inherits from GeneralDeletor" 
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#		\item{this}{An object.}
#		\item{...}{Not used.}
#
# } 
# 
# \value{ 
#	TRUE or FALSE.
# } 
#
# \examples{
#	# create some objects
#	d<-GeneralDeletor()
#	i<-GeneralInsertor()
#	# check if they inherit from GeneralDeletor
#	is.GeneralDeletor(d)
#	is.GeneralDeletor(i)
# } 
# 
# 
# @author 
# 
#*/###########################################################################
setMethodS3(
	"is.GeneralDeletor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.deletor)){return(TRUE)}
    if ( inherits(this, "GeneralDeletor")) {
      this$.is.general.deletor<-TRUE;
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
  class="GeneralDeletor",
  function(
    this,
    ...
  ){
      NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: getEventsAtSite
##	
###########################################################################/**
#
# @RdocMethod getEventsAtSite
# 
# @title "Title" 
# 
# \description{ 
#	@get "title".
#
#	This method generates a list containing a single deletion event object. The rate
#	of the event is calculated as the product of the general rate of the process and the 
#	"rate.multiplier" site-process specific parameter. An empty list is
#       returned if the rate is zero or NA.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralDeletor object.} 
#	\item{target.site}{The target Site object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of event objects.
# } 
# 
# \examples{
#	# create the Sequence object
#	s<-NucleotideSequence(string="ATGCCCGGCGGATTTATTA");
#	# create a GeneralDeletor object
#	# proposed deletion length: 4, always accept
#	d<-GeneralDeletor(
#			name = "Del Bosque",
#			rate = 0.5,
#			propose.by=function(process, sequence, position){ 4 },
#			accept.by=function(process, sequence, range){ TRUE }
# 	)
#	# attach process to site
#	attachProcess(s,d);
#	# set the rate multiplier
#	setRateMultipliers(s,d,2)
#	# get the list of active events at site 6
#	events<-getEventsAtSite(d,s$sites[[6]])
#	events;
#	# print sequence 
#	s
#	# set the position for the event object
#	e<-events[[1]];
#	e$.position<-6;
#	# perform the deletion event
#	Perform(e)
#	# check the results
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
	"getEventsAtSite", 
	class="GeneralDeletor", 
	function(
		this,
		target.site,
		...
	){
		if(!exists(x="PSIM_FAST")){
			if(missing(target.site)) {
				throw("No target site provided!\n");
			}
			if(!is.Site(target.site)) {
				throw("Target site invalid!\n");
			}
			else if(!is.function(this$.propose.by)) {
				throw("proposeBy is not set, cannot propose deletion!\n");
			} 
			else if (!is.function(this$.accept.by)){
				throw("acceptBy is not set, cannot generate deletion event deletion!\n");
			}

		 	# Complain if sequence has a zero length:
		 	if(target.site$.sequence$.length == 0) {
				 throw("Sequence has zero length so there is nothing to delete! How did you get here anyway?\n");
		 	}

		}
		 # Clone the event template object:
		 deletion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 deletion.event$.position<-target.site$.position;
		 # Set the target site:
		 deletion.event$.site<-target.site;
		 # Set event name:
		 deletion.event$.name<-"Deletion";
		 # Set the genrator process:
		 deletion.event$.process<-this;
		
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
	         rate.multiplier<-target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value; 
		 deletion.event$.rate<-(this$.rate * rate.multiplier);

		 # Set the handler for the deletion event:
		 deletion.event$.handler<-this$.handler.template;		

		# Return the event object in a list:
		list(deletion.event);

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
#       a<-GeneralDeletor(rate=1,name="Del Bosque")
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
	class="GeneralDeletor", 
	function(
		object,
		...
	){

		.addSummaryNameId(object);
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

