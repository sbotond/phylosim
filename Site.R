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
#		@classhierarchy
# 
#	
#	
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{state}{}
# 	\item{alphabet}{}
# 	\item{ancestral}{}
# 	\item{processes}{}
# 	\item{sequence}{}
#	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 
# }
# 
# \examples{ 
#		site<-Site();
#		print(site);
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
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency
##	
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
setMethodS3(
	"setState", 
	class="Site", 
	function(
		this,
		new.state,
		...
	){
		
		new.state<-as.character(new.state);	
		# Check if new.state is scalar:
		if (length(new.state) != 1 ){throw("The state must be a vector of length 1!\n")}
		# Check if the site has an alphabet attached:
		else if(is.na(this$alphabet)) {throw("Cannot set state because the site has no alphabet attached!\n")}
		# Check if symbol is in the site alphabet:	
		else if( !hasSymbols(this$.alphabet,new.state)) {throw("Symbol not in site alphabet!\n")}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
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
setMethodS3(
	"setAlphabet", 
	class="Site", 
	function(
		this,
		new.alphabet,
		...
	){
		
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
		else{
			flagTotalRate(this);
		 .flagSeqCumulativeRates(this);
			this$.alphabet<-new.alphabet;
		}

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
## Method: recalculateTotalRate
##	
setMethodS3(
	".recalculateTotalRate", 
	class="Site", 
	function(
		this,
		...
	){

		if(!is.na(getState(this))){
			this<-enableVirtual(this);	
			total.rate<-0;	
			for(e in this$events) {
				total.rate<-(total.rate + getRate(e));
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
setMethodS3(
	"getTotalRate", 
	class="Site", 
	function(
		this,
		...
	){
		
		if(is.na(this$.total.rate)) {
			.recalculateTotalRate(this);
		}

			return(this$.total.rate);
		
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
## Method: .flagSeqCumulativeRates
##	
setMethodS3(
	".flagSeqCumulativeRates", 
	class="Site", 
	function(
		this,
		...
	){
		
		if(is.Sequence(this$.sequence)) {
			.flagCumulativeRates(this$.sequence);
		}

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

##	
## Method: setEvents
##	
setMethodS3(
	"setEvents", 
	class="Site", 
	function(
		this,
		new.rate,
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
setMethodS3(
	"setSequence", 
	class="Site", 
	function(
		this,
		new.seq,
		...
	){

		if(!is.Sequence(new.seq)) {
			throw("Sequence object invalid!\n");
		} else {
			this$.sequence<-new.seq;
		}

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
setMethodS3(
	"as.character", 
	class="Site", 
	function(
		x,
		...
	){

		x$state;		

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

			flagTotalRate(this);
		 .flagSeqCumulativeRates(this);

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

