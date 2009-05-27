##	$Id: Event.R,v 1.25 2009-04-29 13:28:25 sbotond Exp $
##
##	Class: Event
##	Descriprion: 
##	
##	
##	
##	
##	
##	
##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	
setConstructorS3(
	"Event",
	function( 
		name=NA,
		rate=NA,
		site=NA,
		position=NA,
		process=NA,
		handler=NA,
		target.state=NA,
		... 
		){

		this<-extend(
			PSRoot(),
			"Event",
			.name="Anonymous",
			.rate=NA,
			.process=NA,
			.handler=NA,
			.site=NA,
			.position=NA,
			.target.state=NA,
			.write.protected=FALSE,
			.is.event=TRUE
		);

		STATIC<-TRUE;
						
		if(!missing(name)) {
			this$name<-name;
			STATIC<-FALSE;
		}
		
		if(!missing(rate)) {
			this$rate<-rate;
			STATIC<-FALSE;
		}	

		if(!missing(process)) {
			this$process<-process;
			STATIC<-FALSE;
		}	
		
		if(!missing(site)) {
			this$site<-site;
			STATIC<-FALSE;
		}	
		
		if(!missing(position)) {
			this$position<-position;
			STATIC<-FALSE;
		}	
		
		if (!missing(target.state) & missing(site)) {
			throw("Target state is specified, but no site object given!\n");
		}
    else if (!missing(target.state)) {
			this$targetState<-target.state;
		}
		
		# The site object was passed through a getField method,
		# which disabled the virtual fields, so we have to enable them:
		if (!is.na(this$.site)){
				this$.site<-enableVirtual(this$.site);
		}
		this;
	},
	enforceRCC=TRUE
);

##
## Method: is.Event
##
setMethodS3(
  "is.Event",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.event)){return(TRUE)}
    if ( inherits(this, "Event")) {
      this$.is.event<-TRUE;
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
## Method: getName
##	
setMethodS3(
	"getName", 
	class="Event", 
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
setMethodS3(
	"setName", 
	class="Event", 
	function(
		this,
		new.name,
		...
	){
		.checkWriteProtection(this);	
		if(missing(new.name)){throw("No new name provided!\n")}
		new.name<-as.character(new.name);	
		if (new.name == "") {throw("Cannot set empty name!\n")}
		else { this$.name<-new.name}
		
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
setMethodS3(
	"getRate", 
	class="Event", 
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
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){
	
		.checkWriteProtection(this);	
		if(missing(new.rate)){throw("No new rate provided!\n")}
		else if (!is.numeric(new.rate)){throw("The rate must be numeric!\n")}
		else { this$.rate<-new.rate }
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcess
##	
setMethodS3(
	"getProcess", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.process;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProcess
##	
setMethodS3(
	"setProcess", 
	class="Event", 
	function(
		this,
		new.proc,
		...
	){
		
		.checkWriteProtection(this);	
		if(missing(new.proc)){throw("No new rate provided!\n")}
		else if (!is.Process(new.proc)){throw("Process object invalid!\n")}
		else { this$.process<-new.proc}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getHandler
##	
setMethodS3(
	"getHandler", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.handler;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setHandler
##	
setMethodS3(
	"setHandler", 
	class="Event", 
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
## Method: getSite
##	
setMethodS3(
	"getSite", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.site;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getPosition
##	
setMethodS3(
	"getPosition", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.position;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setPosition
##	
setMethodS3(
	"setPosition", 
	class="Event", 
	function(
		this,
		value,
		...
	){
	
		if(value > this$.site$.sequence$.length | value < 1) {
			throw("Invalid position!\n");
		}
		this$.position<-value;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTargetState
##	
setMethodS3(
	"getTargetState", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.target.state;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTargetState
##	
setMethodS3(
	"setTargetState", 
	class="Event", 
	function(
		this,
		new.state,
		...
	){
		
		.checkWriteProtection(this);	
		if (is.na(this$.site)) {
			throw("The event has no target site, so target alphabet is unknown. Refusing to set targetState!\n");
		}	
		else if (!is.Site(this$.site)) {
			throw("Target site is invalid!\n");
		}	
		else if (is.na(new.state)) {
			return(this$.target.state<-new.state);
		}
		else if (!hasSymbols(this$.site$.alphabet,new.state)) { 
			throw("The target state must be in the target site alphabet!\n");	
		}
		else {
			if(this$.site$state != new.state) {
					warning("The proposed new target state is not equal with the current state of the associated site. This is strange, but proceeding anyway.\n");
			}
			this$.target.state<-new.state;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSite
##	
setMethodS3(
	"setSite", 
	class="Event", 
	function(
		this,
		new.site,
		...
	){
			
		.checkWriteProtection(this);	
		if (!is.Site(new.site)) {throw("Site object invalid!\n")}
		new.site<-enableVirtual(new.site);
		if(missing(new.site)) {throw("No site given")}

		else if (!is.na(this$process)){
				if (this$.process$.alphabet != new.site$.alphabet){
					throw("The site and process alphabets are incompatible!\n");
			}
		}
		
		this$.site<-new.site;
		invisible(this$.site)
		
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setHandler
##	
setMethodS3(
	"Perform", 
	class="Event", 
	function(
		this,
		...
	){
	
		if(!is.function(this$.handler)){throw("Event handler is not a function!\n")}		
		else if (!is.Site(this$.site)){throw("The site associated with the event is not valid!\n")}
		else if(is.null(this$.position)){throw("The target site position is unknown!Refusing to perform event!\n")}
		else if (!is.Process(this$.process)) {
			throw("The event has no generator process. Refusing to perform!\n");
		}
		else if (is.na(this$.rate)) {
			throw("The event has no rate. Refusing to perform!\n");
		}
		else if (!is.na(this$.target.state) & (this$.target.state != this$site$state) ) {
			warning(paste("The event acts on state ",this$.target.state," but the target site has the state ",this$site$state,"! Doing nothing!\n",sep=""));
			return(invisible(FALSE));
		}
		else {

			# Better not perform anything on a dirty object!
			# FIXME -  the checking of this flag can be redundant!
			if(this$.site$.sequence$.cumulative.rate.flag) {
				.recalculateCumulativeRates(this$.site$.sequence);
			}
			# Do NOT flag cumulative rate! The vent should take care of that!

			# Flag site if we deal with substitutions:
			if( is.GeneralSubstitution( getProcess(this) ) ) {
				this$.site$.sequence$.flagged.sites<-c(this$.site$.sequence$.flagged.sites, this$.position);
			}

			# Call the event handler to perform event:
			this$.handler(this);

			# Event will self-destruct to prevent trouble:
			.setHandler(this, function(event=this) { throw("You can perform an event only once!\n") } );

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
## Method: .setHandler
##	
setMethodS3(
	".setHandler", 
	class="Event", 
	function(
		this,
		new.handler,
		...
	){
		
		if(missing(new.handler)){throw("No new handler provided!\n")}
		else if (!is.function(new.handler)){throw("The handler must be a function!\n")}
		else { this$.handler<-new.handler}

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
setMethodS3(
  "getWriteProtected",
  class="Event",
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
## Method: setWriteProtected
##
setMethodS3(
  "setWriteProtected",
  class="Event",
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
  class="Event",
  function(
    this,
    value,
    ...
  ){

    if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

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
  class="Event",
  function(
    this,
    ...
  ){
		
		wp<-this$writeProtected;
		if(wp) {
			this$writeProtected<-FALSE;
		}

		may.fail<-function(this) {

			if (is.null(this$.name)){
				throw("Event name is NULL!\n");
			}
			else if (!is.na(this$.name))	{
				this$name<-this$name;
			}

			if (is.null(this$.rate)){
				throw("Event rate is NULL!\n");
			}
			else if (!is.na(this$.rate))	{
				this$rate<-this$rate;
			}


			if (is.null(this$.process)){
				throw("Event rate is NULL!\n");
			}
			else if (!is.na(this$.process))	{
				this$process<-this$process;
			}

			if (is.null(this$.site)){
				throw("Event site is NULL!\n");
			}
			else if (!is.na(this$.site))	{
				this$site<-this$site;
			}


			if (is.null(this$.target.state)){
				throw("Event target state is NULL!\n");
			}
			else if (!is.na(this$.target.state))	{
				this$targetState<-this$targetState;
			}
		}
		tryCatch(may.fail(this), finally=this$writeProtected<-wp);

		return(invisible(TRUE))	

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: as.character.Event
##	
setMethodS3(
	"as.character", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){

		procid<-NA;
		if(!is.na(this$.process)){
			procid<-this$.process$id;
		}	
		paste(this$.name," (",this$.rate,")"," <-- ",procid,sep="");	
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Event
##	
setMethodS3(
	"summary", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){
			
		this$.summary$"Name"<-this$.name;	
		this$.summary$"Rate"<-this$.rate;	

		procid<-NA;
		if(!is.na(this$.process)){
			procid<-this$.process$id;
		}	
		this$.summary$"Generator process"<-procid;
		
		site.state<-NA;
		if(!is.na(this$.site)){
			site.state<-getState(this$.site);
		}	
		
		this$.summary$"Target state"<-this$.target.state;
		this$.summary$"Target site state"<-site.state;
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

