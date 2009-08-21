##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
	"Process",
	function( 
		name=NA,
		alphabet=NA,
		... 
		){

		this<-extend(
			PSRoot(),
			"Process",
			.name=NA,
			.id=NA,
			.alphabet=NA,
			.site.specific.param.list=list(),
			.event.template=NA,
			.write.protected=FALSE,
			.is.process=TRUE
		);
		.addSiteSpecificParameter(
			this,
			id="rate.multiplier",
			name="Rate multiplier",
			value=as.double(1),
			type="numeric"
		);
		
		STATIC<-TRUE;	

		if(!missing(alphabet)){
			this$alphabet<-alphabet;
			STATIC<-FALSE;
		}

		if (!missing(name)){
			this$name<-name;
			STATIC<-FALSE;
		} else {
			this$name<-"Anonymous";
		}

		if(!STATIC) {
			this$.event.template<-Event(process=this);
		}

		this;
	},
	enforceRCC=TRUE
);

##
## Method: is.Process
##
setMethodS3(
  "is.Process",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.process)){return(TRUE)}
    if ( inherits(this, "Process")) {
      this$.is.process<-TRUE;
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
setMethodS3(
  "checkConsistency",
  class="Process",
  function(
    this,
    ...
  ){
		
			if(is.null(this$.alphabet)) {
				throw("Process alphabet is NULL!\n");
			}
			else if(is.null(this$.name)) {
				throw("Process name is NULL!\n");
			}
			if(is.null(this$.site.specific.param.list)) {
				throw("Site specific parameter list is NULL!\n");
			}

			wp<-this$writeProtected;		
			if (wp) {
				this$writeProtected<-FALSE;
			}
			
			may.fail<-function(this) {
				# Do not reset alphabet for substirution processes
				# as that would wipe out the rates:
				if (!is.na(this$alphabet) & !is.GeneralSubstitution(this)) {
					this$alphabet<-this$alphabet;
				}			

			}
			tryCatch(may.fail(this),finally=this$writeProtected<-wp);
			
			.checkSiteSpecificParamList(this,plist=this$.site.specific.param.list);			
	
			return(invisible(TRUE));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkSiteSpecificParamList
##
setMethodS3(
  ".checkSiteSpecificParamList",
  class="Process",
  function(
    this,
		plist,
    ...
  ){
		
			if (missing(plist)) {
					throw("No list given!\n");
			}			

			if(!is.list(plist)) {
				throw("Site specific parameter list is invalid!\n");
			} else {
				for (p in plist) {
						if (!setequal(names(p),c("name","value","type"))) {
								throw("Process-site specific parameter list inconsistent!\n");
						}
						else {
								if (length(p$name) == 0 | !is.character(p$name)) {
									throw("Site specific process parameter name invalid!\n");
								}
								else if (length(p$type) == 0 | !is.character(p$type)) {
									throw("Site specific process parameter type invalid!\n");
								}
								else if (length(intersect(class(p$value),p$type)) == 0 ) {
									throw(paste("The site specific parameter \"",p$name,"\" supposed to be \"",p$type,"\", but it is something else!\n",sep=""));
							}
						}
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
## Method: ==.Process
##
setMethodS3(
  "==",
  class="Process",
  function(
    this,
		that,
    ...
  ){
		
		equals(this, that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: !=.Process
##
setMethodS3(
  "!=",
  class="Process",
  function(
    this,
		that,
    ...
  ){
		
		!equals(this, that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .addSiteSpecificParameter
##
setMethodS3(
  ".addSiteSpecificParameter",
  class="Process",
  function(
    this,
		id,
		name,
		value,
		type,
    ...
  ){
		
		if (missing(id)) {throw("No id given!\n")}
		else if (missing(name)) {throw("No name given!\n")}
		else if (missing(value)) {throw("No value given!\n")}
		else if (length( intersect(class(value),type) ) == 0 ) {
			throw("The specified default value is not of the correct type!\n");
		}
		
		id<-as.character(id);
		this$.site.specific.param.list[[id]]<-list(
            "name"=name,
            "value"=value,
						"type"=type
        );

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getId
##	
setMethodS3(
	"getId", 
	class="Process", 
	function(
		this,
		...
	){

		this$.id;

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
setMethodS3(
	".setId", 
	class="Process", 
	function(
		this,
		...
	){
	
	this.class<-class(this)[1];
	this$.id<-paste(this.class,this$.name,hashCode(this),sep=":");

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
setMethodS3(
	"setId", 
	class="Process", 
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
setMethodS3(
	"getName", 
	class="Process", 
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
	class="Process", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
		.setId(this);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSiteSpecificParamList
##	
setMethodS3(
	"getSiteSpecificParamList", 
	class="Process", 
	function(
		this,
		...	
	){
		this$.site.specific.param.list;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSiteSpecificParamList
##	
setMethodS3(
	"setSiteSpecificParamList", 
	class="Process", 
	function(
		this,
	  value,	
		...	
	){
		throw("You should not set the siteSpecificParamList directly!\n");
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSiteSpecificParamIds
##	
setMethodS3(
	"getSiteSpecificParamIds", 
	class="Process", 
	function(
		this,
		...	
	){
		names(this$.site.specific.param.list);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSiteSpecificParamIds
##	
setMethodS3(
	"setSiteSpecificParamIds", 
	class="Process", 
	function(
		this,
	  value,	
		...	
	){
		throw("You should not set the siteSpecificParamIds directly!\n");
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
	class="Process", 
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
	class="Process", 
	function(
		this,
		new.alphabet,
		...	
	){
		
			.checkWriteProtection(this);	
			if (!is.Alphabet(new.alphabet)) {throw("The alphabet object is invalid!\n")}
			else {
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
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="Process", 
	function(
		this,
		site,
		position,
		...	
	){
		
		# Returns a list of event objects;	
		#e1<-Event(name="A->T",rate=0.2,process=this);	
		#e2<-clone(e1);
		#e1$name<-"Insertion";
		#list(e1,e2);
		list();
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
  class="Process",
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
  class="Process",
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
## Method: hasUndefinedRate
##
setMethodS3(
  "hasUndefinedRate",
  class="Process",
  function(
    this,
    value,
    ...
  ){

		return(FALSE);

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
  class="Process",
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
## Method: clone
##
setMethodS3(
  "clone",
  class="Process",
  function(
    this,
    value,
    ...
  ){

			tmp<-clone.Object(this);
			if(tmp$writeProtected){
					tmp$writeProtected<-FALSE;
			}
		
			# Reassingning name to
			# force Id update.
			tmp$name<-tmp$name;
			tmp;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: as.character.Process
##	
setMethodS3(
	"as.character", 
	class="Process", 
	function(
		this,
		...	
	){
			this$id;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .addSummaryAlphabet
##	
setMethodS3(
	".addSummaryAlphabet", 
	class="Process", 
	function(
		this,
		...	
	){

		
			if(!is.na(this$alphabet)) {
     	 	alphabet_symbols<-paste(this$alphabet$symbols,collapse=" ");
      	this$.summary$"Alphabet"<-paste("\n","  Type: ",this$alphabet$type,"\n","  Symbols: ", alphabet_symbols,sep="");
    	} else {
       	 this$.summary$"Alphabet"<-NA
    	}
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .addSummaryNameId
##	
setMethodS3(
	".addSummaryNameId", 
	class="Process", 
	function(
		this,
		...	
	){

		if(is.null(this$.summary$"Name")){
			this$.summary$"Name"<-this$name;
		}
		if(is.null(this$.summary$"Id")){
			this$.summary$"Id"<-this$id;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Process
##	
setMethodS3(
	"summary", 
	class="Process", 
	function(
		this,
		...	
	){

		.addSummaryNameId(this);

		# Skip the alphabet for InDel processes
		if(!is.GeneralInDel(this)){
			.addSummaryAlphabet(this);
		}
			
			tmp<-character();
			param_list<-this$siteSpecificParamList;
			counter<-0;		
	
			for (id in names(param_list)) {
					param<-param_list[[id]];	
					tmp<-paste(tmp,
												"\n  Id: ",id,
												"\n  Name: ",param$name,
												"\n  Type: ",param$type,
												"\n  Default value: ",param$value,
												sep=""
										);
										counter<-counter+1;
										if ( counter < length(param_list) ){
											tmp<-paste(tmp,"\n");
										}
										
			}	
		
			header<-paste("Site specific parameters (",length(param_list),")",sep="");
			this$.summary[[header]]<-tmp;
			if(getWriteProtected(this)) {
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


