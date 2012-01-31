##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass Process
# 
# @title "The Process class"
# 
# \description{ 
#
#	This is the class representing a generic process acting on Site and Sequence objects. Process objects can be attached
#	to Site objects if the associated Alphabet objects match.
#
#	The processes can have site-process-specific parameters.
#	The templates for site-process-specific parameters and their default values are stored in the Process objects and
#       copied into the Site object when the process is attached. See the documentation of the Site class for more details.
#
#	The rate multiplier parameter (id="rate.multiplier") is
#	present in the Process class and is inherited by all descendant classes.
#	
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the Process object: a character vector of length one.}
#	\item{alphabet}{The associated Alphabet object.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@methods
# }
# 
# \examples{ 
#	# create a Process object
#	p<-Process(name="MyProc",alphabet=AminoAcidAlphabet())	
#	# check if it's a Process object
#	is.Process(p)
#	# check object consistency
#	checkConsistency(p)
#	# set process name
#	p$name<-"Ppppproccc"
#	# get process name
#	p$name
#	# get unique process identifier
#	p$id
#	# get the list of site specific paramters and paramter IDs
#	p$siteSpecificParamList
#	p$siteSpecificParamIds
#	# get Process object summary
#	summary(p)
#	# clone process object
#	pp<-clone(p)
#	# test object identity
#	p == p
#	p == pp
#	# create a site object
#	s<-Site(alphabet=AminoAcidAlphabet())
#	# attach process to Site object
#	attachProcess(s,p)
#	# get events at specified site
#	getEventsAtSite(p,s)	# empty list
#	# detach process via virtual field
#	s$processes<-list()
#	# attach processes via virtual field
#	s$processes<-list(p,pp)
#	# set the value of the rate multiplier for Site s
#	setParameterAtSite(p,s,id="rate.multiplier",value=2)
#	# get the value of the rate multiplier for Site s
#	getParameterAtSite(p,s,id="rate.multiplier")
# }
# 
# @author
#
# \seealso{ 
# 	@seeclass 
# }
# 
#*/###########################################################################
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
###########################################################################/**
#
# @RdocDefault is.Process
# 
# @title "Check if an object is an instance of the Process class" 
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
#	a<-Site();
#	p<-Process()
#	# check if they inherit from Process
#	is.Process(a)
#	is.Process(p)
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
###########################################################################/**
#
# @RdocMethod ==
# \alias{!=.Process}
# 
# @title "Check whether the two supplied Process objects are identical" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{e1}{A Process object.} 
# 	\item{e2}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	TRUE or FALSE; 
# } 
# 
# \examples{
#	# create some Process objects
#	p1<-Process()
#	p2<-clone(p1)
#	# check object equality
#	p1 == p1
#	p1 == p2
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
  "==",
  class="Process",
  function(
    e1,
    e2,
    ...
  ){
		
		equals(e1, e2);

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
    e1,
    e2,
    ...
  ){
		
		!equals(e1, e2);

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
###########################################################################/**
#
# @RdocMethod getId
# 
# @title "Get the unique identifier of a Process object" 
# 
# \description{ 
#	@get "title".
#	The unique identifier is the concatenation of the class, the object name as returned by getName() and the object hash 
#	as returned by hashCode().
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a Process object
#	p<-Process()
#	# get unique id
#	getId(p)
#	# get unique id via virtual field
#	p$id
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
## Method: .setId
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
###########################################################################/**
#
# @RdocMethod setId
#
# @title "Forbidden action: setting the unique Process object identifier"
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
###########################################################################/**
#
# @RdocMethod getName
# 
# @title "Get the name of a Process object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A charcter vector of length one.
# } 
# 
# \examples{
#	# create a Process object
#	p<-Process()
#	# get object name
#	getName(p)
#	# get name via virtual field
#	p$name
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
###########################################################################/**
#
# @RdocMethod setName
# 
# @title "Set the name of a Process object" 
# 
# \description{ 
#	@get "title".
#
#	This method also updates the unique identifier of the Process object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{new.name}{A character vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Returns the new name (invisible).
# } 
# 
# \examples{
#	# create object
#	p<-Process()
#	# get name
#	p$name
#	# set new name
#	getName(p)
#	# get name and id
#	p$name
#	p$id
#	# set new name via virtual field
#	p$name<-"Procey"
#	p$name
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
	class="Process", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
		.setId(this);
		invisible(new.name);
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
###########################################################################/**
#
# @RdocMethod getSiteSpecificParamList
# 
# @title "Get the list of site specific parameters of a Process object"
# 
# \description{ 
#	@get "title".
#	Every site specific parameter is a list storing the name, the identifier and the type of the given parameter.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of site specific parameters.
# } 
# 
# \examples{
#	# create a process object
#	p<-Process()
#	# get the list of site specific parameters
#	getSiteSpecificParamList(p)
#	# get it via virtual field
#	p$siteSpecificParamList
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
###########################################################################/**
#
# @RdocMethod setSiteSpecificParamList
#
# @title "Forbidden action: setting the site specific paramter list for a Process object"
#
# \description{
#       @get "title".
#	Use .addSiteSpecificParameter to add new site specific paramters when implementing new processes.
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
###########################################################################/**
#
# @RdocMethod getSiteSpecificParamIds
# 
# @title "Get the site specific paramter identifiers from a Process object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A charcter vector.
# } 
# 
# \examples{
#	# create process object
#	p<-Process()
#	# get site specific parameter identifiers
#	getSiteSpecificParamIds(p)
#	# via virtual field
#	p$siteSpecificParamIds
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
###########################################################################/**
#
# @RdocMethod setSiteSpecificParamIds
#
# @title "Forbidden action: setting the paramter identifiers of the site specific paramters from a Process object"
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
###########################################################################/**
#
# @RdocMethod getAlphabet
# 
# @title "Get the Alphabet object associated with a given Process object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An Alphabet object.
# } 
# 
# \examples{
#	# create a process object
#	p<-Process(alphabet=NucleotideAlphabet())
#	# get associated Alphabet object
#	getAlphabet(p)
#	# via virtual field
#	p$alphabet
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
###########################################################################/**
#
# @RdocMethod setAlphabet
# 
# @title "Assotiate an Alphabet object with a Process object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{new.alphabet}{A valid Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Alphabet object (invisible).
# } 
# 
# \examples{
#	# create objects
#	a<-AminoAcidAlphabet()
#	p<-Process()
#	# assotiate p with Alphabet object a
#	setAlphabet(p,a)
#	p$alphabet
#	# assotiate p with a new NucleotideAlphabet via virtual field
#	p$alphabet<-NucleotideAlphabet()
#	p$alphabet
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
###########################################################################/**
#
# @RdocMethod getEventsAtSite
# 
# @title "Generate the list of active Event objects given a Site object" 
# 
# \description{ 
#	@get "title".
#	The Process object must be attached to the specified Site object.
#
#	This method is crucial for the simulations. For the Process class it returns an empty list. 
#	Descendant classes should implement meaningful getEventsAtSite methods. 
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{site}{A valid Site object.} 
# 	\item{position}{The position of the site in the enclosing Sequence object (if any).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An empty list.
# } 
# 
# \examples{
#	# create objects
#	a<-NucleotideAlphabet()
#	p<-Process(alphabet=a)
#	s<-Site(alphabet=a)
#	# attach Process p to Site s
#	s$processes<-list(p)
#	# get active Event objects (empty list)
#	getEventsAtSite(p,s)
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
# 	\item{this}{A Process object.} 
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
#       o<-Process()
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
# 	\item{this}{A Process object.} 
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
#	o<-Process()
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
###########################################################################/**
#
# @RdocMethod hasUndefinedRate
# 
# @title "Check if the Process object has undefined rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	For the instances of the Process class this method always returns FALSE.
#	Descendant classes should implement more meaningful methods.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	FALSE
# } 
# 
# \examples{
#	# create object
#	p<-Process()
#	# check if has undefined rates
#	hasUndefinedRate(p)	# return FALSE
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
  "hasUndefinedRate",
  class="Process",
  function(
    this,
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

    if(exists(x="PSIM_FAST")){
 	    return(FALSE);
    }
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
###########################################################################/**
#
# @RdocMethod clone
#
# @title "Clone a process object"
#
# \description{
#       @get "title".
#       Write protection is set to FALSE for the new Process object.
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{A Process object.}
#       \item{...}{Not used.}
# }
#
# \value{
#       A Process object.
# }
#
# \examples{
#       # create a process object
#       p<-Process()
#       # clone process object
#       pp<-clone(p)
#       # check identity
#       p == pp
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
  "clone",
  class="Process",
  function(
    this,
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
###########################################################################/**
#
# @RdocMethod as.character
#
# @title "Get the character representation of a Process object"
#
# \description{
#       @get "title".
#	The string returned is the unique Process object identifier (class name + process name + object hash).
# }
#
# @synopsis
#
# \arguments{
#       \item{x}{A Process object}
#       \item{...}{Not used.}
# }
#
# \value{
#  A character vector of length one.
# }
#
# \examples{
#
#       # create a Process object
#       p<-Process()
#	# get charatcer representation
#	x<-as.character(p)
#	print(x)
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
	"as.character", 
	class="Process", 
	function(
		x,
		...	
	){
			x$id;
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
	class="Process", 
	function(
		object,
		...	
	){
		this<-object;
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

##
## Method: hasSiteSpecificParameter
##
###########################################################################/**
#
# @RdocMethod hasSiteSpecificParameter
# 
# @title "Check if a Process object has the site-process specific parameter with the given id" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{id}{The identifier of the site-process specific parameter of interest.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	# create a process object
#	p<-Process()
#	# check whether it has the "rate.multiplier" site-process specific paramter
#	hasSiteSpecificParameter(p,"rate.multiplier");	# TRUE
#	# check whether it has the "omega" site-process specific paramter
#	hasSiteSpecificParameter(p,"omega");	# FALSE
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
  "hasSiteSpecificParameter",
  class="Process",
  function(
    this,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			else if ( length (intersect((as.vector(this$siteSpecificParamIds) == id),TRUE) ) == 0 ) {
					return(FALSE);
			} else {
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
## Method: getParameterAtSite
##
###########################################################################/**
#
# @RdocMethod getParameterAtSite
# 
# @title "Get the value of a site-process specific paramter from a Site object attached to a Process object" 
# 
# \description{ 
#	@get "title".
#	The Process object must be attached to the Site object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{site}{A Site object.} 
#	\item{id}{The identifier of the site-process specific parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The site-process specific parameter, which is a list containing the following keys: id, name, value, type.
# } 
# 
# \examples{
#	# create a Site and a Process object
#	a<-Alphabet()
#	s<-Site(alphabet=a)
#	p<-Process(alphabet=a)
#	# attach the process
#	attachProcess(s,p)
#	# get the value of the rate multiplier
#	getParameterAtSite(p,s,"rate.multiplier")
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
  "getParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			
			if (.checkTriplett(this,site,id)){
				id<-as.character(id);
				list (
					id=id,
					name=site$.processes[[getId(this)]]$site.params[[id]]$name,
					value=site$.processes[[getId(this)]]$site.params[[id]]$value,
					type=site$.processes[[getId(this)]]$site.params[[id]]$type
				);
			}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .getParameterAtSiteFast
##
setMethodS3(
  ".getParameterAtSiteFast",
  class="Process",
  function(
    this,
    site,
    id,
    ...
  ){
				site$.processes[[this$.id]]$site.params[[as.character(id)]]$value;
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setParameterAtSite
##
###########################################################################/**
#
# @RdocMethod setParameterAtSite
# \alias{setParameterAtSite.FastFieldDeletor} 
# @title "Set the value of a site-process specific paramter in a Site object attached to a Process object" 
# 
# \description{ 
#	@get "title".
#	The Process object must be attached to the Site object. The new value must be compatible with the type
#	of the site-process specific parameter.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A Process object.} 
# 	\item{site}{A Site object.} 
#	\item{id}{The identifier of the site-process specific parameter.}
#	\item{value}{The new value for the parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The site-process specific parameter, which is a list containing the following keys: id, name, value, type.
# } 
# 
# \examples{
#	# create a Site and a Process object
#	a<-BinaryAlphabet()
#	s<-Site(alphabet=a)
#	p<-Process(alphabet=a)
#	# attach the process
#	attachProcess(s,p)
#	# set the value of the rate multiplier
#	setParameterAtSite(p,s,"rate.multiplier",2)
#	# get the value of the rate multiplier
#	getParameterAtSite(p,s,"rate.multiplier")
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
  "setParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
    value,
    ...
  ){

    if(!exists(x="PSIM_FAST")){
	if (missing(id)) {throw("Parameter identifier is missing!\n")};
    }
     	id<-as.character(id);
			
	if (.checkTriplett(this,site,id)){

		type<-site$.processes[[this$id]]$site.params[[id]]$type;
    if(!exists(x="PSIM_FAST")){
		if (length(intersect(class(value),type)) == 0 ) {throw("The new value is of wrong type!\n")}
    }
		site$.processes[[this$id]]$site.params[[id]]$value<-value;
	
	}

	site$.total.rate<-NA;
	if(!is.na(site$.sequence)){
	site$.sequence$.cumulative.rate.flag<-TRUE;
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
## Method: .checkTriplett
##
setMethodS3(
  ".checkTriplett",
  class="Process",
  function(
    this,
    site,
		id,
		...
  ){
		
			if(exists(x="PSIM_FAST")){ return(TRUE) }
					
			if (!is.Site(site)) {throw ("Site object not valid!\n")}
			else if (!hasSiteSpecificParameter(this,id)) {
				throw(paste("The process",this$id,"has no site specific paramter with id:",id,"!\n",sep=" "));
			}
			else if (!isAttached(site,this)) {throw("Process is not attached to site!\n")} else {
				return(TRUE);
			}
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

