##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	

##
## Constructor: PSRootSummary
##
##########################################################################/** 
#
# @RdocClass PSRootSummary
# 
# @title "The PSRootSummary class"
# 
# \description{ 
#	PSRootSummary objects are blessed lists containing summary entries created by
#	\code{summary.*} methods.
#	
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{summary}{A list.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
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
  "PSRootSummary",
  function(summary=list(),...){
			
			# Stepping out of the R.oo framework to provide 
			# the expected behaviour.
			class(summary)<-c("PSRootSummary");
			summary;
  },
  ###
  enforceRCC=FALSE
);

##
## Method: print.PSRootSummary
##
###########################################################################/**
#
# @RdocMethod print
# 
# @title "Print out a PSRootSummary object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A PSRootSummary object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The summary object (invisible).
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
  "print",
  class="PSRootSummary",
  appendVarArgs=FALSE,
  function(
    x,
    ...
  ){
	this<-x;
	cat("\n");
	for (i in names(this)){
        cat(paste(i,": ",this[[i]],"\n",sep=""));
   	}
		cat("\n");
		invisible(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: checkConsistency;
##
###########################################################################/**
#
# @RdocMethod	checkConsistency
# 
# @title "Check object consistency"
# 
# \description{ 
#		@get "title".
#		The consisntency check is not implemented for PSRootSummary objects,
#		the method prints out a warning about that.
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
  class="PSRootSummary",
  function(
		this,
    ...
  ){
		
		warning("Consistency check is not implemented in class ",class(this)[[1]],"!\n");	
		return(invisible(TRUE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##########################################################################/** 
#
# @RdocClass PSRoot
# 
# @title "The root class for all phylosim objects"
# 
# \description{ 
#		The root class for all phylosim objects containig some utility methods. 
#		@classhierarchy
# }
#	
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#	
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#		obj<-PSRoot();
#		ll(obj);
# }
# 
# @author
#
#
# \seealso{ 
# 	Object
# }
# 
#*/###########################################################################
setConstructorS3(
  "PSRoot",
  function(...){
  extend(Object(), "PSRoot",
		.comments=character(0),
		.summary=list()
  );
  },
  ###
  enforceRCC=TRUE
);


##	
## Method: virtualAssignmentForbidden
##	
###########################################################################/**
#
# @RdocMethod virtualAssignmentForbidden
# 
# @title "Throws an error message informing the user about forbidden action on virtual a field" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Throws an error.
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
	"virtualAssignmentForbidden", 
	class="PSRoot", 
	###
	function(
		this,
		...
	){
		throw("You cannot set the value of this virtual field directly!");
	},
	###
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: enableVirtual
##	
###########################################################################/**
#
# @RdocMethod enableVirtual
# 
# @title "Enable the use of virtual fields for a given object" 
# 
# \description{ 
#	@get "title".
#	R.oo disables the virtual field feature inside get/set methods. This method can be used to re-enable virtual fields.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The PSRoot object.
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
	"enableVirtual", 
	class="PSRoot", 
	###
	function(
		this,
		...
	){
			attr(this,"disableGetMethods")<-NULL;
			attr(this,"disableSetMethods")<-NULL;
			this;
	},
	###
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: stringLength
##	
###########################################################################/**
#
# @RdocDefault stringLength
# 
# 
# @title "Returns the string length of the character representation of an object" 
# 
# \description{ 
#	@get "title".
#	More useful as a static method.
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
# 	An integer vector of length one.
# } 
# 
# \examples{
#	x<-"character representaion"
#	# get the strign length of x
#	stringLength(x)
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
	"stringLength", 
	class="default", 
	function(
		this,
		...
	){
		
		this<-as.character(this);	
		if (length(this) != 1){throw("This function can handle only vectors of length 1!")};

		return(length(strsplit(this,split="",fixed=TRUE)[[1]]));	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: stringLengthVector
##	
###########################################################################/**
#
# @RdocDefault stringLengthVector
# 
# 
# @title "Returns the string lengths of the character represenations of a collection of objects" 
# 
# \description{ 
#	@get "title".
#	More useful as a static method.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An array or a list of object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An integer vector with the corresponding lengths.
# } 
# 
# \examples{
#	x<-c("character representaion","other string");
#	# get the strign length of x
#	stringLengthVector(x)
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
	"stringLengthVector", 
	class="default", 
	function(
		this,
		...
	){
	
		as.numeric(apply(as.array(this),1,stringLength));
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getMethodsList
###########################################################################/**
#
# @RdocMethod getMethodsList
# 
# @title "Get the list of applicable methods for an object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of applicable methods.
# } 
# 
# \examples{
#	# create an object
#	o<-PSRoot()
#	# get the applicable methods
#	getMethodsList(o)
#	# get methods via virtual field
#	o$methodsList
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
##
setMethodS3(
  "getMethodsList",
  class="PSRoot",
  function(
    this,
    ...
  ){

			class<-class(this)[[1]];
			mlist<-getMethods.Class(this);

			# If the class has no methods, do not 
			# consider the methods from the parent class.
			if(names(mlist)[[1]] == class){	
      			as.character(names(mlist[[1]]));
			}
			else {
				return(character(0));
			}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: setMethodsList
##
###########################################################################/**
#
# @RdocMethod setMethodsList
#
# @title "Forbidden action: setting the list of applicable methods for an object"
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
  "setMethodsList",
  class="PSRoot",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: ll
##
###########################################################################/**
#
# @RdocMethod ll
# 
# @title "Display detailed information about the virtual fields and methods defined for a given object" 
# 
# \description{ 
#	@get "title".
#	The method prints the class of the object, all the parent classes,
#	and the virtual fields and methods defined in the immediate class.
#
#	This method provides a "quick and minimal" documentation for PhyloSim classes.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
# 	\item{quiet}{Do not print out methods list.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Text.
# } 
# 
# \examples{
#	# create a Site object
#	s<-Site()
#	ll(s)
#	# get information about the Process class
#	ll(Process())
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
  "ll",
  class="PSRoot",
  function(
    this,
    quiet=FALSE,
    ...
  ){
		
		class<-class(this);
		parents<-class[-1];
		class<-class[[1]]
		methods<-getMethodsList(this);
		fields<-getFields(this);
		text<-character(0);	

		pretty.print<-function(vec,text){

				tmp<-"";
				if(length(vec) > 0 ){
					tmp<-paste(tmp,"  ",vec,sep="",collapse="\n");
				  paste(text,tmp,"\n",sep="");
				} else {
					return(text);
				}
		}

	
		text<-paste(text,"\nClass: ",class,"\n",sep="");
		text<-paste(text,"Inherits from: ",paste(parents,collapse=" "),"\n",sep="");
		text<-paste(text,"Fields (",length(fields),"):\n",sep="");
		text<-pretty.print(fields,text);	

		# Discriminate between the methods implementing 
		# virtual fileds and the rest:
	
		vfields<-character(0);
		methods.not.virtual<-character(0);

		num.args<-function(fun){
			length(formals(fun))
		}

		method.to.field<-function(method){

			 method<-sub('^(get|set)(.*)','\\2',method);
			 tmp<-as.array(strsplit(method,"",fixed=TRUE))[[1]];
       tmp[1]<-tolower(tmp[1]);
       paste(tmp,collapse="");			

		}

		classify.method<-function(method,limit) {

				if( num.args( paste(method,".",class(this)[[1]],sep="") ) == limit){
                vfields<<-c(vfields,method.to.field(method));
            } else {
              methods.not.virtual<<-c(methods.not.virtual,method);
            }

		}

		for(method in methods){
			
				# Get methods for virtual fields have 2 aguments: "this" and "...".
				if(length(grep("^get",method,perl=TRUE)) == 1) {
					classify.method (method,limit=2)
				}
				# Set methods for virtual fields have 3 aguments: "this", "..." and "value".
				else if (length(grep("^set",method,perl=TRUE)) == 1) {
					classify.method (method,limit=3)
				} else {
					methods.not.virtual<-c(methods.not.virtual,method);
				}
		
		}
		vfields<-sort(unique(vfields));	

		lapply(methods.not.virtual,
			function(name) {
				tmp<-method.to.field(name);
				if (length(intersect(tmp,vfields)) > 0 ) {
					print(intersect(tmp,vfields));
					throw("Method classification inconsistency! Blaming ",paste(intersect(tmp,vfields),collapse=" "),". \n");
				}
			}
		);
		
		text<-paste(text,"Virtual fields (",length(vfields),"):\n",sep="");
		text<-pretty.print(vfields,text);
		text<-paste(text,"Methods implemented in ",class," (",length(methods.not.virtual),"):\n",sep="");
		text<-pretty.print(sort(methods.not.virtual),text);
		text<-paste(text,"\n",sep="");
		
		if(!quiet){ cat(text) }	

		invisible(text);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: getComments
##
###########################################################################/**
#
# @RdocMethod getComments
# 
# @title "Get the comments associated with an object" 
# 
# \description{ 
#	@get "title".
#
#	The comment field can contain any type of object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The value of the comment field.
# } 
# 
# \examples{
#	# create an object
#	o<-PSRoot()
#	# add some comments
#	setComments(o,"Random comment")
#	# get the comment 
#	getComments(o)
#	# get/set the comment via virtual fiels
#	o$comments<-"Second random comment"
#	o$comments
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
  "getComments",
  class="PSRoot",
  function(
    this,
    ...
  ){
			this$.comments;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: setComments
##
###########################################################################/**
#
# @RdocMethod setComments
# 
# @title "Set the comments associated with an object" 
# 
# \description{ 
#	@get "title".
#
#	The comment field can contain any type of object.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
#	\item{new_value}{An object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the comment field (invisible).
# } 
# 
# \examples{
#	# create an object
#	o<-PSRoot()
#	# add some comments
#	setComments(o,"Random comment")
#	# get the comment 
#	getComments(o)
#	# get/set the comment via virtual fiels
#	o$comments<-"Second random comment"
#	o$comments
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
  "setComments",
  class="PSRoot",
  function(
    this,
    new_value,
    ...
  ){
			this$.comments<-new_value;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: PSRoot$my.all.equal
##
###########################################################################/**
#
# @RdocMethod my.all.equal
# 
# @title "Test if two objects are nearly equal" 
# 
# \description{ 
#	@get "title".
#	
#	This method simply calls \code{all.equal.default} with the tolerance parameter set to 
#	\code{.Machine$double.eps ^ 0.5}. More useful as a static method.
#	
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{static}{A PSRoot object.} 
#	\item{target}{R object.}
#	\item{current}{Other R object, to be compared with target.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	TRUE or FALSE. 
# } 
# 
# \examples{
#	PSRoot$my.all.equal(0.0,0.0001)	
#	PSRoot$my.all.equal(0.0,0.000000001)	
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
  "my.all.equal",
  class="PSRoot",
  function(
    static,
    target,
    current,
    ...
  ){

		static;
		one<-target;
		two<-current;

		TOLERANCE<-.Machine$double.eps ^ 0.5;
		if(missing(one) | missing (two)){
			throw("Two objects are needed for comparison!\n");
		}
		else {
			one<-as.double(one);
			two<-as.double(two);
			return(isTRUE(all.equal(one,two, tolerance=TOLERANCE)));
		}
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: summary.PSRoot
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
#  	Returns a PSRootSummary object.
# }
#
# \examples{
#
#       # create an object
#       a<-PSRoot()
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
  class="PSRoot",
  function(
    object,
    ...
  ){
		this<-object;	
		# Adding the Comments field:
		if(length(this$.comments) > 0 ) {
		this$.summary$Comments<-paste(this$.comments, collapse=", ");
		}
		
		obj<-PSRootSummary(summary=this$.summary);
		this$.summary<-list();
		# Return a summary object:
		return(obj);

	},
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: is.na.PSRoot
##
###########################################################################/**
#
# @RdocMethod is.na
# 
# @title "Check if a PSRoot object is NA" 
# 
# \description{ 
#	@get "title".
#	PSRoot objects accanot be NA, so this method always returns FALSE.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A PSRoot object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	FALSE
# } 
# 
# \examples{
#	is.na(PSRoot());
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
  "is.na",
  class="PSRoot",
  function(
    x,
    ...
  ){
		
		# We don't want our objects to be NA-s!	
		return(FALSE);
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: checkConsistency;
##
###########################################################################/**
#
# @RdocMethod	checkConsistency
# 
# @title "Check object consistency"
# 
# \description{ 
#		@get "title".
#		The consisntency check is not implemented in plain PSRoot objects,
#		the method prints out a warning about that.
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
  class="PSRoot",
  function(
    this,
    ...
  ){
		
		warning("Consistency check is not implemented in class ",class(this)[[1]],"!\n");	
		return(invisible(TRUE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: globalConsistencyCheck
##
###########################################################################/**
#
# @RdocMethod globalConsistencyCheck
# 
# @title "Check the consistency of all objects inheriting form PSRoot in the current environment" 
# 
# \description{ 
#	@get "title".
#
#	This method searches for objects which inherit from PSRoot and calls \code{checkConsistency()} for all of them,
#	which can take a lots of time. Prints the results of the checks as text.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Returns invisible TRUE if all checks are successful.
# } 
# 
# \examples{
#	# create some objects
#	a<-NucleotideAlphabet()
#	s<-Site()
#	p<-Process()
#	# ask for a global consistency check
#	PSRoot$globalConsistencyCheck();
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
  "globalConsistencyCheck",
  class="PSRoot",
  function(
    ...
  ){
		
		for(name in ls(envir=.GlobalEnv)) {
				obj<-get(name,envir=.GlobalEnv);
				if (is.PSRoot(obj)) {
					cat("Checking ",name," ... ");	
					if( checkConsistency((obj)) ) {
							cat("OK\n");
					}
				}
		}
		return(invisible(TRUE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: plot.PSRoot
##
setMethodS3(
  "plot",
  class="PSRoot",
  function(
    ...
  ){
	
		cat("No plot method defined for this object!\n");	
		return(invisible(FALSE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: intersect.list.PSRoot
##
###########################################################################/**
#
# @RdocMethod intersect.list
# 
# @title "Utility method returning the intersection of two lists" 
# 
# \description{ 
#	@get "title".
#	Duplicated elements are not considered.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A PSRoot object.} 
#	\item{one}{A list of objects.}
#	\item{two}{A list of objects.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list containing the intersection.
# } 
# 
# \examples{
#	# create some lists
#	a<-list(1,2,3);
#	b<-c(a,list("a","b","c"))
#	# get the intersection of a and b
#	PSRoot$intersect.list(a,b)
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
  "intersect.list",
  class="PSRoot",
  function(
    this,
    one,
    two,
    ...
  ){

	if(!is.list(one)){
		throw("The first argument is not a list!\n");
	}
	if(!is.list(two)){
		throw("The second argument is not a list!\n");
	}

	one<-unique(one);
	two<-unique(two);
	intersect<-list();
        for (i in one){
        	for (j in two){
        		if(i == j) {
        	   		intersect<-c(intersect,list(i));
                	}
              }
        }
	return(intersect);
		
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: is.PSRoot.default
##
###########################################################################/**
#
# @RdocDefault is.PSRoot
# 
# @title "Check if an object inherits from PSRoot" 
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
#	# create some objects
#	o<-PSRoot()
#	a<-Alphabet()
#	x<-Object()
#	# check if they inherit form PSRoot
#	is.PSRoot(o)
#	is.PSRoot(a)
#	is.PSRoot(x)
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
  "is.PSRoot",
  class="default",
  function(
    this,
    ...
  ){

		if(!is.object(this)) {return(FALSE)}
		inherits(this,"PSRoot");
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

###########################################################################/**
# @RdocMethod clone
#
# @title "Clones an Object"
#
# \description{
#  Creates an identical copy of the object and returns a reference to the
#  new object.
#
#  This is a modified version of the \code{\link{clone.Object}} method from the
#  \code{\link{R.oo}} package.
# }
#
# @synopsis
#
# \arguments{
#   \item{this}{An object inheriting from PSRoot.}
#   \item{...}{Not used.}
# }
#
# \value{
#   A reference to the new object.
# }
#
# \examples{
#   o1 <- PSRoot()
#   o2 <- clone(o1)
#
#   print(equals(o1, o2))
# }
#
# \details{
#   Please note that no constructors are called during the creation of the
#   clone and neither is any static class code called.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
#*/###########################################################################
setMethodS3("clone", "PSRoot", function(this, ...) {

	## WARNING: This method might fail if the internal workings
	## of the R.oo package changes!

	# Copy the reference.
	clone <- this

  	# Create a new environment, i.e. a new Object.
	clone.env <- .Internal(new.env(FALSE,.Internal(parent.frame(1)),size=29L));
	attr(x=clone, which=".env") <- clone.env;

	# Copy all variables in the environment.
	this.env <- attr(this, ".env");

  	for (field in .Internal(ls(attr(this, ".env"), TRUE)) ) {

		value <- .Internal(get(field, this.env, "any", FALSE));
		# The following line must be included to do nothing, but the effect
		# is that it fools the lazy evaluation (?) to create a true copy of
		# the object. If this is not done, it won't be a true clone even if
		# the value is assigned to another environment. Example:
		#  b <- clone(a) 
		#  b$matrix[1,1] <- 2
		# will otherwise also change a$matrix[1,1]. /HB 021023
		attr(value, "R.oo::.clone.Object") <- NULL;
		assign(field, value, envir=clone.env);
		.Internal(assign(field, value, clone.env, FALSE));

 	}
	clone;
	
},
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);
