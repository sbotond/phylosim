##	$Id: PSRoot.R,v 1.24 2009-04-30 09:51:11 sbotond Exp $
##
##	Class: 
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
setMethodS3(
	"stringLength", 
	class="default", 
	function(
		this,
		...
	){
		
		this<-as.character(this);	
		if (length(this) != 1){throw("This function can handle only vectors of length 1!")};

		return(length(strsplit(this,split="")[[1]]));	
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
setMethodS3(
	"stringLengthVector", 
	class="character", 
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
## Method: listMethods
##
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
## Method: listMethods
##
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
			 tmp<-as.array(strsplit(method,""))[[1]];
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
## Method: PSRoot$all.equal
##
setMethodS3(
  "all.equal",
  class="PSRoot",
  function(
    this,
		one,
		two,
    ...
  ){

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
setMethodS3(
  "summary",
  class="PSRoot",
  function(
    this,
		verbose=FALSE,
    ...
  ){
		
		# Adding the Comments field:
		if(length(this$.comments) > 0 ) {
		this$.summary$Comments<-paste(this$.comments, collapse=", ");
		}
		# Adding ll() output in verbose mode:
		if(verbose == TRUE ) {

			tmp<-ll(this,quiet=TRUE);
			tmp<-strsplit(tmp,split="\n",extended=TRUE)[[1]];
			tmp<-paste(tmp, collapse="\n  ");
		  this$.summary$"\nObject information"<-tmp;

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
## Constructor: summary.PSRoot
##
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
## Method: print.summary.PSRoot
##
setMethodS3(
  "print",
  class="PSRootSummary",
  function(
    this,
    ...
  ){
	
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
## Method: is.na.PSRoot
##
setMethodS3(
  "is.na",
  class="PSRoot",
  function(
    this,
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
## Method: is.PSRoot.default
##
setMethodS3(
  "is.PSRoot",
  class="default",
  function(
    this,
    ...
  ){

		# FIXME - some safe speedup here!	
		if(!is.object(this)) {return(FALSE)}
		inherits(this,"PSRoot");
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: checkConsistency;
##
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
	static=TRUE,
  conflict="warning"
);

##
## Method: globalConsistencyCheck;
##
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
	static=TRUE,
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
	static=TRUE,
  conflict="warning"
);
