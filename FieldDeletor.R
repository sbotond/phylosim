##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass FastFieldDeletor
# 
# @title "The FastFieldDeletor class"
# 
# \description{ 
#	The \code{\link{DiscreteDeletor}} and \code{\link{ContinuousDeletor}} processes
#	generate deletion event objects with rates determined by the general rate of the 
#	process and the "rate.multiplier" parameter. The probability of rejecting an event
#	is determined by the product of the "deletion.tolerance" parameters from the affected sites.
#	If the majority of the sites have low deletion tolerance most of the events are rejected, which
#	slows down the simulation without performing much events. 
#
#	The \code{FastFieldDeletor} process scales down the rate and length distribution of the proposed 
#	events based on the highest insertion tolerance value observed in the root sequence 
#	(see the package vignette for details), thus making the simulation more efficient.
#
#	The available length distributions are (see also the package vignette):
#	\itemize{
#		\item Geometric (default) - \code{lengthParam1} is \emph{Lambda}.
#		\item Poisson+1 - \code{lengthParam1} is \emph{Lambda}.
#		\item Conway-Maxwell Poisson+1 - \code{lengthParam1} is \emph{Lambda}, \code{lengthParam2} is \emph{nu}
#		\item Negative Binomial+1 - \code{lengthParam1} is \emph{Lambda}, \code{lengthParam2} is \emph{r}
# 	}
#	
#	Insertion proceses can insert sites with deletion tolerance higher than the largest
#	deletion tolerance observed in the root sequence. The user can specify the largest expected 
#	tolerance value through the \code{toleranceMargin} virtual field. The process is then scaled by
#	max(initial_highest_tolerance, \code{toleranceMargin}).
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
# 	\item{type}{Type of the process (see above).}
# 	\item{length.param.1}{Object name.}
# 	\item{length.param.2}{Object name.}
# 	\item{tolerance.margin}{Object name.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a FastFieldDeletor object, default (geometric) type
#	# length.param.1 is "lambda"
#	p<-FastFieldDeletor(rate=1,length.param.1=0.9, tolerance.margin=0.8)
#	# get type
#	p$type
#	# get object summary
#	summary(p)
#	# set/get lambda	
#	p$lengthParam1<-0.8
#	p$lengthParam1
#	# set/get tolerance margin
#	p$toleranceMargin<-0.5
#	p$toleranceMargin
#	# create a nucleotide sequence, attach process
#	s<-NucleotideSequence(length=30,processes=list(list(p)))
#	# set state pattern
#	s$states<-c("A","A","T","T","G","G","C","C")
#	# sample deletion tolerances
#	setDeletionTolerance(s,p,sample(seq(from=0,to=0.8,by=0.01),30,replace=TRUE))
#	# plot deletion tolerance
#	plotParametersAtSites(s,p,"deletion.tolerance")
#	# simulate
#	sim<-PhyloSim(root.seq=s, phylo=rcoal(2))
#	Simulate(sim)
#	# show alignment
#	sim$alignment
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
  "FastFieldDeletor",
  function( 
		name="Anonymous",
		type="geometric",
		length.param.1=NA,	# mostly "Lambda"
		length.param.2=NA,	
		tolerance.margin=0,	# minimum tolerance value used for scaling.
		... 
	)	{

			# supported types
			ALLOWED.TYPES=c("geometric","poisson","neg.binomial","compoisson");

			# Creating a GeneralDeletor Process.
			this<-GeneralDeletor(
				...
			);

			# Check if the specified type is valid:
			if(length(intersect(ALLOWED.TYPES,type)) != 1){
				throw("The specified field model type is invalid!\n");
			}

			# Load the compoisson package if the type is Conway-Maxwell Poisson:
			if(type == "compoisson"){
				if(!require(compoisson)){
					throw("The compoisson package cannot be loaded, so cannot use the Conway-Maxwell Poisson density for sampling deletion lengths!\n");
				}
			}

			# Extending as FastFieldDeletor:
    	this<-extend(
      	this,
      	"FastFieldDeletor",
				.type=type, 		# field model flavour
				.tolerance.margin=NA,	# minimum tolerance used for scaling
				.tolerance.max=NA,	# maximum tolerance obseved at first call of 
							# getEventAtSites
				.d=NA,			 # is max(.tolerance.max, .tolerance.margin)
				.field.scaling.factor=NA,# the precalculated scaling factor
				.length.param.1=NA,	 # mostly "Lambda"
				.length.param.2=NA,	 
				.ALLOWED.TYPES=ALLOWED.TYPES # supported types
    	);

			# Set length parameter 1 if not missing:
			if(!missing(length.param.1)){
				this$lengthParam1<-length.param.1;
			}
		
			# Set length parameter 2 if not missing:
			if(!missing(length.param.2)){
				this$lengthParam2<-length.param.2;
			}

			# Set tolerance margin:
			setToleranceMargin(this, tolerance.margin);

			# Using virtual field to clear Id cache:
			this$name<-name;

			# Set the function proposing deletion lengths:
	  	this$proposeBy<-function(process=NA,seq=NA,pos=NA){

					# Check the length parameters:
					.checkLengthParams(this);				

					# Check if this$.d is defined:
					if(is.na(this$.d)){
						throw("thid$.d is NA! This shouldn't happen!");;
					}
					d<-this$.d;
	
					# Type specific length sampling expressions:

					# Geometric + 1:	
					if(this$.type == "geometric"){
						express<-expression(1 + rgeom(1,prob=( 1 - ( this$.length.param.1 * d) ) ) );
					}
				
					# Poisson+1:	
					else if(this$.type == "poisson"){
						express<-expression( 1 + rpois(1,lambda=(this$.length.param.1 * d) ) );
					}
					
					# Negative Binomial + 1:
					else if(this$.type == "neg.binomial"){
						express<-expression(1 + rnbinom(1,this$.length.param.2,prob=( 1 - ( this$.length.param.1 * d))) );
					}
					
					# Conway-Maxwell Poisson + 1:
					else if(this$.type == "compoisson"){
						express<-expression(1 + rcom(1,lambda=( this$.length.param.1 * d), nu = this$.length.param.2));
					}
					
					return( round( eval(express) ) );

			} # /proposeBy
		
			# Set the function performing the accept/reject step:
			this$acceptBy<-function(process=NA,sequence=NA,range=NA){

        del.tol<-c();
        for(site in sequence$.sites[range]){
            # Reject if the range contains a site which is not attached to 
            # the process:
            if(!isAttached(site, process)){
              return(FALSE);
            }
            del.tol<-c(del.tol, getParameterAtSite(process, site, "deletion.tolerance")$value);
        }

				# Calculate acceptance probability:			
        accept.prob<-( prod(as.numeric(del.tol)) / this$.d );

        # Accept/reject:
        return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
    }

    return(this);

  },
  enforceRCC=TRUE
);

##	
## Method: .checkLengthParams
##	
setMethodS3(
  ".checkLengthParams",
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

					# Check length parameter 1:
					if(is.na(this$.length.param.1)){
						throw("Length parameter 1 is NA! Cannot generate events or propose lengths!\n");
					}
					# Check length parameter 2:
					if(length(intersect(c("neg.binomial","compoisson"),this$.type)) != 0) {
						if(is.na(this$.length.param.2)){
							throw("Length parameter 1 is NA! Cannot generate events or propose lengths!\n");
						}
					}

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
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
				
				# Check if the type is valid:
				if(length(intersect(this$.ALLOWED.TYPES, this$.type)) != 1){
					throw("The specified field model type is invalid!\n");
				}
				if((!is.numeric(this$.tolerance.margin) & !is.na(this$.tolerance.margin))){
					throw("Tolerance margin is invalid!\n");
				}
				if((!is.numeric(this$.tolerance.max) & !is.na(this$.tolerance.max))){
					throw(".tolerance.max is invalid!\n");
				}
				if((!is.numeric(this$.d) & !is.na(this$.d))){
					throw(".d is invalid!\n");
				} else if(!is.na(this$.tolerance.margin) & !is.na(this$.tolerance.max)) {
						if(this$.d != max(this$.tolerance.margin, this$.tolerance.max)){
							throw(".d is inconsistent!\n");
						}
				}
				if((!is.numeric(this$.field.scaling.factor) & !is.na(this$.field.scaling.factor))){
					throw(".field.scaling.factor is invalid!\n");
				}
				if((!is.numeric(this$.length.param.1) & !is.na(this$.length.param.1))){
					throw("Length parameter 1 is invalid!\n");
				}
				if((!is.numeric(this$.length.param.2) & !is.na(this$.length.param.2))){
					throw("Length parameter 2 is invalid!\n");
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
## Method: summary.FastFieldDeletor
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
#       a<-FastFieldDeletor()
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
  class="FastFieldDeletor",
  function(
    object,
    ...
  ){

		this<-object;
		.addSummaryNameId(this);
		this$.summary$"Type"<-this$.type;
		this$.summary$"Tolerance margin"<-this$.tolerance.margin;
		this$.summary$"Length parameter 1"<-this$.length.param.1;
		this$.summary$"Length parameter 2"<-this$.length.param.2;
		this$.summary$"Scaling factor"<-this$.field.scaling.factor;
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
# @title "Generate a deletion event object given the state of the target site" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A FastFieldDeletor object.} 
#       \item{target.site}{A Site object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of Event objects.
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	GeneralDeletor getEventsAtSite.GeneralDeletor
# } 
# 
#*/###########################################################################
setMethodS3(
  "getEventsAtSite",
  class="FastFieldDeletor",
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
     # Set the generator process:
     deletion.event$.process<-this;

		 # Calculate the field model specific scaling factor if it is not yet calculated:
		 if(is.na(this$.field.scaling.factor)){
				this$.field.scaling.factor<-.getScalingFactor(this,process=this,seq=target.site$.sequence);
		 }

     # Event rate is the product of the general rate, the field model scaling factor and the 
     # site specific rate multiplier:
     deletion.event$.rate<-(this$.rate * (target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value) * this$.field.scaling.factor);

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
## Method: getType
##	
###########################################################################/**
#
# @RdocMethod getType
# 
# @title "Get the type of a FastFieldDeletor object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a FastFieldDeletor, default type (geometric)
#	p<-FastFieldDeletor()
#	# get type
#	getType(p)
#	# create a FastFieldDeletor, poisson type
#	p<-FastFieldDeletor(type="poisson")
#	p$type
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
  "getType",
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

		this$.type;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setType
##	
###########################################################################/**
#
# @RdocMethod setType
#
# @title "Forbidden action: setting the type of a FastFieldDeletor object"
#
# \description{
#       @get "title".
#	
#	The type can be set only through the \code{type} constructor argument.
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
  "setType",
  class="FastFieldDeletor",
  function(
    this,
    value,
    ...
  ){

		throw("The type of the FastFieldDeletor process cannot be modified. Please set it by the constructor argument.");

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getLengthParam1
##	
###########################################################################/**
#
# @RdocMethod getLengthParam1
# 
# @title "Get the first length parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get the first length parameter
#	setLengthParam1(p,0.5)
#	getLengthParam1(p)
#	# set/get the first length parameter via virtual field
#	p$lengthParam1<-0.2
#	p$lengthParam1
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
  "getLengthParam1",
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

		this$.length.param.1;
;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getLengthParam2
##	
###########################################################################/**
#
# @RdocMethod getLengthParam2
# 
# @title "Get the second length parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get the second length parameter
#	setLengthParam2(p,0.5)
#	getLengthParam2(p)
#	# set/get the second length parameter via virtual field
#	p$lengthParam2<-0.2
#	p$lengthParam2
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
  "getLengthParam2",
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

		this$.length.param.2;
;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLengthParam1
##	
###########################################################################/**
#
# @RdocMethod setLengthParam1
# 
# @title "Set the first length parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{value}{A numeric vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible).
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get the first length parameter
#	setLengthParam1(p,0.5)
#	getLengthParam1(p)
#	# set/get the first length parameter via virtual field
#	p$lengthParam1<-0.2
#	p$lengthParam1
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
  "setLengthParam1",
  class="FastFieldDeletor",
  function(
    this,
		value,
    ...
  ){

		if(missing(value)){
			throw("No new length parameter value specified!\n");
		}	
		else if ((!is.numeric(value)) | (length(value) != 1 ) ) {
			throw("The new value must be a numeric vector of length 1!\n");
		}
		else {
			# First set the scaling factor to NA to force the recalculation:
			this$.field.scaling.factor<-NA;
			this$.length.param.1<-value;
		}
		return(invisible(value));


  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLengthParam2
##	
###########################################################################/**
#
# @RdocMethod setLengthParam2
# 
# @title "Set the second length parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{value}{A numeric vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible).
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get the second length parameter
#	setLengthParam2(p,0.5)
#	getLengthParam2(p)
#	# set/get the second length parameter via virtual field
#	p$lengthParam2<-0.2
#	p$lengthParam2
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
  "setLengthParam2",
  class="FastFieldDeletor",
  function(
    this,
    value,
    ...
  ){

		if(missing(value)){
			throw("No new length parameter value specified!\n");
		}	
		else if ((!is.numeric(value)) | (length(value) != 1 ) ) {
			throw("The new value must be a numeric vector of length 1!\n");
		}
		else {
			# First set the scaling factor to NA to force the recalculation:
			this$.field.scaling.factor<-NA;
			this$.length.param.2<-value;
		}
		return(invisible(value));


  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getToleranceMargin
##	
###########################################################################/**
#
# @RdocMethod getToleranceMargin
# 
# @title "Get the tolerance margin" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get tolerance margin
#	setToleranceMargin(p,0.8)
#	getToleranceMargin(p)
#	# set/get tolerance margin via virtual field
#	p$toleranceMargin<-0.75
#	p$toleranceMargin
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
  "getToleranceMargin",
  class="FastFieldDeletor",
  function(
    this,
    ...
  ){

		this$.tolerance.margin;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setToleranceMargin
##	
###########################################################################/**
#
# @RdocMethod setToleranceMargin
# 
# @title "Set the tolerance margin" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A FastFieldDeletor object.} 
# 	\item{value}{A numeric vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	value (invisible).
# } 
# 
# \examples{
#	# create a geometric FastFieldDeletor
#	p<-FastFieldDeletor()
#	# set/get tolerance margin
#	setToleranceMargin(p,0.8)
#	getToleranceMargin(p)
#	# set/get tolerance margin via virtual field
#	p$toleranceMargin<-0.75
#	p$toleranceMargin
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
  "setToleranceMargin",
  class="FastFieldDeletor",
  function(
    this,
		value,
    ...
  ){

		if(missing(value)){
			throw("No new length parameter value specified!\n");
		}	
		else if ((!is.numeric(value)) | (length(value) != 1 ) ) {
			throw("The new value must be a numeric vector of length 1!\n");
		}
		else {
			# First set the scaling factor to NA to force the recalculation:
			this$.field.scaling.factor<-NA;
			this$.tolerance.margin<-value;
		}
		return(invisible(value));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .getScalingFactor
##	
setMethodS3(
  ".getScalingFactor",
  class="FastFieldDeletor",
  function(
    		this,
		process,
		seq,
    		...
  ){

		# Check if the length parameters needed for rate scaling are present:
		.checkLengthParams(this);				

		if(is.na(this$.tolerance.max)){

			# Get all deletion tolerance parameters for this process:					
			deletion.tolerance<-c();

			for(site in seq$.sites){
					if(isAttached(site, process)){
						deletion.tolerance<-c(deletion.tolerance, getParameterAtSite(process, site, id="deletion.tolerance")$value);
						}
					} # for site

			# Get the maximal tolerance value:
			this$.tolerance.max<-max(deletion.tolerance);

		}
		
		d<-max(this$.tolerance.max, this$.tolerance.margin);
		this$.d<-d;

		# The type specific rate scaling factors:		
		exp<-expression();

		# Geometric:
		if(this$.type=="geometric"){
			exp<-expression(d * (1 - this$.length.param.1) / (1 - (d * this$.length.param.1)) );
		}
	
		# Poisson+1:	
		else if(this$.type=="poisson"){
			exp<-expression(d * exp( - ( this$.length.param.1 * (1 - d ) ) ) );
		}

		# Negative Binomial + 1:
		else if(this$.type=="neg.binomial"){
			exp<-expression( d * ( ( (1 - this$.length.param.1) / (1 - (d * this$.length.param.1))) ^ this$.length.param.2) );
		}
	
		# Conway-Maxwell Poisson	+ 1:
		else if(this$.type=="compoisson"){
			exp<-expression( d * (com.compute.z(lambda=this$.length.param.1,nu=this$.length.param.2 ) / com.compute.z(lambda=(d * this$.length.param.1),nu=this$.length.param.2 )) );
		}
			
		return(eval(exp));	

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setParameterAtSite.FastFieldDeletor
##
setMethodS3(
  "setParameterAtSite",
  class="FastFieldDeletor",
  function(
    this,
    site,
    id,
    value,
    ...
  ){
			if(value < 0 | value > 1)	{
				throw("The field deletion model accepts deletion tolerance only from the [0,1] interval!\n");
			}
			NextMethod();
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


