##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
  "FieldDeletor",
  function( 
		name="Anonymous",
		type="geometric",
		length.param.1=NA,	# mostly "Lambda"
		length.param.2=NA,	
		tolerance.margin=0,	# minimum tolerance value used for scaling.
		... 
	)	{

			ALLOWED.TYPES=c("geometric","poisson","neg.binomial","compoisson");	# supported types

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

			# Extending as FieldDeletor:
    	this<-extend(
      	this,
      	"FieldDeletor",
				.type=type, 		# field model flavour
				.tolerance.margin=NA,	# minimum tolerance used for scaling
				.tolerance.max=NA,	# maximum tolerance obseved at first call of getEventAtSites
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
  class="FieldDeletor",
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
  class="FieldDeletor",
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
## Method: summary.FieldDeletor
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
  class="FieldDeletor",
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
setMethodS3(
  "getEventsAtSite",
  class="FieldDeletor",
  function(
    this,
    target.site,
    sloppy=TRUE,
    ...
  ){

    if(missing(target.site)) {
      throw("No target site provided!\n");
    } else if (!sloppy) {
      if(!is.Site(target.site)) {
        throw("Target site invalid!\n");
      }
      else if(!is.function(this$.propose.by)) {
        throw("proposeBy is not set, cannot propose deletion!\n");
      }
      else if (!is.function(this$.accept.by)){
        throw("acceptBy is not set, cannot generate deletion event deletion!\n");
      }
    } #/!sloppy

     # Complain if sequence has a zero length:
     if(target.site$.sequence$.length == 0) {
       throw("Sequence has zero length so there is nothing to delete! How did you get here anyway?\n");
     }

     # Clone the event template object:
     deletion.event<-clone(this$.event.template);
     # Set the target position passed in a temporary field:
 deletion.event$.position<-target.site$.position;
     # Set the target site:
     deletion.event$site<-target.site;
     # Set the target state (good for consistency):
     deletion.event$targetState<-getState(target.site);
     # Set event name:
     deletion.event$name<-"Deletion";
     # Set the generator process:
     deletion.event$process<-this;

		 # Calculate the field model specific scaling factor if it is not yet calculated:
		 if(is.na(this$.field.scaling.factor)){
				this$.field.scaling.factor<-.getScalingFactor(this,process=this,seq=target.site$.sequence);
		 }

     # Event rate is the product of the general rate, the field model scaling factor and the 
     # site specific rate multiplier:
     deletion.event$rate<-(this$rate * (getParameterAtSite(this,target.site,"rate.multiplier")$value) * this$.field.scaling.factor );

     # Set the handler for the deletion event:
     .setHandler(deletion.event, this$.handler.template);

    # Write protect the event object: 
    deletion.event$writeProtected<-TRUE;

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
setMethodS3(
  "getType",
  class="FieldDeletor",
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
setMethodS3(
  "setType",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

		throw("The type of the FieldDeletor process cannot be modified. Please set it by the constructor argument.");

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
setMethodS3(
  "getLengthParam1",
  class="FieldDeletor",
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
setMethodS3(
  "getLengthParam2",
  class="FieldDeletor",
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
setMethodS3(
  "setLengthParam1",
  class="FieldDeletor",
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
setMethodS3(
  "setLengthParam2",
  class="FieldDeletor",
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
setMethodS3(
  "getToleranceMargin",
  class="FieldDeletor",
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
setMethodS3(
  "setToleranceMargin",
  class="FieldDeletor",
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
  class="FieldDeletor",
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
## Method: setParameterAtSite.FieldDeletor
##
setMethodS3(
  "setParameterAtSite",
  class="FieldDeletor",
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


