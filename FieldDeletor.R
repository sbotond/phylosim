##
##	Class: FieldDeletor
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
  "FieldDeletor",
  function( 
		name="Anonymous",
		type="geometric",
		length.param.1=NA,	# "Lambda"
		length.param.2=NA,
		tolerance.margin=0,
		... 
		)	{

		# Creating a GeneralDeletor Process.
		this<-GeneralDeletor(
			...
		);

		# Check if the type is valid:
		if(length(intersect(c("geometric","poisson","logarithmic","neg.binomial","compoisson"),type)) != 1){
			throw("The specified field model type is invalid!\n");
		}

		# Load the compoisson package if the type is Conway-Maxwell-Poisson:
		if(type == "compoisson" & (!require(compoisson))){
			throw("The compoisson package cannot be loaded, so cannot use the Conway-Maxwell-Poisson density for sampling deletion lengths!\n");
		}

		# Extending as FieldDeletor:
    this<-extend(
      this,
      "FieldDeletor",
			.type=type,
			.tolerance.margin=NA,
			.tolerance.max=NA,
			.field.scaling.factor=NA,
			.length.param.1=NA,
			.length.param.2=NA,
			.q.max=NA
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

	  this$proposeBy<-function(process,seq,pos){

					# Check the length parameters:
					if(is.na(this$.length.param.1)){
						throw("Length parameter 1 is NA! Cannot propose length!\n");
					}
				
					# Type specific length sampling expressions:

					# Geometric:	
					if(this$.type == "geometric"){
						express<-expression( rgeom(1,prob=( 1 - ( this$.length.param.1 * this$.tolerance.max) ) ) );
					}
				
					# Poisson+1:	
					if(this$.type == "poisson"){
						express<-expression( 1 + rpois(1,lambda=(this$.length.param.1 * this$.tolerance.max) ) );
					}
					
					# Negative Binomial + 1:
					if(this$.type == "neg.binomial"){
						express<-expression(1 + rnbinom(1,this$.length.param.2,prob=( 1 - ( this$.length.param.1 * this$.tolerance.max))) );
					}
					
					# Conway-Maxwell-Poisson + 1:
					if(this$.type == "compoisson"){
						express<-expression(1 + rcom(1,lambda=( this$.length.param.1 * this$.tolerance.max), nu = this$.length.param.2));
					}
					
					return( round( eval(express) ) );

			} # /proposeBy

		
			# Set the function performing the accept/reject step:
			this$acceptBy<-function(process,sequence,range){

				# Get the deletion tolerance parameters from the proposed range:
				deletion.tolerance<-c();
				
				for(site in seq$.sites[range]){

				if(isAttached(site, process)){

									deletion.tolerance<-c(deletion.tolerance, getParameterAtSite(process, site, id="deletion.tolerance")$value);

							} else {

								# Reject the proposed deletion if that contains sites which are not attached to the process.
								# This will create an edge effect of course! 
								return(FALSE);

							}
					} # for site

				# Calculate the acceptance probability:
				accept.prob<-( prod(deletion.tolerance) / this$.tolerance.max);

        # Accept/reject:
        return ( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
	
			} # /acceptBy
			

    return(this);

  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
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
     # Set the genrator process:
     deletion.event$process<-this;

		 # Calculate the field model specific scaling factor if it is not yet calculated:
		 if(is.na(this$.field.scaling.factor)){
				this$.field.scaling.factor<-.getScaleFactor(this,process=this,seq=target.site$.sequence);
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
		
		d<-this$.tolerance.max;
		if(this$.tolerance.margin > d){
			d<-this$.tolerance.margin;
		}

		# The type specific rate scaling factors:		
		exp<-expression();

		# Geometric:
		if(this$.type=="geometric"){
			exp<-expression(d * (1 - this$.length.param.1) / (1 - (d * this$.length.param.1)) );
		}
	
		# Poisson+1:	
		if(this$.type=="poisson"){
			exp<-expression(d * exp( - ( this$.length.param.1 * (1 - d ) ) ) );
		}

		# Negative Binomial + 1:
		if(this$.type=="neg.binomial"){
			exp<-expression( d * ( ( (1 - this$.length.param.1) / (1 - (d * this$.length.param.1))) ^ this$.length.param.2) );
		}
	
		# Conway-Maxwell-Poisson	+ 1:
		if(this$.type=="compoisson"){
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

