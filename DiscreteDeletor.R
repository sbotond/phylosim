##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
  "DiscreteDeletor",
  function( 
		name="Anonymous",
		rate=NA,
		sizes=NA,
		probs=NA,
		... 
		)	{

		this<-GeneralDeletor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			...
		);
    this<-extend(
      this,
      "DiscreteDeletor",
			.sizes=NA,
			.probs=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(sizes)) {
			this$sizes<-sizes;
			STATIC<-FALSE;
		}
		
		if(!missing(probs)) {
			this$probs<-probs;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(process=NA,...){
			if( !is.numeric(process$.sizes) | !is.numeric(process$.probs) ){
				throw("Cannot propose deletion length because the length distribution is not defined properly!\n");
			}
			if(length(process$.sizes) == 1){
				return(process$.sizes[[1]]);
			} else {
				return(sample(x=process$.sizes,size=1,replace=FALSE,prob=process$.probs));
			}
		}

    return(this);
  },
  enforceRCC=TRUE
);

##
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="DiscreteDeletor",
  function(
    this,
    length,
    ...
  ){

          wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
				
        if (is.numeric(this$sizes)) {
          this$sizes<-this$sizes;
        } 
				else if (!is.na(this$sizes)){
					throw("Deletion size vector is invalid!\n");
				}

        if (is.numeric(this$probs)) {
          this$probs<-this$probs;
        }
				else if (!is.na(this$probs)){
					throw("Deletion size probability vector is invalid!\n");
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
## Method: getSizes
##	
setMethodS3(
	"getSizes", 
	class="DiscreteDeletor", 
	function(
		this,
		...
	){

		this$.sizes;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSizes
##	
setMethodS3(
	"setSizes", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} else if (!is.numeric(value)) {
			throw("The new value must be numeric vector!\n");
		} else {
			if(length(value) == 0 ) {
				warning("Deletion size vector has zero length!\n");
			}
			this$.sizes<-round(value);	
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProbs
##	
setMethodS3(
	"getProbs", 
	class="DiscreteDeletor", 
	function(
		this,
		...
	){

		this$.probs;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProbs
##	
setMethodS3(
	"setProbs", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if(!is.numeric(this$.sizes)) {
			throw("Cannot set probabilities because indel size vector is not defined!\n");
		}
		else if (!is.numeric(value)) {
			throw("The new value must be a numeric vector!\n");
		}
		else if(length(value) != length(this$.sizes)) {
			throw("The length of the probabilities vector must be the same as the length of the deletion sizes vector");	
		} 
		else if( length(value[value < 0 ]) != 0 ) {
			throw("The elements of the probability vector must not be negative!\n");
		}
		else {
  		if(!isTRUE(all.equal(sum(value),1.0))){
        value<-value/sum(value);
        warning("The provided values were rescaled in order to sum to one!\n");
    	}
			this$.probs<-value;
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
setMethodS3(
	"summary", 
	class="DiscreteDeletor", 
	function(
		object,
		...
	){

		this<-object;
		.addSummaryNameId(this);

		expected.length<-NA;	
		sd<-NA;
		if( is.numeric(this$sizes) & is.numeric(this$probs)) {
		expected.length<-weighted.mean(this$sizes, this$probs);		
		# FIXME - check back on this
		sd<-sqrt(sum( (this$sizes - expected.length)^2 * this$probs ));
		}

		this$.summary$"Expected deletion length"<-expected.length;
		this$.summary$"Standard deviation of deletion length"<-format(sd);
		NextMethod();
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
setMethodS3(
	"plot", 
	class="DiscreteDeletor", 
	function(
		x,
		...
	){
			this<-x;
			if( !is.numeric(this$sizes) | !is.numeric(this$probs) ){
				warning("Deletion length distribution is not defined properly! Nothing to plot here!\n");
				return();
			}
			plot.default(	
				x=this$sizes,
				y=this$probs,
				col=c("blue"),
				lwd=2,
				type="h",
				main=paste("Deletion size distribution for:",this$id), 
				xlab="Size",
				ylab="Probability",
				ylim=c(0,1),
				xaxt="n"
			);
			axis(side=1, at=this$sizes, labels=this$sizes);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

