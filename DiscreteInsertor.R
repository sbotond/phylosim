##	$Id: DiscreteInsertor.R,v 1.4 2009-04-27 17:38:09 sbotond Exp $
##
##	Class: DsiscreteInsertor
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
  "DiscreteInsertor",
  function( 
		name="Anonymous",
		rate=NA,
		sizes=NA,
		probs=NA,
		... 
		)	{

		this<-GeneralInsertor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			 generate.by=NA,
			 ...
		);
    this<-extend(
      this,
      "DiscreteInsertor",
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

		this$proposeBy<-function(this=NA,...){
			 if( !is.numeric(this$.sizes) | !is.numeric(this$.probs) ){
        throw("Cannot propose insert length because the length distribution is not defined properly!\n");
      }
			if(length(this$.sizes) == 1){
				return(this$.sizes[[1]]);
			} else {
				return(sample(x=this$.sizes,size=1,replace=FALSE,prob=this$.probs));
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
  class="DiscreteInsertor",
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
          throw("Insertion size vector is invalid!\n");
        }

        if (is.numeric(this$probs)) {
          this$probs<-this$probs;
        }
        else if (!is.na(this$probs)){
          throw("Insertion size probability vector is invalid!\n");
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
	class="DiscreteInsertor", 
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
	class="DiscreteInsertor", 
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
	class="DiscreteInsertor", 
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
	class="DiscreteInsertor", 
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
			throw("Cannot set probabilities because insert sizes vector is not defined!\n");
		}
		else if (!is.numeric(value)) {
			throw("The new value must be a numeric vector!\n");
		}
		else if(length(value) != length(this$.sizes)) {
			throw("The length of the probabilities vector must be the same as the length of the insertion sizes vector");	
		} 
		else if( length(value[value < 0 ]) != 0 ) {
			throw("The elements of the probability vector must not be negative!\n");
		}
		else {
			if(!isTRUE(all.equal(sum(value),1.0))){
				value<-(value/sum(value));	
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
## Method: plot
##	
setMethodS3(
	"plot", 
	class="DiscreteInsertor", 
	function(
		this,
		value,
		...
	){

			if( !is.numeric(this$sizes) | !is.numeric(this$probs) ){
				warning("Insertion length distribution is not defined properly! Nothing to plot here!\n");
				return();
			}
			plot.default(	
				x=this$sizes,
				y=this$probs,
				col=c("red"),
				lwd=2,
				type="h",
				main=paste("Insertion size distribution for:",this$id), 
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

##
## Method: summary
##
setMethodS3(
  "summary",
  class="DiscreteInsertor",
  function(
    this,
    value,
    ...
  ){

    .addSummaryNameId(this);

    expected.length<-NA;
    sd<-NA;
    if( is.numeric(this$sizes) & is.numeric(this$probs)) {
    expected.length<-weighted.mean(this$sizes, this$probs);
    # FIXME - check back on this
    sd<-sqrt(sum( (this$sizes - expected.length)^2 * this$probs ));
    }

    this$.summary$"Expected insertion length"<-expected.length;
    this$.summary$"Standard deviation of insertion length"<-format(sd);
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

