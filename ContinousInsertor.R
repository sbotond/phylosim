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
  "ContinousInsertor",
  function( 
		name="Anonymous",
		rate=NA,
		dist=NA,
		max.length=NA,
		... 
		)	{

		this<-GeneralInsertor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			...
		);
    this<-extend(
      this,
      "ContinousInsertor",
			.dist=NA,
			.max.length=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(dist)) {
			this$dist<-dist;
			STATIC<-FALSE;
		}
		
		if(!missing(max.length)) {
			this$.max.length<-max.length;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(...){
				if(!is.expression(this$.dist)){
					throw("\"dist\" is undefined, so cannot propose insertion length!\n");
				}
				else if(is.na(this$.max.length)){
					throw("\"maxLength\" is undefined, so cannot propose insertion length!\n");
				}
				tmp<-round(eval(this$.dist));
				while( tmp > this$.max.length | tmp < 1){  tmp<-round(eval(this$.dist)) };	
				return(tmp);
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
  class="ContinousInsertor",
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

        if (!is.na(this$maxLength)) {
          this$maxLength<-this$maxLength;
        }

        if (is.expression(this$dist)) {
          this$dist<-this$dist;
        }
        else if (!is.na(this$dist)){
          throw("Insertion length sampler expression is invalid!\n");
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
## Method: getDist
##	
setMethodS3(
	"getDist", 
	class="ContinousInsertor", 
	function(
		this,
		...
	){

		this$.dist;
		
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
	"setDist", 
	class="ContinousInsertor", 
	function(
		this,
		value,
		...
	){
		
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if(!is.expression(value)) {
			throw("The new value must be a valid expression!\n");
		} else {
			# Do a test sampling:
			tmp<-eval(value);
			if( length(tmp) != 1 ) {
				throw("The return value of the length sampler expression must be of length 1!\n");
			}
			if (!is.numeric(tmp)) {
				throw("The return value of the length sampler expression must be numeric!\n");
			}
			else {
				this$.dist<-value;
			}
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
	"getMaxLength", 
	class="ContinousInsertor", 
	function(
		this,
		...
	){

		this$.max.length;
		
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
	"setMaxLength", 
	class="ContinousInsertor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if (!is.numeric(value)) {
			throw("Value vector size should be numeric!\n");
		}
		else if( round(value) != value ) {
			throw("maxLength must be integer!\n");
		} else {
			this$.max.length<-value;
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
  class="ContinousInsertor",
  function(
    this,
		sample.size=NA,
    ...
  ){

		
		if( !is.numeric(this$maxLength) | !is.expression(this$dist) ){
				warning("Insertion length distribution is not defined properly! Nothing to plot here!\n");
				return();
		}
		size<-(this$maxLength * 5);
		if(!missing(sample.size)){
				if(!is.numeric(sample.size) | ( length(sample.size)) !=1 ) {
					throw("Sample size paramter must be a numeric vector of size 1!\n");
				} else {
					size<-round(sample.size);
				}
		}

			sample<-apply(as.array(0:size),1,this$.propose.by);
      plot.default(
				density(sample,from=0,to=this$maxLength),
        main=paste("Estimated insertion size density for:",this$id),
				sub=paste("Sample size:", size),
				type='l',
        xlab="Size",
        ylab="Density",
				col="red",
				lwd=1.5,
				xaxt="n"
      );
			axis(side=1, at=(0:this$maxLength), labels=(0:this$maxLength));

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
  class="ContinousInsertor",
  function(
    this,
    value,
    ...
  ){

    .addSummaryNameId(this);

    this$.summary$"Length sampling expression"<-deparse(this$dist);
    this$.summary$"Maximum insertion length"<-this$maxLength;
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

