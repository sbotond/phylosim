##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass ContinuousInsertor
# 
# @title "The ContinuousInsertor class"
# 
# \description{ 
#       This class implements a process which performs insertions with
#       lengths sampled from a user-specified R expression returning a 
#       numeric value.
#       See \code{GeneralInsertor} for the how the insertion process
#	works.
#
#       @classhierarchy
# }
#       
# @synopsis
#       
# \arguments{
#       \item{name}{The name of the object.}
#       \item{rate}{The general rate.}
#       \item{dist}{The length sampling expression.}
#       \item{max.length}{Maximum event length.}
#       \item{...}{Additional arguments.}
#       }
# 
# \section{Fields and Methods}{ 
#       @allmethods
# }
# 
# \examples{ 
#       # create a ContinuousInsertor process
#       o<-ContinuousInsertor(
#               name="Conty",
#               rate=0.1,
#               dist=expression(1),
#               max.length=2
#       )
#	# set template sequence
#	o$templateSeq<-NucleotideSequence(string="CAC")
#       # get object summary
#       summary(o)
#       # set/get length sampling expression
#       o$dist<-expression(rnorm(1,mean=3,sd=3))
#       o$dist
#       # set/get maximum event length
#       o$maxLength<-4
#       o$maxLength
#       # plot length density
#       plot(o)
#       
#       # The following code illustrates how to use
#       # a ContinuousInsertor process in a simulation
#       
#       # create a sequence object, attach process o
#       s<-NucleotideSequence(string="AAAAAAAAAAGGGGAAAAAAAAAA",processes=list(list(o)))
#       # set the insertion tolerance to zero in range 11:15
#       # creating a region rejecting all insertions
#       setInsertionTolerance(s,o,0,11:15)       
#       # get insertion tolerances
#       getInsertionTolerance(s,o)
#       # create a simulation object
#       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#       # simulate
#       Simulate(sim)
#       # print resulting alignment
#       sim$alignment
# }
# 
# @author
#
# \seealso{ 
#       GeneralInsertor DiscreteInsertor GeneralInDel
# }
# 
#*/###########################################################################
setConstructorS3(
  "ContinuousInsertor",
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
      "ContinuousInsertor",
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

		this$proposeBy<-function(process=NA,...){
			if(!exists(x="PSIM_FAST")){
				if(!is.expression(process$.dist)){
					throw("\"dist\" is undefined, so cannot propose insertion length!\n");
				}
				else if(is.na(process$.max.length)){
					throw("\"maxLength\" is undefined, so cannot propose insertion length!\n");
				}
			}
				tmp<-round(eval(process$.dist));
				while( tmp > process$.max.length | tmp < 1){  tmp<-round(eval(process$.dist)) };	
				return(tmp);
		}

    return(this);
  },
  enforceRCC=TRUE
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
  class="ContinuousInsertor",
  function(
    this,
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
###########################################################################/**
#
# @RdocMethod getDist
# 
# @title "Get the length sampling expression" 
# 
# \description{ 
#	@get "title".
#
#	The length sampling expression can be any valid R expression returning
#	a numeric vector of length one. The value returned by the expression will be 
#	rounded.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A ContinuousInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An R expression object.
# } 
# 
# \examples{
#	# create object
#	o<-ContinuousInsertor(rate=1)
#	# set/get length sampling expression
#	setDist(o, expression(rnorm(1,mean=3, sd=2)))
#	getDist(o)
#	# set/get length sampling expression via virtual field
#	o$dist<-expression(rnorm(1,mean=6,sd=3))
#	o$dist
#	# set maxLength
#	o$maxLength<-10
#	# propose a length
#	proposeLength(o)
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
	"getDist", 
	class="ContinuousInsertor", 
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
## Method: setDist
##	
###########################################################################/**
#
# @RdocMethod setDist
# 
# @title "Set the length sampling expression" 
# 
# \description{ 
#	@get "title".
#
#	The length sampling expression can be any valid R expression returning
#	a numeric vector of length one. The value returned by the expression will be 
#	rounded.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A ContinuousInsertor object.} 
# 	\item{value}{An R expression.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An R expression object.
# } 
# 
# \examples{
#	# create object
#	o<-ContinuousInsertor(rate=1)
#	# set/get length sampling expression
#	setDist(o, expression(rnorm(1,mean=3, sd=2)))
#	getDist(o)
#	# set/get length sampling expression via virtual field
#	o$dist<-expression(rnorm(1,mean=6,sd=3))
#	o$dist
#	# set maxLength
#	o$maxLength<-10
#	# propose a length
#	proposeLength(o)
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
	"setDist", 
	class="ContinuousInsertor", 
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
## Method: getMaxLength
##	
###########################################################################/**
#
# @RdocMethod getMaxLength
# 
# @title "Get the maximum length" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A ContinuousInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create object
#	o<-ContinuousInsertor(rate=1)
#	# set length sampling expression via virtual field
#	o$dist<-expression(rnorm(1,mean=6,sd=3))
#	# set/get maxLength
#	setMaxLength(o, 3)
#	getMaxLength(o)
#	# set/get maxLength via virtual field
#	o$maxLength<-10
#	o$maxLength
#	# propose a length
#	proposeLength(o)
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
	"getMaxLength", 
	class="ContinuousInsertor", 
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
## Method: setMaxLength
##	
###########################################################################/**
#
# @RdocMethod setMaxLength
# 
# @title "Set the maximum length" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A ContinuousInsertor object.} 
#	\item{value}{A numeric (integer) vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new maximum length.
# } 
# 
# \examples{
#	# create object
#	o<-ContinuousInsertor(rate=1)
#	# set length sampling expression via virtual field
#	o$dist<-expression(rnorm(1,mean=6,sd=3))
#	# set/get maxLength
#	setMaxLength(o, 3)
#	getMaxLength(o)
#	# set/get maxLength via virtual field
#	o$maxLength<-10
#	o$maxLength
#	# propose a length
#	proposeLength(o)
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
	"setMaxLength", 
	class="ContinuousInsertor", 
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
###########################################################################/**
#
# @RdocMethod plot
# 
# @title "Plot the density of proposed lengths" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A ContinuousInsertor object.} 
#       \item{sample.size}{Number of lengths sampled for the plot.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The process object (invisible).
# } 
# 
# \examples{
#	# create object
#	o<-ContinuousInsertor(rate=1)
#	# set length sampling expression via virtual field
#	o$dist<-expression(rnorm(1,mean=10,sd=4))
#	# set maxLength
#	setMaxLength(o, 30)
#	# plot density
#	plot(o)
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
  "plot",
  class="ContinuousInsertor",
  function(
    x,
    sample.size=NA,
    ...
  ){

		this<-x;		
		if( !is.numeric(this$maxLength) | !is.expression(this$dist) ){
				warning("Insertion length distribution is not defined properly! Nothing to plot here!\n");
				return();
		}
		size<-(this$maxLength * 10);
		if(!missing(sample.size)){
				if(!is.numeric(sample.size) | ( length(sample.size)) !=1 ) {
					throw("Sample size paramter must be a numeric vector of size 1!\n");
				} else {
					size<-round(sample.size);
				}
		}

			sample<-apply(as.array(0:size),1,function(...){this$.propose.by(this)});
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
#       a<-ContinuousInsertor(rate=1,dist=expression(rnorm(1,mean=5,sd=2)),max.length=7)
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
  class="ContinuousInsertor",
  function(
    object,
    ...
  ){

    this<-object;	
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

