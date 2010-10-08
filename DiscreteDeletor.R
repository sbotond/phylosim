##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass DiscreteDeletor
# 
# @title "The DiscreteDeletor class"
# 
# \description{ 
#	This class implements a process which performs deletions with
#	lengths sampled from a user-specified discrete distribution.
#	See \code{GeneralDeletor} for how the deletion process works.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
# 	\item{rate}{The general rate.}
#	\item{sizes}{The deletion sizes to propose.}
#	\item{probs}{A vector with the probabilites of the deletion sizes.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a DiscreteDeletor process
#	d<-DiscreteDeletor(
#		name="M.D.",
#		rate=0.25,
#		sizes=c(1,2),
#		probs=c(1/2,1/2)
# 	)
#	# get object summary
#	summary(d)
#	# set/get deletions sizes
#	d$sizes<-1:3
#	d$sizes
#	# set/get length probabilities
#	d$probs<-c(3,2,1)/6
#	d$probs
#	# plot length distribution
#	plot(d)
#	
#	# The following code illustrates how to use
#	# a DiscreteDeletor process in a simulation
#	
#	# create a sequence object, attach process d
#	s<-NucleotideSequence(string="AAAAAAAAAAGGGGAAAAAAAAAA",processes=list(list(d)))
#	# set the deletion tolerance to zero in the range 11:15
#	# creating a region rejecting all deletions
#	setDeletionTolerance(s,d,0,11:15)	
#	# get deletion tolerances
#	getDeletionTolerance(s,d)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# simulate
#	Simulate(sim)
#	# print resulting alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GeneralDeletor ContinuousDeletor GeneralInDel
# }
# 
#*/###########################################################################
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
			if(!exists(x="PSIM_FAST")){
			if( !is.numeric(process$.sizes) | !is.numeric(process$.probs) ){
				throw("Cannot propose deletion length because the length distribution is not defined properly!\n");
			}
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
  class="DiscreteDeletor",
  function(
    this,
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
###########################################################################/**
#
# @RdocMethod getSizes
# 
# @title "Get the sizes of the proposed deletions" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A DiscreteDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A vector of integers.
# } 
# 
# \examples{
#	# create a DiscreteDeletor object
#	d<-DiscreteDeletor(rate=1)
#	# set deletion sizes      
#	setSizes(d,c(1,2,3))
#	# get deletion sizes
#	getSizes(d)
#	# set/get sizes via virtual field
#	d$sizes<-1:10
#	d$sizes	
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
###########################################################################/**
#
# @RdocMethod setSizes
# 
# @title "Set the sizes of the proposed deletions" 
# 
# \description{ 
#	@get "title".
#	
#	The provided numeric vector is rounded.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A DiscreteDeletor object.} 
# 	\item{value}{A numeric vector.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A vector of integers (invisible).
# } 
# 
# \examples{
#	# create a DiscreteDeletor object
#	d<-DiscreteDeletor(rate=1)
#	# set deletion sizes      
#	setSizes(d,c(1,2,3))
#	# get deletion sizes
#	getSizes(d)
#	# set/get sizes via virtual field
#	d$sizes<-1:10
#	d$sizes	
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
		return(invisible(this$.sizes));
		
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
###########################################################################/**
#
# @RdocMethod getProbs
# 
# @title "Get the deletion length probabilities" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A DiscreteDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector with the deletion length probabilities.
# } 
# 
# \examples{
#	# create a DiscreteDeletor object
#	d<-DiscreteDeletor(rate=1, sizes=1:3)
#	# set/get length probabilities
#	setProbs(d,c(1/3,1/3,1/3)) # equal probabilites
#	getProbs(d)
#	# set/get length probabilities via virtual field
#	x<-c(2,2,1)
#	# normalize x
#	x<-x/sum(x)
#	d$probs<-x
#	d$probs
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
###########################################################################/**
#
# @RdocMethod setProbs
# 
# @title "Set the deletion length probabilities" 
# 
# \description{ 
#	@get "title".
#
#	The \code{sizes} virtual field must be set before setting the length probabilities.
#	The length of the provided numeric vector must match with the length of the vector
#	stored in the \code{sizes} virtual field. The vector is rescaled if the values do not
#	sum to one and a warning is issued.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A DiscreteDeletor object.} 
#	\item{value}{A numeric vector containg the length probabilities.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The vector of probabilities.
# } 
# 
# \examples{
#	# create a DiscreteDeletor object
#	d<-DiscreteDeletor(rate=1, sizes=1:3)
#	# set/get length probabilities
#	setProbs(d,c(1/3,1/3,1/3)) # equal probabilites
#	getProbs(d)
#	# set/get length probabilities via virtual field
#	x<-c(2,2,1)
#	# normalize x
#	x<-x/sum(x)
#	d$probs<-x
#	d$probs
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
#       # create an object
#       a<-DiscreteDeletor(rate=1,sizes=c(1,2),probs=c(1/2,1/2))
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
###########################################################################/**
#
# @RdocMethod plot
# 
# @title "Plot the deletion length distribution" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A DiscreteDeletor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The DiscreteDeletor object (invisible).
# } 
# 
# \examples{
#	d<-DiscreteDeletor(
#		name="MyDiscDel",
#		sizes=1:6,
#		probs=c(0.25000000, 0.16666667, 0.08333333, 0.08333333, 0.16666667, 0.25000000)
#	)
#	# plot the deletion length distribution
#	plot(d)
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
			return(invisible(this));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

