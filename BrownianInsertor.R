
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass BrownianInsertor
# 
# @title "The BrownianInsertor class"
# 
# \description{ 
#	The \code{BrownianInsertor} class inherits from the \code{DiscreteInsertor}
#	or \code{ContinuousInsertor} class depending on the \code{type} constructor argument 
#	("discrete" or "continuous").
#
#	This process generates the insert sequence based on the sites flanking the insertions as follows:
#	\itemize{
#		\item An insert length is sampled by calling the function stored in the \code{proposeBy} virtual field.
#		\item A sequence object is constructed.
#		\item The processes attached to both flanking sites are attached to the insert sequence. If there are no common processes, the processes from a randomly chosen site will be attached to the insert.
#		\item The site-process specific parameters are sampled from Brownian paths with linear trends having the values from the flanking sites as endpoints.
# 	}
#
#	The "noisiness" of the Brownian path can be controlled through the \code{scale} virtual field/constructor parameter.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{type}{Process type (see above).}
#	\item{scale}{Brownian path scale parameter.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a BrownianInsertor process, discrete type
#	p<-BrownianInsertor(
#                       type="discrete",
#                       scale=0.05, 
#                       sizes=1:4,
#                       probs=c(3/6,1/6,1/6,1/6),
#                       rate=0.05
#                       )
#	# get object summary
#	summary(p)
#	# plot insert length distribution
#	plot(p)
#	# create a nucleotide sequence, attach processes
#	s<-NucleotideSequence(string="AAAAAAAAAAA",processes=list(list(p,JC69())))
#	# create simulation object
#	sim<-PhyloSim(root.seq=s, phylo=rcoal(2))
#	# simulate and show alignment
#	Simulate(sim)
#	sim$alignment
#	# check the rate multipliers and insertion tolerances in one of the sequences
#	res<-sim$sequences[[2]]
#	getRateMultipliers(res,p)
#	getInsertionTolerance(res,p)
# }
# 
# @author
#
# \seealso{ 
# 	DiscreteInsertor ContinuousInsertor GeneralInsertor GeneralInDel
# }
# 
#*/###########################################################################
setConstructorS3(
  "BrownianInsertor",
  function( 
		name="Anonymous",
		type="discrete",
		scale=0.001,
		... 
		)	{

		if(type == "continuous"){
			this<-ContinuousInsertor(
			 ...
			);
		}
		else if (type == "discrete") {
			this<-DiscreteInsertor(
			 ...
			);
		}
		else {
			throw("Invalid insertor process type!\n");
		}
    
		this<-extend(
      			this,
      			"BrownianInsertor",
			.scale = NA,
			.type  = type
    		);
		
		# Using virtual field to clear Id cache:
		this$name<-name;
		# setting scale
		this$scale<-scale;

		this$generateBy<-function(process=NA,length=NA,target.seq=NA,event.pos=NA,insert.pos=NA){
	 
			if(is.na(target.seq)){
				return(NA);
			}
			
			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	
			
			# The start and end of the Brownian path:

			start;
			end;
			proc<-list();
			
			if( (event.pos == 1) || (event.pos == target.seq$.length) ){

				start<-clone(target.seq$.sites[[event.pos]]);
				start$.state=NA;
				end<-clone(start);
				proc<-getProcesses(start);
				
			} else {
				start<-clone(target.seq$.sites[[insert.pos]]);
				start$.state=NA;
				end<-clone(target.seq$.sites[[insert.pos + 1]]);
				end$.state=NA;
			
				proc.start<-getProcesses(start);
				proc.end<-getProcesses(end);

				# Calculate the intersection of process list:

				proc<-PSRoot$intersect.list(proc.start,proc.end);

				# No common processes:

				if(length(proc) == 0){
					coin.flip<-sample(c(0,1),1);
                                        if (coin.flip) {
                                          proc <- getProcesses(start)
                                          end <- clone(start)
                                        } else {
                                          proc <- getProcesses(end)
                                          start <- clone(end)
                                        }
				}
			}
		

			# Create the insert sequence:			

			class.seq<-class(target.seq)[[1]];
			insert<-do.call(class.seq,list(length=length));
			setProcesses(this=insert,value=list(proc));

			# For every process...
			
			for (p in proc){
				
				# ... and site specific parameter:	
				for(param in getSiteSpecificParamIds(p)){
					
					start.value<-getParameterAtSite(p,site=start,id=param)$value;
					end.value<-getParameterAtSite(p,site=end,id=param)$value;
					
					path<-seq(from=start.value,to=end.value,length.out=(insert$.length + 2));
					path<-path[2:(length(path)-1)];
					brownian.path<-abs(BrownianInsertor$BrownianPath(p=path, a=this$.scale));
		
					# Tolerance values are probabilities, do not alow them to wander beyond 1:	
					if(param == "insertion.tolerance" || param == "deletion.tolerance"){
						brownian.path[which(brownian.path > 1)]<-1;	
					}

					setParameterAtSites(
						insert,
						process	= p,
						id	= param,
						value	= brownian.path 
					);	
					
				}
			}

			return(insert);

				
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
  class="BrownianInsertor",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
		
	this$scale<-this$scale;
	if( (this$.type != "discrete") && (this$.type != "continuous") ){
		throw("BrownianInsertor type is invalid!\n");
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
## Method: BrownianPath
##
###########################################################################/**
#
# @RdocMethod BrownianPath 
# 
# @title "Generate a Brownian path" 
# 
# \description{ 
#	@get "title".
#
#	This method generates a Brownian path given the scale parameter a (determining "noisiness") 
#	and the vector p describing the trends. More useful as a static method.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#	\item{this}{A BrownianInsertor object.}
# 	\item{p}{Path parameter (a numeric vector).} 
# 	\item{a}{Scale paramater (a numeric vector of length one).} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	path<-BrownianInsertor$BrownianPath(a=2,p=1:10);
#	path
#	plot(path)
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
  "BrownianPath",
  class="BrownianInsertor",
  function(
	this,
	p=NA,
	a=NA,
    	...
  ){

	generate_brownian<-function(length, a){
		cumsum(rnorm(length,0,sd=a));
	}

	generate_bridge <- function (length,a){
		b <- generate_brownian(length,a)
		b - (1:length)/(length) * b[length]
	}

	generate_path <- function (p,a){
		n <- length(p);
		b <- generate_bridge (n+1,a);
		p + b[1:n];
	}

	return(generate_path(p,a));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning"
);


##	
## Method: getType
##	
###########################################################################/**
#
# @RdocMethod getType
# 
# @title "Get the type of the BrownianInsertor process" 
# 
# \description{ 
#	@get "title".
#
#	If type is \code{discrete}, than the process inherits from \code{DiscreteInsertor}.
#	If the type is \code{continuous}, then the object inherits from \code{ContinuousInsertor}.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A BrownianInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	p<-BrownianInsertor(type="discrete")
#	# get type
#	getType(p)
#	# get upstream classes
#	class(p)
#	p<-BrownianInsertor(type="continuous")
#	# get type via virtual field
#	p$type
#	# get upstream classes
#	class(p)
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
	class="BrownianInsertor", 
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
# @title "Forbidden action: setting the type of a BrownianInsertor object"
#
# \description{
#       @get "title".
#
#	The type can be set only from the \code{type} constructor argument and cannot be changed later.
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
	class="BrownianInsertor", 
	function(
		this,
		value,
		...
	){

		throw("The type of the BrownianInsertor objects can be set only from the constructor!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getScale
##	
###########################################################################/**
#
# @RdocMethod getScale
# 
# @title "Get scale parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A BrownianInsertor object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a BrownianInsertor object
#	p<-BrownianInsertor(scale=0.002)
#	# set/get scale parameter
#	setScale(p,0.1)
#	getScale(p)
#	# set/get scale parameter via virtual field
#	p$scale<-0.1
#	p$scale
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
	"getScale", 
	class="BrownianInsertor", 
	function(
		this,
		...
	){

		this$.scale;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setScale
##	
###########################################################################/**
#
# @RdocMethod setScale
# 
# @title "Set scale parameter" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A BrownianInsertor object.} 
# 	\item{value}{A numeric vector of length one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible).
# } 
# 
# \examples{
#	# create a BrownianInsertor object
#	p<-BrownianInsertor(scale=0.002)
#	# set/get scale parameter
#	setScale(p,0.1)
#	getScale(p)
#	# set/get scale parameter via virtual field
#	p$scale<-0.1
#	p$scale
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
	"setScale", 
	class="BrownianInsertor", 
	function(
		this,
		value,
		...
	){


		.checkWriteProtection(this);	
		if(!is.numeric(value) || (length(value) != 1)){
			throw("The value of the scale paramter must be a numeric vector of length 1!\n");
		}
		this$.scale<-value;
		return(invisible(this$.scale));
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
#	p<-BrownianInsertor(
#                       type="discrete",
#                       scale=0.05, 
#                       sizes=1:4,
#                       probs=c(3/6,1/6,1/6,1/6),
#                       rate=0.05
#                       )
#       # get a summary
#       summary(p)
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
  class="BrownianInsertor",
  function(
    object,
    ...
  ){
	this<-object;
    	.addSummaryNameId(this);
	this$.summary$"Type"<-this$.type;
	this$.summary$"Brownian path scale parameter"<-this$.scale;
	
    	NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

