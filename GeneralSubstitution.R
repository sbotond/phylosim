##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass GeneralSubstitution
# 
# @title "The GeneralSubstitution class"
# 
# \description{ 
#	This a class representing a continuous-time Markov process acting
#	on the state space defined by the symbols stored in the Alphabet object
#	passed to the object constructor. 
#
#	The GeneralSubstitution objects generate
#	Event objects corresponding to substitution events based on the state of the 
#	attached Site objects.
#
#	The GeneralSubstitution objects aggregate a QMatrix object, which stores the 
#	unscaled and scaled rate matrices. The scaled rate matrices, along with the
#	site-process specific rate multiplier parameters define the rates of the generated
#	Event objects.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{alphabet}{The alphabet on which the process acts (Alphabet object).}
#	\item{rate.list}{A list with the substitution rates. It will be passed to \code{setRateList} method.}
#	\item{equ.dist}{The equilibrium distribution (see \code{setEquDist.GeneralSubstitution}).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# Create a GeneralSubstitution object
#	# also provide an Alphabet objects 
#	# and the list of unscaled rates
#	a<-BinaryAlphabet()
#	gs<-GeneralSubstitution(
#			name="MyBinarySubst",
#			alphabet=a,
#			rate.list=list("0->1"=2,"1->0"=1)
#		)
#	# check if inherits from GeneralSubstitution
#	is.GeneralSubstitution(gs)
#	# get an object summary
#	summary(gs)
#	# get a bubble plot
#	plot(gs)
#	# get unscaled rate for "0->1" by event name
#	getRate(gs,"0->1")
#	# get unscaled rate for "0->1" by states
#	getRate(gs,from="0", to="1")
#	# get scaled rate for "0->1" by name
#	getEventRate(gs,"0->1")
#	# get the list of unscaled event rates
#	gs$rateList
#	# set the \emph{unscaled} rates
#	gs$rateList<-list("0->1"=1,"1->0"=1)
#	# reset equilibrium distribution
#	gs$equDist<- 5 * gs$equDist
#	# get the equilibrium distribution
#	gs$equDist
#	# sample a state form the equilibrium distribution
#	sampleState(gs)
#	# get the associated QMatrix object
#	gs$qMatrix
#	# create a site object
#	s<-Site(alphabet=a, state="0")
#	# attach gs to s
#	s$processes<-list(gs)
#	# set rate multiplier for s and gs
#	setParameterAtSite(gs,s,id="rate.multiplier",value=2)
#	# get site specific rate for "0->1"
#	getEventsAtSite(gs,s,"0->1")
#	# get the list of active event objects given the state of s
#	getEventsAtSite(gs,s)
#	# get the associated Alphabet object 
#	gs$alphabet
#	# clone the object
#	gsc<-clone(gs)
#	# modify the alphabet of gsc
#	gsc$alphabet<-NucleotideAlphabet()
#	summary(gsc)
#	# check if gsc has undefined rates
#	hasUndefinedRate(gsc)
# }
# 
# @author
#
# \seealso{ 
# 	Process QMatrix Event Site GeneralIndel GTR WAG
# }
# 
#*/###########################################################################
setConstructorS3(
  "GeneralSubstitution",
  function( 
		name="Anonymous", 
		alphabet=NA,
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

	
		# Set an empty alphabet by default
		# to satisfy the static instance:
		if(missing(alphabet)){
			alphabet<-Alphabet(name="Undefined");
		}

		this<-Process(
			name=name,
			alphabet=alphabet
		);
    		this<-extend(
      			this,
      			"GeneralSubstitution",
			.q.matrix=NA,
			.equ.dist=NA,
			.handler.template=NA,
			.is.general.substitution=TRUE
    		);

		# Initialize with NA-s equDist:
		if (missing(equ.dist)){
			.initEquDist(this);
		} else {
			# or set if we have one:
			this$equDist<-equ.dist;
		}

		# Create the QMatrix object:
		qm<-QMatrix(name=name, alphabet=alphabet);
	
		# Set the rates:	
		if(!missing(rate.list)){
			qm$rateList<-rate.list;
		}
		
		# Attach the QMatrix to the process:
		this$.q.matrix<-qm;
		this$.q.matrix$process<-this;

		# Try to guess the equlibrium distribution:
		if (missing(equ.dist) & !missing(rate.list)){
			if(.setEquDistFromGuess(this)){
				# and perfrom rescaling if suceeded:
				rescaleQMatrix(this);
			}
		}

		# Using virtual field to clear Id cache:
		this$name<-name;

		# Set the template for handling substitution events:
		this$.handler.template<-function(event=NA){
				# Just set the new state base on the event name:
				setState(event$.site, strsplit(event$.name,split="->",fixed=TRUE)[[1]][[2]]);
				# The name *should* be valid and correct, so no more checking is needed.

				# Return details:
				return(
					list(
						type="substitution"
					)
				);	
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
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
			
			# The process must have a valid alphabet object:	
			if(!is.Alphabet(this$.alphabet)){
				throw("Alphabet object is invalid!\n");
			}
		
			# Name:
			if(!is.na(this$name)){
				this$name<-this$name;
			}
		
			# EquDist:
			if(!any(is.na(this$.equ.dist))){
				this$equDist<-this$equDist;
			}
			# Negative rates are impossible:
			if(all(!is.na(this$rateList)) & any(as.numeric(this$rateList) < 0 )){
				throw("The rate matrix has negative off-diagonal elements!\n");	
			}
			
			# QMatrix should never be NA!
			this$QMatrix<-this$QMatrix;
		
			# Further checks if survived the one above:
			checkConsistency(this$.q.matrix,check.process=FALSE);

			if(is.Process(this$.q.matrix$.process)){
              		# Check for alphabet compatibility:
              		if(this$.alphabet != this$.q.matrix$.process$alphabet){
                		throw("Process/QMatrix alphabet mismatch!\n");
              		}
              		# Check if the parent process QMatrix is this object:
              		if(!equals(this$.q.matrix$.process, this) ){
                		throw("QMatrix process is not identical with self!\n");
              		}
			} else if(!is.na(this$.q.matrix$.process)){
					throw("QMatrix process entry is invalid!\n");
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
## Method: getEventsAtSite
##	
###########################################################################/**
#
# @RdocMethod	getEventsAtSite
# 
# @title "Generate the list of active Event objects for a given attached Site object" 
# 
# \description{ 
#	@get "title".
#	
#	This is the single most important method in the \code{GeneralSubstitution} class. It generates a list of the active
#	Event objects given the transition rate matrix (Q matrix) and the "rate.multiplier" Site-Process specific parameter.
#	It returns an empty list if the state of the site is "NA".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{target.site}{A Site object. The GeneralSubstitution object must be attached to the Site object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of the active Event objects.
# } 
# 
# \examples{
#	# create an Alphabet object
#	a<-BinaryAlphabet()
#	# create a Site object
#	s<-Site(alphabet=a);
#	# create a GeneralSubstitution object
#	p<-GeneralSubstitution(alphabet=a,rate.list=list("0->1"=1,"1->0"=1))
#	# attach process p to site object s
#	attachProcess(s,p)	
#	# get the rate of active events
#	getEventsAtSite(p,s)	# empty list
#	# set the state of s
#	s$state<-1;
#	# get the rate of active events
#	getEventsAtSite(p,s)
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
	"getEventsAtSite", 
	class="GeneralSubstitution", 
	function(
		this,
		target.site,
		...
	){

	# The main method of this class,
	# generating a list of event objects given the 
	# state of the target site.
	if(!exists(x="PSIM_FAST")){
	
	if(missing(target.site)) {
      		throw("No target site provided!\n");
    	} 
	
	}
	
	# The following code is commented out to
	# increase speed
		
	#else if (!sloppy) {
	# Additional checks. They can be
	# disabled by sloppy=TRUE			

      	#if(!is.Site(target.site)) {
      	#  throw("Target site invalid!\n");
      	#}
	#else if(!is.QMatrix(this$.q.matrix)){
	#	throw("Cannot provide event objects because the rate matrix is not set!\n");	
	#}
	#else if(!is.numeric(this$.equ.dist)){
	#	throw("Cannot provide event objects because the equilibrium frequencies are not defined!\n");	
	#} 
	#} 

	state<-as.character(target.site$.state);
	# Just return an empty list if the state is NA:
	if(is.na(state)){
		return(list());
	}
	
	# The rate of the event is the product of the general rate and the
     	# site specific rate multiplier:
     	rate.multiplier<-target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value;
		
	# Return empty list if the rate multiplier is zero.
     	if(rate.multiplier == 0 ) {
      		return(list());
     	}	
	
	# Get rate matrix:
	rate.matrix<-this$.q.matrix$.rate.matrix;

	symbols<-this$.alphabet$.symbols;
	rest<-symbols[ which(symbols != state) ];

	# Create the event objects:
	events<-list();
	for(new.state in rest){
		
		name<-paste(state,new.state,sep="->");
	 	# Clone the event template object:
     		event<-clone(this$.event.template);
     		# Set event name:
     		event$.name<-name;
     		# Set the generator process:
     		event$.process<-this;
     		# Set the target position passed in a temporary field,
		# Event objects are not aware of their posiitions in general!
     		event$.position<-target.site$.position;
     		# Set the target site:
     		event$.site<-target.site;
			
		# Set the event rate:	
		event$.rate<-(rate.multiplier * (rate.matrix[state,new.state]));
		# Set the handler for the substitution event:
     		event$.handler<-this$.handler.template;
		
		# Add to events list:	
		events<-c(events, list(event));

	}

	return(events);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setEquDist
##
###########################################################################/**
#
# @RdocMethod setEquDist
# \alias{setEquDist.AminoAcidSubst}
#
# @title "Set the equilibrium distribution for a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	In the case the equlibrium distribution cannot be guessed from the rate matrix one should provide
#	a valid equilibrium distribution. The equilibrium distribution must be compatible with the rate matrix.
#	The provided numeric vector will be resacled in the case the sum of the elemnts is not one.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{value}{A numeric vector containing the equlibrium symbol frequencies. The order of the frequencies must be the same as in the symbol vector of the attached Alphabet object.}
#	\item{force}{Do not check compatibility with thr rate matrix.}
#	\item{silent}{Do not print out warnings.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new equlibrium distribution (invisible).
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(
#                           alphabet=BinaryAlphabet(),
#                           rate.list=list("1->0"=1,"0->1"=1)
#                           )
#	# get equlibrium distribution 
#	getEquDist(p)
#	# get equilibrium distribution via virtual field
#	p$equDist
#	# re-set the equilibrium distribution
#	dist<-p$equDist * 3
#	dist
#	setEquDist(p,dist)
#	p$equDist
#	# re-set equilibrium distribution via virtual field
#	p$equDist<-p$equDist * 2
#	p$equDist
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
  "setEquDist",
  class="GeneralSubstitution",
  function(
    this,
    value,
    force=FALSE,
    silent=FALSE,
    ...
  ){

    .checkWriteProtection(this);
if(!exists(x="PSIM_FAST")){

    if(!is.Alphabet(this$alphabet)){
      	throw("Cannot set equilibrium distribution because the alphabet is undefined!");
    }
    if(missing(value)) {
      throw("No new value provided!\n");}
    else if(!is.numeric(value)) {
      throw("The new value must be numeric!\n");
    }
}
    if(length(value) != this$alphabet$size){
      throw("The new value must be a vector of length ",this$alphabet$size,"!\n");
    }
    if(!PSRoot$my.all.equal(sum(value), 1.0)) {
				value<-(value/sum(value));
				if (silent == FALSE){
					warning("The provided probabilities have been rescaled in order to sum to one!\n");
				}
    }

if(!exists(x="PSIM_FAST")){
	# Check if the provided equlibrium distribution is
	# compatible with the rate matrix:
	if( !.checkEquMatCompat(this, rbind(value)) & force==FALSE){
				throw("The provided equlibrium distribution: ",paste(value,collapse=" ")," is not compatible with the rate matrix! Use force=TRUE to set it anyway!\n");
	}
}
	# Set the value:
      this$.equ.dist<-rbind(value);
			# Set dimnames:
      colnames(this$.equ.dist)<-(this$alphabet$symbols);
      rownames(this$.equ.dist)<-c("Prob:");
 	return(invisible(this$.equ.dist));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: .setEquDistFromGuess
##
setMethodS3(
  ".setEquDistFromGuess",
  class="GeneralSubstitution",
  function(
    this,
    ...
  ){
			
	# Try to guess equlibrium distribution:
	tmp<-.guessEquDist(this);
	# Take care with the condition here!
	# We can get in trouble with any()
	# if the first value is zero!
	if( length(tmp) == 1 & all(tmp == FALSE) ){
		warning("The equlibrium distribution of the substitution process could not be determined based on the rate matrix!\n You have to set yourself the proper distribution in order to use the process!");
		return(FALSE);
	}
	else {
		this$equDist<-tmp;
		return(TRUE);
	}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkEquMatCompat
##
setMethodS3(
  ".checkEquMatCompat",
  class="GeneralSubstitution",
  function(
    this,
    value,
    ...
  ){

    if(missing(value)) {
      throw("No equlibrium distribution provided!\n")
		}
		else if ( length(value) != dim(this$.q.matrix$.orig.matrix)[[2]] ){
				throw("Value vector length should be",dim(this$.q.matrix$.orig.matrix)[[2]],"!\n");	
		}
		else {
			# The following matrix product of the equlibrium distribution
			# and the rate matrix should give the zero vector:
			tmp<-(rbind(value) %*% as.matrix(this$.q.matrix$.orig.matrix));
			if(PSRoot$my.all.equal(tmp, rep(0.0, times=length(tmp))) ){
				return(invisible(TRUE));
			} else {
				return(FALSE);
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
## Method: .guessEquDist
##	
setMethodS3(
	".guessEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot guess equilibrium distribution because the Q matrix is not set!\n");
		}
		
		# Refuse to guess if the rate matrix has zero entries:
		if(length(which(this$.q.matrix$.orig.matrix == 0)) != 0 ){
			warning("Cannot guess equilibrium distribution because the rate matrix has zero entries!\n");
			return(FALSE);
		}

		# Get the left eigenvalues and eigenvectors of the rate matrix:
		eigen<-eigen(t(this$.q.matrix$.orig.matrix));
		dist<-numeric(0);

		if( length(intersect(is.complex(eigen$values),TRUE)) == 0 ) {
			# if all  eigenvalues are real:
			# Choose the largest eigenvalue (which should be zero):
			index<-which( eigen$values == max(eigen$values));
			# Choose the correspondign eigenvector:
			dist<-rbind(eigen$vectors[ ,index]);
		}
		else {
			# If we have complex eigenvalues:
			# Choose the eigenvalue (l) with maximum |e^(l)|  
			tmp<-abs(exp(eigen$values));
			index<-which(tmp == max(tmp));
			# ... and the corresponding eigenvector:
			dist<-as.double(eigen$vectors[,index]);
		}	

		# Normalize the eigenvector:
		return(dist/sum(dist));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
				

##	
## Method: .initEquDist
##	
setMethodS3(
	".initEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		dummy=NA, # to satisfy method classification
		...
	){

		if(!isEmpty(this$.alphabet)){
			# Fill in with NA-s
			this$.equ.dist<-rbind(rep(NA,times=this$.alphabet$size));
			# Set the dimnames:
			colnames(this$.equ.dist)<-this$.alphabet$symbols;
			rownames(this$.equ.dist)<-c("Prob:");
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEquDist
##	
###########################################################################/**
#
# @RdocMethod getEquDist
# 
# @title "Get the equilibrium distribution from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{dummy}{Not used.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new equlibrium distribution (invisible).
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(
#                           alphabet=BinaryAlphabet(),
#                           rate.list=list("1->0"=1,"0->1"=1)
#                         )
#	# get equlibrium distribution 
#	getEquDist(p)
#	# get equilibrium distribution via virtual field
#	p$equDist
#	# re-set the equilibrium distribution
#	dist<-p$equDist * 3
#	dist
#	setEquDist(p,dist)
#	p$equDist
#	# re-set equilibrium distribution via virtual field
#	p$equDist<-p$equDist * 2
#	p$equDist
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
	"getEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		dummy=NA, # to satisfy method classification
		...
	){

		this$.equ.dist;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: sampleState
##	
###########################################################################/**
#
# @RdocMethod	sampleState
# 
# @title "Sample a state from the equlibrium distribution of a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# get equlibrium distribution 
#	getEquDist(p)
#	# get equilibrium distribution via virtual field
#	p$equDist
#	# sample from equilibrium distribution
#	sampleState(p)
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
	"sampleState", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

	if(!exists(x="PSIM_FAST")){
		if(any(is.na(this$.equ.dist))){
			throw("Cannot sample state because the equlibrium distribution is not defined!\n");
		}
		else if (!is.Alphabet(this$.alphabet)){
			throw("Cannot sample state because the alphabet is not valid! That is strange as equlibrium distribution is defined!\n");	
		}
	}
	
	if(this$.alphabet$size == 0){
		throw("The process alphabet is empty, nothing to sample here!\n");
	}
	if(this$.alphabet$size == 1){
	# Special case: single letter in the alphabet:
		return(this$.alphabet$symbols[[1]]);
	}
	else {
	# Sample from the equlibrium distribution:
		sample(x=this$.alphabet$.symbols, size=1, replace=FALSE, prob=this$.equ.dist);
	}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getQMatrix
##	
###########################################################################/**
#
# @RdocMethod getQMatrix
# 
# @title "Get the QMatrix object aggregated by a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method is mostly used internally.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A QMatrix object.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# get the QMatrix object
#	getQMatrix(p)
#	# get the QMatrix object via virtual field
#	q<-p$qMatrix
#	# tweak with the QMatrix
#	setRate(q,"0->1",2)
#	# set a new QMatrix for p
#	setQMatrix(p,q)
#	summary(p)
#	# set new QMatrix via virtual field
#	setRate(q,"1->0",2)
#	p$qMatrix<-q
#	summary(p)
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
	"getQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		this$.q.matrix;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setQMatrix
##	
###########################################################################/**
#
# @RdocMethod setQMatrix
# 
# @title "Set the QMatrix object aggregated by a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method is mostly used internally.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{value}{A QMatrix object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The QMatrix object.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# get the QMatrix object
#	getQMatrix(p)
#	# get the QMatrix object via virtual field
#	q<-p$qMatrix
#	# tweak with the QMatrix
#	setRate(q,"0->1",2)
#	# set a new QMatrix for p
#	setQMatrix(p,q)
#	summary(p)
#	# set new QMatrix via virtual field
#	setRate(q,"1->0",2)
#	p$qMatrix<-q
#	summary(p)
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
	"setQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if(!is.QMatrix(value)){
			throw("The provided object is not a QMatrix!\n");
		}
		else if (!is.Alphabet(getAlphabet(this))){
			throw("Cannot set QMatrix because process alphabet is not defined!\n");
		}
		else if(!is.Alphabet(value$alphabet)){
			throw("Cannot set QMatrix because the alphabet of the provided QMatrix object is not set!\n");	
		}
		else if(getAlphabet(this) != value$alphabet){
			throw("Alphabet mismatch! Cannot set QMatrix!\n");	
		}
	}
		this$.q.matrix<-value;
		return(this$.q.matrix)

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
###########################################################################/**
#
# @RdocMethod	setAlphabet
# 
# @title "Set the Alphabet object aggregated by a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
#	This method will also re-initialize the equlibrium distribution by setting all frequencies to NA.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{value}{An Alphabet object.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The Alphabet object.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object with an attached BinaryAlphabet object
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
#	# get object summary
#	summary(p)
#	# get alphabet
#	getAlphabet(p)
#	# get alphabet via virtual field
#	p$alphabet
#	# set a new alphabet
#	setAlphabet(p,NucleotideAlphabet())
#	summary(p)
#	# set alphabet via virtual field
#	p$alphabet<-BinaryAlphabet()
#	p$alphabet
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
	"setAlphabet", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if (!is.Alphabet(value)){
			throw("Alphabet object is invalid!\n");
		}
	}
		this$.alphabet<-value;
		# Set the QMatrix alphabet
		if(is.QMatrix(this$.q.matrix)){
			setAlphabet(this$.q.matrix, value);
		}
		.initEquDist(this);
		return(this$.alphabet);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
###########################################################################/**
#
# @RdocMethod	getAlphabet
# 
# @title "Get the Alphabet object aggregated by a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	An Alphabet object.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object with an attached BinaryAlphabet object
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
#	# get object summary
#	summary(p)
#	# get alphabet
#	getAlphabet(p)
#	# get alphabet via virtual field
#	p$alphabet
#	# set a new alphabet
#	setAlphabet(p,NucleotideAlphabet())
#	summary(p)
#	# set alphabet via virtual field
#	p$alphabet<-BinaryAlphabet()
#	p$alphabet
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
	"getAlphabet", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		# Just to satisfy method classification:
		this$.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasUndefinedRate
##	
###########################################################################/**
#
# @RdocMethod hasUndefinedRate
# 
# @title "Check if a GeneralSubstitution object has undefined rates" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
#	# check if it has undefined rates
#	hasUndefinedRate(p)	# TRUE
#	# set the missing rates
#	p$rateList<-list("0->1"=1,"1->0"=2)
#	# check for undefined rates again
#	hasUndefinedRate(p)	# FALSE
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
	"hasUndefinedRate", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		if( any(is.na(this$.q.matrix$.orig.matrix)) | any(is.na(this$.q.matrix$.rate.matrix))){
			return(TRUE);
		}
		else {
			return(FALSE);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: getEventRate
##	
###########################################################################/**
#
# @RdocMethod getEventRate
# 
# @title "Get the scaled rate of an event from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method return the element from the scaled rate matrix stored in the associated QMatrix object corresponding to
#	a given event. The event can be specified by the inital and target states ("from" and "to" arguments), or by the
#	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments. 
#
#	This method doesn't take into account the site specific rate multipliers in any way.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{name}{The name of the event.}
#	\item{from}{The initial state.}
#	\item{to}{Target state.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# get the scaled rate of "0->1" by name
#	getEventRate(p,"0->1")	
#	# get the scaled rate of "0->1" by states
#	getEventRate(p,from="0",to="1")
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
	"getEventRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){

		# For getting the scaled event rate:
	if(!exists(x="PSIM_FAST")){
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
		}
	}
		else if(!missing(name) & missing(from) & missing(to)){
			return(getEventRate(this$.q.matrix, name=name));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(getEventRate(this$.q.matrix, from=from, to=to));
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
		
##
## Method: getEventRateAtSite
##
###########################################################################/**
#
# @RdocMethod getEventRateAtSite
# 
# @title "Get the site spcific rate of an event from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method return the element from the associated QMatrix object corresponding to
#	a given event multiplied by the "rate.multiplier" site-process specific parameter stored in the specified site object.
#	The event can be specified by the inital and target states ("from" and "to" arguments), or by the
#	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments. 
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object. It must be attached to the provided Site object.} 
# 	\item{site}{A Site object.} 
#	\item{name}{The name of the event.}
#	\item{from}{The initial state.}
#	\item{to}{Target state.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# create a Site object
#	s<-Site(alphabet=BinaryAlphabet())
#	# attach process p to site s
#	s$processes<-list(p)
#	# set the rate multiplier for s and p
#       setParameterAtSite(p,s,id="rate.multiplier",value=2)
#	# get the site specific rate of "0->1" by name
#	getEventRateAtSite(p,s,"0->1")	
#	# get the site specific rate of "0->1" by states
#	getEventRateAtSite(p,s,from="0",to="1")
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
  "getEventRateAtSite",
  class="GeneralSubstitution",
  function(
    this,
    site,
    name=NA,
    from=NA,
    to=NA,
    ...
  ){

if(!exists(x="PSIM_FAST")){

      if(missing(site)){
        throw("No site provided");
      }
      else if (!isAttached(site, process=this)){
        throw("The process is not attached to the specified site!\n");
      }
}

      global.rate<-numeric();
			
			# Event specified by name:
      if(!missing(name) & missing(from) & missing(to)){
          global.rate<-getEventRate(this$.q.matrix, name=name);
      }
			# Event specified by from= and to=
      else if(missing(name) & !missing(from) & !missing(to)){
          global.rate<-getEventRate(this$.q.matrix, from=from, to=to);
      }
      else {
        throw("The substitution should be specified by name or by the \"from\" and \"to\" arguments!\n");
      }

      return(global.rate * site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value );

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
	
##	
## Method: getRate
##	
###########################################################################/**
#
# @RdocMethod getRate
# 
# @title "Get an unscaled rate of an event from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method gets the element corresponding to a given event form the unscaled Q matrix.
#	a given event. The event can be specified by the inital and target states ("from" and "to" arguments), or by the
#	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments. 
#
#	The rescaled rates (used during simulations) are returned by the \code{getEventRate} method.
#
#	This method doesn't take into account the site specific rate multipliers in any way.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{name}{The name of the event.}
#	\item{from}{The initial state.}
#	\item{to}{Target state.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# get the unscaled rate of "0->1" by name
#	getRate(p,"0->1")	
#	# get the unscaled rate of "0->1" by states
#	getRate(p,from="0",to="1")
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
	"getRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){

	if(!exists(x="PSIM_FAST")){
	
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
		}
	}
		if(!missing(name) & missing(from) & missing(to)){
			return(getRate(this$.q.matrix, name=name));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(getRate(this$.q.matrix, from=from, to=to));
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: setRate
##	
###########################################################################/**
#
# @RdocMethod setRate
#
# @title "Set an unscaled rate for an event from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method sets the element corresponding to a given event in the unscaled Q matrix.
#	The event can be specified by the inital and target states ("from" and "to" arguments), or by the
#	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments. 
#
#	Modifying any rate in the unscaled Q matrix will trigger the re-scaling of the whole matrix.
#	The rescaled rates (used during simulations) are returned by the \code{getEventRate} method.
#
#	This method doesn't modify the site specific rate multipliers.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{name}{The name of the event.}
#	\item{from}{The initial state.}
#	\item{value}{The new value of the rate.}
#	\item{to}{Target state.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A Numeric vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
#	# set the unscaled rate by event name
#	setRate(p,"0->1",2)
#	# get the unscaled rate of "0->1" by name
#	getRate(p,"0->1")	
#	# set the unscaled rate by states
#	setRate(p,"0->1",0.5)
#	# get the unscaled rate of "0->1" by states
#	getRate(p,from="0",to="1")
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
	"setRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		value,
		from=NA,
		to=NA,
		...
	){
		
		.checkWriteProtection(this);
		# Setting unscaled rate:
	if(!exists(x="PSIM_FAST")){
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot set rate as the rate matrix is undefined!\n");
		}
	}
		if(!missing(name) & missing(from) & missing(to)){
			return(setRate(this$.q.matrix, name=name, value=value));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(setRate(this$.q.matrix, from=from, to=to, value=value));
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateList
##	
###########################################################################/**
#
# @RdocMethod	getRateList
# 
# @title "Get a list of events and their unscaled rates from a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	This method returns the list of event rates from the \emph{unscaled} Q matrix (as returbed bvy the \code{getEventRate} method). 
#	The returned list contains the rates associated with the corresponding event names.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of event rates.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
#	# get the event rates from the unscaled Q matrix
#	getRateList(p)
#	# get rates from the unscaled rate matrix via virtual field
#	p$rateList
#	# set rates in the unscaled rate matrix
#	setRateList(p, list("0->1"=1,"1->0"=1))
#	p$rateList
#	# set rates in the unscaled rate matrix via virtual field
#	p$rateList<-list("0->1"=3,"1->0"=1);
#	# check the contenst of the associated QMatrix object
#	summary(p$QMatrix)
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
	"getRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

	if(!exists(x="PSIM_FAST")){
		
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
	}
		return(getRateList(this$.q.matrix));


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateList
##	
###########################################################################/**
#
# @RdocMethod	setRateList
# 
# @title "Setting the rates for a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	
#	This method set the rates in the \emph{unscaled} Q  matrix based on the provided list containing even names
#	and the associated rates. The rate must be specified for every event!
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
#	\item{value}{A list with the events names and the associated rates.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The GeneralSubstitution object (invisible).
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
#	# get the event rates from the unscaled Q matrix
#	getRateList(p)
#	# get rates from the unscaled rate matrix via virtual field
#	p$rateList
#	# set rates in the unscaled rate matrix
#	setRateList(p, list("0->1"=1,"1->0"=1))
#	p$rateList
#	# set rates in the unscaled rate matrix via virtual field
#	p$rateList<-list("0->1"=3,"1->0"=1);
#	# check the contenst of the associated QMatrix object
#	summary(p$QMatrix)
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
	"setRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
	if(!exists(x="PSIM_FAST")){

		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
		else if(missing(value)){
			throw("No new rate list specified!\n");
		}
	}
		return(setRateList(this$.q.matrix, value) );

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: rescaleQMatrix
##	
###########################################################################/**
#
# @RdocMethod rescaleQMatrix
# 
# @title "Rescale the scaled rate matrix of a QMatrix object aggregated by a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	The QMatrix objects aggregated by the GeneralSubstitution objects store two rate matrices: one containes
#	the rates provided by the user (unscaled rate matrix), the other matrix (scaled rate matrix) is rescaled to have the 
#	expected number of subsitutions per unit time equal to one when the process is at equlibrium.
#	This method performes the re-scaling of the scaled rate matrix in the associated QMatrix object based on 
#	the equlibrium distribution and the unscaled rate matrix.
#
#	This method is mainly used internally as the scaled matrix is rescaled every time the unscaled matrix 
#	is modifed.
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	# provide an Alphabet object and the rates
#	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
#	# rescale rate matrix
#	rescaleQMatrix(p)
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
	"rescaleQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

	if(!exists(x="PSIM_FAST")){

		if(is.na(this$.q.matrix)){
			return(invisible(FALSE));
		}
		else if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot rescale rate matrix because it is invalid!\n");
		}
		else if (any(is.na(this$.q.matrix))){
			throw("Cannot rescale rate matrix because not all rates are specified!\n");
		}
		else if(any(is.na(this$.equ.dist))){
			throw("Cannot rescale rate matrix because the equlibrium distribution is not defined properly!\n");
		}
		# Check for alphabet mismatch:
		if(this$alphabet != this$.q.matrix$.alphabet){
			throw("The process alphabet and the QMatrix alphabet is not the same! Refusing to rescale!\n");
		}
	}
		# Set rescaling constant to zero:
		K <- 0; 
		# get the symbols:
		symbols<-this$alphabet$symbols;
		orig.matrix<-this$.q.matrix$.orig.matrix;
			
		# For every symbol:
		for (i in symbols) {
		# Get the equlibrium probability:
		i.equ<-this$.equ.dist[[ which(colnames(this$.equ.dist) == i) ]];
		for(j in symbols){
			if(i == j){next}
			# For every other symbol - update the constant:
			K <- K + (i.equ * orig.matrix[i,j] );
			}
		}
	
    		Scale(this$.q.matrix,constant=(1/K));
		# After rescaling the expected rate of substitutions per site
		# at equlibrium is 1.
		return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: is.GeneralSubstitution
##	
###########################################################################/**
#
# @RdocDefault is.GeneralSubstitution
# 
# @title "Check if an object is an instance of the GeneralSubstitution class" 
# 
# \description{ 
#       @get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{An object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       TRUE or FALSE.
# } 
# 
# \examples{
#	# create some objects
#	p<-GeneralSubstitution()
#	pp<-Process()
#	# chek if they inherit from GeneralSubstitution
#	is.GeneralSubstitution(p)
#	is.GeneralSubstitution(pp)
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
	"is.GeneralSubstitution", 
	class="default", 
	function(
		this,
		...
	){

	if(!is.PSRoot(this)) {return(FALSE)}
	if(!is.null(this$.is.general.substitution)){return(TRUE)}
	if ( inherits(this, "GeneralSubstitution")) {
		this$.is.general.substitution<-TRUE;
      		return(TRUE);
	} else {
      	return(FALSE)
    	}

},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: as.character
##	
###########################################################################/**
#
# @RdocMethod as.character
# 
# @title "Return the character representation of a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#	The character representation is the object id as returned by the 
#	\code{getId.Process} method defined in the parent class.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	p<-GeneralSubstitution(name="MySubst")
#	# get character representation
#	as.character(p)
#	# the same implicitly
#	p
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
	"as.character",
	class="GeneralSubstitution", 
	function(
		x,
		...
	){

		x$.id;

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
#       a<-GeneralSubstitution(alphabet=BinaryAlphabet(),rate.list=list("0->1"=1,"1->0"=2))
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
	class="GeneralSubstitution", 
	function(
		object,
		...
	){

		this<-object;	
		.addSummaryNameId(this);
		.addSummaryAlphabet(this);
		
		if(is.null(this$.summary$"Unscaled rate matrix")){
			this$.summary$"Unscaled rate matrix"<-paste( "\n\t",paste(capture.output(print(this$.q.matrix)),collapse="\n\t"),"\n",sep="");
		}
		this$.summary$"Equilibrium distribution"<-paste( "\n\t",paste(capture.output(print(this$.equ.dist)),collapse="\n\t"),"\n",sep="");
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: clone
##
###########################################################################/**
#
# @RdocMethod clone
# 
# @title "Clone a GeneralSubstitution object" 
# 
# \description{ 
#	@get "title".
#
#	This method also clones the aggregated QMatrix object, but not the aggregated Alphabet
#	object, as that is a good target for recycling.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GeneralSubstitution object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A GeneralSubstitution object.
# } 
# 
# \examples{
#	# create a GeneralSubstitution object
#	p<-GeneralSubstitution(
#                           alphabet=BinaryAlphabet(),
#                           rate.list=list("0->1"=1,"1->0"=2),
#                           name="MyBinary"
#                           )
#	# clone p
#	pp<-clone(p)
#	# do some checks
#	p;pp
#	p == p
#	p == pp
#	equals(p$qMatrix, pp$qMatrix)
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
  "clone",
  class="GeneralSubstitution",
  function(
    this,
    ...
  ){

	# Clone the process object:
	that<-clone.Object(this);
	# Disable write protection:
      	if(that$writeProtected){
        	that$writeProtected<-FALSE;
      	}

	# Clone Q matrix object:
	that$.q.matrix<-clone(this$.q.matrix);
	that$.q.matrix$.process<-that;

	# Reassingning name to force Id update:
	that$name<-that$name;
	return(that);

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
# @title "Create a bubble plot of the substitution process" 
# 
# \description{ 
#	@get "title".
#
#	Bubble plots (\url{http://biowiki.org/BubblePlots}) visualize the characteristics of the 
#	substitution process. The area of the circles is proportional to the rates/probabilities.
#	The plot is not produced if the rate matrix or the equlibrium 
#	distribution has undefined elements.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{An object inheriting from GeneralSubstitution.} 
#	\item{scale}{A scale factor affecting the area of the circles.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The process object (invisible).
# } 
# 
# \examples{
#	plot(BinarySubst(rate.list=list("0->1"=1,"1->0"=1.5)))
#	plot(JC69())
#	# get smaller circles
#	plot(JC69(),scale=0.5)
#	plot(F84(base.freqs=c(3/6,1/6,1/6,1/6)))
#	plot(WAG())
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
  class="GeneralSubstitution",
  function(
    x,
    scale=1,
    ...
  ){

	if(!is.numeric(scale)){
		throw("Scale parameter must be numeric!");
	}
	
	if(scale <= 0){
		throw("Scale parameter must be positive!");
	}

	if(hasUndefinedRate(x))	{
		throw("Cannot plot process: the rate matrix has undefined elements!");
	}

	if(any(is.na(x$equDist)))	{
		throw("Cannot plot process: the equilibrium distribution has undefined elements!");
	}

	qmat<-x$.q.matrix$scaledMatrix;
	# setting up viewports
	point_scale<-40.0;
	
	grid.newpage();

	size<-dim(qmat)[1];
	dsize<-(max(c(1/size,( (0.23 * size - 0.65)/size ) )));

	layout<-grid.layout(nrow=2,ncol=1,heights=c((1 - dsize), dsize),respect=TRUE);

	vp1<-viewport(layout=layout,layout.pos.row=1,layout.pos.col=1);
	vp2<-viewport(layout=layout,layout.pos.row=2,layout.pos.col=1);
	
	pushViewport(vp1);
	# tabulate rates	
	xx<-c();
	yy<-c();
	zz<-c();	

	for(cl in (colnames(qmat))){
		for(rw in (rownames(qmat))){
			if(rw != cl){
				xx<-c(xx,cl)
				yy<-c(yy,rw)
				zz<-c(zz,qmat[as.character(rw), as.character(cl)]);
			}
		}
	}


	# visual aspect tuned by "magic" formulas :)
	my.plot<-(qplot(x=xx,y=yy,size=zz,xlab="To:",ylab="From:",main="Rate matrix") + geom_point(colour="blue") +
	scale_size_area(limits=c(0,max(zz)), name="Size:")
	) + xlim(colnames(qmat)) + ylim(rev(rownames(qmat)));
	print(my.plot, vp=vp1);
	popViewport(1);

	# equlibrium distribution	
	dist<-x$equDist;
	xx<-c();
	yy<-c();
	zz<-c();
	
	for(cl in colnames(dist)){
		xx<-c(xx, cl);
		yy<-c(yy, 1);
		zz<-c(zz,dist[1,as.character(cl)]);
	}

	pushViewport(vp2);
	fr<-max(zz) - min(zz);	
	# visual aspect tuned by "magic" formulas :)
	my.plot<-(qplot(x=xx,y=yy,size=zz,xlab="Symbol",ylab="Prob:",main="Equlibrium distribution") + geom_point(colour="green") + 
	scale_size_area(limits=c(0,max(zz)), name="Size:",breaks=c(min(zz),min(zz) + fr*(1/3),min(zz) + fr*(2/3),max(zz))) + xlim(xx)
	);
	print(my.plot,vp=vp2);
	popViewport(1);

	return(invisible(x));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

