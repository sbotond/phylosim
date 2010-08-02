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
#
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{alphabet}{The alphabet on which the process acts (Alphabet object).}
#	\item{rate.list}{A list with the substitution rates. It will be passed to \code{setRateList}.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#
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
				setState(getSite(event), strsplit(event$name,split="->")[[1]][[2]]);
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

	if(missing(target.site)) {
      		throw("No target site provided!\n");
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

	state<-target.site$.state;
	# Just return an empty list if the state is NA:
	if(is.na(state)){
		return(list());
	}

	symbols<-this$.alphabet$.symbols;
	rest<-symbols[ which(symbols != state) ];
	# Generate the names of the possible events:
	event.names<-paste(state,rest,sep="->");
	
	# The rate of the event is the product of the general rate and the
     	# site specific rate multiplier:
     	rate.multiplier<-target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value;

	# Create the event objects:
	events<-list();
	for(new.state in rest){
		
		# Return empty list if the rate multiplier is zero.
     		if(rate.multiplier == 0 ) {
      			return(list());
     		}	
				
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
     		# Set the target state object (good for consistency):
     		event$.target.state<-state;
			
		# Set the event rate:	
		event$.rate<-(rate.multiplier * (this$.q.matrix$.rate.matrix[as.character(state),as.character(new.state)]));
		# Set the handler for the substitution event:
     		event$.handler<-this$.handler.template;
   		# Write protect the event object:
    		event$.write.protected<-TRUE;
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
    if(!is.Alphabet(this$alphabet)){
      	throw("Cannot set equilibrium distribution because the alphabet is undefined!");
    }
    if(missing(value)) {
      throw("No new value provided!\n");}
    else if(!is.numeric(value)) {
      throw("The new value must be numeric!\n");
    }
    else if(length(value) != this$alphabet$size){
      throw("The new value must be a vector of length ",this$alphabet$size,"!\n");
    }
    else if(!PSRoot$my.all.equal(sum(value), 1.0)) {
				value<-(value/sum(value));
				if (silent == FALSE){
					warning("The provided probabilities were rescaled in order to sum to one!\n");
				}
    }

		# Check if the provided equlibrium distribution is
		# compatible with the rate matrix:
		 if( !.checkEquMatCompat(this, rbind(value)) & force==FALSE){
				throw("The provided equlibrium distribution: ",paste(value,collapse=" ")," is not compatible with the rate matrix! Use force=TRUE to set it anyway!\n");
			}
			
			# Set the value:
      this$.equ.dist<-rbind(value);
			# Set dimnames:
      colnames(this$.equ.dist)<-(this$alphabet$symbols);
      rownames(this$.equ.dist)<-c("Prob:");
			return(invisible(this));


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
			warning("Rate matrix has zero entries!\n");
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
			dist<-as.real(eigen$vectors[,index]);
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
## Method: sampleState;
##	
setMethodS3(
	"sampleState", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		if(any(is.na(this$.equ.dist))){
			throw("Cannot sample state because the equlibrium distribution is not defined!\n");
		}
		else if (!is.Alphabet(this$.alphabet)){
			throw("Cannot sample state because the alphabet is not valid! That is strange as equlibrium distribution is defined!\n");	
		}
		else {
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
setMethodS3(
	"setQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
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
		else {
			this$.q.matrix<-value;
		}

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
setMethodS3(
	"setAlphabet", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if (!is.Alphabet(value)){
			throw("Alphabet object is invalid!\n");
		} else {
			# Set the QMatrix alphabet
			if(is.QMatrix(this$.q.matrix)){
				setAlphabet(this$.q.matrix, value);
			}
			this$.alphabet<-value;
		}	

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
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
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

      if(missing(site)){
        throw("No site provided");
      }
      else if (!isAttached(site, process=this)){
        throw("The process is not attached to the specified site!\n");
      }

      glbal.rate<-numeric();
			
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

      return(global.rate * getParameterAtSite(this, site, "rate.multiplier")$value );

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
		
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
		}
		else if(!missing(name) & missing(from) & missing(to)){
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
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot set rate as the rate matrix is undefined!\n");
		}
		else if(!missing(name) & missing(from) & missing(to)){
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
setMethodS3(
	"getRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
		else {
			return(getRateList(this$.q.matrix));
		}


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
setMethodS3(
	"setRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
		else if(missing(value)){
			throw("No new rate list specified!\n");
		}
		else {
			return(setRateList(this$.q.matrix, value) );
		}

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
setMethodS3(
	"rescaleQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

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
		else {
			
			# Set rescaling constant to zero:
			K <- 0; 
			# Check for alphabet mismatch:
			if(this$alphabet != this$.q.matrix$.alphabet){
				throw("The process alphabet and the QMatrix alphabet is not the same! Refusing to rescale!\n");
			}
			# get the symbols:
			symbols<-this$alphabet$symbols;
			
			# For every symbol:
			for (i in symbols) {
		  	# Get the equlibrium probability:
				i.equ<-this$.equ.dist[[ which(colnames(this$.equ.dist) == i) ]];
				for(j in symbols){
					if(i == j){next}
					# For every other symbol - update the constant:
					K <- K + (i.equ * getRate(this$.q.matrix, from=i, to=j ) );
				}
			}
	
    		Scale(this$.q.matrix,constant=(1/K));
		# After rescaling the expected rate of substitutions per site
		# at equlibrium is 1.
		return(invisible(TRUE));

		}
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
## Method: print
##	
setMethodS3(
	"as.character",
	class="GeneralSubstitution", 
	function(
		x,
		...
	){

		x$id;

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
#       a<-GeneralSubstitution()
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
setMethodS3(
  "clone",
  class="GeneralSubstitution",
  function(
    this,
    value,
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




