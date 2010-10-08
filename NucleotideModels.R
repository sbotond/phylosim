##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass UNREST
# 
# @title "The UNREST class"
# 
# \description{ 
#	This class implements the UNRESTricted nucleotide substitution model.
#	UNREST objects are basically a GeneralSubstitution process acting on a 
#	nucleotide alphabet.
#
#	@classhierarchy
# }
# \references{
# Yang, Z (1994) Estimating the pattern of nucleotide substitution - Journal of Molecular Evolution 
# 39:105-111 \url{http://bit.ly/aFO0cq}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.list}{A list of unscaled rates (see \code{setRateList.GeneralSubstitution}).}
#	\item{equ.dist}{Equlibrium distribution.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#       p<-UNREST(rate.list=list(
#		"T->C"=1, "T->A"=2, "T->G"=3, "C->T"=4, "C->A"=1, 
#		"C->G"=2, "A->T"=3, "A->C"=4, "A->G"=1, "G->T"=2,
#		"G->C"=3, "G->A"=4	
#	))
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GeneralSubstitution GTR
# }
# 
#*/###########################################################################
setConstructorS3(
  "UNREST",
  function( 
		name="Anonymous", # name of the object
		rate.list=NA,	  # list of unscaled rates
		equ.dist=NA,      # equlibrium distribution
		... 
		)	{

		got.rate.list<-!missing(rate.list);
		got.equ.dist<-!missing(equ.dist);
		
			this<-NA;
			# Got rate list and equlibrium distribution:
			if(got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					rate.list=rate.list,
					equ.dist=equ.dist
				);	
				this<-extend(this, "UNREST");
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					rate.list=rate.list
				);	
				this<-extend(this, "UNREST");
			}
			
			# Got equlibrium distribution,
			# we set it, but it will be owerwritten anyway.
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					equ.dist=equ.dist
				);	
				this<-extend(this, "UNREST");
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet()
				);	
				this<-extend(this, "UNREST");
			}

		# Force clearing id cache:		
		this$name<-this$name;
	
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
	class="UNREST", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
				if(!inherits(this$alphabet, "NucleotideAlphabet")){
					throw("This object must have as alphabet a NucleotideAlphabet object!\n");
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
#       p<-UNREST(rate.list=list(
#		"T->C"=1, "T->A"=2, "T->G"=3, "C->T"=4, "C->A"=1, 
#		"C->G"=2, "A->T"=3, "A->C"=4, "A->G"=1, "G->T"=2,
#		"G->C"=3, "G->A"=4	
#	))
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
	class="UNREST", 
	function(
		object,
		...
	){

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Constructor: JC69
##	
##########################################################################/** 
#
# @RdocClass JC69
# 
# @title "The JC69 class"
# 
# \description{ 
#	This class implements Jukes-Cantor nucleotide substitution model.
#
#	@classhierarchy
# }
#
# \references{
#	Jukes, TH and Cantor, CR (1969) Evolution of protein molecules. Pp. 21-123 in H. N. Munro, 
#	ed. Mammalian protein metabolism. Academic Press, New York.
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#       p<-JC69()
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GeneralSubstitution UNREST GTR
# }
# 
#*/###########################################################################
setConstructorS3(
  "JC69",
  function( 
		name="Anonymous", # object name
		... 
		)	{
	
		# Set all rates to be equal.	
		this<-UNREST(rate.list=list(
  		"A->T"=1,
  		"A->C"=1,
  		"A->G"=1,
  		"T->A"=1,
  		"T->C"=1,
  		"T->G"=1,
  		"C->A"=1,
  		"C->T"=1,
  		"C->G"=1,
  		"G->A"=1,
  		"G->T"=1,
  		"G->C"=1
		));
		
		this<-extend(this,"JC69");
		this$name<-name;
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
	class="JC69", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {

					qmat<-this$.q.matrix$Matrix;	
					# This is a dumb method to check the sanity of the rates, but consistency checking
					# should not be called frequently.
					diag(qmat)<-1;
					if(any(qmat != 1)){
						throw("The unscaled rate matrix is not consistent with the JC69 model!\n");
					}

					if ( !all.equal(as.numeric(this$.equ.dist), as.numeric(rep(0.25,times=4)) ) ){
						throw("The equlibrium distribution of the JC69 model should be uniform!\n");
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
#       p<-JC69()
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
	class="JC69", 
	function(
		object,
		...
	){

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


#
# Constructor: GTR
#
##########################################################################/** 
#
# @RdocClass GTR
# 
# @title "The GTR class"
# 
# \description{ 
#	This class implements the general time-reversible nucleotide substitution model (GTR, REV).
#	The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
#
#	The default value for the rate parameters is 1 and the default value for the base 
#	frequencies is 0.25. So the GTR objects are equivalent to JC69 objects by default.
#
#	@classhierarchy
# }
# \references{
#	Tavare, S (1986) "Some Probabilistic and Statistical Problems in the Analysis of DNA Sequences". 
#	American Mathematical Society: Lectures on Mathematics in the Life Sciences 17:57-86
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{A list of unscaled rates (see \code{setRateList.GeneralSubstitution}).}
#	\item{base.freqs}{Equlibrium distribution.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-GTR(
#		rate.params=list(
#			"a"=1, "b"=2, "c"=3,
#			"d"=1, "e"=2, "f"=3
#		),
#		base.freqs=c(2,2,1,1)/6
#	)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object and run simulation
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GeneralSubstitution UNREST HKY
# }
# 
#*/###########################################################################
setConstructorS3(
  "GTR",
  function( 
		name="Anonymous", 
		# The GTR rate parameters:
		rate.params=list(
				"a"=1,
				"b"=1,
				"c"=1,
				"d"=1,
				"e"=1,
				"f"=1
		),	
		# Base frequencies (equlibrium distribution):
		base.freqs=rep(0.25,times=4),
		... 
		)	{

		this<-UNREST();

		this<-extend(
			this,
			"GTR",
			.gtr.params=list(
					"a"=NA,
					"b"=NA,
					"c"=NA,
					"d"=NA,
					"e"=NA,
					"f"=NA
				)
		);
		this$name<-name;
		setEquDist(this,value=base.freqs,force=TRUE)	
		setRateParamList(this,value=rate.params);

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
	class="GTR", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
			# All rate parameters should be positive:					
			if (any( as.numeric(this$.gtr.params) < 0 ) ){
				throw("Found negative GTR rate parameters!\n");
			}
			else {
			
				rates<-this$.q.matrix$Matrix;
		
						if (	
                 !PSRoot$my.all.equal( rates[["T","C"]] , (this$.gtr.params[["a"]] * this$.equ.dist[1,"C"] ) )|
                 !PSRoot$my.all.equal( rates[["C","T"]] , (this$.gtr.params[["a"]] * this$.equ.dist[1,"T"] )	)|
                 !PSRoot$my.all.equal( rates[["T","A"]] , (this$.gtr.params[["b"]] * this$.equ.dist[1,"A"] )	)|
                 !PSRoot$my.all.equal( rates[["A","T"]] , (this$.gtr.params[["b"]] * this$.equ.dist[1,"T"] )	)|
                 !PSRoot$my.all.equal( rates[["T","G"]] , (this$.gtr.params[["c"]] * this$.equ.dist[1,"G"] )	)|
                 !PSRoot$my.all.equal( rates[["G","T"]] , (this$.gtr.params[["c"]] * this$.equ.dist[1,"T"] )	)|
		 !PSRoot$my.all.equal( rates[["C","A"]] , (this$.gtr.params[["d"]] * this$.equ.dist[1,"A"] )	)|
                 !PSRoot$my.all.equal( rates[["A","C"]] , (this$.gtr.params[["d"]] * this$.equ.dist[1,"C"] )	)|
                 !PSRoot$my.all.equal( rates[["C","G"]] , (this$.gtr.params[["e"]] * this$.equ.dist[1,"G"] )	)|
                 !PSRoot$my.all.equal( rates[["G","C"]] , (this$.gtr.params[["e"]] * this$.equ.dist[1,"C"] )	)|
                 !PSRoot$my.all.equal( rates[["A","G"]] , (this$.gtr.params[["f"]] * this$.equ.dist[1,"G"] )	)|
                 !PSRoot$my.all.equal( rates[["G","A"]] , (this$.gtr.params[["f"]] * this$.equ.dist[1,"A"] )	)
						) {
						
								throw("Rate matrix is not consistent with the GTR rate parameters!\n");
						
						}

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
## Method: .getRateParam
##	
setMethodS3(
	".getRateParam", 
	class="GTR", 
	function(
		this,
		name,
		param.list,
		...
	){

		if(length(intersect(name,names(param.list))) == 0){
			throw("The specified rate parameter name is not valid!\n");
		}
		else {
			return(param.list[[name]]);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
#	\item{name}{The name of the rate parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# construct a GTR object
#	p<-GTR();
#	# set/get a rate parameter
#	setRateParam(p,"a",4)
#	getRateParam(p,"a")
#	# get object summary
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
	"getRateParam", 
	class="GTR", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.gtr.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .setRateParam
##	
setMethodS3(
	".setRateParam", 
	class="GTR", 
	function(
		this,
		name,
		value,
		param.list,
		...
	){

		.checkWriteProtection(this);

	if(!exists(x="PSIM_FAST")){

		if(length(intersect(name,names(param.list))) == 0){
			throw("The specified rate parameter name is not valid!\n");
		}
		else if(missing(value)){
			throw("No new value given!\n")
		}
		else if(length(value) != 1|any(!is.numeric(value))){
			throw("The new value must be a numeric vector of length 1!\n");	
		}
		else if(any(is.na(this$.equ.dist))){
			throw("Cannot set rate parameter because the nucleotide frequencies are not defined properly!\n");
		}
	}
		param.list[[name]]<-value;
				
		# We call setRateParamList to rebuild the whole rate
		# matrix with the new values if one of the rates changed:
		setRateParamList(this, param.list);
									

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
#	\item{name}{The name of the rate parameter.}
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#	# construct a GTR object
#	p<-GTR();
#	# set/get a rate parameter
#	setRateParam(p,"a",4)
#	getRateParam(p,"a")
#	# get object summary
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
	"setRateParam", 
	class="GTR", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.gtr.params);
		}
		return(invisible(value));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A list of rate parameters.
# } 
# 
# \examples{
#	# create GTR object
#	p<-GTR()
#	# set/get rate parameters
#	setRateParamList(p,list(
#                       "a"=1, "b"=2, "c"=3,
#                       "d"=1, "e"=2, "f"=3
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#                       "a"=4, "b"=1, "c"=4,
#                       "d"=1, "e"=4, "f"=1
#        )
#	p$rateParamList
#	# get object summary
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
	"getRateParamList", 
	class="GTR", 
	function(
		this,
		...
	){

		this$.gtr.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkRateParamList
##	
setMethodS3(
	".checkRateParamList", 
	class="GTR", 
	function(
		this,
		names,
		value.names,
		...
	){

		# Check for illegal rate parameter names:
		if(length((illegal<-setdiff(value.names, names))) != 0){
			throw("The following rate parameter names are illegal: ",paste(illegal, collapse=", ")," !\n");
		}
		else {
			missing<-setdiff(names, value.names);
			if(length(missing) > 0) {
				throw("Cannot build the model because the following rate parameters are missing: ",paste(missing,collapse=", ")," \n");	
		} else {
					return(TRUE);
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
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
#	\item{value}{A list containing the rate parameters.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters (invisible).
# } 
# 
# \examples{
#	# create GTR object
#	p<-GTR()
#	# set/get rate parameters
#	setRateParamList(p,list(
#                       "a"=1, "b"=2, "c"=3,
#                       "d"=1, "e"=2, "f"=3
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#                       "a"=4, "b"=1, "c"=4,
#                       "d"=1, "e"=4, "f"=1
#        )
#	p$rateParamList
#	# get object summary
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
	"setRateParamList", 
	class="GTR", 
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
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	
	}
	
	# Get the rate parameter names:
	names<-names(this$.gtr.params);
	value.names<-names(value);

	if(.checkRateParamList(this,names,value.names)) {
	
	# Set the rate parameters:
	# The parmeters are named as in 
	# "Ziheng Yang: Computational Molecular Evolution, 
	# Oxford university Press, Oxford, 2006", pp. 34.

	rate.list=list(

                	"T->C"=(value[["a"]] * this$.equ.dist[1,"C"] ),
                	"C->T"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                	"T->A"=(value[["b"]] * this$.equ.dist[1,"A"] ),
                	"A->T"=(value[["b"]] * this$.equ.dist[1,"T"] ),
                	"T->G"=(value[["c"]] * this$.equ.dist[1,"G"] ),
                	"G->T"=(value[["c"]] * this$.equ.dist[1,"T"] ),
			"C->A"=(value[["d"]] * this$.equ.dist[1,"A"] ),
			# Can you spot the pattern here: "A->C" .* "d" .* "c" :)
                	"A->C"=(value[["d"]] * this$.equ.dist[1,"C"] ),
                	"C->G"=(value[["e"]] * this$.equ.dist[1,"G"] ),
                	"G->C"=(value[["e"]] * this$.equ.dist[1,"C"] ),
                	"A->G"=(value[["f"]] * this$.equ.dist[1,"G"] ),
                	"G->A"=(value[["f"]] * this$.equ.dist[1,"A"] )

        );
	# Setting the parameter field:
	this$.gtr.params<-value;
	# Calling setRateList, which will set the 
	# elements of the rate matrix.
	setRateList(this,rate.list);
	}
	return(invisible(value));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequencies.
# } 
# 
# \examples{
#	# construct a GTR object
#	p<-GTR()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"getBaseFreqs", 
	class="GTR", 
	function(
		this,
		...
	){

		# Its just the .equ.dist field from UNREST.
		this$.equ.dist;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
# 
# @title "Set the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GTR object.} 
#	\item{value}{A vector of base frequencies.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible)
# } 
# 
# \examples{
#	# construct a GTR object
#	p<-GTR()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"setBaseFreqs", 
	class="GTR", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		setEquDist(this,value,force=TRUE);
		setRateParamList.GTR(this,value=this$.gtr.params);
		return(invisible(value));
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.GTR
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
#       a<-GTR()
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
	class="GTR", 
	function(
		object,
		...
	){

		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "GTR") {
		this$.summary$"Rate parameters"<-paste(names(this$.gtr.params),this$.gtr.params,sep=" = ",collapse=", ");
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of GTR methods ############

##	
## Constructor: TN93
##	
## Tamura, K., and M. Nei. 1993. Estimation of the number of nucleotide substitutions 
## in the control region of mitochondrial DNA in humans and chimpanzees. 
## Molecular Biology and Evolution 10:512-526.
##
##########################################################################/** 
#
# @RdocClass TN93
# 
# @title "The TN93 class"
# 
# \description{ 
#	This class implements the Tamura-Nei 93 GTR-submodel.
#
#	The rate parameters are the following: "Alpha1", "Alpha2","Beta".
#	@classhierarchy
# }
# \references{
# Tamura, K, and Nei, M (1993) Estimation of the number of nucleotide substitutions 
# in the control region of mitochondrial DNA in humans and chimpanzees -
# Molecular Biology and Evolution 10:512-526 \url{http://bit.ly/bNkCqn}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
#	\item{base.freqs}{Base frequency parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-TN93(rate.params=list( "Alpha1"=4,"Alpha2"=3,"Beta"=2),
#		base.freqs=c(2,2,1,3)/9
#	)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR HKY UNREST GeneralSubstitution
# }
# 
#*/###########################################################################
setConstructorS3(
  "TN93",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha1"  =1,
      				"Alpha2"  =1,
      				"Beta"    =1
			),
		base.freqs=c(0.25,0.25,0.25,0.25),
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"TN93",
			.tn93.params=list(
					"Alpha1"	=NA,
					"Alpha2"	=NA,
					"Beta"		=NA
				)
			);

		this$name<-name;
		this$baseFreqs<-base.freqs;
		this$rateParamList<-rate.params;

		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha1, Alpha2, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters.
# } 
# 
# \examples{
#	# create TN93 object
#	p<-TN93()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha1"=1,
#		"Alpha2"=2,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha1"=1,
#		"Alpha2"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"getRateParamList", 
	class="TN93", 
	function(
		this,
		...
	){

		this$.tn93.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha1, Alpha2, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
#	\item{value}{A list containing the rate parameters.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters (invisible).
# } 
# 
# \examples{
#	# create TN93 object
#	p<-TN93()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha1"=1,
#		"Alpha2"=2,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha1"=1,
#		"Alpha2"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"setRateParamList", 
	class="TN93", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.tn93.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				this$.tn93.params<-value;
				# Setting the GTR rate parameters:
				gtr.params<-list(
					"a"=value[["Alpha1"]],
					"b"=value[["Beta"]],
					"c"=value[["Beta"]],
					"d"=value[["Beta"]],
					"e"=value[["Beta"]],
					"f"=value[["Alpha2"]]
				);
				setRateParamList.GTR(this, value=gtr.params);

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
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha1, Alpha2, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
#	\item{name}{The name of the rate parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# construct a TN93 object
#	p<-TN93();
#	# set/get a rate parameter
#	setRateParam(p,"Beta",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"getRateParam", 
	class="TN93", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.tn93.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha1, Alpha2, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
#	\item{name}{The name of the rate parameter.}
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#	# construct a TN93 object
#	p<-TN93();
#	# set/get a rate parameter
#	setRateParam(p,"Beta",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"setRateParam", 
	class="TN93", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.tn93.params);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequencies.
# } 
# 
# \examples{
#	# construct a TN93 object
#	p<-TN93()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"getBaseFreqs", 
	class="TN93", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
# 
# @title "Set the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A TN93 object.} 
#	\item{value}{A vector of base frequencies.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible)
# } 
# 
# \examples{
#	# construct a TN93 object
#	p<-TN93()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"setBaseFreqs", 
	class="TN93", 
	function(
		this,
		value,
		...
	){

		setBaseFreqs.GTR(this,value);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: checkConsistency.TN93
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
	class="TN93", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
				# Rate parameters should not be negative:
				if(any(this$.tn93.params < 0 )){
					throw("Found negative TN93 rate parameters!\n");
				}
				else {

					if(
				  	this$.gtr.params[["a"]]!=this$.tn93.params[["Alpha1"]]|
          	this$.gtr.params[["b"]]!=this$.tn93.params[["Beta"]]|
          	this$.gtr.params[["c"]]!=this$.tn93.params[["Beta"]]|
          	this$.gtr.params[["d"]]!=this$.tn93.params[["Beta"]]|
          	this$.gtr.params[["e"]]!=this$.tn93.params[["Beta"]]|
          	this$.gtr.params[["f"]]!=this$.tn93.params[["Alpha2"]]
						) {
							throw("The TN93 parameters are not consistent with the GTR parameters!\n");
						}
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
## Method: summary.TN93
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
	class="TN93", 
	function(
		object,
		...
	){
		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "TN93") {
		this$.summary$"Rate parameters"<-paste(names(this$.tn93.params),this$.tn93.params,sep=" = ",collapse=", ");
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of TN93 methods ############

##	
## Constructor: HKY
##	
## Hasegawa, M., H. Kishino, and T. Yano. (1985) Dating of human-ape splitting by a molecular clock
## of mitochondrial DNA. Journal of Molecular Evolution, 22, 160-174.
##
##########################################################################/** 
#
# @RdocClass HKY
# 
# @title "The HKY class"
# 
# \description{ 
#	This class implements the HKY GTR-submodel.
#
#	The rate parameters are the following: "Alpha", "Beta".
#	@classhierarchy
# }
# \references{
# Hasegawa, M Kishino, H and Yano, T (1985) Dating of human-ape splitting by a molecular clock
# of mitochondrial DNA Journal of Molecular Evolution 22:160-174 \url{http://bit.ly/a9AxKm}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
#	\item{base.freqs}{Base frequency parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-HKY(rate.params=list( "Alpha"=10,"Beta"=2),
#		base.freqs=c(4,3,2,1)/10
#	)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution TN93
# }
# 
#*/###########################################################################
setConstructorS3(
  "HKY",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"   =1,
      				"Beta"    =1
			),	
		base.freqs=c(0.25,0.25,0.25,0.25),
			... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"HKY",
			.hky.params=list(
					"Alpha"	 	=NA,
					"Beta"		=NA
				)
			);

		this$name<-name;
		this$baseFreqs<-base.freqs;
		this$rateParamList<-rate.params;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters.
# } 
# 
# \examples{
#	# create HKY object
#	p<-HKY()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"getRateParamList", 
	class="HKY", 
	function(
		this,
		...
	){

		this$.hky.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
#	\item{value}{A list containing the rate parameters.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters (invisible).
# } 
# 
# \examples{
#	# create HKY object
#	p<-HKY()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"setRateParamList", 
	class="HKY", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.hky.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				this$.hky.params<-value;
				# Setting the GTR rate parameters:
				gtr.params<-list(
					"a"=value[["Alpha"]],
					"b"=value[["Beta"]],
					"c"=value[["Beta"]],
					"d"=value[["Beta"]],
					"e"=value[["Beta"]],
					"f"=value[["Alpha"]]
				);
				setRateParamList.GTR(this, value=gtr.params);

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
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
#	\item{name}{The name of the rate parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters.
# } 
# 
# \examples{
#	# construct HKY object
#	p<-HKY();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"getRateParam", 
	class="HKY", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.hky.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
#	\item{name}{The name of the rate parameter.}
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#	# construct HKY object
#	p<-HKY();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"setRateParam", 
	class="HKY", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.hky.params);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequency parameters.
# } 
# 
# \examples{
#	# construct object
#	p<-HKY()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"getBaseFreqs", 
	class="HKY", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
# 
# @title "Set the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An HKY object.} 
#	\item{value}{A vector of base frequencies.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible)
# } 
# 
# \examples{
#	# construct object
#	p<-HKY()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"setBaseFreqs", 
	class="HKY", 
	function(
		this,
		value,
		...
	){

		setBaseFreqs.GTR(this,value);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.HKY
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
	class="HKY", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# Rate parameters should not be negative:
				if(any(this$.hky.params < 0 )){
					throw("Found negative HKY rate parameters!\n");
				}
				else {

					if(
				  	this$.gtr.params[["a"]]!=this$.hky.params[["Alpha"]]|
          	this$.gtr.params[["b"]]!=this$.hky.params[["Beta"]]|
          	this$.gtr.params[["c"]]!=this$.hky.params[["Beta"]]|
          	this$.gtr.params[["d"]]!=this$.hky.params[["Beta"]]|
          	this$.gtr.params[["e"]]!=this$.hky.params[["Beta"]]|
          	this$.gtr.params[["f"]]!=this$.hky.params[["Alpha"]]
						) {
							throw("The HKY parameters are not consistent with the GTR parameters!\n");
						}
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
## Method: summary.HKY
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
	class="HKY", 
	function(
		object,
		...
	){
		
		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "HKY") {
			this$.summary$"Rate parameters"<-paste(names(this$.hky.params),this$.hky.params,sep=" = ",collapse=", ");
		}
			this$.summary$"Transition/transversion rate ratio"<-(this$.hky.params[["Alpha"]]/this$.hky.params[["Beta"]]);

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of HKY methods ############

##	
## Constructor: F81
##	
## Felsenstein, J. (1981) Evolutionary trees from DNA sequences: a maximum likelihood approach.
## Journal of Molecular Evolution, 17, 368-376.
##
##########################################################################/** 
#
# @RdocClass F81
# 
# @title "The F81 class"
# 
# \description{ 
#	This class implements the F81 GTR-submodel.
#
#	@classhierarchy
# }
# \references{
# Felsenstein, J (1981) Evolutionary trees from DNA sequences: a maximum likelihood approach -
# Journal of Molecular Evolution 17:368-376 \url{http://dx.doi.org/10.1007/BF01734359}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{base.freqs}{Base frequency parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-F81(base.freqs=c(1,2,3,4)/10)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution HKY
# }
# 
#*/###########################################################################
setConstructorS3(
  "F81",
  function( 
		name="Anonymous",
		base.freqs=c(0.25,0.25,0.25,0.25),
		...
		)	{
		
		this<-GTR(...);
		
		this<-extend(
			this,
			"F81"
			);

		this$name<-name;
		this$baseFreqs<-base.freqs;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
#
# @title "Forbidden action: getting the list of rate parameters"
#
# \description{
#       @get "title".
#	
#	This model has no rate parameters.
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
#	The object.
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
	"getRateParamList", 
	class="F81", 
	function(
		this,
		...
	){

		cat("The F81 model has no rate parameters!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
#
# @title "Forbidden action: getting the list of rate parameters"
#
# \description{
#       @get "title".
#	
#	This model has no rate parameters.
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
#	The object.
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
	"setRateParamList", 
	class="F81", 
	function(
		this,
		value,
		...
	){

		cat("The F81 model has no rate parameters!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
#
# @title "Forbidden action: the value of a rate parameters"
#
# \description{
#       @get "title".
#	
#	This model has no rate parameters.
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{name}{The name of the rate parameter.}
#       \item{...}{Not used.}
# }
#
# \value{
#	The object.
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
	"getRateParam", 
	class="F81", 
	function(
		this,
		name,
		...
	){

		cat("The F81 model has no rate parameters!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
#
# @title "Forbidden action: getting the list of rate parameters"
#
# \description{
#       @get "title".
#	
#	This model has no rate parameters.
# }
#
# @synopsis
#
# \arguments{
#       \item{this}{An object.}
#       \item{name}{Not used.}
#       \item{value}{Not used.}
#       \item{...}{Not used.}
# }
#
# \value{
#	The object.
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
	"setRateParam", 
	class="F81", 
	function(
		this,
		name,
		value,
		...
	){

		cat("The F81 model has no rate parameters!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A F81 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequency parameters.
# } 
# 
# \examples{
#	# construct a F81 object
#	p<-F81()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"getBaseFreqs", 
	class="F81", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
# 
# @title "Set the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A F81 object.} 
#	\item{value}{A vector of base frequencies.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible)
# } 
# 
# \examples{
#	# construct a F81 object
#	p<-F81()
#	# set/get base frequency parameters
#	setBaseFreqs(p,c(2,1,2,1)/6)
#	getBaseFreqs(p)
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs<-c(4,4,1,1)/10
#	p$baseFreqs
#	# get object summary
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
	"setBaseFreqs", 
	class="F81", 
	function(
		this,
		value,
		...
	){

		setBaseFreqs.GTR(this,value);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.F81
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
	class="F81", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {

				if(any(as.numeric(this$.gtr.params) != 1)){
					throw("GTR parameters are not consistent with the F81 model!\n");
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
## Method: summary.F81
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
	class="F81", 
	function(
		object,
		...
	){

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


######### end of F81 methods ############

##	
## Constructor: K80
##
## Kimura, M. (1980) A simple method for estimating evolutionary rates of base substitutions 
## through comparative studies of nucleotide sequences. Journal of Molecular Evolution, 16, 111-120.
##
##########################################################################/** 
#
# @RdocClass K80
# 
# @title "The K80 class"
# 
# \description{ 
#	This class implements the K80 (Kimura 2-parameter) GTR-submodel.
#
#	The rate parameters are the following: "Alpha", "Beta".
#	@classhierarchy
# }
# \references{
#	Kimura, M. (1980) A simple method for estimating evolutionary rates of base substitutions 
#	through comparative studies of nucleotide sequences. Journal of Molecular Evolution 16:111-120
#	\url{http://dx.doi.org/10.1007/BF01731581}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-K80(rate.params=list( "Alpha"=6,"Beta"=2),
#		base.freqs=c(4,3,2,1)/10
#	)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution TN93
# }
# 
#*/###########################################################################
setConstructorS3(
  "K80",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"   =1,
      				"Beta"    =1
			),
			... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"K80",
			.k80.params=list(
					"Alpha"	 	=NA,
					"Beta"		=NA
				)
			);

		this$name<-name;
		this$rateParamList<-rate.params;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K80 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters.
# } 
# 
# \examples{
#	# create K80 object
#	p<-K80()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"getRateParamList", 
	class="K80", 
	function(
		this,
		...
	){

		this$.k80.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K80 object.} 
#	\item{value}{A list containing the rate parameters.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters (invisible).
# } 
# 
# \examples{
#	# create K80 object
#	p<-K80()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3
#        )
#	p$rateParamList
#	# get object summary
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
	"setRateParamList", 
	class="K80", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.k80.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				this$.k80.params<-value;
				# Setting the GTR rate parameters:
				gtr.params<-list(
					"a"=value[["Alpha"]],
					"b"=value[["Beta"]],
					"c"=value[["Beta"]],
					"d"=value[["Beta"]],
					"e"=value[["Beta"]],
					"f"=value[["Alpha"]]
				);
				setRateParamList.GTR(this, value=gtr.params);

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
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K80 object.} 
#	\item{name}{The name of the rate parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# construct a K80 object
#	p<-K80();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"getRateParam", 
	class="K80", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.k80.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K80 object.} 
#	\item{name}{The name of the rate parameter.}
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#	# construct a K80 object
#	p<-K80();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Beta")
#	# get object summary
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
	"setRateParam", 
	class="K80", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.k80.params);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects. The K80 model has equal base frequencies.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K80 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequency parameters.
# } 
# 
# \examples{
#	# construct object
#	p<-K80()
#	# get base frequency parameters
#	getBaseFreqs(p) # uniform
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs # uniform
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
	"getBaseFreqs", 
	class="K80", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
#
# @title "Forbidden action: setting the base frequency parameters for a K80 object"
#
# \description{
#       @get "title".
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
	"setBaseFreqs", 
	class="K80", 
	function(
		this,
		value,
		...
	){

		# Do not allow to modify the default base frequency distribution, which is uniform.
		throw("You are not allowed to set the base frequencies for the K80 model!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.K80
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
	class="K80", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# Rate parameters should not be negative:
				if(any(this$.k80.params < 0 )){
					throw("Found negative K80 rate parameters!\n");
				}
				else {

					if(
				  		this$.gtr.params[["a"]]!=this$.k80.params[["Alpha"]]|
          					this$.gtr.params[["b"]]!=this$.k80.params[["Beta"]]|
          					this$.gtr.params[["c"]]!=this$.k80.params[["Beta"]]|
          					this$.gtr.params[["d"]]!=this$.k80.params[["Beta"]]|
          					this$.gtr.params[["e"]]!=this$.k80.params[["Beta"]]|
          					this$.gtr.params[["f"]]!=this$.k80.params[["Alpha"]]
					 ) {
							throw("The K80 parameters are not consistent with the GTR parameters!\n");
						}
						else if ( !all.equal(as.numeric(this$.equ.dist), as.numeric(rep(0.25,times=4)) ) ){
							throw("The equlibrium distribution of the K80 model should be uniform!\n");
						}
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
## Method: summary.K80
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
	class="K80", 
	function(
		object,
		...
	){

		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "K80") {
			this$.summary$"Rate parameters"<-paste(names(this$.k80.params),this$.k80.params,sep=" = ",collapse=", ");
		}
		this$.summary$"Transition/transversion rate ratio"<-(this$.k80.params[["Alpha"]]/this$.k80.params[["Beta"]]);

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of K80 methods ############

##	
## Constructor: K81
##	
## M. Kimura, Estimation of evolutionary sequences between homologous nucleotide sequences,
## Proc. Natl. Acad. Sci. USA 78 (1981), pp. 454–458.
##
##########################################################################/** 
#
# @RdocClass K81
# 
# @title "The K81 class"
# 
# \description{ 
#	This class implements the K81 (Kimura 3-parameter) GTR-submodel.
#
#	The rate parameters are the following: "Alpha", "Beta","Gamma".
#	@classhierarchy
# }
# \references{
# Kimura, M (1981) Estimation of evolutionary sequences between homologous nucleotide sequences -
# Proc. Natl. Acad. Sci. USA 78:454-458 \url{http://dx.doi.org/10.1073/pnas.78.1.454}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-K81(rate.params=list( "Alpha"=10,"Beta"=2,"Gamma"=5))
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution HKY
# }
# 
#*/###########################################################################
setConstructorS3(
  "K81",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"  	=1,
      				"Beta"    	=1,
				"Gamma"		=1
			),
			... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"K81",
			.k81.params=list(
					"Alpha"	 	=NA,
					"Beta"		=NA,
					"Gamma"		=NA
				)
			);

		this$name<-name;
		this$rateParamList<-rate.params;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta, Gamma.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K81 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters.
# } 
# 
# \examples{
#	# create K81 object
#	p<-K81()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5,
#		"Gamma"=2
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3,
#		"Gamma"=2
#        )
#	p$rateParamList
#	# get object summary
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
	"getRateParamList", 
	class="K81", 
	function(
		this,
		...
	){

		this$.k81.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#	@get "title".
#
#	The rate parameters are: Alpha, Beta, Gamma.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K81 object.} 
#	\item{value}{A list containing the rate parameters.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The list of rate parameters (invisible).
# } 
# 
# \examples{
#	# create K81 object
#	p<-K81()
#	# set/get rate parameters
#	setRateParamList(p,list(
#		"Alpha"=1,
#		"Beta"=0.5,
#		"Gamma"=2
#        ))
#	getRateParamList(p)
#	# set/get rate parameters via virtual field
#	p$rateParamList<-list(
#		"Alpha"=1,
#		"Beta"=3,
#		"Gamma"=2
#        )
#	p$rateParamList
#	# get object summary
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
	"setRateParamList", 
	class="K81", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.k81.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				this$.k81.params<-value;
				# Setting the GTR rate parameters:
				gtr.params<-list(
					"a"=value[["Alpha"]],
					"b"=value[["Beta"]],
					"c"=value[["Gamma"]],
					"d"=value[["Gamma"]],
					"e"=value[["Beta"]],
					"f"=value[["Alpha"]]
				);
				setRateParamList.GTR(this, value=gtr.params);

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
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta, Gamma.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K81 object.} 
#	\item{name}{The name of the rate parameter.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The value of the rate parameter.
# } 
# 
# \examples{
#	# construct a K81 object
#	p<-K81();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Gamma")
#	# get object summary
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
	"getRateParam", 
	class="K81", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.k81.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#	@get "title".
#
#	 The rate parameters are: Alpha, Beta, Gamma.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K81 object.} 
#	\item{name}{The name of the rate parameter.}
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#	# construct a K80 object
#	p<-K81();
#	# set/get a rate parameter
#	setRateParam(p,"Alpha",4)
#	getRateParam(p,"Gamma")
#	# get object summary
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
	"setRateParam", 
	class="K81", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.k81.params);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#	@get "title".
#
#	The order of the frequency parameters must match with the order of symbols
#	in the NucleotideAlphabet objects. The K81 model has equal base frequencies.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A K81 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the base frequency parameters.
# } 
# 
# \examples{
#	# construct object
#	p<-K81()
#	# get base frequency parameters
#	getBaseFreqs(p) # uniform
#	# set/get base frequency parameters via virtual field
#	p$baseFreqs # uniform
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
	"getBaseFreqs", 
	class="K81", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
#
# @title "Forbidden action: setting the base frequency parameters for a K81 model"
#
# \description{
#       @get "title".
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
	"setBaseFreqs", 
	class="K81", 
	function(
		this,
		value,
		...
	){

		throw("You are not allowed to set the base frequencies for the K81 model!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.K81
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
	class="K81", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# Rate parameters should not be negative:
				if(any(this$.k81.params < 0 )){
					throw("Found negative K81 rate parameters!\n");
				}
				else {

					if(
				  	this$.gtr.params[["a"]]!=this$.k81.params[["Alpha"]]|
          	this$.gtr.params[["b"]]!=this$.k81.params[["Beta"]]|
          	this$.gtr.params[["c"]]!=this$.k81.params[["Gamma"]]|
          	this$.gtr.params[["d"]]!=this$.k81.params[["Gamma"]]|
          	this$.gtr.params[["e"]]!=this$.k81.params[["Beta"]]|
          	this$.gtr.params[["f"]]!=this$.k81.params[["Alpha"]]
						) {
							throw("The K81 parameters are not consistent with the GTR parameters!\n");
						}
						else if ( !all.equal(as.numeric(this$.equ.dist), as.numeric(rep(0.25,times=4)) ) ){
							throw("The equlibrium distribution of the K81 model should be uniform!\n");
						}
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
## Method: summary.K81
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
	class="K81", 
	function(
		object,
		...
	){
		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "K81") {
		this$.summary$"Rate parameters"<-paste(names(this$.k81.params),this$.k81.params,sep=" = ",collapse=", ");
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of K81 methods ############

##	
## Constructor: T92
##	
## Tamura, K. 1992. Estimation of the number of nucleotide substitutions when
## there are strong transition-transversion and G+C content biases. Molecular Biology and Evolution 9:678-687. 
##
##########################################################################/** 
#
# @RdocClass T92
# 
# @title "The T92 class"
# 
# \description{ 
#	This class implements the T92 GTR-submodel.
#
#	The rate parameters are the following: "Alpha", "Beta","Gamma".
#	The \code{theta} virtual field stores the GC content parameter.
#
#	@classhierarchy
# }
# \references{
# Tamura, K. (1992) Estimation of the number of nucleotide substitutions when
# there are strong transition-transversion and G+C content biases - Molecular Biology and 
# Evolution 9:678-687 \url{http://bit.ly/c6Pe0q}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
#	\item{theta}{GC content (0.5 by default).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-T92(rate.params=list( "Alpha"=10,"Beta"=2),theta=0.8)
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution HKY
# }
# 
#*/###########################################################################
setConstructorS3(
  "T92",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"  	=1,
      				"Beta"    	=1
			),
		theta=0.5, # GC content
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"T92",
			.theta=NA,
			.t92.params=list(
					"Alpha"	 	=NA,
					"Beta"		=NA
				)
			);

		this$name<-name;
		this$theta<-theta;
		this$rateParamList<-rate.params;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#       @get "title".
#
#       The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The list of rate parameters.
# } 
# 
# \examples{
#       # create a T92 object
#       p<-T92()
#       # set/get rate parameters
#       setRateParamList(p,list(
#               "Alpha"=1,
#               "Beta"=0.5
#        ))
#       getRateParamList(p)
#       # set/get rate parameters via virtual field
#       p$rateParamList<-list(
#               "Alpha"=1,
#               "Beta"=3
#        )
#       p$rateParamList
#       # get object summary
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
	"getRateParamList", 
	class="T92", 
	function(
		this,
		...
	){

		this$.t92.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#       @get "title".
#
#       The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{value}{A list containing the rate parameters.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The list of rate parameters (invisible).
# } 
# 
# \examples{
#       # create a T92 object
#       p<-T92()
#       # set/get rate parameters
#       setRateParamList(p,list(
#               "Alpha"=1,
#               "Beta"=0.5
#        ))
#       getRateParamList(p)
#       # set/get rate parameters via virtual field
#       p$rateParamList<-list(
#               "Alpha"=1,
#               "Beta"=3
#        )
#       p$rateParamList
#       # get object summary
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
	"setRateParamList", 
	class="T92", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.t92.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				# Set the rate parameters:
				# The parmeters are named as in 
				# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.
			
				this$.t92.params<-value;
				# Setting the GTR rate parameters:
				gtr.params<-list(
          "a"=value[["Alpha"]],
          "b"=value[["Beta"]],
          "c"=value[["Beta"]],
          "d"=value[["Beta"]],
          "e"=value[["Beta"]],
          "f"=value[["Alpha"]]
        );
				setRateParamList.GTR(this, value=gtr.params);

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
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#       @get "title".
#
#        The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{name}{The name of the rate parameter.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A numeric vector of length one.
# } 
# 
# \examples{
#       # construct a T92 object
#       p<-T92();
#       # set/get a rate parameter
#       setRateParam(p,"Alpha",4)
#       getRateParam(p,"Beta")
#       # get object summary
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
	"getRateParam", 
	class="T92", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam(this,name,this$.t92.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#       @get "title".
#
#        The rate parameters are: Alpha, Beta.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{name}{The name of the rate parameter.}
#       \item{value}{A numeric vector of length one.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#       # construct a T92 object
#       p<-T92();
#       # set/get a rate parameter
#       setRateParam(p,"Alpha",4)
#       getRateParam(p,"Beta")
#       # get object summary
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
	"setRateParam", 
	class="T92", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam(this,name,value,this$.t92.params);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTheta
##	
###########################################################################/**
#
# @RdocMethod getTheta
# 
# @title "Get the GC content" 
# 
# \description{ 
#       @get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A numeric vector of length one.
# } 
# 
# \examples{
#       # construct a T92 object
#       p<-T92()
#       # set/get GC content
#	setTheta(p,0.6)
#	getTheta(p)
#	# set/get GC content via virtual field
#	p$theta<-0.3
#	p$theta
#	# get object summary
#	summary(p)
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
	"getTheta", 
	class="T92", 
	function(
		this,
		...
	){

		this$.theta;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTheta
##	
###########################################################################/**
#
# @RdocMethod setTheta
# 
# @title "Set the GC content" 
# 
# \description{ 
#       @get "title".
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A T92 object.} 
#       \item{value}{A numeric vector of length one.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The new value of theta (invisible).
# } 
# 
# \examples{
#       # construct a T92 object
#       p<-T92()
#       # set/get GC content
#	setTheta(p,0.6)
#	getTheta(p)
#	# set/get GC content via virtual field
#	p$theta<-0.3
#	p$theta
#	# get object summary
#	summary(p)
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
	"setTheta", 
	class="T92", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if(!is.numeric(value)){
			throw("Theta must be numeric!\n");
		}
		else if (length(value) != 1){
			throw("The value of theta must be a vector of length 1!\n");
		}
		else if(value > 1){
			throw("Theta (GC content) cannot be larger than 1!\n");
		}
		else {
			this$.theta<-value;
			# WARNING - here we rely on the T C G A symbol order in the nucleotide alphabet.
			base.freqs<-c(
				((1-this$.theta)/2),	# T
				(this$.theta/2),	# C
				((1-this$.theta)/2),	# A
				((this$.theta)/2)	# G
		);
	}
		# Set the GTR base frequencies:
		setBaseFreqs.GTR(this,base.freqs);	
		return(invisible(value));
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.T92
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
	class="T92", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# Rate parameters should not be negative:
				if(any(this$.t92.params < 0 ) | (this$.theta < 0)){
					throw("Found negative T92 rate parameters!\n");
				}
				else {

					if(
				  	this$.gtr.params[["a"]]!=this$.t92.params[["Alpha"]]|
          	this$.gtr.params[["b"]]!=this$.t92.params[["Beta"]]|
          	this$.gtr.params[["c"]]!=this$.t92.params[["Beta"]]|
          	this$.gtr.params[["d"]]!=this$.t92.params[["Beta"]]|
          	this$.gtr.params[["e"]]!=this$.t92.params[["Beta"]]|
          	this$.gtr.params[["f"]]!=this$.t92.params[["Alpha"]]
						) {
							throw("The HKY parameters are not consistent with the GTR parameters!\n");
						}

						# Checking if the equlibrium distribution is consistent with theta:
						base.freqs<-c(
							((1-this$.theta)/2),	# T
							(this$.theta/2),			# C
							((1-this$.theta)/2),	# A
							((this$.theta)/2)		  # G
						);
					
						if(any(!PSRoot$my.all.equal(base.freqs, this$equDist))){
								throw("Equlibrium distribution is not consistent with the theta value!\n");
						}

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
## Method: summary.T92
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
#       a<-T92(theta=0.8)
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
	class="T92", 
	function(
		object,
		...
	){
		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "T92") {
			this$.summary$"Rate parameters"<-paste(names(this$.t92.params),this$.t92.params,sep=" = ",collapse=", ");
			this$.summary$"Theta (GC content)"<-this$.theta;
			this$.summary$"Transition/transversion rate ratio"<-(this$.t92.params[["Alpha"]]/this$.t92.params[["Beta"]]);
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of T92 methods ############

##	
## Constructor: F84
##	
## Hasegawa, M., H. Kishino, and T. Yano. (1985) Dating of human-ape splitting by a molecular clock
## of mitochondrial DNA. Journal of Molecular Evolution, 22, 160-174.
##
##########################################################################/** 
#
# @RdocClass F84
# 
# @title "The F84 class"
# 
# \description{ 
#	This class implements the F84 GTR-submodel.
#
#	The rate parameters are the following: Kappa.
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Object name.}
#	\item{rate.params}{Rate parameters.}
#	\item{base.freqs}{Base frequency parameters.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution process object
#       p<-F84(rate.params=list( "Kappa"=2), base.freqs=c(1,2,3,4))
#       # get a summary
#       summary(p)
#	# display a bubble plot
#	plot(p)
#
#	# The following code demonstrates how to use 
#	# the process in a simulation.
#
#	# create a sequence, attach process p
#	s<-NucleotideSequence(length=20,processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make the first five positions invariable
#	setRateMultipliers(s,p,0,1:5)
#	# get rate multipliers
#	getRateMultipliers(s,p)
#	# create a simulation object
#	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#	# run simulation
#	Simulate(sim)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GTR UNREST GeneralSubstitution HKY
# }
# 
#*/###########################################################################
setConstructorS3(
  "F84",
  function( 
		name="Anonymous",
		rate.params=list(
			"Kappa"   = 0
			),
		base.freqs=c(0.25,0.25,0.25,0.25)
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"F84",
			.f84.params=list(
					"Kappa"	 	=NA
				)
			);

		this$name<-name;
		this$baseFreqs<-base.freqs;
		this$rateParamList<-rate.params;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
###########################################################################/**
#
# @RdocMethod getRateParamList
# 
# @title "Get the rate parameters" 
# 
# \description{ 
#       @get "title".
#
#       The rate parameters are: Kappa.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The list of rate parameters.
# } 
# 
# \examples{
#       # create F84 object
#       p<-F84()
#       # set/get rate parameters
#       setRateParamList(p,list("Kappa"=3))
#       getRateParamList(p)
#       # set/get rate parameters via virtual field
#       p$rateParamList<-list("Kappa"=2.5)
#       p$rateParamList
#       # get object summary
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
	"getRateParamList", 
	class="F84", 
	function(
		this,
		...
	){

		this$.f84.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
###########################################################################/**
#
# @RdocMethod setRateParamList
# 
# @title "Set the rate parameters" 
# 
# \description{ 
#       @get "title".
#
#       The rate parameters are: Kappa.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{value}{A list containing the rate parameters.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The list of rate parameters (invisible).
# } 
# 
# \examples{
#       # create F84 object
#       p<-F84()
#       # set/get rate parameters
#       setRateParamList(p,list("Kappa"=3))
#       getRateParamList(p)
#       # set/get rate parameters via virtual field
#       p$rateParamList<-list("Kappa"=2.5)
#       p$rateParamList
#       # get object summary
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
	"setRateParamList", 
	class="F84", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.f84.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				this$.f84.params<-value;
				# Setting the GTR rate parameters:
  			kappa<-value[["Kappa"]];
        y<-(this$.equ.dist[1,"T"] + this$.equ.dist[1,"C"] );
        r<-(this$.equ.dist[1,"A"] + this$.equ.dist[1,"G"] );
        gtr.params<-list(
          "a"=(1 + (kappa/y) ),
          "b"=1,
          "c"=1,
          "d"=1,
          "e"=1,
          "f"=(1 + (kappa/r) )
        );
				setRateParamList.GTR(this, value=gtr.params);

		}

	}
		return(invisible(value));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateParam
##	
###########################################################################/**
#
# @RdocMethod getRateParam
# 
# @title "Get the value of a rate parameter" 
# 
# \description{ 
#       @get "title".
#
#        The rate parameters are: Kappa.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{name}{The name of the rate parameter.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A numeric vector of length one.
# } 
# 
# \examples{
#       # create F84 object
#       p<-F84()
#       # set/get rate parameters
#       setRateParamList(p,list("Kappa"=3))
#       getRateParamList(p)
#       # set/get rate parameters via virtual field
#       p$rateParamList<-list("Kappa"=2.5)
#       p$rateParamList
#       # get object summary
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
	"getRateParam", 
	class="F84", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else {
			.getRateParam.GTR(this,name,this$.f84.params);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
###########################################################################/**
#
# @RdocMethod setRateParam
# 
# @title "Set the value of a rate parameter" 
# 
# \description{ 
#       @get "title".
#
#        The rate parameters are: Kappa.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{name}{The name of the rate parameter.}
#       \item{value}{A numeric vector of length one.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       The new value of the rate parameter (invisible).
# } 
# 
# \examples{
#       # construct a F84 object
#       p<-F84();
#       # set/get a rate parameter
#       setRateParam(p,"Kappa",4)
#       getRateParam(p,"Kappa")
#       # get object summary
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
	"setRateParam", 
	class="F84", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		} else {
			.setRateParam.GTR(this,name,value,this$.f84.params);
		}
		return(invisible(value));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getKappa
##	
###########################################################################/**
#
# @RdocMethod getKappa
# 
# @title "Get the transition transversion rate ratio" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A F84 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#       # construct a F84 object
#       p<-F84();
#	# set/get Kappa
#	setKappa(p,2)
#	getKappa(p)
#	# set/get Kappa via virtual field
#	p$kappa<-4
#	p$kappa
#	# get object summary
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
	"getKappa", 
	class="F84", 
	function(
		this,
		...
	){

			getRateParam.F84(this, "Kappa");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setKappa
##	
###########################################################################/**
#
# @RdocMethod setKappa
# 
# @title "Get the transition transversion rate ratio" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A F84 object.} 
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of Kappa (invisible).
# } 
# 
# \examples{
#       # construct a F84 object
#       p<-F84();
#	# set/get Kappa
#	setKappa(p,2)
#	getKappa(p)
#	# set/get Kappa via virtual field
#	p$kappa<-4
#	p$kappa
#	# get object summary
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
	"setKappa", 
	class="F84", 
	function(
		this,
		value,
		...
	){

			setRateParam.F84(this,"Kappa",value);
			return(invisible(value));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod getBaseFreqs
# 
# @title "Get the base frequency parameters" 
# 
# \description{ 
#       @get "title".
#
#       The order of the frequency parameters must match with the order of symbols
#       in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A matrix containing the base frequency parameters.
# } 
# 
# \examples{
#       # construct object
#       p<-F84()
#       # set/get base frequency parameters
#       setBaseFreqs(p,c(2,1,2,1)/6)
#       getBaseFreqs(p)
#       # set/get base frequency parameters via virtual field
#       p$baseFreqs<-c(4,4,1,1)/10
#       p$baseFreqs
#       # get object summary
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
	"getBaseFreqs", 
	class="F84", 
	function(
		this,
		...
	){

		getBaseFreqs.GTR(this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
###########################################################################/**
#
# @RdocMethod setBaseFreqs
# 
# @title "Set the base frequency parameters" 
# 
# \description{ 
#       @get "title".
#
#       The order of the frequency parameters must match with the order of symbols
#       in the NucleotideAlphabet objects.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A F84 object.} 
#       \item{value}{A vector of base frequencies.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       value (invisible)
# } 
# 
# \examples{
#       # construct object
#       p<-F84()
#       # set/get base frequency parameters
#       setBaseFreqs(p,c(2,1,2,1)/6)
#       getBaseFreqs(p)
#       # set/get base frequency parameters via virtual field
#       p$baseFreqs<-c(4,4,1,1)/10
#       p$baseFreqs
#       # get object summary
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
	"setBaseFreqs", 
	class="F84", 
	function(
		this,
		value,
		...
	){

		setBaseFreqs.GTR(this,value);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: checkConsistency.F84
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
	class="F84", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# Rate parameters should not be negative:
				if( any(this$.f84.params < 0 ) ){
					throw("Found negative F84 rate parameters!\n");
				}
				else {
  			kappa<-this$.f84.params[["Kappa"]];
        y<-(this$.equ.dist[1,"T"] + this$.equ.dist[1,"C"] );
        r<-(this$.equ.dist[1,"A"] + this$.equ.dist[1,"G"] );

        gtr.params<-list(
          "a"=(1 + (kappa/y) ),
          "b"=1,
          "c"=1,
          "d"=1,
          "e"=1,
          "f"=(1 + (kappa/r) )
        );

				if(any(names(gtr.params) != names(this$.gtr.params)) | any(!PSRoot$my.all.equal(as.numeric(gtr.params), as.numeric(this$.gtr.params) ) )){
						throw("The Kappa value is not consistent with the GTR rate parameters!\n");
				}

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
## Method: summary.F84
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
#       a<-F84(rate.params=list("Kappa"=3))
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
	class="F84", 
	function(
		object,
		...
	){
		this<-object;
		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "F84") {
			this$.summary$"Rate parameters"<-paste(names(this$.f84.params),this$.f84.params,sep=" = ",collapse=", ");
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of F84 methods ############
