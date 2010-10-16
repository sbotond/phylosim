##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## CodonUNREST
##
##########################################################################/** 
#
# @RdocClass CodonUNREST
# 
# @title "The CodonUNREST class"
# 
# \description{ 
#	This class implements a time-continuous Markov process acting on a state
#	space defined by the symbol set of a CodonAlphabet object. The rate matrix of this model
#	is unrestricted, so it can be used to implement empircal codon models or more
#	restricted mechanistic models.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
# 	\item{table.id}{The identifier of the genetic code table (see \code{CodonAlphabet}).}
# 	\item{rate.list}{A list of unscaled substitution rates (see \code{setRateList.GeneralSubstitution}).}
# 	\item{equ.dist}{Equilibrium distribution.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a CodonUNREST object
#	p<-CodonUNREST(table.id=2)
#	# get object summary
#	summary(p)
# }
# 
# @author
#
# \seealso{ 
# 	GeneralSubstitution GY94
# }
# 
#*/###########################################################################
setConstructorS3(
  "CodonUNREST",
  function( 
		name="Anonymous", # name of the object
		table.id=1,	  # the id of the genetic code table to use
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
					alphabet=CodonAlphabet(table.id=table.id),
					rate.list=rate.list,
					equ.dist=equ.dist
				);	
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id),
					rate.list=rate.list
				);	
			}
			
			# Got equlibrium distribution,
			# we set it, but it will be owerwritten anyway.
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id),
					equ.dist=equ.dist
				);	
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id)
				);	
			}
				
			this<-extend(this, "CodonUNREST");

		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
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
##	
setMethodS3(
	"checkConsistency", 
	class="CodonUNREST", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
				if(!inherits(this$alphabet, "CodonAlphabet")){
					throw("This object must have as alphabet a CodonAlphabet object!\n");
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
# Method: is.CodonUNREST
##  
###########################################################################/**
#
# @RdocDefault is.CodonUNREST
# 
# @title "Check whether an object inherits from CodonUNREST" 
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#		\item{this}{An object.}
#		\item{...}{Not used.}
#
# } 
# 
# \value{ 
#	TRUE of FALSE.
# } 
#
# \examples{
#	# create some objects
#	p<-CodonUNREST()
#	pp<-GTR()
#	# check if they inherit from CodonUNREST
#	is.CodonUNREST(p)
#	is.CodonUNREST(pp)
# }
# 
# @author 
# 
#*/###########################################################################
setMethodS3(
  "is.CodonUNREST",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.GeneralSubstitution(this)) {return(FALSE)}
    if ( inherits(this, "CodonUNREST")) {
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
## GY94
##
##########################################################################/** 
#
# @RdocClass GY94
# 
# @title "The GY94 class"
# 
# \description{ 
#	This class implements the codon substitution model of Goldman and Yang (1994). 
#	The transition/transversion rate ratio is stored in the \code{kappa} virtual field. 
# 	The nonsynonymous/synonymous substitution rate ratio (\code{omega}) is a site-process specific parameter 
#	with a default value of one.
#	Hence, after the attachment of the process the variation of omega ratios among sites follows 
#	the M0 model (see Yang et al. 2000).
#
#       The rate matrix of the \code{\link{GY94}} model is scaled in a way that the expected number
#       of potential substiutions per site is equal to one at equlibrium. 
#       The \emph{codeml} program from the PAML package scales the rate matrix in order to have 
#       the expected number of accepted substiutions per site equal to one. Use the
#	\code{\link{getOmegaScalingFactor.GY94}} method to claculate a branch length scaling factor
#	which allows to switch to a PAML-style scaling given an average omega.
#
#	If the \code{scale.nuc} constructor argument is TRUE, the rates of the returned \code{Event} objects
#	will be multiplied by \code{3} to obtain a process which has the expected number of nucleotide substitutions
#	(not \code{codon} substitutions) equal to one at equilibrium. This is useful when simulating
#	mixed sequences. This option doesn't affect the rate matrix in any way.
#
#	The M1-M4 models are implemented in the \code{omegaVarM[1-4].CodonSeqeunce} methods.
#	Simulations under more complex models (M5-M13) can be achieved by first discretizing them 
#	using the \code{M5-13} tool from the INDELible software 
#	package (\url{http://abacus.gene.ucl.ac.uk/software/indelible/}). 
#	After discretization, the M5-M13 models can be simulated through the M3 (discrete) model.
#
#	@classhierarchy
# }
#
# \references{
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
#
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Name of the object.}
#	\item{table.id}{The identifier of the genetic code table to use (1 by default).}
#	\item{kappa}{The transition/transversion rate ratio (1 by default).}
#	\item{omega.default}{The default value of the omega site-process specific parameter (1 by default).}
#	\item{codon.freqs}{A vector of codon frequencies.}
#	\item{scale.nuc}{Scale to nucleotide substitutions if TRUE (see above).}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# check if inherits from GY94
#	is.GY94(p)
#	# get object summary
#	summary(p)
#	# display a bubble plot
#	plot(p)
#	# create a codon sequence, attach process
#	s<-CodonSequence(length=10, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# make first three positions invariable
#	setRateMultipliers(s,p,0,1:3)
#	# sample omega values from the M3 (discrete) model.
#	omegaVarM3(s,p,omegas=c(0,1,2,3),probs=c(2/5,1/5,1/5,1/5))
#	# get a histogram of omega values in s
#	omegaHist(s,p,breaks=50)
#       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#       # run simulation
#       Simulate(sim)
#	# get the list of recorded per-branch event counts
#	getBranchEvents(sim)
#	# export the number of synonymous substitutions as a phylo object
#	syn.subst<-exportStatTree(sim,"nr.syn.subst")
#	syn.subst
#	# plot the exported phylo object
#	plot(syn.subst)
#	# print alignment
#	sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	CodonUNREST GeneralSubstitution CodonSequence GTR WAG
# }
# 
#*/###########################################################################
setConstructorS3(
  "GY94",
  function( 
		name="Anonymous", # name of the object
		table.id=1,	  # id of the genetic code table to use
		kappa=1,	  # transition/transversion rate ratio
		omega.default=1,  # the default value of the omega site-process specific parameter
		codon.freqs=NA,   # codon frequencies 
		scale.nuc=FALSE,  # scale Q matrix to nucleotide substitutions
		... 
		)	{

		# Create a CodonUNREST object.
		this<-CodonUNREST(table.id=table.id);

		# Set codon frequencies to uniform if they are not provided:
		if(missing(codon.freqs)){
			codon.freqs<-rep((1/this$alphabet$size),this$alphabet$size);
		}

		# Extend:
		this<-extend(
			this,
			"GY94",
			.kappa=NA,
			.is.ny98=TRUE,
			.scale.const=as.double(1.0),
			.syn.cache=NA
		);

		# Add the "omega" site-process specific parameter:
		.addSiteSpecificParameter(
      			this,
      			id="omega",
      			name="Omega",
      			value=as.double(omega.default),
      			type="numeric"
    );

		# Set the codon frequencies/equilibrium distribution.
		setEquDist(this,value=codon.freqs,force=TRUE);	
		# Set kappa:
		this$kappa<-kappa;
		
		# Scale to nucleotide if requested:
		if(scale.nuc){
			this$.scale.const<-as.double(3.0);
		}

		# Set object name:
		this$name<-name;

		 # Force clearing id cache:              
                this$name<-this$name;

                # create syn/nsyn matrix cache
                # Get translation table:
                trans.table<-this$.alphabet$.trans.table;
                symbols<-this$.alphabet$symbols;

                syn.cache<-matrix(nrow=this$.alphabet$size,ncol=this$.alphabet$size);
		colnames(syn.cache)<-symbols;
		rownames(syn.cache)<-symbols;

                for(i in symbols){
                        for(j in symbols){
                                if(i == j) { next }

                                 if( (trans.table[[i]]$aa) == (trans.table[[j]]$aa) ){
                                        syn.cache[i,j]<-1;
                                 }
                                 else{ 
                                        syn.cache[i,j]<-0;
                                 }
                        }
                }

                this$.syn.cache<-syn.cache;

		return(this);

  },
  enforceRCC=TRUE
);

##  
## Method: is.GY94
##  
###########################################################################/**
#
# @RdocDefault is.GY94
# 
# @title "Check whether an object inherits from GY94" 
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
#		\item{this}{An object.}
#		\item{...}{Not used.}
# } 
# 
# \value{ 
#	TRUE of FALSE.
# } 
#
# \examples{
#	# create some objects
#	p<-CodonUNREST()
#	pp<-GY94()
#	# check if they inherit from CodonUNREST
#	is.GY94(p)
#	is.GY94(pp)
# }
# 
# @author 
# 
#*/###########################################################################
setMethodS3(
  "is.GY94",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.ny98)){return(TRUE)}
    if ( inherits(this, "GY94")) {
      this$.is.process<-TRUE;
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
## Method: getEventsAtSite.GY94
##	
###########################################################################/**
#
# @RdocMethod getEventsAtSite
# 
# @title "Generate the list of active Event objects for a given attached Site object" 
# 
# \description{ 
#       @get "title".
# 
# This method is almost an exact duplicate of the getEventsAtSite.GeneralSubstitution,
# with the exception of the portions dealing with the omega site-process specific parameter. 
#       
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A GY94 object.} 
#       \item{target.site}{A Site object. The GY94 object must be attached to the Site object.} 
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A list of the active Event objects.
# } 
# 
# 
# @author 
# 
# \seealso{ 
#       getEventsAtSite.GeneralSubstitution GeneralSubstitution
# } 
# 
#*/###########################################################################
setMethodS3(
	"getEventsAtSite", 
	class="GY94", 
	function(
		this,
		target.site,
		...
	){

	if (!exists(x="PSIM_FAST")) {

      			if(!is.Site(target.site)) {
      			  throw("Target site invalid!\n");
      			}
	 		
			if(is.na(this$.q.matrix)){
				throw("Cannot provide event objects because the rate matrix is not set!\n");	
			}
			if(!is.numeric(this$.equ.dist)){
				throw("Cannot provide event objects because the equilibrium frequencies are not defined!\n");	
			} 
		} 

			state<-as.character(target.site$.state);
		  	# Just return an empty list if the state is NA:
			if(is.na(state)){
				return(list());
			}

			 # Get rate matrix:
        		 rate.matrix<-this$.q.matrix$.rate.matrix;

			# Get translation table:
			trans.table<-target.site$.alphabet$.trans.table;
		
			# Get scaling constant:
			scale.const<-this$.scale.const;

			# get syn cache:
			syn.cache<-this$.syn.cache;

			symbols<-this$.alphabet$.symbols;
			rest<-symbols[ which(symbols != state) ];

			# Create the event objects:
			events<-list();
			  
			# The rate of the event is the product of the general rate and the
     			# site-process specific rate multiplier:
			rate.multiplier<-target.site$.processes[[this$.id]]$site.params[["rate.multiplier"]]$value;
			# Return empty list if the rate multiplier is zero.
     			if(rate.multiplier == 0 ) {
      				return(list());
     			}	
    
			# Get the omega site-process specific parameter: 
			omega<-target.site$.processes[[this$.id]]$site.params[["omega"]]$value;
			
			for(new.state in rest){
				
				# Get the base rate:
				base.rate<-rate.matrix[state,new.state];

				# Skip event if base rate is zero:
				if(base.rate == 0){
					next;
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

				# Figure out wether the event is a synonymous mutation ...
				
				if( syn.cache[state,new.state] ){
					# and ignore omega in that case
					event$.rate<-(scale.const * rate.multiplier * base.rate);		
					# Mark substitution as synonymous.
					event$.type<-"synonymous";
				} else {
					# incorporate omega otherwise
					event$.rate<-(scale.const * rate.multiplier * omega * base.rate);
					# Mark substitution as non-synonymous.
					event$.type<-"non-synonymous";
				}

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
	class="GY94", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {

      # Check kappa:
      if(!is.numeric(this$.kappa)){
        throw("Kappa must be numeric!\n");
      }
      # Check codon.freqs
      this$codonFreqs<-this$codonFreqs;

			# Check rate sanity:
			symbols<-this$alphabet$symbols;
			alphabet<-this$alphabet;

			for(from in symbols){
				for(to in symbols){
		
				# Skip diagonal elements:
				if(from == to) {next()};
				
				# Figure out codon differences:
				diff<-sort(.codonDiff(alphabet, c(from,to)));					

				# Single transition:
				if( all(diff == sort(c(0,0,"TI"))) ){
					if( !PSRoot$my.all.equal ( this$.q.matrix$.orig.matrix[from, to], (this$.kappa * this$.equ.dist[1,to]) ) ){
							throw("GY94 rate inconsistency. From:",from," To:",to,"!\n");

						}
				}
				# Single transversion:
				else if( all(diff == sort(c(0,0,"TV"))) ){
					if( !PSRoot$my.all.equal ( this$.q.matrix$.orig.matrix[from, to], (this$.equ.dist[1,to]) ) ){
					  throw("GY94 rate inconsistency. From:",from," To:",to,"!\n");
					}
				}
				# Multiple nucleotide substitution:
				else {
					if( this$.q.matrix$.orig.matrix[from, to] != 0.0 ){
						throw("GY94 rate inconsistency. From:",from," To:",to,"!\n");
					}
				}
	
				} #/for to
			} #/for from
	
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
# @RdocMethod getOmegaScalingFactor
# 
# @title "Get the omega scaling factor" 
# 
# \description{ 
#	@get "title".
#
#       The rate matrix of the \code{\link{GY94}} model is scaled in a way that the expected number
#       of potential substiutions per site is equal to one at equlibrium. 
#       The \emph{codeml} program from the PAML package scales the rate matrix in order to have 
#       the expected number of accepted substiutions per site equal to one. 
#
#	This method calculates the branch length multiplier needed for switching 
#	to PAML-style scaling given a fixed omega.
#
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GY94 object.} 
#	\item{omega}{The value of omega.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# construct a GY94 process object
#	p<-GY94(kappa=4)
#	# Calculate scaling factor for omega=2
#	getOmegaScalingFactor(p,omega=2)
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
	"getOmegaScalingFactor", 
	class="GY94", 
	function(
		this,
		omega,
		...
	){

		if(missing(omega)){
			throw("No omega provided!");
		}
		if(!is.numeric(omega)){
			throw("Omega must be numeric!");
		}

		neutral.K<-0.0;

		K <- 0.0;
                # get the symbols:
                symbols<-this$.alphabet$symbols;

		# Get translation table and rate matrix:
                trans.table<-this$.alphabet$.trans.table;
		rate.matrix<-this$.q.matrix$.rate.matrix;

                # For every symbol:
                for (i in symbols) {

                # Get the equlibrium probability:
                i.equ<-this$.equ.dist[[ which(colnames(this$.equ.dist) == i) ]];
                for(j in symbols){
                        if(i == j){next}
				base.rate<-rate.matrix[i,j];
				neutral.K<- neutral.K + (i.equ * base.rate);

				if( (trans.table[[i]]$aa) == (trans.table[[j]]$aa) ){
                        		K <- K + (i.equ * base.rate);
				}
				else {
                        		K <- K + (i.equ * omega * base.rate);
				}
                        }

                }

		return(neutral.K/K);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .buildGY94Rates
##	
setMethodS3(
	".buildGY94Rates", 
	class="GY94", 
	function(
		this,
		...
	){

		
		# Wiping out the rate matrices to prevent rescaling after
		# modifying an individual rate. This could be more elegant.

		# Wiping out the original rate matrix:
		this$QMatrix$.orig.matrix[]<-NA;
		# Wiping out the scaled rate matrix:
		this$QMatrix$.rate.matrix[]<-NA;

		alphabet<-this$.alphabet;
		symbols<-alphabet$symbols;

		purines<-c("A","G");
    		pyrimidines<-c("C","T");

		for(i in symbols){
			for(j in symbols){

				# Split codons:
				nuc.i<-strsplit(i,"",fixed=TRUE)[[1]];
				nuc.j<-strsplit(j,"",fixed=TRUE)[[1]];
	
				diff<-which( (nuc.i == nuc.j) == FALSE);

				# Skip diagonal elements:
				if( i == j) { 
						next;
				}
				else if( length( diff ) > 1){
					# We have multiple nucleotiode substiutions:
					this$.q.matrix$.orig.matrix[i,j]<-0;
				}
				else if ( 
									length( intersect( purines, c(nuc.i[diff[1]],nuc.j[diff[1]])) ) == 2 |
									length( intersect( pyrimidines, c(nuc.i[diff[1]],nuc.j[diff[1]])) ) == 2
								){
					# We have a single transition:
					this$.q.matrix$.orig.matrix[i,j]<-(this$.kappa * this$.equ.dist[1,j]);
				} else {
					# The only possibility left is a single transversion:
					this$.q.matrix$.orig.matrix[i,j]<-(this$.equ.dist[1,j]);
				}

			} # /for j
		} # /for i

		# Set the new diagonal element in the original rates matrix:
		for(codon in symbols){
        	this$.q.matrix$.orig.matrix[codon, codon]<-.calculateDiagonal(this$.q.matrix, symbol=codon);
		}
		
		# Call rate rescaling, suppress equlibrium distribution guessing:
		.callRateRescaling(this$.q.matrix,guess.equ=FALSE);

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
# @title "Set an unscaled rate for an event from a GY94 object" 
# 
# \description{ 
#       @get "title".
#       
#	See \code{\link{setRate.GeneralSubstitution}}.
# } 
# 
# @synopsis 
# 
# \arguments{ 
#       \item{this}{A GeneralSubstitution object.} 
#       \item{name}{The name of the event.}
#       \item{from}{The initial state.}
#       \item{value}{The new value of the rate.}
#       \item{to}{Target state.}
#       \item{...}{Not used.} 
# } 
# 
# \value{ 
#       A Numeric vector of length one.
# } 
# 
# 
# @author 
# 
# \seealso{ 
#       @seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
  "setRate",
  class="GY94",
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
	if(!exists(x="PSIM_FAST")) {
    		if(!is.QMatrix(this$.q.matrix)){
      			throw("Cannot set rate as the rate matrix is undefined!\n");
    		}
	}
    
	if(!missing(name) & missing(from) & missing(to)){
      		return(setRate(this$.q.matrix, name=name, value=value,guess.equ=FALSE));
    	}
    	else if (missing(name) & !missing(from) & !missing(to)){
      		return(setRate(this$.q.matrix, from=from, to=to, value=value,guess.equ=FALSE));
    	}


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
# @title "Get the transition/transversion rate ratio" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GY94 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94()
#	# set/get kappa
#	setKappa(p,2)
#	getKappa(p)
#	# set/get kappa via virtual field
#	p$kappa<-3
#	p$kappa
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
  class="GY94",
  function(
    this,
    ...
  ){

		this$.kappa;

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
# @title "Set the transition/transversion rate ratio" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GY94 object.} 
#	\item{value}{A numeric vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new value of kappa.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94()
#	# set/get kappa
#	setKappa(p,2)
#	getKappa(p)
#	# set/get kappa via virtual field
#	p$kappa<-3
#	p$kappa
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
  class="GY94",
  function(
    this,
    value,
    ...
  ){
	
	.checkWriteProtection(this);		
	if(!exists(x="PSIM_FAST")){
			if(missing(value)){
				throw("No new value provided");
			}
			else if (length(value) != 1 | !is.numeric(value)){
				throw("The new value must be a numeric vector of length 1!\n");
			}
	}
			
			this$.kappa<-value;
			.buildGY94Rates(this);
			return(value);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: getCodonFreqs
##  
###########################################################################/**
#
# @RdocMethod getCodonFreqs
# 
# @title "Get codon frequencies" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GY94 object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A matrix containing the codon frequencies.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94()	
#	# get codon frequencies
#	getCodonFreqs(p)
#	p$codonFreqs
#	# set codon frequencies
#	p$codonFreqs<-rep(c(1,2,3,4),length.out=61)
#	p$codonFreqs
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
  "getCodonFreqs",
  class="GY94",
  function(
    this,
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
## Method: setCodonFreqs
##  
###########################################################################/**
#
# @RdocMethod setCodonFreqs
# 
# @title "Get codon frequencies" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A GY94 object.} 
#	\item{value}{A vector of codon frequencies.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	value (invisible)
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94()	
#	# get codon frequencies
#	getCodonFreqs(p)
#	p$codonFreqs
#	# set codon frequencies
#	p$codonFreqs<-rep(c(1,2,3,4),length.out=61)
#	p$codonFreqs
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
  "setCodonFreqs",
  class="GY94",
  function(
    this,
    value,
    ...
  ){

		.checkWriteProtection(this);
		setEquDist(this,value,force=TRUE);
		.buildGY94Rates(this);
		return(invisible(value));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: summary.GY94
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
#       a<-GY94(kappa=2)
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
  class="GY94",
  function(
    object,
    ...
  ){

    this<-object;
    .addSummaryNameId(this);
		this$.summary$"Kappa"<-this$.kappa;
		this$.summary$"Genetic code table id"<-this$.alphabet$tableId;
    .addSummaryAlphabet(this);
		this$.summary$"Unscaled rate matrix"<-"not shown";

    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

