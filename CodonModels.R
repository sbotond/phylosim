##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## CodonUNREST
##
setConstructorS3(
  "CodonUNREST",
  function( 
		name="Anonymous", # name of the object
		table.id=1,				# the id of the genetic code table to use
		rate.list=NA,	    # list of unscaled rates
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
				this<-extend(this, "CodonUNREST");
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id),
					rate.list=rate.list
				);	
				this<-extend(this, "CodonUNREST");
			}
			
			# Got equlibrium distribution,
			# we set it, but it will be owerwritten anyway.
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id),
					equ.dist=equ.dist
				);	
				this<-extend(this, "CodonUNREST");
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(table.id=table.id)
				);	
				this<-extend(this, "CodonUNREST");
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
## NY98
##
##
## Nielsen, R., Yang, Z. 1998 Likelihood Models for Detecting Positively Selected Amino Acid Sites
## and Applications to the HIV-1 Envelope Gene. Genetics 148:929-936.
##
setConstructorS3(
  "NY98",
  function( 
		name="Anonymous", # name of the object
		table.id=1,				# id of the genetic code table to use
		kappa=1,					# transition/transversion rate ratio
		omega.default=1,  # the default value of the omega site-process specific parameter
		codon.freqs=NA,   # codon frequencies 
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
			"NY98",
			.kappa=NA,
			.is.ny98=TRUE
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

		# Set object name:
		this$name<-name;
		return(this);

  },
  enforceRCC=TRUE
);

##  
## Method: is.NY98
##  
setMethodS3(
  "is.NY98",
  class="default",
  function(
    this,
    ...
  ){

		if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.ny98)){return(TRUE)}
    if ( inherits(this, "NY98")) {
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
## Method: getEventsAtSite.NY98
##	
setMethodS3(
	"getEventsAtSite", 
	class="NY98", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		# This method is almost an exact duplicate of the getEventsAtSite.GeneralSubstitution,
		# with the exception of the portions dealing with the omega site-process specific parameters.
		# Duplicating the method is not too elegant, but this way we can avoid the additonal method calls
		# sloving down the simualtion.

	 if(missing(target.site)) {
      throw("No target site provided!\n");
    } else if (!sloppy) {
			# Additional checks. They can be
			# disabled by sloppy=TRUE			

      if(!is.Site(target.site)) {
        throw("Target site invalid!\n");
      }
			# Commenting this out for performance:
	 		#else if(!is.QMatrix(this$.q.matrix)){
			#	throw("Cannot provide event objects because the rate matrix is not set!\n");	
			#}
			else if(!is.numeric(this$.equ.dist)){
				throw("Cannot provide event objects because the equilibrium frequencies are not defined!\n");	
			} 

		} 

			state<-getState(target.site);
		  # Just return an empty list if the state is NA:
			if(is.na(state)){
				return(list());
			}

			symbols<-this$alphabet$symbols;
			rest<-symbols[ which(symbols != state) ];

			# Create the event objects:
			events<-list();
			  
			# The rate of the event is the product of the general rate and the
     	# site specific rate multiplier:
     	rate.multiplier<-getParameterAtSite(this,target.site,"rate.multiplier")$value;
    
			# Get the omega site-process specific parameter: 
			omega<-getParameterAtSite(this,target.site,"omega")$value;
			
			for(new.state in rest){

			  name<-paste(state,new.state,sep="->");
		 		# Clone the event template object:
     		event<-clone(this$.event.template);
     		# Set event name:
     		event$name<-name;
     		# Set the generator process:
     		event$process<-this;
     		# Set the target position passed in a temporary field,
				# Event objects are not aware of their posiitions in general!
     		event$.position<-target.site$.position;
     		# Set the target site:
     		event$site<-target.site;
     		# Set the target state object (good for consistency):
     		event$targetState<-state;

				# Return empty list if the rate multiplier is zero.
     		if(rate.multiplier == 0 ) {
      		return(list());
     		}	
		
				# Figure out wether the event is a synonymous mutation ...
				if(areSynonymous(target.site$.alphabet,c(state,new.state))){
					# and ignore omega in that case
					event$rate<-(rate.multiplier * getEventRate(this$.q.matrix, from=state, to=new.state ));
					# Mark substitution as synonymous.
					event$.type<-"synonymous";
				} else {
					# incorporate omega otherwise
					event$rate<-(rate.multiplier * omega * getEventRate(this$.q.matrix, from=state, to=new.state ));
					# Mark substitution as non-synonymous.
					event$.type<-"non-synonymous";
				}

				# Set the handler for the substitution event:
     		.setHandler(event, this$.handler.template);
   			 # Write protect the event object:
    		event$writeProtected<-TRUE;
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
setMethodS3(
	"checkConsistency", 
	class="NY98", 
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
				diff<-sort(codonDiff(alphabet, c(from,to)));					

				# Single transition:
				if( all(diff == sort(c(0,0,"TI"))) ){
					if( !PSRoot$all.equal ( this$.q.matrix$.orig.matrix[from, to], (this$.kappa * this$.equ.dist[1,to]) ) ){
							throw("NY98 rate inconsistency. From:",from," To:",to,"!\n");

						}
				}
				# Single transversion:
				else if( all(diff == sort(c(0,0,"TV"))) ){
					if( !PSRoot$all.equal ( this$.q.matrix$.orig.matrix[from, to], (this$.equ.dist[1,to]) ) ){
					  throw("NY98 rate inconsistency. From:",from," To:",to,"!\n");
					}
				}
				# Multiple nucleotide substitution:
				else {
					if( this$.q.matrix$.orig.matrix[from, to] != 0.0 ){
						throw("NY98 rate inconsistency. From:",from," To:",to,"!\n");
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

##	
## Method: .buildNY98Rates
##	
setMethodS3(
	".buildNY98Rates", 
	class="NY98", 
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
				nuc.i<-strsplit(i,"")[[1]];
				nuc.j<-strsplit(j,"")[[1]];
	
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
setMethodS3(
  "setRate",
  class="NY98",
  function(
    this,
    name=NA,
    value,
    from=NA,
    to=NA,
    ...
  ){

		# FIXME: try to do this faster!
    .checkWriteProtection(this);
    # Setting unscaled rate:
    if(!is.QMatrix(this$.q.matrix)){
      throw("Cannot set rate as the rate matrix is undefined!\n");
    }
    else if(!missing(name) & missing(from) & missing(to)){
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
setMethodS3(
  "getKappa",
  class="NY98",
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
setMethodS3(
  "setKappa",
  class="NY98",
  function(
    this,
    value,
    ...
  ){

			.checkWriteProtection(this);		
			if(missing(value)){
				throw("No new value provided");
			}
			else if (length(value) != 1 | !is.numeric(value)){
				throw("The new value must be a numeric vector of length 1!\n");
			}
			else {
				this$.kappa<-value;
				.buildNY98Rates(this);
				return(value);
			}

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
setMethodS3(
  "getCodonFreqs",
  class="NY98",
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
setMethodS3(
  "setCodonFreqs",
  class="NY98",
  function(
    this,
    value,
    ...
  ){

		.checkWriteProtection(this);
		setEquDist(this,value,force=TRUE);
		.buildNY98Rates(this);
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: summary.NY98
##  
setMethodS3(
  "summary",
  class="NY98",
  function(
    this,
    ...
  ){

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

##  
## Method: getOmegas
##  
setMethodS3(
  "getOmegas",
  class="CodonSequence",
  function(
    this,
		process,
		index,
    ...
  ){

		if(missing(process)){
      throw("No process given!\n");
    }
    else if(!is.NY98(process)){
      throw("The specified process is not a NY98 codon substitution process!\n");
    }
    rm<-getParameterAtSites(this=this,process=process,id="omega",index=index);
    return(as.numeric(lapply(rm,function(param){param$value})));
	

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
##  
## Method: setOmegas
##  
setMethodS3(
  "setOmegas",
  class="CodonSequence",
  function(
    this,
		process,
    value,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(value)){
      throw("No new value specified!\n");
    }
    else if(!all(is.numeric(value)) ){
      throw("The new value must be a numeric vector!\n");
    }
    else {

      if(missing(index)){
        index<-seq(along=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

      setParameterAtSites(this, process=process, id="omega",value=value,index=index);

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM0 - one ratio
##  
setMethodS3(
  "omegaVarM0",
  class="CodonSequence",
  function(
    this,
		process,
    omega,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(omega)){
      throw("No new omega value specified!\n");
    }
    else if((!is.numeric(omega))| (length(omega) != 1)){
      throw("The new value must be a numeric vector of length 1!\n");
    }
		else if(omega < 0){
			throw("The omega parameter must be greater than zero!\n");
		}
    else {

      if(missing(index)){
        index<-seq(along=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

      setParameterAtSites(this, process=process, id="omega",value=omega,index=index);

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## The omegaVarMx methods mostly follow: Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M.
## 2000. Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites.
## Genetics 155:431-449.
##

##  
## Method: omegaVarM1 - neutral
##  
setMethodS3(
  "omegaVarM1",
  class="CodonSequence",
  function(
    this,
		process,
    p0,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0))| (length(p0) != 1)){
      throw("The new value must be a numeric vector of length 1!\n");
    }
		else if(p0 < 0 | p0 > 1){
			throw("The p0 parameter must be in the [0,1] interval!\n");
		}
    else {

      if(missing(index)){
        index<-seq(along=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

			for(site in this$.sites[index]){
				setParameterAtSite(this=process,site=site, id="omega", value=sample(c(0,1), size=1, replace=FALSE, prob=c(p0,(1-p0))));	
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
## Method: omegaVarM2 - selection
##  
setMethodS3(
  "omegaVarM2",
  class="CodonSequence",
  function(
    this,
		process,
    p0,
		p1,
		omega,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0))| (length(p0) != 1)){
      throw("The p0 value must be a numeric vector of length 1!\n");
    }
		else if(p0 < 0 | p0 > 1){
			throw("The p0 parameter must be in the [0,1] interval!\n");
		}
    else if(missing(p1)){
      throw("No p1 value specified!\n");
    }
    else if((!is.numeric(p1))| (length(p1) != 1)){
      throw("The p1 value must be a numeric vector of length 1!\n");
    }
		else if(p1 < 0 | p1 > 1){
			throw("The p1 parameter must be in the [0,1] interval!\n");
		}
    else if(missing(omega)){
      throw("No omega value specified!\n");
    }
    else if((!is.numeric(omega))| (length(omega) != 1)){
      throw("The omega value must be a numeric vector of length 1!\n");
    }
		else if(omega < 0){
			throw("The omega parameter must be greater than zero!\n");
		}
    else {

      if(missing(index)){
        index<-seq(along=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

			for(site in this$.sites[index]){
				setParameterAtSite(this=process,site=site, id="omega", value=sample(c(0,1,omega), size=1, replace=FALSE, prob=c(p0,p1,(1-p0-p1))));	
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
## Method: omegaVarM3 - discrete
##  
setMethodS3(
  "omegaVarM3",
  class="CodonSequence",
  function(
    this,
		process,
		omegas,
		probs,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(omegas)){
      throw("No omega values specified!\n");
    }
    else if((!is.numeric(omegas))){
      throw("The omegas must be numeric!\n");
    }
		else if(any(omegas < 0)){
			throw("The omegas must be greater than zero!\n");
		}
		else if(missing(probs)){
			throw("No probabilities specified!\n");
		}
		else if(!is.numeric(probs)){
			throw("The omegas must be greater than zero!\n");
		}
		else if(length(omegas) != length(probs)){
			throw("The length of the \"omegas\" and \"probs\" vector must be the same!\n");
		}
		else if(!PSRoot$all.equal(sum(probs),1.0)){
			probs<-(probs/sum(probs));
			warning("The provided probabilities were scaked in order to sum to one!\n");
		}

    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(omegas, size=1, replace=FALSE, prob=probs));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM4 - freqs
##  
setMethodS3(
  "omegaVarM4",
  class="CodonSequence",
  function(
    this,
		process,
		probs,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
		else if(missing(probs)){
			throw("No probabilities specified!\n");
		}
		else if(!is.numeric(probs)){
			throw("The omegas must be greater than zero!\n");
		}
		else if( length(probs) != 5){
			throw("The length of the \"probs\" vector must be 5!\n");
		}
		else if(!PSRoot$all.equal(sum(probs),1.0)){
			probs<-(probs/sum(probs));
			warning("The provided probabilities were scaked in order to sum to one!\n");
		}

    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		omegas<-c(0,(1/3),(2/3),1,3);
		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(omegas, size=1, replace=FALSE, prob=probs));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM5 - gamma
##  
setMethodS3(
  "omegaVarM5",
  class="CodonSequence",
  function(
    this,
		process,
		alpha,
		beta,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(alpha)){
      throw("No alpha (shape) value specified!\n");
    }
    else if((!is.numeric(alpha)) | (length(alpha) != 1)){
      throw("The alpha (shape) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha < 0){
			throw("The alpha (shape) must be greater than zero!\n");
		}
    else if(missing(beta)){
      throw("No beta (scale) value specified!\n");
    }
    else if((!is.numeric(beta)) | (length(beta) != 1)){
      throw("The beta (scale) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta <= 0){
			throw("The beta (scale) must be strictly positive!\n");
		}

    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=rgamma(1,shape=alpha,scale=beta));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM6 - 2gamma
##  
setMethodS3(
  "omegaVarM6",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		alpha0,
		beta0,
		alpha1,
		beta1,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(alpha0)){
      throw("No alpha0 (shape0) value specified!\n");
    }
    else if((!is.numeric(alpha0)) | (length(alpha0) != 1)){
      throw("The alpha0 (shape0) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha0 < 0){
			throw("The alpha0 (shape0) must be greater than zero!\n");
		}
    else if(missing(beta0)){
      throw("No beta0 (scale0) value specified!\n");
    }
    else if((!is.numeric(beta0)) | (length(beta0) != 1)){
      throw("The beta0 (scale0) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta0 <= 0){
			throw("The beta0 (scale0) must be greater than zero!\n");
		}
    else if(missing(alpha1)){
      throw("No alpha1 (shape1) value specified!\n");
    }
    else if((!is.numeric(alpha1)) | (length(alpha1) != 1)){
      throw("The alpha1 (shape1) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha1 < 0){
			throw("The alpha1 (shape1) must be greater than zero!\n");
		}
    else if(missing(beta1)){
      throw("No beta1 (scale1) value specified!\n");
    }
    else if((!is.numeric(beta1)) | (length(beta1) != 1)){
      throw("The beta1 (scale1) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta1 <= 0){
			throw("The beta1 (scale1) must be greater than zero!\n");
		}


    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rgamma(1,shape=alpha0,scale=beta0),rgamma(1,shape=alpha1,scale=beta1)),size=1,replace=FALSE,prob=c(p0,(1-p0)) ));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM7 - beta
##  
setMethodS3(
  "omegaVarM7",
  class="CodonSequence",
  function(
    this,
		process,
		p,
		q,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p parameter must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=rbeta(1,shape1=p,shape2=q));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM8 - beta&omega
##  
setMethodS3(
  "omegaVarM8",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p,
		q,
		omega,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.NY98(process)){
      throw("The sepcified process is not a NY98 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("The p0 parameter must be from the [0,1] interval!\n");
		}
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p parameter must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q parameter must be positive!\n");
		}
    else if(missing(omega)){
      throw("No omega value specified!\n");
    }
    else if((!is.numeric(omega)) | (length(omega) != 1)){
      throw("The omega parameter must be a numeric vector of length 1!\n");
    }
		else if(omega < 0){
			throw("The omega parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rbeta(1,shape1=p,shape2=q),omega),size=1,replace=FALSE,prob=(c(p0,(1-p0)))));	
		}


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
