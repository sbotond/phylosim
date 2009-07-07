##
##	Class: *
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

##
## CodonUNREST
##
setConstructorS3(
  "CodonUNREST",
  function( 
		name="Anonymous", # name of the object
		table.id=1,
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
## Method: is.CodonUNREST
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
		table.id=1,				
		kappa=1,
		omega.default=1,
		codon.freqs=NA,
		... 
		)	{

		this<-CodonUNREST(table.id=table.id);

		if(missing(codon.freqs)){
			codon.freqs<-rep((1/this$alphabet$size),this$alphabet$size);
		}

		this<-extend(
			this,
			"NY98",
			.kappa=NA
		);

		.addSiteSpecificParameter(
      this,
      id="omega",
      name="Omega",
      value=as.double(omega.default),
      type="numeric"
    );

		setEquDist(this,value=codon.freqs,force=TRUE);	
		this$kappa<-kappa;

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

    if(!is.GeneralSubstitution(this)) {return(FALSE)}
    if ( inherits(this, "NY98")) {
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

			  # The rate of the event is the product of the general rate and the
     		# site specific rate multiplier:
     		rate.multiplier<-getParameterAtSite(this,target.site,"rate.multiplier")$value;

				# Return empty list if the rate multiplier is zero.
     		if(rate.multiplier == 0 ) {
      		return(list());
     		}	
		
				# Figure out wether the event is a synonymous mutation ...
				if(areSynonymous(target.site$.alphabet,c(state,new.state))){
					# and ignore omega in that case
					event$rate<-(rate.multiplier * getEventRate(this$.q.matrix, name ));
				} else {
					# deal with omega otherwise
     			omega<-getParameterAtSite(this,target.site,"omega")$value;
					event$rate<-(rate.multiplier * omega * getEventRate(this$.q.matrix, name ));
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

		alphabet<-this$alphabet;
		symbols<-alphabet$symbols;

		for(i in symbols){
			for(j in symbols){
				
				# diagonal element:
				if(i == j) { next }
				# transition:
				if( all(sort(codonDiff(alphabet,c(i,j))) == sort( c("0","0","TI"))) ){
					setRate(this,from=i,to=j,value=(this$.kappa * this$.equ.dist[1,j]));
				}
				# transversion:
				else if( all(sort(codonDiff(alphabet,c(i,j))) == sort( c("0","0","TV"))) ){
					setRate(this,from=i,to=j,value=(this$.kappa * this$.equ.dist[1,j]));
				}
				# double substitution:
				else {
					setRate(this,from=i,to=j,value=0);
				}
	
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
