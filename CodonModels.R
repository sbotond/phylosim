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

		# FIXME

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
