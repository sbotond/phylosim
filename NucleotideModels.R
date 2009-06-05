##	$Id: NucleotideModels.R,v 1.1 2009-05-01 16:06:14 sbotond Exp $
##
##	Class: NucleotideModels*
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
setConstructorS3(
  "UNREST",
  function( 
		name="Anonymous", 
		rate.list=NA,	
		equ.dist=NA,
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
## Constructor: JC69
##	
setConstructorS3(
  "JC69",
  function( 
		name="Anonymous",
		... 
		)	{
		
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
				
				# FIXME - what's to do here?	
		
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

#
# Constructor: GTR
#
setConstructorS3(
  "GTR",
  function( 
		name="Anonymous", 
		rate.params=list(
				"a"=1,
				"b"=1,
				"c"=1,
				"d"=1,
				"e"=1,
				"f"=1
		),	
		base.freqs=rep(0.25,times=4),
		... 
		)	{

		this<-UNREST();

		this<-extend(
			this,
			"GTR",
			.rate.params=list(
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
		if(!missing(rate.params)){
			setRateParamList(this,value=rate.params);
		}

		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
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
				
				# FIXME - what's to do here?	
		
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
## Method: getRateParam
##	
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
		else if(length(intersect(name,names(this$.rate.params))) == 0){
			throw("The specified rate parameter name is not valid!\n");
		}
		else {
			return(this$.rate.params[[name]]);
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
		}
		else if(length(intersect(name,names(this$.rate.params))) == 0){
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
		else {
			this$.rate.params[[name]]<-value;

			# The parmeters are named as in 
			# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.

			this$rateParamList<-this$.rate.params;
			# FIXME - explain this!
										
		}

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
setMethodS3(
	"getRateParamList", 
	class="GTR", 
	function(
		this,
		...
	){

		this$.rate.params;

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
setMethodS3(
	"setRateParamList", 
	class="GTR", 
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
		names<-names(this$.rate.params);
		value.names<-names(value);

		# Check for illegal rate parameter names:
		if(length((illegal<-setdiff(value.names, names))) != 0){
			throw("The following parameter names are illegal: ",paste(illegal, collapse=", ")," !\n");
		}
		else {

			missing<-setdiff(names, value.names);
			if(length(missing) > 0) {
				throw("Cannot build the model because the following rate parameters are missing: ",paste(missing,coll=", ")," \n");	
			}
			else {
				# Set the rate parameters:
				# The parmeters are named as in 
				# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.

				rate.list=list(

                "T->C"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "C->T"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "T->A"=(value[["b"]] * this$.equ.dist[1,"A"] ),
                "A->T"=(value[["b"]] * this$.equ.dist[1,"T"] ),
                "T->G"=(value[["c"]] * this$.equ.dist[1,"G"] ),
                "G->T"=(value[["c"]] * this$.equ.dist[1,"C"] ),
                "C->A"=(value[["d"]] * this$.equ.dist[1,"A"] ),
                "A->C"=(value[["d"]] * this$.equ.dist[1,"C"] ),
                "C->G"=(value[["e"]] * this$.equ.dist[1,"G"] ),
                "G->C"=(value[["e"]] * this$.equ.dist[1,"C"] ),
                "A->G"=(value[["f"]] * this$.equ.dist[1,"G"] ),
                "G->A"=(value[["f"]] * this$.equ.dist[1,"A"] )

                );
			this$.rate.params<-value;
			setRateList(this,rate.list);
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
## Method: getBaseFreqs
##	
setMethodS3(
	"getBaseFreqs", 
	class="GTR", 
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
## Method: setBaseFreqs
##	
setMethodS3(
	"setBaseFreqs", 
	class="GTR", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		# FIXME - explain this + more chekings
		setEquDist(this,value,force=TRUE);
		setRateParamList(this,value=this$.rate.params);

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
setMethodS3(
	"summary", 
	class="GTR", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "GTR") {
		this$.summary$"Rate parameters"<-paste(names(this$.rate.params),this$.rate.params,sep=" = ",collapse=", ");
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
setConstructorS3(
  "TN93",
  function( 
		name="Anonymous",
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(this,"TN93");

		this$name<-name;

		return(this);
	
  },
  enforceRCC=TRUE
);

######### end of TN93 methods ############

##	
## Constructor: HKY
##	
setConstructorS3(
  "HKY",
  function( 
		name="Anonymous",
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(this,"HKY");

		this$name<-name;

		return(this);
	
  },
  enforceRCC=TRUE
);

######### end of HKY methods ############
