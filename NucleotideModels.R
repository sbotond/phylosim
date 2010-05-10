##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
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
## Constructor: JC69
##	
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

#
# Constructor: GTR
#
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
		else {
			param.list[[name]]<-value;
				
			# We call setRateParamList to rebuild the whole rate
			# matrix with the new values if one of the rates changed:
			setRateParamList(this, param.list);
										
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
		} else {
			.setRateParam(this,name,value,this$.gtr.params);
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
				throw("Cannot build the model because the following rate parameters are missing: ",paste(missing,coll=", ")," \n");	
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
		setRateParamList.GTR(this,value=this$.gtr.params);

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
setConstructorS3(
  "TN93",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha1"  =1,
      				"Alpha2"  =1,
      				"Beta"    =1
			),
		... 
		)	{
		
		this<-GTR(...);
		
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
		this$rateParamList<-rate.params;

		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
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
setConstructorS3(
  "HKY",
  function( 
		name="Anonymous",
		rate.params=list(
							"Alpha"   =1,
      				"Beta"    =1
			),
			... 
		)	{
		
		this<-GTR(...);
		
		this<-extend(
			this,
			"HKY",
			.hky.params=list(
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
setConstructorS3(
  "F81",
  function( 
		name="Anonymous",
		...
		)	{
		
		this<-GTR(...);
		
		this<-extend(
			this,
			"F81"
			);

		this$name<-name;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
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
setConstructorS3(
  "K80",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"  	=1,
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
setConstructorS3(
  "K81",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"  	=1,
      	"Beta"    =1,
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
				# FIXME - doublecheck this!
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
setConstructorS3(
  "T92",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha"  	=1,
      	"Beta"    =1
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
				(this$.theta/2),			# C
				((1-this$.theta)/2),	# A
				((this$.theta)/2)		  # G
		);
	}
		# Set the GTR base frequencies:
		setBaseFreqs.GTR(this,base.freqs);	

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
setConstructorS3(
  "F84",
  function( 
		name="Anonymous",
		rate.params=list(
							"Kappa"   =0
			),
			... 
		)	{
		
		this<-GTR(...);
		
		this<-extend(
			this,
			"F84",
			.f84.params=list(
					"Kappa"	 	=NA
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
setMethodS3(
	"setKappa", 
	class="F84", 
	function(
		this,
		value,
		...
	){

			setRateParam.F84(this,"Kappa",value);

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
