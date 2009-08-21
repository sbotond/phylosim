##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
	"AlphabetsList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"AlphabetsList",
			.seq=NA
		);

		 if(!missing(seq)) {
     	if(!is.Sequence(seq)) {
        throw("Sequence object not valid!\n");
      } else {
        this$.seq=seq;
      }
    }

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.AlphabetsList
##	
setMethodS3(
	"is.AlphabetsList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "AlphabetsList");

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
	class="AlphabetsList", 
	function(
		this,
		...
	){

   if(!is.Sequence(this$.seq)){
      throw("Alphabets list sequence reference is invalid!\n");
    }
    return(TRUE);


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [.AlphabetsList
##	
setMethodS3(
	"[", 
	class="AlphabetsList", 
	function(
		this,
		index
	){

		getAlphabets(this$.seq,index);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.AlphabetsList
##	
setMethodS3(
	"[<-", 
	class="AlphabetsList", 
	function(
		this,
		index,
		value
	){

		setAlphabets(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.AlphabetsList
##	
setMethodS3(
	"[[", 
	class="AlphabetsList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getAlphabets(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.AlphabetsList
##	
setMethodS3(
	"[[<-", 
	class="AlphabetsList", 
	function(
		this,
		index,
		value
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		} else if (length(value) > 1) {
			warning("Value vector longer than one!\n");
		}
		setAlphabets(this$.seq,value,index);

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
setMethodS3(
	"as.character", 
	class="AlphabetsList", 
	function(
		this,
		...
	){
		
		this[];	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

