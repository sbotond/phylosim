##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
	"StatesList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"StatesList",
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
## Method: is.StatesList
##	
setMethodS3(
	"is.StatesList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "StatesList");

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
	class="StatesList", 
	function(
		this,
		...
	){

   if(!is.Sequence(this$.seq)){
      throw("States list sequence reference is invalid!\n");
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
## Method: [.StatesList
##	
setMethodS3(
	"[", 
	class="StatesList", 
	function(
		this,
		index
	){

		getStates(this$.seq,index);	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.StatesList
##	
setMethodS3(
	"[<-", 
	class="StatesList", 
	function(
		this,
		index,
		value
	){

		setStates(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.StatesList
##	
setMethodS3(
	"[[", 
	class="StatesList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getStates(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.StatesList
##	
setMethodS3(
	"[[<-", 
	class="StatesList", 
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
		setStates(this$.seq,value,index);

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
	class="StatesList", 
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

