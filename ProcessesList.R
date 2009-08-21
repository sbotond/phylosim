##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
	"ProcessesList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"ProcessesList",
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
## Method: is.ProcessesList
##	
setMethodS3(
	"is.ProcessesList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "ProcessesList");

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
	class="ProcessesList", 
	function(
		this,
		...
	){

		if(!is.Sequence(this$.seq)){
			throw("Process list sequence reference is invalid!\n");
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
## Method: [.ProcessesList
##	
setMethodS3(
	"[", 
	class="ProcessesList", 
	function(
		this,
		index
	){

		getProcesses(this$.seq,index);	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.ProcessesList
##	
setMethodS3(
	"[<-", 
	class="ProcessesList", 
	function(
		this,
		index,
		value
	){

		setProcesses(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.ProcessesList
##	
setMethodS3(
	"[[", 
	class="ProcessesList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getProcesses(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.ProcessesList
##	
setMethodS3(
	"[[<-", 
	class="ProcessesList", 
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
		setProcesses(this$.seq,value,index);

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
	class="ProcessesList", 
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

