##
##	Class: PhyloSim
##	Descriprion: logging methods for the PhyloSim class.
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
## Method: getLogFile
##	
setMethodS3(
	"getLogFile", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.log.file;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLogFile
##	
setMethodS3(
	"setLogFile", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			if(missing(value)){
				throw("No value provided!\n");
			}
			value<-as.character(value);
			if( length(value) != 1 ){
				throw("The new value must be a charcter vector of length 1!\n");
			}
			else{ 
				if( file.access(value,mode=0) == c(0) ){
					warning("The specified file already exists and it will be overwritten during simulation!\n");
				}
				this$.log.file<-value;

			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

