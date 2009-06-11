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
);

##	
## Method: getLogLevel
##	
setMethodS3(
	"getLogLevel", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.log.level;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLogLevel
##	
setMethodS3(
	"setLogLevel", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			if(missing(value)){
				throw("No value provided!\n");
			}
			if((!is.numeric(value)) | length(value) != 1 ){
				throw("The new value must be a numeric vector of length 1!\n");
			}
			else{ 
    		# Create/wipe out log file.
				if(value >= 0 ){
						if(file.access(this$.log.file,mode=0) == c(0)){
							warning("The log file already existed, so it was wiped out!\n");
						}
      			cat(file=this$.log.file, append=FALSE,"");
				}
				this$.log.level<-value;
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .getMessageTemplate
##	
setMethodS3(
	".getMessageTemplate", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		template<-list(
			time=paste("[",Sys.time(),"]",sep=""),
			level="Info",
			event=""
		);

		return(template);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .logMessage
##	
setMethodS3(
	".logMessage", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){

			if(missing(message)){
				throw("No message given!\n");
			}
			else if (!is.list(message)){
				throw("The message should be a list");
			}
			else if( length(intersect(names(message),c("time","level","event"))) != 3){
				throw("The \"time\", \"level\" and \"event\" elements are mandatory in the message list!\n");
			}
			else {
				cat(file=this$.log.file, append=TRUE,message[["time"]]," ",sep="");
				message[["time"]]<-NULL;
				cat(file=this$.log.file, append=TRUE,message[["level"]],": ",sep="");
				message[["level"]]<-NULL;
				cat(file=this$.log.file, append=TRUE,message[["event"]]," ",sep="");
				message[["event"]]<-NULL;
				cat(file=this$.log.file,append=TRUE,paste(message,collapse=", "),sep="");
				cat(file=this$.log.file,append=TRUE,"\n");
				return(TRUE);
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: Log
##	
setMethodS3(
	"Log", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){
	
			if(this$.log.level < 0){
				return(invisible(FALSE))
			}
			if(missing(message)){
				throw("No message given!\n");
			} else {
				template<-.getMessageTemplate(this);
				template$level<-"Info";
				message<-c(template,as.list(message));
				.logMessage(this, message);
				return(invisible(message));
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: Debug
##	
setMethodS3(
	"Debug", 
	class="PhyloSim", 
	function(
		this,
		message,
		...
	){
		
			if(missing(message)){
				throw("No message given!\n");
			} 
			else if( this$.log.level <= 0){
				return(invisible(FALSE))
			}
			else {
				template<-.getMessageTemplate(this);
				template$level<-"DEBUG";
				message<-c(template,as.list(message));
				.logMessage(this, message);
				return(invisible(message));
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

