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
				throw("The new value must be a character vector of length 1!\n");
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
							warning("The log file already existed and it was wiped out!\n");
						}
						# Creating the assotiated connection:
						this$.log.connection<-file(paste(this$.log.file),"w+");
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
				writeLines(paste(message[["time"]]," "),con=this$.log.connection,sep="");
				message[["time"]]<-NULL;
				writeLines(paste(message[["level"]]," "),con=this$.log.connection,sep="");
				message[["level"]]<-NULL;
				writeLines(paste(message[["event"]]," "),con=this$.log.connection,sep="");
				message[["event"]]<-NULL;
				writeLines(paste(message,collapse=", "),con=this$.log.connection,sep="");
				writeLines("\n",con=this$.log.connection,sep="");
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

##	
## Method: UpdateBranchStats
##	
setMethodS3(
	"UpdateBranchStats", 
	class="PhyloSim", 
	function(
		this,
		event,
		details,
		branch.number,
		...
	){

		if(details$type == "substitution"){
			
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$substitution)){
				this$.branch.stats[[as.character(branch.number)]]$substitution<-1;
			} else {
			this$.branch.stats[[as.character(branch.number)]]$substitution<-(this$.branch.stats[[as.character(branch.number)]]$substitution + 1);
			}
			name<-event$name;
	
			if(is.null(this$.branch.stats[[as.character(branch.number)]][[name]])){
				this$.branch.stats[[as.character(branch.number)]][[name]]<-1;
			}
			else {
				this$.branch.stats[[as.character(branch.number)]][[name]]<-(this$.branch.stats[[as.character(branch.number)]][[name]] + 1);
			}
		
			# Special stuff for the NY98 codon model:	
			if(is.NY98(event$.process)){
				# Increment synonymous counter:
				if(event$.type == "synonymous"){
					if(is.null(this$.branch.stats[[as.character(branch.number)]][["syn.subst"]])){
						# First event of this type on this branch, initialize the list element to 1.
						this$.branch.stats[[as.character(branch.number)]][["syn.subst"]]<-1;
					}
					else {
					this$.branch.stats[[as.character(branch.number)]][["syn.subst"]]<-(this$.branch.stats[[as.character(branch.number)]][["syn.subst"]] + 1);
					}
				}
				# Increment non-synonymous counter:
				else if(event$.type == "non-synonymous"){
					if(is.null(this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]])){
						# First event of this type on this branch, initialize the list element to 1.
						this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]]<-1;
					}
					else {
						this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]]<-(this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]] + 1);
					}
				} else {
					throw("The event generated by the NY98 has no type!\n");
				}

				# Update dn.vs.ds counter:  
				# FIXME - unnecessary slowdown here
				if( (!is.null(this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]])) & (!is.null(this$.branch.stats[[as.character(branch.number)]][["syn.subst"]])) ){
					this$.branch.stats[[as.character(branch.number)]][["nsyn.vs.syn.subst"]]<-( this$.branch.stats[[as.character(branch.number)]][["nsyn.subst"]] / this$.branch.stats[[as.character(branch.number)]][["syn.subst"]] );
				}
			}

		}
		else if(details$type == "deletion"){
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$deletion)){
				this$.branch.stats[[as.character(branch.number)]]$deletion<-1;
			}
			else {
				this$.branch.stats[[as.character(branch.number)]]$deletion<-(this$.branch.stats[[as.character(branch.number)]]$deletion + 1);
			}
		}
		else if(details$type == "insertion"){
			if(is.null(this$.branch.stats[[as.character(branch.number)]]$insertion)){
			this$.branch.stats[[as.character(branch.number)]]$insertion<-1;					
			}
			else {
			this$.branch.stats[[as.character(branch.number)]]$insertion<-(this$.branch.stats[[as.character(branch.number)]]$insertion + 1);					

			}
		}
		else {
			throw("Invalid event type!\n");
		}
			

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getBranchEvents
##	
setMethodS3(
	"getBranchEvents", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		tmp<-character();
		for(branch in this$.branch.stats){
			tmp<-c(tmp,names(branch));
		}
		return(unique(sort(tmp)));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBranchEvents
##	
setMethodS3(
	"setBranchEvents", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

			virtualAssignmentForbidden(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: exportStatTree
##	
setMethodS3(
	"exportStatTree", 
	class="PhyloSim", 
	function(
		this,
		event,
		...
	){

 		if(length(this$.branch.stats) != this$nedges){
      throw("Simulation is not complete, cannot export statistics!\n");
    }
		else if(missing(event)){
			throw("No event name specified!\n");
		}
		else if(length(intersect(event, this$branchEvents)) != 1 ){
			throw("Invalid even name!");
		}
		else {

			phylo.copy<-this$phylo;
			phylo.copy$edge.length<-.getStatBrlen(this, event);
			return(phylo.copy);		
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .getStatBrlen
##	
setMethodS3(
	".getStatBrlen", 
	class="PhyloSim", 
	function(
		this,
		event,
		...
	){

		tmp<-numeric();
		for(i in dimnames(this$edges)[[1]]){
				if(is.null(this$.branch.stats[[i]][[event]])){
					tmp[[as.numeric(i)]]<-0;
				}
				else {
					tmp[[as.numeric(i)]]<-this$.branch.stats[[i]][[event]];
				}
		}
		return(tmp);


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
