##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## Method: hasSiteSpecificParameter
##
setMethodS3(
  "hasSiteSpecificParameter",
  class="Process",
  function(
    this,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			else if ( length (intersect((as.vector(this$siteSpecificParamIds) == id),TRUE) ) == 0 ) {
					return(FALSE);
			} else {
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
## Method: getParameterAtSite
##
setMethodS3(
  "getParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			
			if (.checkTriplett(this,site,id)){
				id<-as.character(id);
				list (
					id=id,
					name=site$.processes[[getId(this)]]$site.params[[id]]$name,
					value=site$.processes[[getId(this)]]$site.params[[id]]$value,
					type=site$.processes[[getId(this)]]$site.params[[id]]$type
				);
			}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .getParameterAtSiteFast
##
setMethodS3(
  ".getParameterAtSiteFast",
  class="Process",
  function(
    this,
    site,
    id,
    ...
  ){
				site$.processes[[this$.id]]$site.params[[as.character(id)]]$value;
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setParameterAtSite
##
setMethodS3(
  "setParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
		value,
		...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")};
      id<-as.character(id);
			
			if (.checkTriplett(this,site,id)){

				type<-site$.processes[[this$id]]$site.params[[id]]$type;
				if (length(intersect(class(value),type)) == 0 ) {throw("The new value is of wrong type!\n")}
				site$.processes[[this$id]]$site.params[[id]]$value<-value;
	
			}
			flagTotalRate(site);
		 .flagSeqCumulativeRates(site);
			invisible(this);
				
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkTriplett
##
setMethodS3(
  ".checkTriplett",
  class="Process",
  function(
    this,
    site,
		id,
		...
  ){
					
			if (!is.Site(site)) {throw ("Site object not valid!\n")}
			else if (!hasSiteSpecificParameter(this,id)) {
				throw(paste("The process",this$id,"has no site specific paramter with id:",id,"!\n",sep=" "));
			}
			else if (!isAttached(site,this)) {throw("Process is not attached to site!\n")} else {
				return(TRUE);
			}
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: isAttached
##
setMethodS3(
  "isAttached",
  class="Site",
  function(
    this,
    process,
    ...
  ){
			
			if (!is.Process(process)) {throw("Process object invalid!\n")}	
			attached_processes<-getProcesses(this);
			if (length(attached_processes) == 0 ){ return(FALSE)}
			
			tmp<-lapply(
					attached_processes,
					function(proc) { equals(proc, process)}
			);	
			tmp<-unique(tmp);
			
			if(length(tmp) == 1 ) {
					# If we have only one process attached,
					# than simply return the result of the equals() function.
					return(tmp[[1]]);
			} else {
					
					# Additional check to make sure that the .process entry is here.	
				#	if (length (intersect(class(this$.processes[[getId(process)]]),"list")) == 0) {
				#		throw("Something evil is happening! The process is attached, but the .process entry is invalid!\n");
				#	}
					# If length(tmp) > 1, than one of its elements must me TRUE,
					# so returning TRUE.
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
## Method: attachProcess
##
setMethodS3(
  "attachProcess",
  class="Site",
  function(
    this,
    process,
    ...
  ){
	
		if(!is.Process(process)) {
			throw("Process object is not valid!\n"); }
		else if( is.na(process$alphabet) ){
				throw("The process has no alphabet attached!\n"); }
		else if( is.na(this$alphabet) ){
				throw("The site has no alphabet attached!\n"); }
		else if (this$alphabet != process$alphabet) {
				throw("The site and process alphabets are incompatible!\n"); }
		else if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); 
		}
		# FIXME - checking for template sequence 
		else if( hasUndefinedRate(process) ){
				warning("The process",process$id," has undefined rates!\n");
		}
		else {
			this$.processes[[process$id]]<-list (
				object 				= 	process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params		=		process$siteSpecificParamList	
			);
		}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .attachProcessSloppy
##
setMethodS3(
  ".attachProcessSloppy",
  class="Site",
  function(
    this,
    process,
    ...
  ){
	
		if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); }
		else {
			this$.processes[[process$id]]<-list (
				object 				= 	process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params		=		process$siteSpecificParamList	
			);
		}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: detachProcess
##
setMethodS3(
  "detachProcess",
  class="Site",
  function(
    this,
    process,
    ...
  ){
			
		if(!is.Process(process)) {
			throw("Process object is not valid!\n");
		}
		else if (!isAttached(this,process)) {
				warning("Process is not attached, doing nothing!\n");
		}
		
		# Setting the list entry to NULL,
		# so it will wanish from the list.
		this$.processes[[process$id]]<-NULL;
	
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: getProcesses
##
setMethodS3(
  "getProcesses",
  class="Site",
  function(
    this,
    ...
  ){
  
		lapply(names(this$.processes),function(id){this$.processes[[id]][["object"]]});
		
	},
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: setProcesses
##
setMethodS3(
  "setProcesses",
  class="Site",
  function(
    this,
		value,
    ...
  ){

		if(missing(value)) {throw("No new value given!\n")}
		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		for(i in value) {
			if(!is.Process(i)){
					throw("The accepted argument is a list of processes!\nVectors and lists are not euivalent, take care!\n");
			}
		}
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcess(this,process)});
		lapply(to.attach, function(process) {attachProcess(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .setProcessesSloppy
##
setMethodS3(
  ".setProcessesSloppy",
  class="Site",
  function(
    this,
		value,
    ...
  ){

		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcess(this,process)});
		lapply(to.attach, function(process) {.attachProcessSloppy(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );
