##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	
setConstructorS3(
	"Sequence",
	function(
		name=NA,
		string=NA,
		length=NA,
		alphabets=NA,
		processes=NA,
		ancestral.obj=NA,
		...
	){

		# Marking the instance as static by default:	
		STATIC<-TRUE;	

		# Extending the PSRoot class:
		this<-extend(
			PSRoot(),
			"Sequence",
			.name="Anonymous",
			.length=NA,
			.sites=list(),
			.ancestral.obj=NA,
			.cumulative.rates=NA,
			.total.rates=NA,
			.cumulative.rate.flag=TRUE,
			.flagged.sites=integer(0),
			.write.protected=FALSE,
			.is.sequence=TRUE,
			.root.ins=NA
		);

		# Initializing the variables for length and states:
		len<-0;
		str<-list();

		# Optional argument: name
		if(!missing(name)) {
			this$name<-name;
			STATIC<-FALSE;
		}
		
		# The user can specify a sequence
		# or the sequence length, but not both.		
		if (!missing(string) & !missing(length)) {
			throw("You can specify the sequence, or the sequence length, but not both!\n");}
		else if (!missing(string)){
			STATIC<-FALSE;
			# An alphabet list must be specified along the sequence! 
			if(missing(alphabets)) {throw("A list of valid alphabets must be specified when a string is given!\n");}
		}

		# Deal with the string or length argument:
		if (!missing(length)) {
			STATIC<-FALSE;
			len<-length;
		}
		else if( !missing(string) ) {
					str<-strsplit(string,split="")[[1]];
					len<-length(str);
		}
		this$.length<-len;

		root.ins<-NA;
		if (!is.Process(Sequence$.root.ins)) {
			# Dummy proces used as ancestral object for sites.
			this$.root.ins<-Process(name="Root insertion process");
			this$.root.ins$comments<-"This is just a dummy process object serving as ancestral for newly created site and sequence objects.";
			root.ins<-this$.root.ins;
		} else {
			root.ins<-Sequence$.root.ins
		}

		# Site template object:
			 site.template<-Site(
            ancestral=root.ins,
            sequence=this
          );	
	

		# Clone and store the site objects:
		if(!STATIC) {
			if ( len > 0 ) {
				for(position in 1:len) {
					 this$.sites[[position]]<-clone(site.template);
				}
			}

		}	
		
		# Optional argument: ancestral object
		if (!missing(ancestral.obj)) {
			STATIC<-FALSE;
			this$ancestral<-ancestral.obj;
		} else {
			this$ancestral<-root.ins;
		}

		# Setting the alphabets:
		if(!missing(alphabets)) {
			STATIC<-FALSE;
			# setAlphabets will check the arguments
			setAlphabets(this, alphabets);
		}
	
		# Setting the processes:
		if (!missing(processes)) {
			STATIC<-FALSE;
			# setProcesses will take care about the arguments
			# and deal with alphabet mismatch.
			setProcesses(this,processes)	
		}

			# Initializing these vectors properly is
			# importtant for the insertion method!
			this$.total.rates<-double(len);
			this$.cumulative.rates<-double(len);

		if(!STATIC){

			# Now we are prepared to set the states:	
			if (!missing(string)) {
				setStates(this, str);
			}

			# Calculate cumulative rates for the first time, but only if
			# states are defined. This is expensive, as total rates are calculated.
			if (!missing(string) & (length(str) > 0) ) {
				.recalculateCumulativeRates(this);
			}

		}

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.Site
##	
setMethodS3(
	"is.Sequence", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.sequence)){return(TRUE)}
    if ( inherits(this, "Sequence")) {
      this$.is.sequence<-TRUE;
      return(TRUE);
    } else {
      return(FALSE)
    }

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
	class="Sequence", 
	function(
		this,
		ommit.sites=FALSE,
		...
	){

		if(this$length != length(this$.sites)) {
			throw("Sequence length inconsistency detected");
		} else if (length(this$.cumulative.rates) != this$.length) {
			throw("Cumulative rates vector length mismatch!\n");
		} else if (length(this$.total.rates) != this$.length) {
			throw("Total rates vector length mismatch!\n");
		} else if (!identical(this$.cumulative.rates, cumsum(this$.total.rates))) {
			throw("Cumulative rates vector is not consistent with total rates vector!\n");
		}

		if(!is.numeric(this$.flagged.sites)) {
			throw("Flagged sites vector is not numeric!\n");
		} else if (length(this$.flagged.sites) > 0) {
			if ( (min(this$.flagged.sites) < 1) | ( max(this$.flagged.sites) > this$.length) ) {
				throw("Inconsistency in the flagged sites vector!\n");
			}	
		}

		if(!is.character(this$.name)) {
			throw("Sequence name is invalid!\n");
		} else if(stringLength(this$.name) == 0) {
			throw("Sequence name is of length zero!\n");
		}

		if(!is.Sequence(this$.ancestral.obj) & !is.Process(this$.ancestral.obj)) {
			throw("The ancestral object is invalid!\n");
		}
		if(!is.logical(this$.cumulative.rate.flag)) {
			throw("Cumulative rate flag is not logical!\n");
		}

		# Calling consistency check on sites.
		# This will be painfully slow!
		if(!ommit.sites){
			for(site in this$.sites){
				checkConsistency(site);
			}
		}
		return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getId
##
setMethodS3(
  "getId",
  class="Sequence",
  function(
    this,
    ...
  ){

  this.class<-class(this)[1];
  id<-paste(this.class,this$.name,hashCode(this),sep=":");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setId
##	
setMethodS3(
	"setId", 
	class="Sequence", 
	function(
		this,
	  value,	
		...
	){

	throw("Id is generated automatically and it cannot be set!\n");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getName
##	
setMethodS3(
	"getName", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$.name;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setName
##	
setMethodS3(
	"setName", 
	class="Sequence", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getString
##	
setMethodS3(
	"getString", 
	class="Sequence", 
	function(
		this,
		...
	){
			str<-character();
			for(site in this$.sites){
					if(is.na(site$.state)){
							if(is.Alphabet(site$.alphabet)){
								str<-paste(str,paste(rep("?",site$.alphabet$symbolLength),collapse=""),sep="");
							}
							else {
								str<-paste(str,"?",sep="");
							}
					}
					else {
						str<-paste(str,site$.state,sep="");
					}
			}
			return(str);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setString
##	
setMethodS3(
	"setString", 
	class="Sequence", 
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
## Method: getLength
##	
setMethodS3(
	"getLength", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$.length;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLength
##	
setMethodS3(
	"setLength", 
	class="Sequence", 
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
## Method: getSites
##	
setMethodS3(
	"getSites", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		this$.sites;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getStates
##	
setMethodS3(
	"getStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		 if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$.state;
      }
    );

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setStates
##	
setMethodS3(
	"setStates", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
	  else if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
		# Recycling value vector by using rep().
    if (length(value) < length(this$.sites) ) {
        value<-rep(as.character(value),length.out=length(index))
    }
		
    for (i in 1:length(index)) {
        this$.sites[[ index[[i]] ]]$state<-value[i];
    }
		# Flagging the changed sites:
		.flagCumulativeRates(this);
		this$.flagged.sites<-c(this$.flagged.sites, index);
		.recalculateCumulativeRates(this);

    invisible(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getStatesList
##	
setMethodS3(
	"getStatesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		StatesList(seq=this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabets
##	
setMethodS3(
	"getAlphabets", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$alphabet;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabets
##	
setMethodS3(
	"setAlphabets", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		sloppy=FALSE,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
				for(a in value) {
					if(!is.Alphabet(a)) {
						throw("The value parameter must be a list of valid alphabet objects!\n");
					}
				}
		}

	  if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!

    value.counter<-1;
    for (i in index)  {
        if(value.counter > length(value)) {
          value.counter<-1;
        }
				# Arguments were verified before, using the sloppy method:
				if(sloppy) {
					.setAlphabetSloppy(this$.sites[[i]], value[[value.counter]]);
				} else {
					setAlphabet(this$.sites[[i]], value[[value.counter]]);
				}

        value.counter<-(value.counter + 1);
    }
    invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabetsList
##	
setMethodS3(
	"getAlphabetsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$alphabetsList<-NULL;
		AlphabetsList(seq=this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueAlphabets
##	
setMethodS3(
	"getUniqueAlphabets",
	class="Sequence", 
	function(
		this,
		...
	){
	
		tmp<-list();			
    lapply(
      this$.sites,
      function(site) {
				tmp<<-c(tmp,list(site$.alphabet))
      }
		);
		return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueAlphabets
##	
setMethodS3(
	"setUniqueAlphabets", 
	class="Sequence", 
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
## Method: attachProcess
##	
setMethodS3(
	"attachProcess", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

		for(i in index){
				attachProcess(this$.sites[[i]],process);
		}
		return(invisible(this));

	},
	private=FALSE,
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
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          detachProcess(site,process);
      }
    );	
		return(invisible(this));

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
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}

    lapply(
      this$.sites[index],
      function(site) {
          site$processes;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueProcesses
##	
setMethodS3(
	"getUniqueProcesses", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		tmp<-list();			
    lapply(
      this$.sites,
      function(site) {
				tmp<<-c(tmp,site$processes)
      }
		);
		return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueProcesses
##	
setMethodS3(
	"setUniqueProcesses", 
	class="Sequence", 
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
## Method: setProcesses
##	
setMethodS3(
	"setProcesses", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		sloppy=FALSE,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}

		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
			lapply(
					value,
					function(element) {
						if(!is.list(element)){
							throw("The value parameter must be a list of lists containing process objects!\n");
						}
					}
			);
		}

	  if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

    value.counter<-1;

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!
    for (i in index)  {
        if(value.counter > length(value)) {
          value.counter<-1;
        }
				if (sloppy == FALSE) {
						setProcesses(this$.sites[[i]], value[[value.counter]]);
				} else {
					.setProcessesSloppy(this$.sites[[i]], value[[value.counter]]);
				}
        value.counter<-(value.counter + 1);
    }
    invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setParameterAtSites
##	
setMethodS3(
	"setParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		value,
		index,
		...
	){

		.checkWriteProtection(this);
		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		} else if (missing(value)){
			throw("No new value given!\n");
		}

		if (missing(index)) {
			index<-seq(along=this$.sites);
		} else {
			index<-.checkIndexSanity(this, index);
		}
	
		if(length(value) == 1) {
			lapply(
				this$.sites[index],
				function(site){
					setParameterAtSite(process,site,id,value);
				}
			);
		} else {
			
			counter<-1;
			lapply(
				this$.sites[index],
				function(site){
					if( counter > length(value) ){
						counter<<-1;
					}	
					setParameterAtSite(process,site,id,value[[counter]]);
					counter<<-(counter+1);

				}
			);
				
		}

		return(invisible(this));

		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateMultipliers
##	
setMethodS3(
	"setRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if(missing(value)){
			throw("No value provided!\n");
		}
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
		}
		setParameterAtSites(this=this,process=process,id="rate.multiplier",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateMultipliers
##	
setMethodS3(
	"getRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="rate.multiplier",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);



##	
## Method: getParameterAtSites
##	
setMethodS3(
	"getParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index,
		...
	){

		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		}

		if (missing(index)) {
			index<-seq(along=this$.sites);
		} else {
				index<-.checkIndexSanity(this, index);
		}	
		
		lapply(
			this$.sites[index],
			function(site){
				getParameterAtSite(process,site,id);
			}
		);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcessesList
##	
setMethodS3(
	"getProcessesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		ProcessesList(seq=this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEvents
##	
setMethodS3(
	"getEvents", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
		
 		if (missing(index)) {
      index<-seq(along.with=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

		tmp<-list();
    for (i in index){
					# Setting the .positions field for then Events.
					this$.sites[[i]]$.position<-i;
          tmp<-c(tmp, getEvents(this$.sites[[i]]));
					# Deleting the .position field;
					this$.sites[[i]]$.position<-NULL;
    }
		tmp;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventsList
##	
setMethodS3(
	"getEventsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		getEvents(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setEventsList
##	
setMethodS3(
	"setEventsList", 
	class="Sequence", 
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
## Method: getTotalRates
##	
setMethodS3(
	"getTotalRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){


 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

	
		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.total.rates[index];

 		#if (missing(index)) {
    #  index<-seq(along=this$.sites);
    #}
		#lapply(
		#		this$.sites[index],
		#		function(site){
		#				getTotalRate(site);
		#			}
		#		);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRates
##	
setMethodS3(
	"getTotalRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getTotalRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTotalRatesList
##	
setMethodS3(
	"setTotalRates", 
	class="Sequence", 
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
## Method: getAncestralSequence
##	
setMethodS3(
	"getAncestral", 
	class="Sequence", 
	function(
		this,
		...
	){

	this$.ancestral.obj;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRates
##	
setMethodS3(
	"getCumulativeRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

	
		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.cumulative.rates[index];


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .recalculateCumulativeRates
##	
setMethodS3(
	".recalculateCumulativeRates", 
	class="Sequence", 
	function(
		this,
		target.site,
		...
	){
		
		# No flagged sites, we have a fresh start:	

		if( length(this$.flagged.sites) == 0 ) {
			if( this$.length > 0 ) {
				# Calculate total rates:
				for(i in 1:this$.length) {
					this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
				}
				this$.cumulative.rates<-cumsum(this$.total.rates);	
				this$.flagged.sites<-integer(0);

			}
	
		} else {
			if( this$.length > 0 ) {

				# We have some flagged sites, recalculate just their total rates:
        for(i in this$.flagged.sites) {
          this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
        }

				# The site before the first flagged site: 
				min.index<-(min(this$.flagged.sites) - 1);
				
				if(min.index > 1){
					
					# Do the cumsum only on the dirty part:
					new.cumrates<-this$.cumulative.rates[1:(min.index-1)];
					new.cumrates<-c(new.cumrates, cumsum( c(this$.cumulative.rates[min.index], this$.total.rates[(min.index+1):length(this$.total.rates) ] ) ) );
					this$.cumulative.rates<-new.cumrates;

				} else {
        	this$.cumulative.rates<-cumsum(this$.total.rates);
				}
				
				# Cleaning out flagged sites.
				this$.flagged.sites<-integer(0);
				
      } #/else
	
		}
	
		this$.cumulative.rate.flag<-FALSE;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .flagCumulativeRates
##	
setMethodS3(
	".flagCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){
		
		this$.cumulative.rate.flag<-TRUE;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRates
##	
setMethodS3(
	"getCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getCumulativeRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setCumulativeRates
##	
setMethodS3(
	"setCumulativeRates", 
	class="Sequence", 
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
## Method: getBigRate
##	
setMethodS3(
	"getBigRate", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		if (length(this$.sites) > 0) {
			if(this$.cumulative.rate.flag)	{
				.recalculateCumulativeRates(this);	
			}
			getCumulativeRatesFromRange(this, this$.length);
		} else {
			return(NA);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBigRate
##	
setMethodS3(
	"setBigRate", 
	class="Sequence", 
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
## Method: setAncestralSeq
##	
setMethodS3(
	"setAncestral", 
	class="Sequence", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);	
		if (!is.Sequence(value) & ! is.Process(value)) {
			throw("Ancestral object must be a sequence or a process!\n");
		}	else {
			this$.ancestral.obj<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: clone.Sequence
##	

setMethodS3(
	"clone", 
	class="Sequence", 
	function(
		this,
		...
	){

		# Cloning the whole sequence object:
		that<-clone.Object(this);

		# Disabling write protection:

		if(that$writeProtected) {
			that$writeProtected<-FALSE;
		}

		# Setting the ancestral sequence:
		that$.ancestral.obj<-this;

		# Resetting comments:
		that$.comments<-list();

		# Cloning sites;
		if(this$length > 0) {
					for (i in 1:this$.length) {
						site<-this$.sites[[i]];
						clone<-clone(site);
						clone$.ancestral<-site;
						clone$.sequence<-that;
						that$.sites[[i]]<-clone;
			}
		}

		# Setting the name:
		that$name<-paste("clone of",this$.name);
	
		return(that);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: getWriteProtected
##
setMethodS3(
  "getWriteProtected",
  class="Sequence",
  function(
    this,
    ...
  ){

    this$.write.protected;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkIndexSanity
##
setMethodS3(
  ".checkIndexSanity",
  class="Sequence",
  function(
    this,
    index,
    ...
  ){

		if (length(index) == 0 ) {
				return(c());
		}
	  if( length(index) == 1 ) {
			if(is.na(index)) {
				warning("Index vector is NA! Coercing to empty vector!\n");
				return(c());
			} 
			if (is.nan(index)) {
				warning("Index vector is NaN! Coercing to empty vector!\n");
				return(c());
			}

		}

		if(min(index) < 1 ) {
			throw("Index vector element ",min(index)," too small!\n");
		}
		if( max(index) > this$.length ) {
			throw("Index vector element ",max(index)," too big!\n");
		}
		return(index);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
##
## Method: setWriteProtected
##
setMethodS3(
  "setWriteProtected",
  class="Sequence",
  function(
    this,
    value,
    ...
  ){

    if(!is.logical(value)) {throw("The new value must be logical!\n")}
    else {
      this$.write.protected<-value;
    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkWriteProtection
##
setMethodS3(
  ".checkWriteProtection",
  class="Sequence",
  function(
    this,
    value,
    ...
  ){

    if(getWriteProtected(this)) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Sequence
##	
setMethodS3(
	"summary", 
	class="Sequence", 
	function(
		this,
		...
	){

		 this$.summary$"Name"<-this$name;
		 this$.summary$"Id"<-this$id;
		 this$.summary$"Length"<-this$length;
		 this$.summary$"Big rate"<-this$bigRate;
		 this$.summary$"Ancestral object"<-this$ancestral$id;

		 if(this$.cumulative.rate.flag) {
		 	this$.summary$"Cumulative rate flag"<-TRUE;
		 }
		 if(length(this$.flagged.sites) > 0 ) {
		 	this$.summary$"Flagged sites"<-paste(this$.flagged.sites,collapse=" ");
		 }
		 if(this$writeProtected) {
		 	this$.summary$"Write protected"<-TRUE;
		 }
		


			NextMethod();
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: as.character.Sequence
##	
setMethodS3(
	"as.character", 
	class="Sequence", 
	function(
		this,
		...
	){

		getString(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
setMethodS3(
	"plot", 
	class="Sequence", 
	function(
		this,
		index=NA,
		...
	){


    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
		if( length(unique(this$totalRates)) == 1 & any(is.na(unique(this$totalRates))) ){
      warning("The total rates are undefined, nothing to plot here!\n");
      return(invisible(FALSE));
		}
    else {
      if(missing(index)) {
        index<-seq(along=1:this$length,by=1);
      }
      if(this$.cumulative.rate.flag){
        .recalculateCumulativeRates(this);
      }
      what<-this$.total.rates[c(index)]

      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Total rate plot for sequence", this$id),
        xlab="Position",
        ylab="Total rate",
				ylim=c(0,max(what)),
        xlim=c(min(index),max(index)),
				xaxt="n"
      );
			axis(side=1, at=index, labels=index);
    }


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plotParameterAtSites
##	
setMethodS3(
	"plotParametersAtSite", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index=NA,
		...
	){


    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
    if(missing(index)) {
      index<-seq(along=3:this$length,by=1);
    }
		what<-apply(as.array(index),1,
			function(pos){
				tmp<-getParameterAtSites(this,process,id,pos)[[1]]$value;
				if(!is.numeric(tmp)){
					throw("Plot method failed becuase encountered non-numeric parameter value!\n");
				}
				return(tmp);
			}
		);
      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Plot of parameter",id,"for process",process$id),
        xlab="Position",
        ylab="Value",
        xlim=c(min(index),max(index)),
				ylim=c(0,max(what)),
				xaxt="n"
      );
			axis(side=1, at=index, labels=index);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setDeletionTolerance
##	
setMethodS3(
	"setDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
		setParameterAtSites(this=this,process=process,id="deletion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getDeletionTolerance
##	
setMethodS3(
	"getDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="deletion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertionTolerance
##	
setMethodS3(
	"setInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
		setParameterAtSites(this=this,process=process,id="insertion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertionTolerance
##	
setMethodS3(
	"getInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="insertion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: sampleStates
##	
setMethodS3(
	"sampleStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		if(!missing(index)){
			index<-.checkIndexSanity(this, index);
		}
		else {
			index<-seq(along=this$.sites);
		}
	
    for(site in this$.sites[index]){
    # Sample states from the equlibrium distributions if
    # the state is NA:
      if(is.na(site$state)){
        # Assemble the list of substitution processes:
        subst.proc<-list();
        for (proc in site$processes){
          if(is.GeneralSubstitution(proc)){
              subst.proc<-c(subst.proc, list(proc));
            }
        }

        # Complain if we have no substitution processes to sample from:
        if(length(subst.proc) == 0){
          throw("Site state is NA and no substitution processes are attached. Cannot sample state!\n");
        }

        site.rates<-as.numeric(lapply(
            subst.proc,
            function(proc){
              return(getParameterAtSite(proc, site,"rate.multiplier")$value);
            }
        ));

        # Normalizing site rates:
        site.rates<-site.rates/sum(site.rates);

        # Single subst process:
        if(length(subst.proc) == 1){
          site$state<-sampleState(subst.proc[[1]]);
        }
        else {
          # Sample a substitution process according to the rate multipliers:
          nproc<-sample(x=c(1:length(subst.proc)),size=1, replace=FALSE, prob=site.rates);
          # Sample the state from the winner process:
          site$state<-sampleState(subst.proc[[nproc]]);
        }
 		} # if is.na...

    }

		return(invisible(this));
	
		
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
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
		# FIXME	- dummy method to force the creation of the generic function
		warning("This is just a dummy method!");

		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getOmegas
##	
setMethodS3(
	"getOmegas", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
		# FIXME	- dummy method to force the creation of the generic function
		warning("This is just a dummy method!");

		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSymbolFreqs
##	
setMethodS3(
	"getSymbolFreqs", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){
		
			if(!missing(index)){
				index<-.checkIndexSanity(this, index);
			} else {
				index<-seq(along=this$.sites);
			}

			prop.table(table(as.character(lapply(this$.sites[index],getState))));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

