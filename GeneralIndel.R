##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	
setConstructorS3(
  "GeneralInDel",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		any.alphabet<-Alphabet(type="*ANY*");
		any.alphabet$.any.flag<-TRUE;
		this<-Process(
			alphabet=any.alphabet
		);
    this<-extend(
      this,
      "GeneralInDel",
			.rate=rate,
			.propose.by=NA,
			.accept.by=NA,
			.is.general.indel=TRUE
    );

		# Using virtual field to clear Id cache:
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
	class="GeneralInDel", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$rate)) {
          this$rate<-this$rate;
        }

				if(!is.function(this$proposeBy)){
					if(!is.na(this$proposeBy)){
						throw("proposeBy is invalid!\n");
					}
				}
				
				if(!is.function(this$acceptBy)){
					if(!is.na(this$acceptBy)){
						throw("acceptBy is invalid!\n");
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
## Method: getRate
##	
setMethodS3(
	"getRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.rate;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasUndefinedRate
##	
setMethodS3(
	"hasUndefinedRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		return(is.na(this$.rate));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
			throw("No new value provided!\n");}
		else if(!is.numeric(value)) {
			throw("Rate must be numeric!\n");
		} else {
			this$.rate<-value;
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProposeBy
##	
setMethodS3(
	"getProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.propose.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
setMethodS3(
	"setProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of proposeBy must be a function.!\n");	
		} else {
			this$.propose.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptBy
##	
setMethodS3(
	"getAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.accept.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptBy
##	
setMethodS3(
	"setAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of acceptBy must be a function.!\n");	
		} else {
			this$.accept.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: proposeLength
##	
setMethodS3(
	"proposeLength", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		return( this$.propose.by());

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: is.GeneralIndel
##	
setMethodS3(
	"is.GeneralInDel", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.indel)){return(TRUE)}
    if ( inherits(this, "GeneralInDel")) {
      this$.is.general.indel<-TRUE;
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
## Method: summary
##	
setMethodS3(
	"summary", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){
	
		.addSummaryNameId(this);
		this$.summary$"General rate"<-this$rate;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################
# Class:GeneralInsertor
setConstructorS3(
  "GeneralInsertor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		template.seq=NA,
		insert.hook=NA,
		accept.win=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralInsertor",
			.generate.by=NA,
			.handler.template=NA,
			.template.seq=NA,
			.insert.hook=NA,
			.accept.win=1,
			.is.general.insertor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="insertion.tolerance",
      name="Insertion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );
	
		if(!missing(template.seq)){
			this$templateSeq<-template.seq;
		}

		this$acceptBy<-function(process=NA,sequence=NA,range=NA){
			
				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Discard the site if the process is not attached to it:
						if(!isAttached(site, process)){
							next();
						}
						else {
							accept.prob<-c(accept.prob, getParameterAtSite(process, site, "insertion.tolerance")$value);
						}
				}
				accept.prob<-prod(as.numeric(accept.prob));


			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}

	###	

	 this$generateBy<-function(process=NA,length=NA){

			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	
			else if(is.na(process$.template.seq)){
				throw("Cannot generate insert without template sequence!\n");
			}

			times<-( ceiling( length/this$.template.seq$.length) );
			to.delete<-( ( (this$.template.seq$.length) * times) - length);

			tmp<-clone(this$.template.seq);
		
			if( (times-1) > 0){
				for(i in 1:(times-1)){
					insertSequence(tmp,process$.template.seq,tmp$length);
				}
			}

			if(to.delete > 0){
				deleteSubSequence(tmp,(tmp$length - to.delete + 1):tmp$length);
			}
			return(tmp);
				
	 }

	if(!missing(insert.hook)){
		this$insertHook<-insert.hook;
	}

	###	
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 WINDOW.SIZE<-this$.accept.win;
					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 process<-event$.process;
					 sequence<-getSequence(getSite(event));
					 details<-list();
					 details$type<-"insertion";

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Set insertion tolerance window:
					 # FIXME - more general handling
					 window<-integer();
					 insert.pos<-position;
					 if(direction == "LEFT") {
							window<-(position-WINDOW.SIZE):position;
					 		insert.pos<-(position-1);
					 }
					 else if (direction == "RIGHT"){
							window<-position:(position+WINDOW.SIZE);
					 }
					 else {
						throw("You should never see this message!\n");
					}

					details$position<-insert.pos;
					details$accepted<-FALSE;

					# Discard illegal positions:
					window<-window[ window > 0 & window <= sequence$.length];
				  if(process$.accept.by(process=process,sequence,window)){
							details$accepted<-TRUE;
							insert<-generateInsert(process);
							details$length<-insert$length;
							# Call the insert hook:
							if(is.function(this$.insert.hook)){
								insert<-this$.insert.hook(insert);
							}
							insertSequence(sequence,insert, insert.pos,process=process);
					}
					return(details);
					
				}
		 }
		###

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralInsertor
##	
setMethodS3(
	"is.GeneralInsertor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.insertor)){return(TRUE)}
    if ( inherits(this, "GeneralInsertor")) {
      this$.is.general.insertor<-TRUE;
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
	class="GeneralInsertor", 
	function(
		this,
		length,
		...
	){

		      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$templateSeq)) {
          this$templateSeq<-this$templateSeq;
        }

        if(!is.function(this$generateBy)){
          if(!is.na(this$generateBy)){
            throw("generateBy is invalid!\n");
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
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="GeneralInsertor", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		if(missing(target.site)) {
			throw("No target site provided!\n");
		} else if (!sloppy) {
			if(!is.Site(target.site)) {
				throw("Target site invalid!\n");
			}
			else if(!is.function(this$.propose.by)) {
				throw("proposeBy is not set, cannot propose insertion!\n");
			} 
			else if (!is.function(this$.accept.by)){
				throw("acceptBy is not set, cannot generate insertion event!\n");
			}
		} #/!sloppy

		# Check if process is attached?

		# Just return an empty list if the rate is undefined or zero:
		if( is.na(this$.rate) | this$.rate == 0) {
			return(list());
		}

		 # Clone the event template object:
		 insertion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 insertion.event$.position<-target.site$.position;
		 # Set the target site:
		 insertion.event$site<-target.site;
		 # Set the target state (good for consistency):
		 insertion.event$targetState<-getState(target.site);
		 # Set event name:
		 insertion.event$name<-"Insertion";
		 # Set the generator process:
		 insertion.event$process<-this;
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
		 rate.multiplier<-getParameterAtSite(this,target.site,"rate.multiplier")$value;
		 if(rate.multiplier == 0 ) {
			return(list());
		 }
		 insertion.event$rate<-(this$rate * rate.multiplier );

		 # Set the handler for the insertion event:
		 .setHandler(insertion.event, this$.handler.template);

		# Write protect the event object:	
		insertion.event$writeProtected<-TRUE;	

		# Return the event object in a list:
		list(insertion.event);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: generateInsert
##	
setMethodS3(
	"generateInsert", 
	class="GeneralInsertor", 
	function(
		this,
		length=NA,
		...
	){

		if(missing(length)){
			length<-this$.propose.by(process=this);
		}
		insert<-this$.generate.by(process=this,length);
		sampleStates(insert);	
		return(insert);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getGenerateBy
##	
setMethodS3(
	"getGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.generate.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setGenerateBy
##	
setMethodS3(
	"setGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of generateBy must be a function.!\n");	
		} else {
			this$.generate.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTemplateSeq
##	
setMethodS3(
	"getTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.template.seq;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTemplateSeq
##	
setMethodS3(
	"setTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new template sequence provided!\n");	
		}
		else if(!is.Sequence(value)){
			 throw("Sequence object is invalid!\n");	
		}
		else if(value$length == 0) {
			throw("Cannot set template sequence of length zero!\n");
		}
		else {
			this$.template.seq<-clone(value);
			for (site in this$.template.seq$.sites){
				site$.ancestral<-this;
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
## Method: getGenerateBy
##	
setMethodS3(
	"getAcceptBy", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.accept.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptWin
##	
setMethodS3(
	"getAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.accept.win;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptWin
##	
setMethodS3(
	"setAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No new value provided");
		}
		else if(!all(is.numeric(value)) | (length(value) != 1)){
			throw("The new value must be a numeric vector of length one.");
		}
		else{
			this$.accept.win<-value;
		}
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary
##	
setMethodS3(
	"summary", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
		this$.summary$"Accept window size"<-this$.accept.win;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###########################################################################
# Class:GeneralDeletor
setConstructorS3(
  "GeneralDeletor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralDeletor",
			.handler.template=NA,
			.is.general.deletor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="deletion.tolerance",
      name="Deletion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );

		this$acceptBy<-function(process=NA,sequence=NA,range=NA){

				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Reject if the range contains a site which is not attached to 
						# the process:
						if(!isAttached(site, process)){
							return(FALSE);
						}
						accept.prob<-c(accept.prob, getParameterAtSite(process, site, "deletion.tolerance")$value);
				}

				# Calculate the product of the per-site 
				# acceptance probabilities.
				accept.prob<-prod(as.numeric(accept.prob));

			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}
		
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 process<-event$.process;
					 sequence<-getSequence(getSite(event));
					 details<-list();
					 details$type<-"deletion";
					 details$accepted<-FALSE;

					 # Propose a sequence length:
					 length<-process$proposeBy(process=process,seq=sequence, pos=position);

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Calculate the sites to delete:
					 range<-numeric();	
					 if(direction == "RIGHT") {
							range<-position:(position+length-1);
					 } else if(direction == "LEFT") {
						  range<-(position-length+1):position;
					 } else {
							throw("You should never see this message!\n");
					 }

					 # Discard potential negative values and values larger than the sequence length:
					 range<-range[ range > 0 & range <= sequence$.length];
					 details$range<-c(min(range),max(range));
					 
					 # Perform the deletion if it is accepted:
					 if (process$.accept.by(process=process,sequence=sequence,range=range) == TRUE) {
						details$accepted<-TRUE;
					 	deleteSubSequence(sequence,range);
					}					
			
					# Return event details:	
					return(details);

				}
		 }

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralDeletor
##	
setMethodS3(
	"is.GeneralDeletor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.deletor)){return(TRUE)}
    if ( inherits(this, "GeneralDeletor")) {
      this$.is.general.deletor<-TRUE;
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
  class="GeneralDeletor",
  function(
    this,
    length,
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


##	
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="GeneralDeletor", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		if(missing(target.site)) {
			throw("No target site provided!\n");
		} else if (!sloppy) {
			if(!is.Site(target.site)) {
				throw("Target site invalid!\n");
			}
			else if(!is.function(this$.propose.by)) {
				throw("proposeBy is not set, cannot propose deletion!\n");
			} 
			else if (!is.function(this$.accept.by)){
				throw("acceptBy is not set, cannot generate deletion event deletion!\n");
			}
		} #/!sloppy

		 # Complain if sequence has a zero length:
		 if(target.site$.sequence$.length == 0) {
			 throw("Sequence has zero length so there is nothing to delete! How did you get here anyway?\n");
		 }

		 # Clone the event template object:
		 deletion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 deletion.event$.position<-target.site$.position;
		 # Set the target site:
		 deletion.event$site<-target.site;
		 # Set the target state (good for consistency):
		 deletion.event$targetState<-getState(target.site);
		 # Set event name:
		 deletion.event$name<-"Deletion";
		 # Set the genrator process:
		 deletion.event$process<-this;
		
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
		 deletion.event$rate<-(this$rate * (getParameterAtSite(this,target.site,"rate.multiplier")$value) );

		 # Set the handler for the deletion event:
		 .setHandler(deletion.event, this$.handler.template);

		# Write protect the event object:	
		deletion.event$writeProtected<-TRUE;	

		# Return the event object in a list:
		list(deletion.event);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary
##	
setMethodS3(
	"summary", 
	class="GeneralDeletor", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertHook
##	
setMethodS3(
	"getInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.insert.hook;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertHook
##	
setMethodS3(
	"setInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.Sequence(this$.template.seq)){
			throw("Cannot set insert hook because the template sequence is not defined!\n");
		}
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The insert hook must be a function.!\n");	
		}
		else if( length(intersect(names(formals(value)), "seq")) == 0 ){
      throw("The insert hook function must have a an argument named \"seq\"");
		}
		else if(!is.Sequence(value(generateInsert(this,length=1)))){
			throw("The insert hook function must return a Sequence object!\n");	
		} else {
			this$.insert.hook<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);




