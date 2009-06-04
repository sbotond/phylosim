##	$Id: ContinousDeletor.R,v 1.4 2009-04-24 10:12:27 sbotond Exp $
##
##	Class: FieldDeletor
##	Descriprion: 
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
setConstructorS3(
  "FieldDeletor",
  function( 
		name="Anonymous",
		type="exponential",
		... 
		)	{

		this<-GeneralDeletor(
			...
		);
    this<-extend(
      this,
      "FieldDeletor",
			.type=type,
			.q.max=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		if(type == "exponential"){

			this$proposeBy<-function(seq,pos){

					# Find out the maximum deletion tolerance parameter:					
					# FIXME - this is very slow!
					q<-c();

					for(site in seq$.sites){
							if(isAttached(site, this)){
									tmp<-getParameterAtSite(this, site, id="deletion.tolerance")$value;
									tmp<-exp(-tmp);
									q<-c(q,tmp);
							}
					} # for site
					q.max<-max(q);
					
					express<-expression(rgeom(1,(1-q.max))+1);
					tmp<-round(eval(express));
					this$.q.max<-q.max;
					cat("Proposed",tmp,"\n");
					return(tmp);

			} # if exponetial
		
			this$acceptBy<-function(sequence,range){

				q.max<-this$.q.max;
				this$.q.max<-NA;

				q<-c();
				# FIXME - missing process conceptual issue!
				for(site in seq$.sites[range]){
							if(isAttached(site, this)){
									tmp<-getParameterAtSite(this, site, id="deletion.tolerance")$value;
									q<-c(q,tmp);
							}
					} # for site
				
				K<-length(q);
				q<-prod(exp(-q));

				accept.prob<-(q/(q.max^K));
        # Accept/reject:
        tmp<-( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
				return(tmp);
	
			}
		
		
		} # /exponential


    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
setMethodS3(
  "checkConsistency",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

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
## Method: proposeLength
##	
setMethodS3(
  "proposeLength",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

			throw("Disabled for FieldDeletion processes!\n");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: getType
##	
setMethodS3(
  "getType",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

		this$.type;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setType
##	
setMethodS3(
  "setType",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

		throw("The type of the FieldDeletor process cannot be modified. Please set it by the constructor argument.");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);




