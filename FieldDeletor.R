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
		type="geometric",
		... 
		)	{

		# Creating a GeneralDeletor Process.
		this<-GeneralDeletor(
			...
		);

		# Extending as FieldDeletor:
    this<-extend(
      this,
      "FieldDeletor",
			.type=type,
			.q.max=NA
    );

		# Using virtual field to clear Id cache:
		this$name<-name;

		# Set the function proposing deletion lengths:

	  this$proposeBy<-function(process,seq,pos){

					# Get all deletion tolerance parameters for this process:					
					deletion.tolerance<-c();

					for(site in seq$.sites){
							if(isAttached(site, process)){
									deletion.tolerance<-c(deletion.tolerance, getParameterAtSite(process, site, id="deletion.tolerance")$value);
							}
					} # for site
					
					# Calculate the "q vector":
					q<-exp(-deletion.tolerance);

					# Get the maximal q value:
					q.max<-max(q);
				
					# Set the actual q.max for this process. acceptyBy will use this:
					process$.q.max<-q.max;
					
					# express<-expression(rgeom(1,(1-q.max))+1);

				  # FIXME

					return( round( eval(express) ) );
					


			} # /proposeBy
		
			# Set the function performing the accept/reject step:
			this$acceptBy<-function(process,sequence,range){

				# Get the actual q.max
				q.max<-process$.q.max;
				# And remove from the process object to guard against trouble:
				process$.q.max<-NA;

				# Get the deletion tolerance parameters from the proposed range:
				deletion.tolerance<-c();
				
				for(site in seq$.sites[range]){

				if(isAttached(site, process)){

									deletion.tolerance<-c(deletion.tolerance, getParameterAtSite(process, site, id="deletion.tolerance")$value);

							} else {

								# Reject the proposed deletion if that contains sites which are not attached to the process.
								# This will create an edge effect of course! 
								return(FALSE);

							}
					} # for site

				# Get the length of the proposed deletion:				
				K<-length(deletion.tolerance);

				# Calculate the q.prod:
				q.prod<-prod(exp(-deletion.tolerance));
		
				# Calculate the acceptance probability:
				accept.prob<-(q.prod/(q.max^K));

        # Accept/reject:
        return ( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
	
			} # /acceptBy
			

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




