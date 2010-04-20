##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
  "BrownianInsertor",
  function( 
		name="Anonymous",
		... 
		)	{

		this<-ContinousInsertor(
			 ...
		);
    
		this<-extend(
      this,
      "BrownianInsertor"
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
  class="BrownianInsertor",
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

			# FIXME

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
## Method: summary
##
setMethodS3(
  "summary",
  class="BrownianInsertor",
  function(
    this,
    value,
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

