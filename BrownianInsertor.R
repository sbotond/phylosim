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

		this$proposeBy<-function(process=NA,...){
				if(!is.expression(process$.dist)){
					throw("\"dist\" is undefined, so cannot propose insertion length!\n");
				}
				else if(is.na(process$.max.length)){
					throw("\"maxLength\" is undefined, so cannot propose insertion length!\n");
				}
				tmp<-round(eval(process$.dist));
				while( tmp > process$.max.length | tmp < 1){  tmp<-round(eval(process$.dist)) };	
				return(tmp);
		}
	 
		this$generateBy<-function(process=NA,length=NA,target.seq=NA,target.pos=NA){
	
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

	###	
   
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

