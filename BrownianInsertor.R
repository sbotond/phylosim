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

		# Set insert hook function:
		this$.insert.hook<-function(seq=NA,target.seq=NA,target.pos=NA){
			print(target.seq);
			print(target.pos);
			print(seq);
			return(seq);
		}
	 
		this$generateBy<-function(process=NA,length=NA,target.seq=NA,target.pos=NA){
	
			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	

			this$.template.seq<-copySubSequence(target.seq,index=c(target.pos));		
			clearStates(this$.template.seq);
			tmp<-clone(this$.template.seq);

			times<-( ceiling( length/this$.template.seq$.length) );
			to.delete<-( ( (this$.template.seq$.length) * times) - length);

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
## Method: BrownianPath.PSRoot
##
setMethodS3(
  "BrownianPath",
  class="PSRoot",
  function(
	this,
	p=NA,
	a=NA,
    	...
  ){

	generate_brownian<-function(length, a){
		cumsum(rnorm(length,0,sd=a));
	}

	generate_bridge <- function (length,a){
		b <- generate_brownian(length,a)
		b - (1:length)/(length) * b[length]
	}

	generate_path <- function (p,a){
		n <- length(p);
		b <- generate_bridge (n+1,a);
		p + b[1:n];
	}

	generate_path(p,a);


  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
        static=TRUE,
  conflict="warning"
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

