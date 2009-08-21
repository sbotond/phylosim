##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
setConstructorS3(
  "BinarySubst",
  function( 
		name="Anonymous", 
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

		got.rate.list<-!missing(rate.list);
		got.equ.dist<-!missing(equ.dist);

		this<-NA;
		
		if(got.rate.list & got.equ.dist){
			this<-GeneralSubstitution(name=name, rate.list=rate.list, equ.dist=equ.dist, alphabet=BinaryAlphabet());
		}
		else if(got.rate.list & !got.equ.dist){
			this<-GeneralSubstitution(name=name, rate.list=rate.list, alphabet=BinaryAlphabet());
		}
		else if(!got.rate.list & got.equ.dist){
			this<-GeneralSubstitution(name=name, equ.dist=equ.dist, alphabet=BinaryAlphabet());
		}
		else if(!got.rate.list & !got.equ.dist){
			this<-GeneralSubstitution(name=name, alphabet=BinaryAlphabet());
		}
		else {
			throw("You should never see this message!\n");
		}
		
		this<-extend(this, "BinarySubst");
		this$name<-this$name;

		return(this);
	
  },
  enforceRCC=TRUE
);

##
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="BinarySubst",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

				is.binary.alphabet<-inherits(this$alphabet, "BinaryAlphabet");
				if(!is.binary.alphabet){
					throw("The alphabet must be a BinaryAlphabet object!\n");
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


