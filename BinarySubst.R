##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass BinarySubst
# 
# @title "The BinarySubst class"
# 
# \description{ 
#	This is a class implementing a continuous-time Markov process acting on 
#	the state space defined by the \code{BinaryAlphabet} class.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
# 	\item{rate.list}{A list of substitution rates (see \code{setRateList.GeneralSubstitution}).}
#	\item{equ.dist}{Equilibrium distribution.}
# 	\item{...}{Additional arguments.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
# 	# The following code demonstrates 
#       # the use of the BinarySubst process    
#       # during a simulation.
#       p<-BinarySubst(rate=0.25,name="Binary",rate.list=list("0->1"=2,"1->0"=1))
#       # create a sequence object, attach process p
#       s<-BinarySequence(string="000000000000000000",processes=list(list(p)));
#       # make the first five positions invariable
#       setRateMultipliers(s,p,0,1:5)
#       # get rate multipliers
#       getRateMultipliers(s,p)
#       # simulate
#       sim<-PhyloSim(root.seq=s,phylo=rcoal(3))
#       Simulate(sim)
#       # print alignment
#       sim$alignment
# }
# 
# @author
#
# \seealso{ 
# 	GeneralSubstitution GTR WAG
# }
# 
#*/###########################################################################
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
###########################################################################/**
#
# @RdocMethod	checkConsistency
# 
# @title "Check object consistency"
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
#
# \arguments{ 
#       \item{this}{An object.} 
#       \item{...}{Not used.} 
# } 
# 
# 
# \value{ 
#		Returns an invisible TRUE if no inconsistencies found in the object, throws 
#		an error otherwise. 
# } 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
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

##
## Method: summary
##
###########################################################################/**
#
# @RdocMethod summary
#
# @title "Summarize the properties of an object"
#
# \description{
#       @get "title".
# }
#
# @synopsis
#
# \arguments{
#       \item{object}{An object}
#       \item{...}{Not used.}
# }
#
# \value{
#  Returns a PSRootSummary object.
# }
#
# \examples{
#
#       # create an object
#       p<-BinarySubst(rate=0.25,name="Binary",rate.list=list("0->1"=2,"1->0"=1))
#       # get a summary
#       summary(p)
# }
#
# @author
#
# \seealso{
#       @seeclass
# }
#
#*/###########################################################################
setMethodS3(
  "summary",
  class="BinarySubst",
  function(
    object,
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



