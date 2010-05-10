##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## CodonSequence
##
setConstructorS3(
  "CodonSequence",
  function(
    name=NA,
    string=NA,
    length=NA,
		table.id=1,
    processes=NA,
    ancestral.obj=NA,
    ...
	){

		if (!missing(string)){

			len<-((stringLength(string)/3));
			flen<-floor(len);

			this<-Sequence(length=flen,alphabets=list(CodonAlphabet(table.id=table.id)));
			if(len > flen){
				warning("The length of the provided string was not multiple of 3. The incomplete codon was discarded!\n");
			}

			nuc<-strsplit(string,"")[[1]];


			j<-1;	# counter for the codon position
			for(i in seq(from=1,to=(flen * 3),by=3)){

					# get the codon:
					state<-paste(nuc[c(i,i+1,i+2)],collapse="");

					# Check for stop codons:
					if( isStopCodon(this$.sites[[j]]$.alphabet, state) ){
						throw("The CodonSequence objects does not accept stop codons as valid states!\n");
					}

					# Set the state:			
					setStates(this,state,j);		
					j<-j+1;
			}
			
		}
		else if(!missing(length)){
			this<-Sequence(length=length,alphabets=list(CodonAlphabet(table.id=table.id)));
		}
		else {
			this<-Sequence(alphabets=list(CodonAlphabet(table.id=table.id)));
		}

		this<-extend(this, "CodonSequence");
		
		if(!missing(name)){
			this$name<-name;
		}
		
		if(!missing(processes)){
			setProcesses(this,processes);
		}
	
		if(!missing(ancestral.obj)){
			this$.ancestral<-ancestral.obj;
		}

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
	class="CodonSequence", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
				for(pos in seq(along=this$.sites)){
					if(!is.CodonAlphabet(this$.sites[[pos]]$.alphabet)){
						throw("The alphabet attached to site ",pos," is not a codon alphabet!\n");
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
## Method: Translate
##  
setMethodS3(
  "Translate",
  class="CodonSequence",
  function(
    this,
    ...
  ){ 

			# Create an amino acid sequence of proper length:
			that<-AminoAcidSequence(
					name=paste("Translation of",this$name),
					length=this$length
			);

			# Setting the states, using the site alphabet to translate codons for greater flexibility.
			for (pos in seq(along=this$.sites)){
				setState(that$.sites[[pos]], translateCodon(this$.sites[[pos]]$alphabet, this$.sites[[pos]]$state) );
			}

			return(that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

