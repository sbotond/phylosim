##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## BinarySequence
##
##########################################################################/** 
#
# @RdocClass BinarySequence
# 
# @title "The BinarySequence class"
# 
# \description{ 
#	Sequence objects aggregating Site objects having a BinaryAlphabet attached by default.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Name of the Sequence object.}
# 	\item{string}{A string specifying the length and the states of the Sequence object.}
#	\item{length}{The length of the sequence. Mutually exclusive with "string".}
# 	\item{processes}{A list of list of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
#	\item{ancestral.obj}{The ancestral object of the Sequence object (a valid Sequence or Process object).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create an empty BinarySequence object
#	s<-BinarySequence(length=50)
#	s
#	# set states
#	s$states<-c(0,0,1,0,1,1)
#	s
#	# create a sequence object by specifying a string
#	s<-BinarySequence(string="00000110010001111")
#	s
# }
# 
# @author
#
# \seealso{ 
# 	Sequence BinaryAlphabet
# }
# 
#*/###########################################################################
setConstructorS3(
  "BinarySequence",
  function(
    name=NA,
    string=NA,
    length=NA,
    processes=NA,
    ancestral.obj=NA,
    ...
	){

		if(!missing(string)){
			this<-Sequence(string=string,alphabets=list(BinaryAlphabet()));
		}
		else if(!missing(length)) {
			this<-Sequence(length=length,alphabets=list(BinaryAlphabet()));
		}
		else {
			this<-Sequence(alphabets=list(BinaryAlphabet()));
		}

		this<-extend(this, "BinarySequence");
		
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
## NucleotideSequence
##
##########################################################################/** 
#
# @RdocClass NucleotideSequence
# 
# @title "The NucleotideSequence class"
# 
# \description{ 
#	Sequence objects aggregating Site objects having a NucleotideAlphabet attached by default.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Name of the Sequence object.}
# 	\item{string}{A string specifying the length and the states of the Sequence object.}
#	\item{length}{The length of the sequence. Mutually exclusive with "string".}
# 	\item{processes}{A list of list of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
#	\item{ancestral.obj}{The ancestral object of the Sequence object (a valid Sequence or Process object).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create an empty NucleotideSequence object
#	s<-NucleotideSequence(length=50)
#	s
#	# set states
#	s$states<-c("A","A","G","T")
#	s
#	# create a sequence object by specifying a string
#	s<-NucleotideSequence(string="ATGCCGATTAGCAAA")
#	s
# }
# 
# @author
#
# \seealso{ 
# 	Sequence NucleotideAlphabet
# }
# 
#*/###########################################################################
setConstructorS3(
  "NucleotideSequence",
  function(
    name=NA,
    string=NA,
    length=NA,
    processes=NA,
    ancestral.obj=NA,
    ...
	){

		if(!missing(string)){
			this<-Sequence(string=string,alphabets=list(NucleotideAlphabet()));
		}
		else if(!missing(length)) {
			this<-Sequence(length=length,alphabets=list(NucleotideAlphabet()));
		}
		else {
			this<-Sequence(alphabets=list(NucleotideAlphabet()));
		}
		this<-extend(this, "NucleotideSequence");
		
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
## AminoAcidSequence
##
##########################################################################/** 
#
# @RdocClass AminoAcidSequence
# 
# @title "The AminoAcidSequence class"
# 
# \description{ 
#	Sequence objects aggregating Site objects having an AminoAcidAlphabet attached by default.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{Name of the Sequence object.}
# 	\item{string}{A string specifying the length and the states of the Sequence object.}
#	\item{length}{The length of the sequence. Mutually exclusive with "string".}
# 	\item{processes}{A list of list of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
#	\item{ancestral.obj}{The ancestral object of the Sequence object (a valid Sequence or Process object).}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create an empty AminoAcidSequence object
#	s<-AminoAcidSequence(length=50)
#	s
#	# set states
#	s$states<-c("C","C","G","Y")
#	s
#	# create a sequence object by specifying a string
#	s<-AminoAcidSequence(string="CNGGYCCNGYYYY")
#	s
# }
# 
# @author
#
# \seealso{ 
# 	Sequence AminoAcidAlphabet
# }
# 
#*/###########################################################################
setConstructorS3(
  "AminoAcidSequence",
  function(
    name=NA,
    string=NA,
    length=NA,
    processes=NA,
    ancestral.obj=NA,
    ...
	){

		if(!missing(string)){
			this<-Sequence(string=string,alphabets=list(AminoAcidAlphabet()));
		}
		else if(!missing(length)) {
			this<-Sequence(length=length,alphabets=list(AminoAcidAlphabet()));
		}
		else {
			this<-Sequence(alphabets=list(AminoAcidAlphabet()));
		}
		this<-extend(this, "AminoAcidSequence");
		
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
## CodonSequence
##
##########################################################################/** 
#
# @RdocClass CodonSequence
# 
# @title "The CodonSequence class"
# 
# \description{ 
#	Sequence objects aggregating Site objects having a CodonAlphabet attached by default.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
#       \item{name}{Name of the Sequence object.}
#       \item{string}{A string specifying the length and the states of the Sequence object.}
#       \item{length}{The length of the sequence. Mutually exclusive with "string".}
#	\item{table.id}{The genetic code table to use in the attached CodonAlphabet object ("Standard" by default).}
#       \item{processes}{A list of list of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
#       \item{ancestral.obj}{The ancestral object of the Sequence object (a valid Sequence or Process object).}
#       \item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#       # create an empty CodonSequence object
#       s<-CodonSequence(length=50)
#       s
#       # set states
#       s$states<-c("ATG","CGA","TTT","CTA")
#       s
#       # create a sequence object by specifying a string
#       s<-CodonSequence(string="ATCTTTCGAATG")
#       s
# }
# 
# @author
#
# \seealso{ 
# 	@seeclass 
# }
# 
#*/###########################################################################
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
###########################################################################/**
#
# @RdocMethod Translate
# 
# @title "Translate a CodonSequence object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The translation as an AminoAcidSequence object.
# } 
# 
# \examples{
#	# create a CodonSequence object
#	s<-CodonSequence(string="ATCTTTCGAATGGGGCCCTCCCGA")
#	# get the translation as an AminoAcidSequence object
#	as<-Translate(s)
#	as
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

