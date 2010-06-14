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


