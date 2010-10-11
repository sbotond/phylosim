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
# 	\item{processes}{A list of lists of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
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
# 	\item{processes}{A list of lists of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
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
## revComp
##
###########################################################################/**
#
# @RdocMethod revComp
# 
# @title "Reverse complmenet a NucleotideSequence object" 
# 
# \description{ 
#	@get "title".
#	
#	The method reverse complements the sequence "in place", no object cloning is performed.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A NucleotideSequence object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	s<-NucleotideSequence(string="ATGC")
#	s
#	revComp(s)
#	s
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
        "revComp",
        class="NucleotideSequence",
        function(
                this,
                ...
        ){

		states<-rev(as.character(getStates(this)));	
		len<-length(states);
		if(len == 0){return(invisible(TRUE))}
		for(i in 1:len){
			if(states[[i]] == "NA"){
				state<-NA;
			}
			else if(states[i] == "A"){
				state<-"T";
			}
			else if(states[i] == "T"){
				state<-"A";
			}
			else if(states[i] == "G"){
				state<-"C";
			}
			else if(states[i] == "C"){
				state<-"G";
			} else {
				throw("Symbol not in NucleotideAlphabet!");
			}
			this$.sites[[i]]$.state<-state;
		}
                return(invisible(TRUE));
        },
        private=TRUE,
        protected=FALSE,
        overwrite=FALSE,
        conflict="warning",
        validators=getOption("R.methodsS3:validators:setMethodS3")
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
# 	\item{processes}{A list of lists of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
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
#       \item{processes}{A list of lists of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
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

			nuc<-strsplit(string,"",fixed=TRUE)[[1]];


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
## Method: getOmegas
##  
###########################################################################/**
#
# @RdocMethod getOmegas
# \alias{getOmegas.Sequence} 
# 
# @title "Get the omegas from a collection of sites" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
#	\item{process}{A process object inheriting from GY94.}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a GY94 process
#	p<-GY94()
#	# create a CodonSequence object,
#	# attach a process p
#	s<-CodonSequence(length=20,processes=list(list(p)))
#	# set omega values in range 1:5
#	setOmegas(s,p,c(0.5,1,1.5),1:5)
#	# get omega values from siutes 1,2,3,10, and 20
#	getOmegas(s,p,c(1:3,10,20))
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
  "getOmegas",
  class="CodonSequence",
  function(
    this,
		process,
		index,
    ...
  ){

		if(missing(process)){
      throw("No process given!\n");
    }
    else if(!is.GY94(process)){
      throw("The specified process is not a GY94 codon substitution process!\n");
    }
    rm<-getParameterAtSites(this=this,process=process,id="omega",index=index);
    return(as.numeric(lapply(rm,function(param){param$value})));
	

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaHist
##  
###########################################################################/**
#
# @RdocMethod omegaHist
# 
# @title "Plot a histogram of omega values from a range" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
#	\item{process}{A process object inheriting from GY94.}
#	\item{breaks}{\code{breaks} parameter for \code{hist()}.}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The CodonSequence object (invisible).
# } 
# 
# \examples{
#	# create a GY94 process
#	p<-GY94()
#	# create a CodonSequence object,
#	# attach a process p
#	s<-CodonSequence(length=20,processes=list(list(p)))
#       # set omega values through omegaVarM2.CodonSequence
#       omegaVarM2(s,p,p0=0.5,p1=0.2,omega=1.5)
#       # get a histogram of omega values from the range 1:15
#       omegaHist(s,p,breaks=10,1:15)
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
  "omegaHist",
  class="CodonSequence",
  function(
    this,
		process,
		breaks,
		index,
    ...
  ){

		if(missing(process)){
      throw("No process given!\n");
    }
    else if(!is.GY94(process)){
      throw("The specified process is not a GY94 codon substitution process!\n");
    }

		if(missing(index)){
			index<-seq(along.with=this$.sites);
		}

		if(missing(breaks)){
			hist(getOmegas(this,process,index));
		}
		else {
			omegas<-getOmegas(this,process,index);
			hist(omegas,breaks=breaks,main="Histogram of omega values",xlab="Omega",freq=FALSE);
		}
		return(invisible(this));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: setOmegas
###########################################################################/**
#
# @RdocMethod setOmegas
# \alias{setOmegas.Sequence} 
# 
# @title "Set the omegas for a collection of sites" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
#	\item{process}{A process object inheriting from GY94.}
#	\item{value}{A vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector.}
#	\item{index}{A vector of positions. It is set to 1:seq$length if omitted.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector.
# } 
# 
# \examples{
#	# create a GY94 process
#	p<-GY94()
#	# create a CodonSequence object,
#	# attach a process p
#	s<-CodonSequence(length=20,processes=list(list(p)))
#	# set omega values in range 1:5
#	setOmegas(s,p,c(0.5,1,1.5),1:5)
#	# get omega values from siutes 1,2,3,10, and 20
#	getOmegas(s,p,c(1:3,10,20))
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
  "setOmegas",
  class="CodonSequence",
  function(
	this,
	process,
	value,
	index,
    	...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(value)){
      throw("No new value specified!\n");
    }
    else if(!all(is.numeric(value)) ){
      throw("The new value must be a numeric vector!\n");
    }
    else {

      if(missing(index)){
        index<-seq(along.with=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

      setParameterAtSites(this, process=process, id="omega",value=value,index=index);
			return(invisible(TRUE));

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM0 - one ratio
##  
###########################################################################/**
#
# @RdocMethod omegaVarM0
# 
# @title "The M0 (one-ratio) model of variable omega ratios among sites" 
# 
# \description{ 
#	@get "title".
#
#	This method sets the \code{omega} site-process specific parameter
#	in the specified range to values sampled from the M0 (one-ratio) model of
#	variable omega ratios among sites.
#
#       Distribution of omega values:
#       \preformatted{
#	CATEGORY	PROBABILITY
#
#	omega		1
#       }
# } 
#
# \references{
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
#
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
# 	\item{process}{A process object inheriting from GY94.} 
#	\item{omega}{The fixed omega value.}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# create a CodonSequence object, attach process p
#	s<-CodonSequence(length=20, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# sample omegas in range 1:5 from model M0
#	omegaVarM0(s,p,omega=2,1:5)
#	# get omega values	
#	getOmegas(s,p)
#	# get a histogram of omega values in range 1:5
#	omegaHist(s,p,breaks=50,1:5)
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
  "omegaVarM0",
  class="CodonSequence",
  function(
    	this,
	process,
    	omega,
	index,
    	...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(omega)){
      throw("No new omega value specified!\n");
    }
    else if((!is.numeric(omega))| (length(omega) != 1)){
      throw("The new value must be a numeric vector of length 1!\n");
    }
    else {

      if(missing(index)){
        index<-seq(along.with=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

      setParameterAtSites(this, process=process, id="omega",value=omega,index=index);
      return(invisible(TRUE));

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM1 - neutral
##  
###########################################################################/**
#
# @RdocMethod omegaVarM1
# 
# @title "The M1 (neutral) model of variable omega ratios among sites" 
# 
# \description{ 
#	@get "title".
#
#	This method sets the \code{omega} site-process specific parameter
#	in the specified range to values sampled from the M1 (neutral) model of
#	variable omega ratios among sites.
#
#       Distribution of omega values:
#       \preformatted{
#	CATEGORY	PROBABILITY
#
#	omega_0 = 0	p0
#	omega_1 = 1	1-p0
#       }
# } 
#
# \references{
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
#
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
# 	\item{process}{A process object inheriting from GY94.} 
#	\item{p0}{See above.}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# create a CodonSequence object, attach process p
#	s<-CodonSequence(length=25, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# sample omegas in range 1:20 from model M1
#	omegaVarM1(s,p,p0=0.5,1:20)
#	# get omega values	
#	getOmegas(s,p)
#	# get a histogram of omega values in range 1:20
#	omegaHist(s,p,breaks=50,1:20)
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
  "omegaVarM1",
  class="CodonSequence",
  function(
    	this,
    	process,
   	p0,
	index,
    	...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0))| (length(p0) != 1)){
      throw("The new value must be a numeric vector of length 1!\n");
    }
		else if(p0 < 0 | p0 > 1){
			throw("The p0 parameter must be in the [0,1] interval!\n");
		}
    else {

      if(missing(index)){
        index<-seq(along.with=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

			for(site in this$.sites[index]){
				setParameterAtSite(this=process,site=site, id="omega", value=sample(c(0,1), size=1, replace=FALSE, prob=c(p0,(1-p0))));	
			}
			return(invisible(TRUE));

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM2 - selection
##  
###########################################################################/**
#
# @RdocMethod omegaVarM2
# 
# @title "The M2 (selection) model of variable omega ratios among sites" 
# 
# \description{ 
#	@get "title".
#
#	This method sets the \code{omega} site-process specific parameter
#	in the specified range to values sampled from the M2 (selection) model of
#	variable omega ratios among sites.
#
#       Distribution of omega values:
#       \preformatted{
#	CATEGORY	PROBABILITY
#
#	omega_0 = 0	p0
#	omega_1 = 1	p1
#	omega_2 	1-p0-p1
#       }
# } 
#
# \references{
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
#
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
# 	\item{process}{A process object inheriting from GY94.} 
#	\item{p0}{See above.}
#	\item{p1}{See above.}
#	\item{omega_2}{See above.}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# create a CodonSequence object, attach process p
#	s<-CodonSequence(length=25, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# sample omegas in range 1:20 from model M2
#	omegaVarM2(s,p,p0=0.2,p1=0.3,omega_2=4,1:20)
#	# get omega values	
#	getOmegas(s,p)
#	# get a histogram of omega values in range 1:20
#	omegaHist(s,p,breaks=50,1:20)
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
  "omegaVarM2",
  class="CodonSequence",
  function(
    		this,
		process,
    		p0,
		p1,
		omega_2,
		index,
    		...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0))| (length(p0) != 1)){
      throw("The p0 value must be a numeric vector of length 1!\n");
    }
		else if(p0 < 0 | p0 > 1){
			throw("The p0 parameter must be in the [0,1] interval!\n");
		}
    else if(missing(p1)){
      throw("No p1 value specified!\n");
    }
    else if((!is.numeric(p1))| (length(p1) != 1)){
      throw("The p1 value must be a numeric vector of length 1!\n");
    }
		else if(p1 < 0 | p1 > 1){
			throw("The p1 parameter must be in the [0,1] interval!\n");
		}
    else if(missing(omega_2)){
      throw("No omega value specified!\n");
    }
    else if((!is.numeric(omega_2))| (length(omega_2) != 1)){
      throw("The omega value must be a numeric vector of length 1!\n");
    }
    else {

      if(missing(index)){
        index<-seq(along.with=this$.sites);
      }
      else {
        index<-.checkIndexSanity(this, index);
      }

			for(site in this$.sites[index]){
				setParameterAtSite(this=process,site=site, id="omega", value=sample(c(0,1,omega_2), size=1, replace=FALSE, prob=c(p0,p1,(1-p0-p1))));	
			}
			return(invisible(TRUE));

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM3 - discrete
##  
###########################################################################/**
#
# @RdocMethod omegaVarM3
# 
# @title "The M3 (discrete) model of variable omega ratios among sites" 
# 
# \description{ 
#	@get "title".
#
#	This method sets the \code{omega} site-process specific parameter
#	in the specified range to values sampled from the M3 (discrete) model of
#	variable omega ratios among sites.
#
#       Distribution of omega values:
#       \preformatted{
#	CATEGORY	PROBABILITY
#
#	omega_0		p0
#	omega_1		p1
#	omega_2 	p2
#	...		...
#	omega_k		pk
#       }
# } 
#
# \references{
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
#
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
# 	\item{process}{A process object inheriting from GY94.} 
#	\item{omegas}{A vector of omega values (omega_0 ... omega_k).}
#	\item{probs}{A vector of probabilities (p0 ... pk).}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# create a CodonSequence object, attach process p
#	s<-CodonSequence(length=25, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# sample omegas in range 1:20 from model M3
#	omegaVarM3(s,p,omegas=c(0,2,4),probs=c(1/3,1/3,1/3),1:20)
#	# get omega values	
#	getOmegas(s,p)
#	# get a histogram of omega values in range 1:20
#	omegaHist(s,p,breaks=50,1:20)
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
  "omegaVarM3",
  class="CodonSequence",
  function(
    		this,
		process,
		omegas,
		probs,
		index,
    		...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(omegas)){
      throw("No omega values specified!\n");
    }
    else if((!is.numeric(omegas))){
      throw("The omegas must be numeric!\n");
    }
		else if(missing(probs)){
			throw("No probabilities specified!\n");
		}
		else if(!is.numeric(probs)){
			throw("The probabilities must be numeric!\n");
		}
		else if(length(omegas) != length(probs)){
			throw("The length of the \"omegas\" and \"probs\" vector must be the same!\n");
		}
		else if(!PSRoot$my.all.equal(sum(probs),1.0)){
			probs<-(probs/sum(probs));
			warning("The provided probabilities were scaked in order to sum to one!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(omegas, size=1, replace=FALSE, prob=probs));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: omegaVarM4 - freqs
##  
###########################################################################/**
#
# @RdocMethod omegaVarM4
# 
# @title "The M4 (freqs) model of variable omega ratios among sites" 
# 
# \description{ 
#	@get "title".
#
#	This method sets the \code{omega} site-process specific parameter
#	in the specified range to values sampled from the M4 (freqs) model of
#	variable omega ratios among sites.
#
#       Distribution of omega values:
#       \preformatted{
#	CATEGORY	PROBABILITY
#
#	omega_0 = 0	p0
#	omega_1 = 1/3	p1
#	omega_2 = 2/3	p2
#	omega_3	= 1	p3
#	omega_4 = 3	p4
#       }
# } 
#
# \references{
# Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
#
# Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
# }
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSequence object.} 
# 	\item{process}{A process object inheriting from GY94.} 
#	\item{probs}{A vector of probabilities (p0 ... p4).}
#	\item{index}{A vector of positions.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE.
# } 
# 
# \examples{
#	# create a GY94 object
#	p<-GY94(kappa=2)
#	# create a CodonSequence object, attach process p
#	s<-CodonSequence(length=25, processes=list(list(p)))
#	# sample states
#	sampleStates(s)
#	# sample omegas in range 1:20 from model M4
#	omegaVarM4(s,p,probs=c(2/5,1/5,1/5,1/10,1/10),1:20)
#	# get omega values	
#	getOmegas(s,p)
#	# get a histogram of omega values in range 1:20
#	omegaHist(s,p,breaks=50,1:20)
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
  "omegaVarM4",
  class="CodonSequence",
  function(
    		this,
		process,
		probs,
		index,
    		...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
		else if(missing(probs)){
			throw("No probabilities specified!\n");
		}
		else if(!is.numeric(probs)){
			throw("The probs must be greater than zero!\n");
		}
		else if( length(probs) != 5){
			throw("The length of the \"probs\" vector must be 5!\n");
		}
		else if(!PSRoot$my.all.equal(sum(probs),1.0)){
			probs<-(probs/sum(probs));
			warning("The provided probabilities were scaked in order to sum to one!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		omegas<-c(0,(1/3),(2/3),1,3);
		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(omegas, size=1, replace=FALSE, prob=probs));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM5Cont - gamma
##  
setMethodS3(
  ".omegaVarM5Cont",
  class="CodonSequence",
  function(
    this,
		process,
		alpha,
		beta,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(alpha)){
      throw("No alpha (shape) value specified!\n");
    }
    else if((!is.numeric(alpha)) | (length(alpha) != 1)){
      throw("The alpha (shape) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha < 0){
			throw("The alpha (shape) must be greater than zero!\n");
		}
    else if(missing(beta)){
      throw("No beta (scale) value specified!\n");
    }
    else if((!is.numeric(beta)) | (length(beta) != 1)){
      throw("The beta (scale) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta <= 0){
			throw("The beta (scale) must be strictly positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=rgamma(1,shape=alpha,scale=beta));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM6Cont - 2gamma
##  
setMethodS3(
  ".omegaVarM6Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		alpha0,
		beta0,
		alpha1,
		beta1,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(alpha0)){
      throw("No alpha0 (shape0) value specified!\n");
    }
    else if((!is.numeric(alpha0)) | (length(alpha0) != 1)){
      throw("The alpha0 (shape0) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha0 < 0){
			throw("The alpha0 (shape0) must be greater than zero!\n");
		}
    else if(missing(beta0)){
      throw("No beta0 (scale0) value specified!\n");
    }
    else if((!is.numeric(beta0)) | (length(beta0) != 1)){
      throw("The beta0 (scale0) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta0 <= 0){
			throw("The beta0 (scale0) must be strictly positive!\n");
		}
    else if(missing(alpha1)){
      throw("No alpha1 (shape1) value specified!\n");
    }
    else if((!is.numeric(alpha1)) | (length(alpha1) != 1)){
      throw("The alpha1 (shape1) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha1 < 0){
			throw("The alpha1 (shape1) must be greater than zero!\n");
		}
    else if(missing(beta1)){
      throw("No beta1 (scale1) value specified!\n");
    }
    else if((!is.numeric(beta1)) | (length(beta1) != 1)){
      throw("The beta1 (scale1) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta1 <= 0){
			throw("The beta1 (scale1) must be strictly positive!\n");
		}


    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rgamma(1,shape=alpha0,scale=beta0),rgamma(1,shape=alpha1,scale=beta1)),size=1,replace=FALSE,prob=c(p0,(1-p0)) ));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM7Cont - beta
##  
setMethodS3(
  ".omegaVarM7",
  class="CodonSequence",
  function(
    this,
		process,
		p,
		q,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p parameter must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=rbeta(1,shape1=p,shape2=q));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM8Cont - beta&omega
##  
setMethodS3(
  ".omegaVarM8Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p,
		q,
		omega,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("The p0 parameter must be from the [0,1] interval!\n");
		}
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p parameter must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q parameter must be positive!\n");
		}
    else if(missing(omega)){
      throw("No omega value specified!\n");
    }
    else if((!is.numeric(omega)) | (length(omega) != 1)){
      throw("The omega parameter must be a numeric vector of length 1!\n");
    }
		else if(omega < 0){
			throw("The omega parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rbeta(1,shape1=p,shape2=q),omega),size=1,replace=FALSE,prob=(c(p0,(1-p0)))));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM9 - beta&gamma
##  
setMethodS3(
  ".omegaVarM9Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p,
		q,
		alpha,
		beta,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q must be greater than zero!\n");
		}
    else if(missing(alpha)){
      throw("No alpha (shape) value specified!\n");
    }
    else if((!is.numeric(alpha)) | (length(alpha) != 1)){
      throw("The alpha (shape) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha < 0){
			throw("The alpha (shape) must be greater than zero!\n");
		}
    else if(missing(beta)){
      throw("No beta (scale) value specified!\n");
    }
    else if((!is.numeric(beta)) | (length(beta) != 1)){
      throw("The beta (scale) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta <= 0){
			throw("The beta (scale) must be strictly positive!\n");
		}


    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rbeta(1,shape1=p,shape2=q),rgamma(1,shape=alpha,scale=beta)),size=1,replace=FALSE,prob=c(p0,(1-p0)) ));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM10Cont - beta&gamma+1
##  
setMethodS3(
  ".omegaVarM10Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p,
		q,
		alpha,
		beta,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q must be greater than zero!\n");
		}
    else if(missing(alpha)){
      throw("No alpha (shape) value specified!\n");
    }
    else if((!is.numeric(alpha)) | (length(alpha) != 1)){
      throw("The alpha (shape) parameter must be a numeric vector of length 1!\n");
    }
		else if(alpha < 0){
			throw("The alpha (shape) must be greater than zero!\n");
		}
    else if(missing(beta)){
      throw("No beta (scale) value specified!\n");
    }
    else if((!is.numeric(beta)) | (length(beta) != 1)){
      throw("The beta (scale) parameter must be a numeric vector of length 1!\n");
    }
		else if(beta <= 0){
			throw("The beta (scale) must be strictly positive!\n");
		}


    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }
	
		# It's not too elegant to fork the whole method for just shifting the gamma with 1...
		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rbeta(1,shape1=p,shape2=q),(1 + rgamma(1,shape=alpha,scale=beta)) ),size=1,replace=FALSE,prob=c(p0,(1-p0)) ));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM11Cont - beta&normal>1
##  
setMethodS3(
  ".omegaVarM11Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p,
		q,
		mean,
		sd,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(p)){
      throw("No p value specified!\n");
    }
    else if((!is.numeric(p)) | (length(p) != 1)){
      throw("The p parameter must be a numeric vector of length 1!\n");
    }
		else if(p < 0){
			throw("The p must be greater than zero!\n");
		}
    else if(missing(q)){
      throw("No q value specified!\n");
    }
    else if((!is.numeric(q)) | (length(q) != 1)){
      throw("The q parameter must be a numeric vector of length 1!\n");
    }
		else if(q < 0){
			throw("The q must be greater than zero!\n");
		}
    else if(missing(mean)){
      throw("No mean specified!\n");
    }
    else if((!is.numeric(mean)) | (length(mean) != 1)){
      throw("The mean parameter must be a numeric vector of length 1!\n");
    }
    else if(missing(sd)){
      throw("No sd value specified!\n");
    }
    else if((!is.numeric(sd)) | (length(sd) != 1)){
      throw("The sd parameter must be a numeric vector of length 1!\n");
    }
		else if( sd < 0){
      throw("The sd parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		rnorm.gt.1<-function(mean=NA,sd=NA){
			# This is probably the most primitive way to truncate the distribution!
			tmp<-rnorm(1,mean=mean,sd=sd);
			while( tmp <= 1){
				tmp<-rnorm(1,mean=mean,sd=sd);
			};
			return(tmp);
		}

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample(c(rbeta(1,shape1=p,shape2=q), (rnorm.gt.1(mean=mean,sd=sd))),size=1,replace=FALSE,prob=c(p0,(1-p0)) ));	
		}
		return(invisible(TRUE));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM12Cont - 0&2normal>0
##  
setMethodS3(
  ".omegaVarM12Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p1,
		sd1,
		mean2,
		sd2,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(p1)){
      throw("No p1 value specified!\n");
    }
    else if((!is.numeric(p1)) | (length(p1) != 1)){
      throw("The p1 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p1 < 0) | (p1 > 1)){
			throw("p1 must be in the [0,1] interval!\n");
		}
    else if(missing(sd1)){
      throw("No sd1 value specified!\n");
    }
    else if((!is.numeric(sd1)) | (length(sd1) != 1)){
      throw("The sd1 parameter must be a numeric vector of length 1!\n");
    }
		else if( sd1 < 0){
      throw("The sd1 parameter must be positive!\n");
		}
    else if(missing(mean2)){
      throw("No mean2 specified!\n");
    }
    else if((!is.numeric(mean2)) | (length(mean2) != 1)){
      throw("The mean2 parameter must be a numeric vector of length 1!\n");
    }
    else if(missing(sd2)){
      throw("No sd2 value specified!\n");
    }
    else if((!is.numeric(sd2)) | (length(sd2) != 1)){
      throw("The sd2 parameter must be a numeric vector of length 1!\n");
    }
		else if( sd2 < 0){
      throw("The sd2 parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		rnorm.gt.0<-function(mean=NA,sd=NA){
			# This is probably the most primitive way to truncate the distribution!
			tmp<-rnorm(1,mean=mean,sd=sd);
			while( tmp <= 0){
				tmp<-rnorm(1,mean=mean,sd=sd);
			};
			return(tmp);
		}

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample( 
				c(0,rnorm.gt.0(mean=1,sd=sd1),rnorm.gt.0(mean=mean2,sd=sd2)),
				size=1,
				replace=FALSE,
				prob=c(p0,((1-p0)*p1),((1-p0)*(1-p1)))
			));	
		}
		return(invisible(TRUE));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##  
## Method: .omegaVarM13Cont - 3normal>0
##  
setMethodS3(
  ".omegaVarM13Cont",
  class="CodonSequence",
  function(
    this,
		process,
		p0,
		p1,
		sd0,
		sd1,
		mean2,
		sd2,
		index,
    ...
  ){

  if(missing(process)){
      throw("No process specified!\n");
    }
    if(!is.GY94(process)){
      throw("The sepcified process is not a GY94 codon substitution process!\n");
    }
    else if(missing(p0)){
      throw("No p0 value specified!\n");
    }
    else if((!is.numeric(p0)) | (length(p0) != 1)){
      throw("The p0 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p0 < 0) | (p0 > 1)){
			throw("p0 must be in the [0,1] interval!\n");
		}
    else if(missing(p1)){
      throw("No p1 value specified!\n");
    }
    else if((!is.numeric(p1)) | (length(p1) != 1)){
      throw("The p1 parameter must be a numeric vector of length 1!\n");
    }
		else if( (p1 < 0) | (p1 > 1)){
			throw("p1 must be in the [0,1] interval!\n");
		}
    else if(missing(sd0)){
      throw("No sd0 value specified!\n");
    }
    else if((!is.numeric(sd0)) | (length(sd0) != 1)){
      throw("The sd0 parameter must be a numeric vector of length 1!\n");
    }
		else if( sd0 < 0){
      throw("The sd0 parameter must be positive!\n");
		}
    else if(missing(sd1)){
      throw("No sd1 value specified!\n");
    }
    else if((!is.numeric(sd1)) | (length(sd1) != 1)){
      throw("The sd1 parameter must be a numeric vector of length 1!\n");
    }
		else if( sd1 < 0){
      throw("The sd1 parameter must be positive!\n");
		}
    else if(missing(mean2)){
      throw("No mean2 specified!\n");
    }
    else if((!is.numeric(mean2)) | (length(mean2) != 1)){
      throw("The mean2 parameter must be a numeric vector of length 1!\n");
    }
    else if(missing(sd2)){
      throw("No sd2 value specified!\n");
    }
    else if((!is.numeric(sd2)) | (length(sd2) != 1)){
      throw("The sd2 parameter must be a numeric vector of length 1!\n");
    }
		else if( sd2 < 0){
      throw("The sd2 parameter must be positive!\n");
		}

    if(missing(index)){
    index<-seq(along.with=this$.sites);
    }
    else {
      index<-.checkIndexSanity(this, index);
    }

		rnorm.gt.0<-function(mean=NA,sd=NA){
			# This is probably the most primitive way to truncate the distribution!
			tmp<-rnorm(1,mean=mean,sd=sd);
			while( tmp <= 0){
				tmp<-rnorm(1,mean=mean,sd=sd);
			};
			return(tmp);
		}

		for(site in this$.sites[index]){
			setParameterAtSite(this=process,site=site, id="omega", value=sample( 
				c(rnorm.gt.0(mean=0,sd=sd0),rnorm.gt.0(mean=1,sd=sd1),rnorm.gt.0(mean=mean2,sd=sd2)),
				size=1,
				replace=FALSE,
				prob=c(p0,((1-p0)*p1),((1-p0)*(1-p1)))
			));	
		}
		return(invisible(TRUE));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
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
	
				for(pos in seq(along.with=this$.sites)){
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
			for (pos in seq(along.with=this$.sites)){
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

