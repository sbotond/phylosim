##	$Id: PopularSequences.R,v 1.1 2009-04-27 08:47:17 sbotond Exp $
##
##	Class: *
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

##
## BinarySequence
##
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


