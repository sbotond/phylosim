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

			# FIXME - superfluous counter
			j<-1;
			nuc<-strsplit(string,"")[[1]];
			for(i in seq(from=1,to=(flen * 3),by=3)){
					state<-paste(nuc[c(i,i+1,i+2)],collapse="");
					print(state);
					setStates(this,state,j);		
					j<-j+1;
			}
			# FIXME - handle stop codon
			
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

