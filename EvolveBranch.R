##
##	Class: PhyloSim
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
## Method: evolveBranch
##
setMethodS3(
  "evolveBranch",
  class="PhyloSim",
  function(
    this,
    start.seq=NA,
    branch.length=NA,
		old.node=NA,
		new.node=NA,
    ...
  ){

		if(missing(start.seq)){
			throw("No starting sequence provided!\n");
		}
		else if(missing(branch.length)){
			throw("No branch length provided!\n");
		}
		else if(!is.numeric(branch.length)){
			throw("The branch length must be numeric!\n");
		}
		else if(.checkSeq(this, start.seq) ){
		
			# Cloning the starting sequence:
			seq<-clone(start.seq);
			
			# Call the node hook if exists:
			hook<-this$.node.hooks[[as.character(old.node)]];
			if(!is.null(hook) & is.function(hook)){
				seq<-hook(seq=seq);	
				if(!is.Sequence(seq)){
					throw("Node hook returned an invalid sequence object!\n");
				}
				else if(is.na(seq$bigRate)){
					throw("Node hook returned sequence with NA bigRate!\n");
				}
				else if(seq$bigRate == 0.0){
					throw("Node hook returne sequence with zero bigRate!\n");
				}
				else{
				 checkConsistency(seq, ommit.sites=TRUE);
				}
			}

			# Set the name of the sequence object:
			if(is.tip(this, new.node)){
				seq$name<-this$tipLabels[[new.node]];
			}
			else {
				seq$name<-paste("Node",new.node);
			}
			.GillespieDirect(this, seq=seq, branch.length=branch.length);

			# Return the resulting sequence object:
			return(seq);
		
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .GillespieDirect
##
setMethodS3(
  ".GillespieDirect",
  class="PhyloSim",
  function(
    this,
    seq=NA,
    branch.length=NA,
    ...
  ){

		# Initialize time:
		time<-0.0;
		
		# Sample the next waiting time until
		# the branch length is consumed:	
		while( (time<-time + rexp(1, rate=seq$bigRate)) <= branch.length){

			# Generate a random number between zero and the bigRate:
			E<-runif(1,min=0,max=seq$bigRate);
			# Identify the target site:
			site.number<-which(seq$cumulativeRates >= E)[[1]];
			# Get the events from the target site:
			events<-getEvents(seq,index=site.number);
			# Get the rates:
			rates<-as.numeric(lapply(
				events,
				getRate
			));
		
			# Calculate the corresponding cumulative rates:	
			if(site.number > 1){
				rates<-cumsum(c(seq$cumulativeRates[[site.number - 1]], rates));
			}
			else {
				rates<-cumsum(c(0.0, rates));
			}
			# Pick the event:
			event.number<-which(rates >= E)[[1]] - 1;
			# Perform the event:
			Perform(events[[event.number]]);

			# Abort if sequence length shrunk to zero:
			if(seq$.length == 0){
				throw("Terminating the simulation because the length of the sequence ",seq$name," shrunk to zero! Please be more careful when tuning the indel rates!\n");
			}

		}

		# FIXME - Calling the garbage collector:
		gc();	

		return(seq);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);



