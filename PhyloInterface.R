##	$Id: GeneralSubstitution.R,v 1.24 2009-05-01 16:06:13 sbotond Exp $
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
## Method: getEdges
##
setMethodS3(
  "getEdges",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
				if(attr(this$.phylo, "order") != "cladewise"){
					throw("The order of the phylo object is not cladewise! Someone must have been messing with that!\n");
				}
				tmp<-cbind(this$.phylo$edge,this$.phylo$edge.length);
				colnames(tmp)<-c("from","to","length");
				rownames(tmp)<-1:dim(tmp)[[1]];
				return(tmp);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setEdges
##
setMethodS3(
  "setEdges",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNtips
##
setMethodS3(
  "getNtips",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
								return(length(this$.phylo$tip.label));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNtips
##
setMethodS3(
  "setNtips",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTipLabels
##
setMethodS3(
  "getTipLabels",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					tmp<-rbind(this$.phylo$tip.label);
					rownames(tmp)<-c("Labels:");
					colnames(tmp)<-c(1:length(tmp));
					return(tmp);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTipLabels
##
setMethodS3(
  "setTipLabels",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNodes
##
setMethodS3(
  "getNodes",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# This is dumb but safe:
					#return(sort(unique(as.vector(this$.phylo$edge))));
					return(1:( 2*getNtips(this) - 1));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getNedges
##
setMethodS3(
  "getNedges",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					return(dim(this$.phylo$edge)[[1]]);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNedges
##
setMethodS3(
  "setNedges",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setNodes
##
setMethodS3(
  "setNodes",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTips
##
setMethodS3(
  "getTips",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# This is dumb but safe:
					#return(sort(unique(as.vector(this$.phylo$edge))));
					return(1:(getNtips(this)));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTips
##
setMethodS3(
  "setTips",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getRootNode
##
setMethodS3(
  "getRootNode",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
					# Relying on cladewise order:
					return(this$.phylo$edge[1,1]);
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setRootNode
##
setMethodS3(
  "setRootNode",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: is.tip
##
setMethodS3(
  "is.tip",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node number specified!\n");
		}
		else if(!is.numeric(node)){
			throw("The node number must be numeric!\n");
		}
		else {
			return(round(node) <= this$ntips);
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getEdge
##
setMethodS3(
  "getEdge",
  class="PhyloSim",
  function(
    this,
    number=NA,
    ...
  ){

    if(missing(number)){
      throw("No object provided!\n");
    }
    else if(!is.numeric(number)){
      throw("The edge number must be numeric!\n");
    }
    else {
				number<-round(number);
				tmp<-rbind(c(this$.phylo$edge[number,],this$.phylo$edge.length[number]));
				colnames(tmp)<-c("from","to","length");
				rownames(tmp)<-c("Edge:");
				return(tmp);

    }

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getTreeLength
##
setMethodS3(
  "getTreeLength",
  class="PhyloSim",
  function(
    this,
    ...
  ){
		
		if(!all(is.na(this$.phylo))){
			if(is.phylo(this$.phylo)){
				return(sum(this$.phylo$edge.length));
			}
			else{
				throw("The phylo object is invalid!\n");
			}
		}
		else{
			throw("The phylo object is not set!\n");
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setTreeLength
##
setMethodS3(
  "setTreeLength",
  class="PhyloSim",
  function(
    this,
		value,
    ...
  ){
		
		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);






