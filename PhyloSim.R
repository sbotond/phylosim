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
setConstructorS3(
"PhyloSim",
  function(
		phylo=NA,
		root.seq=NA,
		name=NA,
		... 
		)	{

		this<-PSRoot();	
		this<-extend(this,
			"PhyloSim",
			.name="Anonymous",
			.phylo=NA,
			.root.sequence=NA, 
			.sequences=list(),	# references to the sequence objects
			.node.hooks=list(),	# references to the node hook functions.
			.alignment=NA				# the resulting alignment in fasat format.
		);

		if(!all(is.na(phylo))){
			this$phylo<-phylo;
		}

		if(!all(is.na(root.seq))){
			this$rootSeq<-root.seq;
		}
	
		if(!missing(name)){
			this$name<-name;
		}

		return(this);

  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
setMethodS3(
	"checkConsistency", 
	class="PhyloSim", 
	function(
		this,
		...
	){

      may.fail<-function(this) {
					
				# Checking the name:	
				this$name<-this$name;
				# Checking the phylo object:
				if (!any(is.na(this$.phylo)) & !is.phylo(this$.phylo) ){
					throw("The phylo object is invalid!\n");
				}
				# Checking the sequences:
				for (seq in this$.sequences){
					if(is.Sequence(seq)){
						checkConsistency(seq);
					}
				}
				# Checking node hooks:
				for (hook in this$.node.hooks){
					if(!is.null(hook) & !is.function(hook)){
						throw("Invalid node hook found!\n");
					}
					# FIXME - check seq argument.
				}
				# Checking the alignment:
				if(!any(is.na(this$.alignment))){
					.checkAlignmentConsistency(this, this$.alignment);
				}
		
      }
      tryCatch(may.fail(this));
			return(TRUE);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: is.phylo.default
##	
setMethodS3(
	"is.phylo", 
	class="default", 
	function(
		this,
		...
	){

		inherits(this,"phylo");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: setPhylo
##	
setMethodS3(
	"setPhylo", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No object provided!\n");
		}
		else if(!is.phylo(value)){
			throw("The new value must be a \"phylo\" object!\n");
		}
		else if(!is.rooted(value)){
			throw("The new value must be a rooted \"phylo\" object!\n");
		}
		else {

			this$.phylo<-value;
			this$.phylo<-reorder(this$.phylo, order="cladewise");
			for (i in this$nodes){
				this$.sequences[[i]]<-NA;
			}

		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: getPhylo
##	
setMethodS3(
	"getPhylo", 
	class="PhyloSim", 
	function(
		this,
		...
	){

		this$.phylo;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: setRootSeq
##	
setMethodS3(
	"setRootSeq", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No object provided!\n");
		}
		else if(!is.Sequence(value)){
			throw("The new value must be a sequence object!\n");
		}
		else {

			this$.root.sequence<-clone(value);
			this$.root.sequence$name<-paste("Root node",this$rootNode);

		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: getRootSeq
##	
setMethodS3(
	"getRootSeq", 
	class="PhyloSim", 
	function(
		this,
		...
	){

			this$.root.sequence;


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##	
## Method: as.character.PhyloSim
##	
setMethodS3(
	"as.character", 
	class="PhyloSim", 
	function(
		this,
		value,
		...
	){

		return(getId(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
)

##
## Method: getName
##
setMethodS3(
  "getName",
  class="PhyloSim",
  function(
    this,
    ...
  ){

    this$.name;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setName
##
setMethodS3(
  "setName",
  class="PhyloSim",
  function(
    this,
    new.name,
    ...
  ){

    this$.name<-as.character(new.name);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getId
##
setMethodS3(
  "getId",
  class="PhyloSim",
  function(
    this,
    ...
  ){

  this.class<-class(this)[1];
  id<-paste(this.class,this$.name,hashCode(this),sep=":");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setId
##
setMethodS3(
  "setId",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

  throw("Id is generated automatically and it cannot be set!\n");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: Simulate
##
setMethodS3(
  "Simulate",
  class="PhyloSim",
  function(
    this,
    ...
  ){


		# Check for the phylo object:	- FIXME: discrimintae NA-s
		if(!is.phylo(this$.phylo)){
			throw("Cannot simulate because the phylo object is not set or it is invalid!\n");
		}
		# Check for the root sequence:
		else if(!is.Sequence(this$.root.sequence)){
			throw("Cannot simulate because the root sequence is not set or it is invalid!\n");
		}
		# Check bigRate validity:
		else if(is.na(this$.root.sequence$bigRate)){
			throw("Cannot simulate because the bigRate of the root sequence is NA!\n");
		}
		else{

			# Warn for zero bigRate:
			if(this$.root.sequence$bigRate == 0){
				warning("The bigRate of the root sequence is zero! You are running a pointless simulation!\n");
			}
		
			# Attach root sequence to root node:
			attachSeqToNode(this, node=getRootNode(this),seq=this$.root.sequence);
			# Write protecting the root sequence:
			this$.root.sequence$writeProtected<-TRUE;
	
			# Traverse the tree and simulate:
			edge.counter<-1;
			n.edges<-this$nedges;
			for(edge in 1:n.edges){
				cat("Simulating edge ",edge," of ", n.edges,"\n");
				simulateEdge(this,number=edge);
				edge.counter<-edge.counter+1;
			}
		}
		this$.alignment<-.recoverAlignment(this);

		return(invisible(this));
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: simulateEdge
##
setMethodS3(
  "simulateEdge",
  class="PhyloSim",
  function(
    this,
		number=NA,
    ...
  ){

		# Get edge:
		edge<-getEdge(this, number);
		# Get parent node:
		start.seq<-getSeqFromNode(this, edge[[1,"from"]]);
		# Evolve sequence:
		new.seq<-evolveBranch(this, start.seq=start.seq, branch.length=edge[1,"length"], old.node=edge[[1,"from"]],new.node=edge[[1,"to"]]);
		# Write protect the sequence:
		new.seq$writeProtected<-TRUE;
		# Attach sequence to children node:
		attachSeqToNode(this, node=edge[1,"to"], seq=new.seq);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: attachSeqToNode
##
setMethodS3(
  "attachSeqToNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
		seq=NA,
    ...
  ){

		if(!is.phylo(this$.phylo)){
			throw("The phylo object is not set, sequence to node is not possible!\n");
		}
		if(missing(node)){
			throw("No node specified!\n");
		}
		else if(missing(seq)){
			throw("No sequence object given");
		}
		else if(.checkNode(this,node) & .checkSeq(this, seq)){
			
			if(is.Sequence(this$.sequences[[node]])){
				throw("The node has already an attached sequence. Detach that before trying to attach a new one!\n");
			}
			else {
				this$.sequences[[as.numeric(node)]]<-seq;
				return(invisible(this));
			}

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
## Method: attachHookToNode
##
setMethodS3(
  "attachHookToNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
		fun=NA,
    ...
  ){

		if(!is.phylo(this$.phylo)){
			throw("The phylo object is not set, attaching node hook is not possible!\n");
		}
		if(missing(node)){
			throw("No node specified!\n");
		}
		else if(missing(fun)){
			throw("No function given!");
		}
		else if(!is.function(fun)){
			throw("The argument \"fun\" must be a function!\n");
		}
		else if( length(intersect(names(formals(fun)), "seq")) == 0 ){
			throw("The function argument must have a an argument named \"seq\"");
		}
		else if(!is.Sequence(fun(Sequence(length=1)))){
      throw("The insert hook function must return a Sequence object!\n");
		}
		else if( .checkNode(this,node) ){
			if(is.function(this$.node.hooks[[as.character(node)]])){
				throw("The node has already an attached node hook. Detach that before trying to attach a new one!\n");
			}
			else {
				this$.node.hooks[[as.character(node)]]<-fun;
				return(invisible(this));
			}

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
## Method: .checkNode
##
setMethodS3(
  ".checkNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		} else if( length(intersect(node, getNodes(this))) != 1){
			throw("The specified node is invalid!\n");	
		}
		else {
			return(TRUE);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkSeq
##
setMethodS3(
  ".checkSeq",
  class="PhyloSim",
  function(
    this,
		seq=NA,
    ...
  ){

		if(missing(seq)){
			throw("No sequence specified!\n");
		} else if(!is.Sequence(seq)){
			throw("The sequence object is invalid!\n");	
		}
		else {
			return(TRUE);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);



##
## Method: detachSeqFromNode
##
setMethodS3(
  "detachSeqFromNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				this$.sequences[[as.numeric(node)]]<-NA;
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
## Method: detachHookFromNode
##
setMethodS3(
  "detachHookFromNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				this$.node.hooks[[as.character(node)]]<-NA;
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
## Method: getSeqFromNode
##
setMethodS3(
  "getSeqFromNode",
  class="PhyloSim",
  function(
    this,
		node=NA,
    ...
  ){

		if(missing(node)){
			throw("No node specified!\n");
		}
		else if( .checkNode(this,node) ){
			
				return(this$.sequences[[as.numeric(node)]]);
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getSequences
##
setMethodS3(
  "getSequences",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		slist<-list();
		for (node in getNodes(this)){
			slist[[node]]<-getSeqFromNode(this, node=node);
		}
		return(slist);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setSequences
##
setMethodS3(
  "setSequences",
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
## Method: getAlignment
##
setMethodS3(
  "getAlignment",
  class="PhyloSim",
  function(
    this,
    ...
  ){

		this$.alignment;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setAlignment
##
setMethodS3(
  "setAlignment",
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
## Method: .recoverAlignment
##
setMethodS3(
  ".recoverAlignment",
  class="PhyloSim",
  function(
    this,
		paranoid=FALSE,
    ...
  ){

		# Refuse to build alignment if at least one of the sequences is NA:
		for (seq in this$.sequences){
			if(!is.Sequence(seq)){
				throw("Cannot build alignment because the simulation is incomplete!\n");
			}
		}

		# The list holding all the partial alignment matrices:
		aln.mat<-list();
		row.names<-NA;

		# Initialize the variables:

		init.vars<-function(){

			# Getting the edge:
			edge<<-getEdge(this, edge.number);
			
			# Getting the nodes:
			from.node<<-edge[[1,"from"]];
			to.node<<-edge[[1,"to"]];

			# Getting the sequence objects:
			from.seq<<-getSeqFromNode(this, from.node)
			to.seq<<-getSeqFromNode(this, to.node)
	
			# Getting sequence names:	
			from.name<<-from.seq$name;
			to.name<<-to.seq$name;

		}

		# Initialize the aligment matrices:
		init.aln.mats<-function(){

			# Initialize "from" element in aln.mat if necessary:	
			if( is.null(aln.mat[[from.name]] )){
					# Create a row of the states:
					tmp<-rbind(as.character(lapply(from.seq$.sites, getState)));
					# Label the columns by the site position:
					colnames(tmp)<-seq(along=from.seq$.sites);
					# Label the row with the sequence name:
					rownames(tmp)<-from.name;
					# Set the corresponding list element in aln.mat:
					aln.mat[[ from.name ]]<-tmp;
			}
			# Set from.mat
			from.mat<<-aln.mat[[ from.name ]];
			
			# Initialize "to" element int aln.mat if necessary
			if( is.null(aln.mat[[to.name]]) ){
				# Create a new entry if we are dealing with a tip:
				if(is.tip(this, to.node)){
					# Create a vector of states:
					tmp<-rbind(as.character(lapply(to.seq$.sites, getState)));
					# Label columns by position:
					colnames(tmp)<-seq(along=to.seq$.sites);
					# Label row by sequence name:
					rownames(tmp)<-to.name;
					aln.mat[[ to.name ]]<-tmp;
				}
				else {
					# A "to" element can be null only if its a tip:
					throw("aln.mat inconsistency!\n");
				}
			}
			# Set to.mat:
			to.mat<<-aln.mat[[ to.name ]];

			# Save row names:
			# The order is important! First "from", than "to"!
			row.names<<-c(rownames(from.mat), rownames(to.mat));					

		}

		# Get the sequence position of a given alignment column from
		# the column labels:
		get.seq.pos<-function(mat=NA, col=NA){
						# Column number cannot be larger than length:
						if(col > dim(mat)[[2]]){
							throw("Invalid column number!\n");
						}
						# Get the corresponding column name:
						tmp<-colnames(mat)[[col]];		
						# Return if NA:
						if(is.na(tmp)){
							return(NA);
						}
						else{
							return(as.numeric(tmp));	
						}
	  }
	
		# Check if two positions from the two *sequences* are homologous.	
		is.homologous<-function(from.pos=NA, to.pos=NA){
				# Check position validity:
				if(to.pos > to.seq$length){
					throw("to.pos too big ",to.pos);
				}
				if(from.pos > from.seq$length){
					throw("from.pos too big ",from.pos);
				
				}
				# Check if the ancestral from to.seq/to.pos is from.seq/from.pos:
				return(equals(to.seq$.sites[[ to.pos ]]$.ancestral, from.seq$.sites[[ from.pos ]]));
		}

		make.gap.in.from<-function(label=NA){
				# Create the vector with gaps:
 				gaps<-cbind(rep(c("-"), times=dim(from.mat)[[1]] ));
				# Label the column:
        colnames(gaps)<-c(label);
				# Bind the gaps with the corresponding column from to.mat,
				# and than bind with res.mat:
        res.mat<<-cbind(res.mat, rbind( gaps, cbind(to.mat[,j])  ) );
				# Restore rownames:	
        rownames(res.mat)<<-row.names;
				# Increment counter for to.mat:
        j<<-j+1;
		}

		make.gap.in.to<-function(label=NA){
			# See above.
			gaps<-cbind(rep(c("-"), times=dim(to.mat)[[1]] ));
			colnames(gaps)<-c(label);
			res.mat<<-cbind(res.mat, rbind(  cbind(from.mat[,i]), gaps  ) );
			rownames(res.mat)<<-row.names;
			i<<-i+1;

		}

		emmit.homologous<-function(){
			# Bind the two columns into one column:
			tmp<-cbind(rbind( cbind(from.mat[,i]), cbind(to.mat[,j]) ) );
			# Label the column by from.pos:	
			colnames(tmp)<-c(from.pos);
			# Set res.mat
			res.mat<<-cbind(res.mat, tmp );
			# resotre rownames:
			rownames(res.mat)<<-row.names;
			
			i<<-i+1;
			j<<-j+1;
		}

		# FIXME - extend to work with tupples!

		# Iterate over the reverse of the edege matrix:	
		for (edge.number in rev(seq(from=1, to=this$nedges))){

			# Call variable initialization:
			init.vars();

			# Initialize partial alignment matrices:
			init.aln.mats();
		
			# The matrix holding the resulting partial alignment:	
			res.mat<-c();

			# Column counter for from.mat:
			i<-1;
			# Column counter for to.mat:
			j<-1;
			
			while(i <=dim(from.mat)[[2]] | j <=dim(to.mat)[[2]] ){

					# First of all, check for counter overflow:
					if(i > dim(from.mat)[[2]]){
							# If i is greater than the length of from.mat, but we 
							# are still iterating, that means that we have parts left from
							# to.mat, so we have to create gaps in from.mat, and increment j.
							make.gap.in.from();
							next();
					}
					else if (j > dim(to.mat)[[2]]){
							# If j is greater than the length of to.mat and we still iterating,
							# that means that we have still some columns in from.mat, so we create
							# gaps in to.mat and increment i. We label the new column with from.pos.
							from.pos<-get.seq.pos(mat=from.mat, col=i);
							make.gap.in.to(label=from.pos);
							next();
					}

					# Now figure out the positions:
					from.pos<-get.seq.pos(mat=from.mat, col=i);
					to.pos<-get.seq.pos(mat=to.mat, col=j);

					# Now check fot the gaps wich were introduced before:

					if(is.na(from.pos)){
						# If we have a gap in from.mat,
						# than emmit the columnt with a gap in "to":
						make.gap.in.to();
						next();
					}

					if(is.na(to.pos)){
						# Existent gap in to.mat:
						make.gap.in.from();
						next();
					}

					# Now we have some real alignment to do here:

					if(is.homologous(from.pos=from.pos, to.pos=to.pos)){
						# We have to homologous columns, bind them, and emmit:
						emmit.homologous();
						next();
					}
					else if(is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
						# The two columns are not homologous. The column in "to"
						# was inserted by a process. Make gap in from:
						make.gap.in.from();
						next();

					} 
					else {
						# The only possibility left is a deletion in the child sequence.
						# Make gaps in "to", label new column by from.pos:
						make.gap.in.to(label=from.pos);
						next();
					}

		 } # while i | j
			
			# Replace the "from" element in aln.mat with the resulting partial
			# alignment matrix: 
			aln.mat [[ from.name ]]<-res.mat;

		} # for edge.number
		alignment <-aln.mat[[ this$rootSeq$name ]];
		# Check the correcteness of the alignment if paranoid:
		# FXME - disable checking by default!
		paranoid<-TRUE;
		if(paranoid){
			.checkAlignmentConsistency(this, alignment);
		}

		# The whole alignment is assotiated with the root node:
		return(alignment);
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkAlignmentConsistency
##
setMethodS3(
  ".checkAlignmentConsistency",
  class="PhyloSim",
  function(
    this,
    aln,
    ...
  ){
	
	# First check if the sequences are intact:
	for(node in this$nodes){
		seq.node<-getSeqFromNode(this, node);
		seq.aln<-aln[seq.node$name, ];
		seq.aln<-seq.aln[ which(seq.aln != "-") ];
		seq.aln<-paste(seq.aln, collapse="");
		if(seq.aln != seq.node$string){
			throw("The alignment is inconsistent with the sequence objects!\n Blaming ",seq.node$name, ".\n");
		}
	}

	for(edge.number in rev(seq(from=1, to=this$nedges))){

			# Getting the edge:
			edge<-getEdge(this, edge.number);
			
			# Getting the nodes:
			from.node<-edge[[1,"from"]];
			to.node<-edge[[1,"to"]];

			# Getting the sequence objects:
			from.seq<-getSeqFromNode(this, from.node)
			to.seq<-getSeqFromNode(this, to.node)
	
			# Getting sequence names:	
			from.name<-from.seq$name;
			to.name<-to.seq$name;

			# Initializing positions:
			from.pos<-1;
			to.pos<-1;
			
			# Iterate over edges:
			for (i in 1:dim(aln)[[2]]){

					# Overflow in "from" counter,
					if(from.pos > from.seq$length){
						to.char<-aln[to.name,i];			
						if(to.char != "-"){
							# we have a final insertion:
							if(!is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
								throw("Alignment insertion inconsistency!\n");
							}
							to.pos<-to.pos+1;
							next();
						} else {
							break();
						}
					}				
					
					# Overflow in "to" counter (final deletion):
					if(to.pos > to.seq$length){
						break();
					}				
					
					# Get the symbols from alignment:	
					from.char<-aln[from.name,i];			
					to.char<-aln[to.name,i];			
					
					# If both are gaps:	
					if(from.char == "-" | to.char == "-"){
						if(from.char != "-"){
							from.pos<-(from.pos+1);
						}
						if(to.char != "-"){
							# Check ancestral pointer for inserted sites:
							if(!is.Process(to.seq$.sites[[to.pos]]$.ancestral)){
								throw("Alignment insertion inconsistency!\n");
							}
							to.pos<-(to.pos+1);
						}
						next();
					} else {
							 # We must have a homology here:						
							 if(!equals(to.seq$.sites[[ to.pos ]]$.ancestral, from.seq$.sites[[ from.pos ]])){
									throw("Non-homologous sites aligned! Alignment is inconsistent!\n");	
							}
							 from.pos<-(from.pos+1);
							 to.pos<-(to.pos+1);
					}
	
			}

	}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
##
## Method: saveAlignment
##
setMethodS3(
  "saveAlignment",
  class="PhyloSim",
  function(
    this,
    file="phylosim.fas",
		paranoid=FALSE,
    ...
  ){

		if(any(is.na(this$.alignment))){
			warning("Alignment is undefined, so nothing was saved!\n");
			return();
		}
		else {
			if(paranoid){
				.checkAlignmentConsistency(this, this$.alignment);
			}
			sink(file);
			for(i in 1:dim(this$.alignment)[[1]]){
				cat(">",rownames(this$.alignment)[[i]],"\n");
				cat(paste(this$.alignment[i,],collapse=""),"\n");
			}
			sink(NULL);
		}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: plot.Phylosim
##
setMethodS3(
  "plot",
  class="PhyloSim",
  function(
    this,
    value,
    ...
  ){

		plot(this$.phylo);
		nodelabels();

		return(invisible(this));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: summary.Sequence
##
setMethodS3(
  "summary",
  class="PhyloSim",
  function(
    this,
    ...
  ){

     this$.summary$"Name"<-this$name;
     this$.summary$"Id"<-this$id;

		 root.seq<-NA;
		 if(is.Sequence(this$rootSeq)){
			root.seq<-this$rootSeq$id;
		 }
		 this$.summary$"Root Sequence"<-root.seq;

		 if(is.Sequence(this$rootSeq)){
		 	this$.summary$"Root Sequence big rate"<-this$rootSeq$bigRate;
		 }
     this$.summary$"Tree length"<-this$treeLength;
			
		 phylo.details<-grep(pattern="[[:alnum:]]+",x=capture.output(print(this$.phylo)),perl=TRUE,value=TRUE);
		 
		 phylo.details<-paste("\n",phylo.details,collapse="",sep="\t");
		 this$.summary$"Phylo object details"<-phylo.details;

		 aln<-"undefined";
		 if(is.matrix(this$alignment)){
				aln<-"defined";	
		 }
     this$.summary$"Alignment"<-aln;

     NextMethod();


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

