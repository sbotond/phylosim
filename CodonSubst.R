##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	
##########################################################################/** 
#
# @RdocClass CodonSubst
# 
# @title "The CodonSubst class"
# 
# \description{ 
#       This is a class implementing a continuous-time Markov process acting on 
#       the state-space defined by the \code{CodonAlphabet} class. 
#
#	The rate matrix can be built from PAML files specified by the \code{paml.file} argument.
#	Alternatively the rates can be specified as a list through the \code{rate.list} parameter.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{name}{The name of the object.}
#	\item{paml.file}{The name of the PAML file used to construct the rate matrix.}
#	\item{rate.list}{A list of unscaled substitution rates (see \code{setRateList.GeneralSubstitution}).}
#	\item{equ.dist}{Equilibrium distribution.}
# 	\item{...}{Additional arguments.}
# }
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create an object
#	p<-CodonSubst()
#	# build rate matrix from paml file
#	# buildFromPAML(p,"path_to_paml_file")	# do not run this
#	# get object summary	
#	summary(p)
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
  "CodonSubst",
  function( 
		name="Anonymous", 
		paml.file=NA,
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

		got.rate.list<-!missing(rate.list);
		got.equ.dist<-!missing(equ.dist);
		
		extend.with.s<-function(this){		
			this<-extend(this, "CodonSubst",
				.s.matrix=data.matrix(matrix(ncol=this$.alphabet$size,nrow=this$.alphabet$size)),
				.paml.file=NA
				);
			rownames(this$.s.matrix)<-this$.alphabet$.symbols;
			colnames(this$.s.matrix)<-this$.alphabet$.symbols;

			# Setting diagonal elements to zero:			
			diag(this$.s.matrix)<-0;

			return(this);
		
		}
		this<-NA;
		if(missing(paml.file)){
			#  No PAML file given
			
			# Got rate list and equlibrium distribution:
			if(got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(),
					rate.list=rate.list,
					equ.dist=equ.dist
				);	
				this<-extend(this, "CodonSubst");
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(),
					rate.list=rate.list
				);	
				this<-extend.with.s(this);
			}
			
			# Got equlibrium distribution,
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet(),
					equ.dist=equ.dist
				);	
				this<-extend(this, "CodonSubst");
				this<-extend.with.s(this);
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=CodonAlphabet()
				);	
				this<-extend.with.s(this);
			}

		}
		else {
			# PAML file given:
			if( got.rate.list){
				warning("Building process from PAML file, the \"rate.list\" parameter is ignored!\n");
			}

			this<-GeneralSubstitution(
				name=name,
				alphabet=CodonAlphabet()
			);	

			this<-extend.with.s(this);
	
			if(got.equ.dist){
				setEquDist(this,equ.dist,force=TRUE);
			}
	
			buildFromPAML(this, paml.file);		

		}

		# Force clearing id cache:		
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
	class="CodonSubst", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

			if(!inherits(this$alphabet, "CodonAlphabet")){
				throw("This process must have as alphabet an CodonAlphabet object!\n");
			}

			if(!any(is.na(this$.s.matrix))){
				for(i in this$.alphabet$.symbols){
					for(j in this$.alphabet$.symbols){
						if(i != j){
								expected<-this$.s.matrix[i, j] * this$.equ.dist[1,j];
								actual<-this$.q.matrix$.orig.matrix[i,j];
								if(!PSRoot$my.all.equal(expected, actual)){
									throw("The rate matrix is not compatible with the exchangeability matrix and the equilibrium distribution!\n");
								}
						}
					}
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
## Method: buildFromPAML
##	
###########################################################################/**
#
# @RdocMethod buildFromPAML
# 
# @title "Build rate matrix from PAML file" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{A CodonSubst object.} 
#	\item{paml.file}{Path to the PAML file.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The CodonSubst object (invisible).
# } 
# 
# \examples{
#	# create an object
#	p<-CodonSubst()
#	# build rate matrix from paml file
#	# buildFromPAML(p,"path_to_paml_file")	# do not run this
#	# get object summary	
#	summary(p)
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
	"buildFromPAML", 
	class="CodonSubst", 
	function(
		this,
		paml.file,
		...
	){

		if(!missing(paml.file)){
			data<-.readFromPAML(this, paml.file=paml.file);
			this$.paml.file<-paml.file;
			
			if(all(is.na(this$equDist))){
				setEquDist(this, value=data[["pi"]], force=TRUE, silent=TRUE)
			}
			S<-data$S;
		
			this$.s.matrix<-S;	
		
			for(i in this$.alphabet$.symbols){
				for(j in this$.alphabet$.symbols){
					if(i != j){
						setRate(this$.q.matrix,from=i,to=j,value=(S[i,j] * this$.equ.dist[1, j]),scale=FALSE);
					}
				}
			}
			
			.callRateRescaling(this$.q.matrix,guess.equ=FALSE);
		
		}
		else{
			throw("PAML data file not specified");
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
## Method: .readFromPAML
##	
setMethodS3(
	".readFromPAML", 
	class="CodonSubst", 
	function(
		this,
		paml.file,
		...
	){

		if(missing(paml.file)){
			throw("No PAML data file specified!\n");
		}	
		else if(file.access(c(paml.file), mode=0) != c(0)){
			throw("The specified PAML data file \"",paml.file,"\" does not exist!\n",sep="");
		}
		else if(file.access(c(paml.file), mode=4) != c(0)){
			throw("The specified PAML data file \"",paml.file,"\" cannot be opened for reading because of insufficient permissions!\n",sep="");
		}
		else {
		
		size<-this$alphabet$size;
		symbols<-this$alphabet$symbols;

		lines<-scan(file=paml.file,what="character",sep="\n",blank.lines.skip=FALSE,quiet=TRUE);
		
		is.blank<-function(line){
			if(length(grep(pattern="^\\s*$",x=line,perl=TRUE,value=FALSE)) > 0 ){
				return(TRUE);
			}
			else if(length(grep(pattern="\\d",x=line,perl=TRUE,value=FALSE)) < 1){
				# If the line has no decimal number than is considered blank!
				return(TRUE);

			}
			return(FALSE);
		}

		# Skip blank lines:
		count<-1;
		while(is.blank(lines[[count]])){
			count<-count+1;
		}
		skip<-count-1;

		# Find the beggining of the equilibrium distribution:
		count<-skip+size+1;
		while(is.blank(lines[[count]])){
      count<-count+1;
    }
		equ.skip<-count-1;

		# How many lines has the equilibrium distribution?
		count<-equ.skip;
		while(!is.blank(lines[[count<-count+1]])){ }
		equ.nlines<-count-equ.skip-1;

		# We don't need the lines any more.
		rm(lines);

		# Reading the exchangeability matrix:

		# Assuming here that the order of the
		# codons is the same as in the CodonAlphabet
		# object.	
	
		numbers<-scan(file=paml.file,what=0.0,skip=skip,nlines=(size-1),quiet=TRUE);

		if(length(numbers) != ((size^2-size)/2)){
			throw("Error reading exchangeability matrix from PAML data file!\n");
		}

		s.matrix<-matrix(nrow=size,ncol=size);
		diag(s.matrix)<-0;
		colnames(s.matrix)<-symbols;
		rownames(s.matrix)<-symbols;

		counter<-1;
		for(i in 1:size)	{

			for(j in 1:i){
					if( i!= j){
						s.matrix[i, j]<-numbers[counter];
						s.matrix[j, i]<-numbers[counter];
						counter<-counter + 1;	
					}
				}

		}

		# Reading the equilibrium distribution:
		equ.dist<-(scan(file=paml.file,what=0.0,skip=equ.skip, nlines=equ.nlines, quiet=TRUE));

		if(length(equ.dist) != size){
			throw("Error reading equlibrium distribution from PAML data file!\n");
		}
		equ.dist<-rbind(equ.dist);
		colnames(equ.dist)<-symbols;

		return(list(
			"S"=s.matrix,
			"pi"=equ.dist
		));

		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setEquDist
##
setMethodS3(
  "setEquDist",
  class="CodonSubst",
  function(
    this,
    value,
    force=FALSE,
    silent=FALSE,
    ...
  ){

		# Behave like GeneralSubstitution if the S matrix is empty.
		if(any(is.na(this$.s.matrix))){
			return(NextMethod());
		}

    .checkWriteProtection(this);
    if(!is.Alphabet(this$alphabet)){
      throw("Cannot set equilibrium distribution because the alphabet is undefined!");
    }
    if(missing(value)) {
      throw("No new value provided!\n");}
    else if(!is.numeric(value)) {
      throw("The new value must be numeric!\n");
    }
    else if(length(value) != this$alphabet$size){
      throw("The new value must be a vector of length ",this$alphabet$size,"!\n");
    }
    else if(!PSRoot$my.all.equal(sum(value), 1.0)) {
        value<-(value/sum(value));
        if (silent == FALSE){
          warning("The provided probabilities were rescaled in order to sum to one!\n");
        }
    }

		value<-rbind(value);
		colnames(value)<-this$.alphabet$symbols;
		this$.equ.dist<-value;	

			for(i in this$.alphabet$.symbols){
				for(j in this$.alphabet$.symbols){
					if(i != j){
						setRate(this$.q.matrix,from=i,to=j,value=(this$.s.matrix[i,j] * value[1, j]),scale=FALSE);
					}
				}
			}

			.callRateRescaling(this$QMatrix,guess.equ=FALSE);	
      return(invisible(this));


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
#       a<-CodonSubst()
#       # get a summary
#       summary(a)
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
  class="CodonSubst",
  function(
    object,
    ...
  ){

    this<-object;
    .addSummaryNameId(this);
    .addSummaryAlphabet(this);

		if(is.na(this$.paml.file)){
    	this$.summary$"Unscaled rate matrix"<-paste( "\n\t",paste(capture.output(print(this$.q.matrix$matrix,digits=5)
),collapse="\n\t"),"\n",sep="");
		}
		else {
    	this$.summary$"PAML data file:"<-this$.paml.file;
    	this$.summary$"Unscaled rate matrix"<-"not shown";
		}
    this$.summary$"Equilibrium distribution"<-paste( "\n\t",paste(capture.output(print(this$.equ.dist)),collapse="\n\t"
),"\n",sep="");
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: newMatrix 
##
###########################################################################/**
#
# @RdocMethod newMatrix
# 
# @title "New codon substitution matrix from PAML file" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{name}{Object name} 
# 	\item{paml.file}{PAML file.} 
#	\item{equ.dist}{Equilibrium distribution.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A process object inheriting from CodonSubst.
# } 
# 
# 
# @author 
# 
# \seealso{ 
# 	@seeclass 
# } 
# 
#*/###########################################################################
setMethodS3(
	"newMatrix", 
	class="CodonSubst", 
	function(
		name=NA,
		paml.file=NA,
		equ.dist=NA,
		...
	){


		PAMLDIR<-"./PAMLdat";
		RDATDIR<-"./RData";

		# Use the package data directory if loaded:
		if(length(intersect(search(),c("package:phylosim"))) == 1){
			RDATDIR<-paste(path.package("phylosim"),"/data/",sep="");
			PAMLDIR<-paste(path.package("phylosim"),"/extdata/",sep="");
		}
		
		rdname<-paste(RDATDIR,"/",name,".RData",sep="");

		if( ( file.access(c(rdname), mode=0) == c(0) ) & (file.access(c(rdname), mode=4) == c(0))){
			this<-clone(Object$load(rdname));
	
		}
		else {
		
			file<-paste(PAMLDIR,"/",paml.file,sep="");
			this<-CodonSubst(paml.file=file);
			this<-extend(this,name);
			this$name<-this$name;
			save(this, file=rdname);
			
		
		}

		if(!any(is.na(equ.dist))){
			setEquDist(this,value=equ.dist,force=TRUE);
		}

		return(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

			
