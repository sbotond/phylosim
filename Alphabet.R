##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##
##########################################################################/** 
#
# @RdocClass Alphabet
# 
# @title "The Alphabet class"
# 
# \description{ 
#		Class representing an alphabet (a set of symbols).		
#		@classhierarchy
#	
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{symbols}{A character vector containing the symbols.}
# 	\item{type}{An identifier for the Alphabet object.}
#	}
# 
# \section{Fields and Methods}{ 
#		@allmethods 
# }
# 
# \examples{ 
#		# create an alphabet object
#		a<-Alphabet(type="Binary",symbols=c("0","1"));
#		# print summary
#		summary(a);
#		# change the identifier
#		a$type<-"Nucleotide";
#		# change the symbol set
#		a$symbols<-c("A","T","G","C");
#		# print summary again
#		summary(a);
#		# clone the alphabet object
#		b<-clone(a);
#		# test the equality of the symbol sets
#		a == b;
# }
# 
# @author
#
#
# \seealso{ 
# 	Site, Process, Event
# }
# 
#*/###########################################################################
setConstructorS3(
	"Alphabet", 
	function(
		symbols=NA,
		type="Generic", # just a name
		...	
	){

	symbol_length<-NA;	
	if(!missing(symbols)){
		symbols<-as.character(symbols);
		symbol_length<-.checkSymbolLengths(symbols);
		.checkSymbolDuplicates(symbols);

	}
	
	size<-NA;
	if(!is.na(symbol_length)){
		size<-length(symbols);
	}
	extend(PSRoot(), "Alphabet",
			.symbols=symbols,
			.symbolLength=symbol_length,
			.size=size,
			.type=type,
			.write.protected=FALSE,
			.is.alphabet=TRUE
	);
	},
	###
	enforceRCC=TRUE
);

##	
## Method: .checkSymbolLengths
##	
setMethodS3(
	".checkSymbolLengths", 
	class="character", 
	function(
		this,
		...
	){

		if(length(this) == 0 ){return(0)}			
		symbol_lengths<-stringLengthVector(this);
		if(length(unique(symbol_lengths)) != 1) {
			throw("The symbols must have the same length!");
		} else {
			symbol_lengths[[1]];
		}

	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkSymbolDuplicates
##	
setMethodS3(
	".checkSymbolDuplicates", 
	class="character", 
	function(
		this,
		...
	){

		# Check for the gap character "-", and die if present:
		if(length(grep("^-+$",this)) != 0){
			throw("The symbol sets cannot contain the character \"-\" as that is reserved as a gap symbol!\n");
		}

		if(length(this) != length(unique(this))){
			throw("The alphabet must not contain duplicated symbols!");
		} else {
			return(invisible(TRUE));
		}				

	},
	###
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkConsistency
##	
setMethodS3(
	".checkConsistency", 
	class="character", 
	function(
		this,
		...
	){
		
		.checkSymbolLengths(this);
		.checkSymbolDuplicates(this);

	},
	private=TRUE,
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
# @title "Checks the consistency of Alphabet objects"
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
# 
# \value{ 
#		Returns an invisible TRUE if no inconsistencies found, throws an error otherwise. 
# } 
# 
# \examples{
#		# create an alphabet object
#		a<-Alphabet(symbols=c(0,1));
#		# check consistency
#		print(checkConsistency(a));
#		# mess up with the internals
#		a$.symbols[1]<-"BAD";
#		# NOT run: cosistency check now will throw an error
#		\dontrun{ print(checkConsistency(a)); }
#
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
	class="Alphabet", 
	function(
		this,
		...
	){
		
		if(is.null(this$.symbols)) {
			throw("Alphabet symbols is NULL!\n");
		}	
		else if(is.null(this$.size)) {
			throw("Alphabet size is NULL!\n");
		}	
		else if(is.null(this$.symbolLength)) {
			throw("Alphabet symbol length is NULL!\n");
		}	
		else if(is.null(this$.type)) {
			throw("Alphabet type is NULL!\n");
		}	
		# Disable write protection for a while.
		wp<-this$writeProtected;
		if(wp) {
			this$writeProtected<-FALSE;
		}	
		
		may.fail<-function(this){
			this$symbols<-this$symbols;
			if( length(this$symbols) != this$size) {
				throw("Alphabet object inconsistent! Length mismatch!\n");
			}
		}
		tryCatch(may.fail(this), finally=this$writeProtected<-wp);

		.checkConsistency(this$.symbols);

	},
	###
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: ==
##	
setMethodS3(
	"==", 
	class="Alphabet", 
	function(
		e1,	
		e2,
		...
	){

		# First check by reference:
		if ( equals(e1,e2) ) {return(TRUE)}
		# Check if both objects inherit from Alphabet:
		if (!length(intersect(intersect(class(e1),class(e2)),c("Alphabet")))){
			throw("Alphabet object compared to something else!");
		}
		# Check ANY flag:
		if(!is.null(e1$.any.flag) | !is.null(e2$.any.flag)) { return(TRUE) }
		# then check by value:
		setequal(e1$.symbols,e2$.symbols);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: !=
##	
setMethodS3(
	"!=", 
	class="Alphabet", 
	function(
		e1,	
		e2,
		...
	){
		!'=='(e1,e2);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSymbols
##	
setMethodS3(
	"getSymbols", 
	class="Alphabet", 
	function(
		this,
		...
	){
		
		as.character(this$.symbols);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSymbols
##	
setMethodS3(
	"setSymbols", 
	class="Alphabet", 
	function(
		this,
		set,	# the new symbol set
		...	
	){
			if (is.null(set))	{
				throw("Cannot set NULL as symbols!\n");
			}
			.checkWriteProtection(this);	
			set<-as.character(set);					
			.checkSymbolDuplicates(set)
			this$.symbolLength<-.checkSymbolLengths(set);
			this$.size<-length(set);
			this$.symbols<-set;	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSymbolLength
##	
setMethodS3(
	"getSymbolLength", 
	class="Alphabet", 
	function(
		this,
		...
	){

		this$.symbolLength;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSymbolLength
##	
setMethodS3(
	"setSymbolLength", 
	class="Alphabet", 
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
## Method: getSize
##	
setMethodS3(
	"getSize", 
	class="Alphabet", 
	function(
		this,
		...
	){

		this$.size;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getType
##	
setMethodS3(
	"getType", 
	class="Alphabet", 
	function(
		this,
		...
	){

		this$.type;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setType
##	
setMethodS3(
	"setType", 
	class="Alphabet", 
	function(
		this,
		new_type,
		...
	){


		.checkWriteProtection(this);	
		if (length(new_type) != 1) {throw("The new type must be a character vector of length 1!")}	
		if (new_type == "" ){ throw("Cannot set empty type!")}
		this$.type<-new_type;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasSymbols
##	
setMethodS3(
	"hasSymbols", 
	class="Alphabet", 
	function(
		this,
		sym,
		...
	){
	
		sym<-unique(as.character(sym));
		if (length(intersect(this$.symbols,sym)) == length(sym)){
			return(TRUE);
		}
		# Check ANY flag:
		else if(!is.null(this$.any.flag)) { return(TRUE) }
		else {
			return(FALSE);
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getWriteProtected
##	
setMethodS3(
	"getWriteProtected", 
	class="Alphabet", 
	function(
		this,
		...
	){

		this$.write.protected;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setWriteProtected
##	
setMethodS3(
	"setWriteProtected", 
	class="Alphabet", 
	function(
		this,
		value,
		...
	){
		
		if(!is.logical(value)) {throw("The new value must be logical!\n")} 
		else {
			this$.write.protected<-value;
		}
			
	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkWriteProtection
##	
setMethodS3(
	".checkWriteProtection", 
	class="Alphabet", 
	function(
		this,
		...
	){
		
		if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
		else {return(invisible(FALSE))}
			
	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSymbolLength
##	
setMethodS3(
	"setSize", 
	class="Alphabet", 
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
## Method: as.character
##	
setMethodS3(
	"as.character", 
	class="Alphabet", 
	function(
		x,
		...
	){

		x$.symbols;				

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summray.Alphabet
##	
setMethodS3(
	"summary", 
	class="Alphabet", 
	function(
		object,
		...
	){
	
			this<-object;	
			this$.summary$Type<-this$type;
			this$.summary$Size<-this$size;
			this$.summary$Symbols<-paste(this$symbols,collapse=' ');
			this$.summary$"Symbol length"<-this$symbolLength;
			if(getWriteProtected(this)){
				this$.summary$"Write protected"<-TRUE;	
			}
		
			NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
	);

##	
## Method: is.Alphabet
##	
setMethodS3(
	"is.Alphabet", 
	class="default", 
	function(
		this,
		...
	){
	
    if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.alphabet)){return(TRUE)}
    if ( inherits(this, "Alphabet")) {
      this$.is.alphabet<-TRUE;
      return(TRUE);
    } else {
      return(FALSE)
    }
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: isEmpty
##	
setMethodS3(
	"isEmpty", 
	class="Alphabet", 
	function(
		this,
		...
	){
	
	if(is.na(this$.size) | this$.size == 0 ){
		return(TRUE);
	}
	else {
		return(FALSE);
	}
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);




