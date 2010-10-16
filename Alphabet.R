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
#
#		Class representing an alphabet (a set of symbols).
#		@classhierarchy
#	
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{symbols}{A character vector containing the symbols for the alphabet. All elements must have the 
#	same length and no duplicates are allowed. No element of the vector may contain the dash symbol, which is reserved for gaps.}
# 	\item{type}{An identifier for the Alphabet object.}
#	\item{...}{Not used.}
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
# 	Site Process Event BinaryAlphabet NucleotideAlphabet AminoAcidAlphabet
# }
# 
#*/###########################################################################
setConstructorS3(
	"Alphabet", 
	function(
		symbols=NA,
		type="Generic",
		...	
	){

	symbol_length<-NA;	
	if(!missing(symbols)){
		symbols<-as.character(symbols);
		symbol_length<-.checkSymbolLengths(symbols);
		if(!exists(x="PSIM_FAST")){
			.checkSymbolDuplicates(symbols);
		}

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
# @title "Check object consistency"
# 
# \description{ 
#		@get "title".
# } 
# 
# @synopsis 
#
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{...}{Not used.} 
# } 
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
###########################################################################/**
#
# @RdocMethod ==
# \alias{!=.Alphabet}
# @title "Check if two alaphabet objects have the same symbol set" 
#  
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{e1}{An Alphabet object.} 
# 	\item{e2}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE 
# } 
# 
# \examples{
#	
#	# create an alpahabet object
#	a<-Alphabet(symbols=c(0,1));	
#	# clone object
#	b<-clone(a)
#	# compare the two objects
#	print(a == b)
#	# modify symbol set in b
#	b$symbols<-c("AT","GC");
#	print(a == b)
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
		if(!exists(x="PSIM_FAST")){
			if (!length(intersect(intersect(class(e1),class(e2)),c("Alphabet")))){
				throw("Alphabet object compared to something else!");
			}
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
###########################################################################/**
#
# @RdocMethod getSymbols
# 
# @title "Get the symbol set from an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	A character vector containing the symbol set of the Alphabet object.
# } 
# 
# \examples{
#	
#	# create a new alphabet object
#	a<-Alphabet(symbols=c("AC","GT"));
#	# get the symbols
#	getSymbols(a)
#	# get the symbols by using the virtual field
#	a$symbols
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
###########################################################################/**
#
# @RdocMethod setSymbols
# 
# @title "Specify a new symbol set for an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{set}{The vector containing the new symbols set, automatically converted 
#	into a character vector.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new symbol set as a character vector.
# } 
# 
# \examples{
#
#	# create a new alphabet object	
#	a<-Alphabet()
#	a
#	# specify a new symbol set
#	setSymbols(a,c(0,1))
#	a
#	# the same, but now use the virtual field
#	a$symbols<-c("A","T","G","C")
#	a
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
			if(!exists(x="PSIM_FAST")){
				.checkSymbolDuplicates(set)
			}
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
###########################################################################/**
#
# @RdocMethod getSymbolLength
# 
# @title "Get the length of the symbols in a given alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A numeric vector of length one.
# } 
# 
# \examples{
#
#	# create an alphabet object	
#	a<-Alphabet(symbols=c("AAA","AAC"));
#	# get symbol length
#	getSymbolLength(a);
#	# get symbol length via virtual field
#	a$symbolLength
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
###########################################################################/**
#
# @RdocMethod setSymbolLength
# 
# @title "Forbidden action: setting the symbol length for an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{value}{Not used.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Throws an error.
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
###########################################################################/**
#
# @RdocMethod getSize
# 
# @title "Get the number of symbols in an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#	An integer vector of length one.
# } 
# 
# \examples{
#	
#	# create an alphabet object
#	a<-Alphabet(symbols=c(0,1,2,3,4,5))
#	a
#	# get alphabet size
#	getSize(a)
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
###########################################################################/**
#
# @RdocMethod getType
# 
# @title "Get Alphabet obejct type" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	A character vector of length one.
# } 
# 
# \examples{
#	
#	# create alphabet object
#	a<-Alphabet(symbols=c(0,1),type="Binary");
#	# get alphabet type
#	getType(a)
#	a$type
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
###########################################################################/**
#
# @RdocMethod setType
# 
# @title "Set Alphabet object type" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
#	\item{new_type}{A character vector of length one.}
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	The new type (invisible).
# } 
# 
# \examples{
#	# create an alphabet object
#	a<-Alphabet(symbols=c(1,2,3))
#	# set a new type
#	setType(a,"MyAlphabet")
#	a$type
#	# set type via virtual field
#	a$type<-"MorphChars"
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
	"setType", 
	class="Alphabet", 
	function(
		this,
		new_type,
		...
	){

		
		.checkWriteProtection(this);	
		if(!exists(x="PSIM_FAST")){
			if (length(new_type) != 1) {throw("The new type must be a character vector of length 1!")}	
			if (new_type == "" ){ throw("Cannot set empty type!")}
		}
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
###########################################################################/**
#
# @RdocMethod hasSymbols
# 
# @title "Check if an Alphabet object has a given set of symbols" 
# 
# \description{ 
#	@get "title".
#	Returns true if the class of the object is "AnyAlphabet".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{sym}{A character vector.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE
# } 
# 
# \examples{
#	
#	# create alphabet object
#	a<-Alphabet(symbols=c("A","T","G","C"));
#	# check if it has the symbols "A" and "G"
#	hasSymbols(a,c("A","G"))
#	# check if has the symbol "X"
#	hasSymbols(a,"X")
#	# any alphabet returns true for every symbol
#	b<-AnyAlphabet();	
#	hasSymbols(b,"X")
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
###########################################################################/**
#
# @RdocMethod getWriteProtected
#  
# @title "Check if the object is write protected" 
# 
# \description{ 
#	@get "title".
#	Write protected objects cannot be modified through get/set methods and virtual fields.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE.
# } 
# 
# \examples{
#
#       # create an object
#       o<-Alphabet()
#       # toggle write protection
#       o$writeProtected<-TRUE
#       # check if it's write protected
#       getWriteProtected(o)
#       # check write protection via virtual field
#       o$writeProtected
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
###########################################################################/**
#
# @RdocMethod setWriteProtected
#  
# @title "Set the write protection field for an object" 
# 
# \description{ 
#	@get "title".
#	Write protected objects cannot be modified through get/set methods and virtual fields.
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{value}{A logical vector of size one.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Invisible TRUE or FALSE.
# } 
# 
# \examples{
#
#	# create an object
#	o<-Alphabet()
#	# toggle write protection
#	setWriteProtected(o,TRUE)
#	# check write protection
#	o$writeProtected
#	# set write protection via virtual field
#	o$writeProtected<-FALSE
#	o$writeProtected
#	
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
		if(exists(x="PSIM_FAST")){ return(FALSE) }	

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
## Method: setSize
##	
###########################################################################/**
#
# @RdocMethod setSize
# 
# @title "Forbidden action: setting the symbol set size of an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{value}{Not used.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Throws an error.
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
###########################################################################/**
#
# @RdocMethod as.character
# 
# @title "Get the character representation of an Alphabet object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{x}{An Alphabet object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	Returns the character vector containing the symbol set.
# } 
# 
# \examples{
#
#	# create alphabet object
#	a<-Alphabet(symbols=c("A","T","G","C","N"))
#	# get charcter representation
#	as.character(a)
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
## Method: summary.Alphabet
##	
###########################################################################/**
#
# @RdocMethod summary
# 
# @title "Summarize the properties of an object" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{object}{An object} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
#  Returns a PSRootSummary object.
# } 
# 
# \examples{
#
#	# create an object
#	a<-NucleotideAlphabet()
#	# get a summary
#	summary(a)
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
###########################################################################/**
#
# @RdocDefault is.Alphabet
# 
# @title "Check if an object is an instance of the Alphabet class" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE
# } 
# 
# \examples{
#
#	# create an alphabet object
#	a<-Alphabet()
#	# create a PSRoot object
#	o<-PSRoot()
#	# check if they are alphabet objects
#	is.Alphabet(a)
#	is.Alphabet(o)
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
###########################################################################/**
#
# @RdocMethod isEmpty
# 
# @title "Check if the symbol set is empty" 
# 
# \description{ 
#	@get "title".
# } 
# 
# @synopsis 
# 
# \arguments{ 
# 	\item{this}{An Alphabet object.} 
# 	\item{...}{Not used.} 
# } 
# 
# \value{ 
# 	TRUE or FALSE
# } 
# 
# \examples{
#
#	# create an empty alphabet
#	a<-Alphabet();
#	# check whether it is empty
#	isEmpty(a)
#	# specify a new symbol set
#	a$symbols<-c(0,1)
#	isEmpty(a)
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




