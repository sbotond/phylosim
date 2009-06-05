##	$Id: Alphabet.R,v 1.27 2009-04-30 11:26:45 sbotond Exp $
##
##	Class: Alphabet
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
		this,	
		that,
		...
	){
	
		# First check by reference:
		if ( equals(this,that) ) {return(TRUE)}
		# Check if both objects inherit from Alphabet:
		if (!length(intersect(intersect(class(this),class(that)),c("Alphabet")))){
			throw("Alphabet object compared to something else!");
		}
		# Check ANY flag:
		# FIXME - this is not elegant!
		if(!is.null(this$.any.flag) | !is.null(that$.any.flag)) { return(TRUE) }
		# then check by value:
		setequal(this$.symbols,that$.symbols);	

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
		this,	
		that,
		...
	){
		!'=='(this,that);
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
	private=FALSE,
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
		value,
		...
	){
		
		if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
		else {return(invisible(FALSE))}
			
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
		this,
		...
	){

		this$.symbols;				

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
		this,
		...
	){
		
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




##	$Id: AlphabetsList.R,v 1.2 2009/04/14 16:08:13 sbotond Exp $
##
##	Class: AlphabetsList
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
	"AlphabetsList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"AlphabetsList",
			.seq=NA
		);

		 if(!missing(seq)) {
     	if(!is.Sequence(seq)) {
        throw("Sequence object not valid!\n");
      } else {
        this$.seq=seq;
      }
    }

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.AlphabetsList
##	
setMethodS3(
	"is.AlphabetsList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "AlphabetsList");

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
setMethodS3(
	"checkConsistency", 
	class="AlphabetsList", 
	function(
		this,
		...
	){

   if(!is.Sequence(this$.seq)){
      throw("Alphabets list sequence reference is invalid!\n");
    }
    return(TRUE);


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [.AlphabetsList
##	
setMethodS3(
	"[", 
	class="AlphabetsList", 
	function(
		this,
		index
	){

		getAlphabets(this$.seq,index);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.AlphabetsList
##	
setMethodS3(
	"[<-", 
	class="AlphabetsList", 
	function(
		this,
		index,
		value
	){

		setAlphabets(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.AlphabetsList
##	
setMethodS3(
	"[[", 
	class="AlphabetsList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getAlphabets(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.AlphabetsList
##	
setMethodS3(
	"[[<-", 
	class="AlphabetsList", 
	function(
		this,
		index,
		value
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		} else if (length(value) > 1) {
			warning("Value vector longer than one!\n");
		}
		setAlphabets(this$.seq,value,index);

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
	class="AlphabetsList", 
	function(
		this,
		...
	){
		
		this[];	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: AminoAcidSubst.R,v 1.1 2009-05-01 16:06:13 sbotond Exp $
##
##	Class: Amino acid model constructors.
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

setMethodS3(
	".newAAMatrix", 
	class="AminoAcidSubst", 
	function(
		name=NA,
		paml.file=NA,
		equ.dist=NA,
		...
	){


		PAMLDIR<-"PAMLdat";
		RDATDIR<-"RData";
		
		rdname<-paste(RDATDIR,"/",name,".Rdgz",sep="");

		if( ( file.access(c(rdname), mode=0) == c(0) ) & (file.access(c(rdname), mode=4) == c(0))){
			this<-clone(Object$load(rdname));
	
		}
		else {
		
			file<-paste(PAMLDIR,"/",paml.file,sep="");
			this<-AminoAcidSubst(paml.file=file);
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

##
## cpREV
##
setConstructorS3(
  "cpREV",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="cpREV",
			paml.file="cpREV.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## PAM
##
setConstructorS3(
  "PAM",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="PAM",
			paml.file="dayhoff.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## PAM-dcmut
##
setConstructorS3(
  "PAM.dcmut",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="PAM.dcmut",
			paml.file="dayhoff-dcmut.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## JTT
##
setConstructorS3(
  "JTT",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="JTT",
			paml.file="jones.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## JTT.dcmut
##
setConstructorS3(
  "JTT.dcmut",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="JTT.dcmut",
			paml.file="jones-dcmut.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## LG
##
setConstructorS3(
  "LG",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="LG",
			paml.file="lg.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## mtArt
##
setConstructorS3(
  "mtArt",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="mtArt",
			paml.file="mtArt.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## mtMam
##
setConstructorS3(
  "mtMam",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="mtMam",
			paml.file="mtmam.dat",
			equ.dist=equ.dist
		);
		
		return(this);

  },
  enforceRCC=FALSE
);

##
## mtREV24
##
setConstructorS3(
  "mtREV24",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="mtREV24",
			paml.file="mtREV24.dat",
			equ.dist=equ.dist
		);
		
		return(this);

  },
  enforceRCC=FALSE
);

##
## MtZoa
##
setConstructorS3(
  "MtZoa",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="MtZoa",
			paml.file="MtZoa.dat",
			equ.dist=equ.dist
		);
		
		return(this);

  },
  enforceRCC=FALSE
);

##
## WAG
##
setConstructorS3(
  "WAG",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$.newAAMatrix(
			name="WAG",
			paml.file="wag.dat",
			equ.dist=equ.dist
		);
		return(this);

  },
  enforceRCC=FALSE
);






##	$Id: AminoAcidSubst.R,v 1.1 2009-05-01 16:06:13 sbotond Exp $
##
##	Class: AminoAcidSubst
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
  "AminoAcidSubst",
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
			this<-extend(this, "AminoAcidSubst",
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
					alphabet=AminoAcidAlphabet(),
					rate.list=rate.list,
					equ.dist=equ.dist
				);	
				this<-extend(this, "AminoAcidSubst");
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=AminoAcidAlphabet(),
					rate.list=rate.list
				);	
				this<-extend.with.s(this);
			}
			
			# Got equlibrium distribution,
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=AminoAcidAlphabet(),
					equ.dist=equ.dist
				);	
				this<-extend(this, "AminoAcidSubst");
				this<-extend.with.s(this);
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=AminoAcidAlphabet()
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
				alphabet=AminoAcidAlphabet()
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
setMethodS3(
	"checkConsistency", 
	class="AminoAcidSubst", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

			if(!inherits(this$alphabet, "AminoAcidAlphabet")){
				throw("This process must have as alphabet an AminoAcidAlphabet object!\n");
			}

			if(!any(is.na(this$.s.matrix))){
				for(i in this$.alphabet$.symbols){
					for(j in this$.alphabet$.symbols){
						if(i != j){
								expected<-this$.s.matrix[i, j] * this$.equ.dist[1,j];
								actual<-this$.q.matrix$.orig.matrix[i,j];
								if(!PSRoot$all.equal(expected, actual)){
									throw("The rate matrix is not compatible with the exchangeability matrix and the equilibrium distribution!\n");
								}
						}
					}
				}
			}
	
      }
      tryCatch(may.fail(this),finally=this$writeProtected<-wp);
			# FIXME - do we really need this NextMethod?
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
setMethodS3(
	"buildFromPAML", 
	class="AminoAcidSubst", 
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
	class="AminoAcidSubst", 
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
		# amino acids is the same as in the AminoAcidAlphabet
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
  class="AminoAcidSubst",
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
    else if(!PSRoot$all.equal(sum(value), 1.0)) {
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
setMethodS3(
  "summary",
  class="AminoAcidSubst",
  function(
    this,
    value,
    ...
  ){

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

				
##	$Id: AminoAcidSubst.R,v 1.1 2009-05-01 16:06:13 sbotond Exp $
##
##	Class: BinarySubst
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
  "BinarySubst",
  function( 
		name="Anonymous", 
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

		got.rate.list<-!missing(rate.list);
		got.equ.dist<-!missing(equ.dist);

		this<-NA;
		
		if(got.rate.list & got.equ.dist){
			this<-GeneralSubstitution(name=name, rate.list=rate.list, equ.dist=equ.dist, alphabet=BinaryAlphabet());
		}
		else if(got.rate.list & !got.equ.dist){
			this<-GeneralSubstitution(name=name, rate.list=rate.list, alphabet=BinaryAlphabet());
		}
		else if(!got.rate.list & got.equ.dist){
			this<-GeneralSubstitution(name=name, equ.dist=equ.dist, alphabet=BinaryAlphabet());
		}
		else if(!got.rate.list & !got.equ.dist){
			this<-GeneralSubstitution(name=name, alphabet=BinaryAlphabet());
		}
		else {
			throw("You should never see this message!\n");
		}
		
		this<-extend(this, "BinarySubst");
		this$name<-this$name;

		return(this);
	
  },
  enforceRCC=TRUE
);

##
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="BinarySubst",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

				is.binary.alphabet<-inherits(this$alphabet, "BinaryAlphabet");
				if(!is.binary.alphabet){
					throw("The alphabet must be a BinaryAlphabet object!\n");
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


##	$Id: ContinousDeletor.R,v 1.4 2009-04-24 10:12:27 sbotond Exp $
##
##	Class: DsiscreteDeletor
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
  "ContinousDeletor",
  function( 
		name="Anonymous",
		rate=NA,
		dist=NA,
		max.length=NA,
		... 
		)	{

		this<-GeneralDeletor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			...
		);
    this<-extend(
      this,
      "ContinousDeletor",
			.dist=NA,
			.max.length=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(dist)) {
			this$dist<-dist;
			STATIC<-FALSE;
		}
		
		if(!missing(max.length)) {
			this$.max.length<-max.length;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(...){
				if(!is.expression(this$.dist)){
					throw("\"dist\" is undefined, so cannot propose deletion length!\n");
				}
				else if(is.na(this$.max.length)){
					throw("\"maxLength\" is undefined, so cannot propose deletion length!\n");
				}
				tmp<-round(eval(this$.dist));
				while( tmp > this$.max.length | tmp < 1){  tmp<-round(eval(this$.dist)) };	
				return(tmp);
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
  class="ContinousDeletor",
  function(
    this,
    length,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$maxLength)) {
          this$maxLength<-this$maxLength;
        }

        if (is.expression(this$dist)) {
          this$dist<-this$dist;
        }
        else if (!is.na(this$dist)){
          throw("Deletion length sampler expression is invalid!\n");
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
## Method: getDist
##	
setMethodS3(
	"getDist", 
	class="ContinousDeletor", 
	function(
		this,
		...
	){

		this$.dist;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSizes
##	
setMethodS3(
	"setDist", 
	class="ContinousDeletor", 
	function(
		this,
		value,
		...
	){
		
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if(!is.expression(value)) {
			throw("The new value must be a valid expression!\n");
		} else {
			# Do a test sampling:
			tmp<-eval(value);
			if( length(tmp) != 1 ) {
				throw("The return value of the length sampler expression must be of length 1!\n");
			}
			if (!is.numeric(tmp)) {
				throw("The return value of the length sampler expression must be numeric!\n");
			}
			else {
				this$.dist<-value;
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
## Method: getProbs
##	
setMethodS3(
	"getMaxLength", 
	class="ContinousDeletor", 
	function(
		this,
		...
	){

		this$.max.length;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProbs
##	
setMethodS3(
	"setMaxLength", 
	class="ContinousDeletor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if (!is.numeric(value)) {
			throw("Value vector size should be numeric!\n");
		}
		else if( round(value) != value ) {
			throw("maxLength must be integer!\n");
		} else {
			this$.max.length<-value;
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: plot
##
setMethodS3(
  "plot",
  class="ContinousDeletor",
  function(
    this,
		sample.size=NA,
    ...
  ){

		if( !is.numeric(this$maxLength) | !is.expression(this$dist) ){
				warning("Deletion length distribution is not defined properly! Nothing to plot here!\n");
				return();
		}
		size<-(this$maxLength * 5);
		if(!missing(sample.size)){
				if(!is.numeric(sample.size) | ( length(sample.size)) !=1 ) {
					throw("Sample size paramter must be a numeric vector of size 1!\n");
				} else {
					size<-round(sample.size);
				}
		}

			sample<-apply(as.array(0:size),1,this$.propose.by);
      plot.default(
				density(sample,from=0,to=this$maxLength),
        main=paste("Estimated deletion size density for:",this$id),
				sub=paste("Sample size:", size),
				type='l',
        xlab="Size",
        ylab="Density",
				xlim=c(1,this$maxLength),
				col="blue",
				lwd=1.5,
				xaxt="n"
      );
			 axis(side=1, at=c(0:this$maxLength), labels=c(0:this$maxLength));

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
setMethodS3(
  "summary",
  class="ContinousDeletor",
  function(
    this,
    value,
    ...
  ){

    .addSummaryNameId(this);

    this$.summary$"Length sampling expression"<-deparse(this$dist);
    this$.summary$"Maximum deletion length"<-this$maxLength;
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: ContinousInsertor.R,v 1.2 2009-04-23 16:46:26 sbotond Exp $
##
##	Class: DsiscreteInsertor
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
  "ContinousInsertor",
  function( 
		name="Anonymous",
		rate=NA,
		dist=NA,
		max.length=NA,
		... 
		)	{

		this<-GeneralInsertor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			...
		);
    this<-extend(
      this,
      "ContinousInsertor",
			.dist=NA,
			.max.length=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(dist)) {
			this$dist<-dist;
			STATIC<-FALSE;
		}
		
		if(!missing(max.length)) {
			this$.max.length<-max.length;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(...){
				if(!is.expression(this$.dist)){
					throw("\"dist\" is undefined, so cannot propose insertion length!\n");
				}
				else if(is.na(this$.max.length)){
					throw("\"maxLength\" is undefined, so cannot propose insertion length!\n");
				}
				tmp<-round(eval(this$.dist));
				while( tmp > this$.max.length | tmp < 1){  tmp<-round(eval(this$.dist)) };	
				return(tmp);
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
  class="ContinousInsertor",
  function(
    this,
    length,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$maxLength)) {
          this$maxLength<-this$maxLength;
        }

        if (is.expression(this$dist)) {
          this$dist<-this$dist;
        }
        else if (!is.na(this$dist)){
          throw("Insertion length sampler expression is invalid!\n");
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
## Method: getDist
##	
setMethodS3(
	"getDist", 
	class="ContinousInsertor", 
	function(
		this,
		...
	){

		this$.dist;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSizes
##	
setMethodS3(
	"setDist", 
	class="ContinousInsertor", 
	function(
		this,
		value,
		...
	){
		
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if(!is.expression(value)) {
			throw("The new value must be a valid expression!\n");
		} else {
			# Do a test sampling:
			tmp<-eval(value);
			if( length(tmp) != 1 ) {
				throw("The return value of the length sampler expression must be of length 1!\n");
			}
			if (!is.numeric(tmp)) {
				throw("The return value of the length sampler expression must be numeric!\n");
			}
			else {
				this$.dist<-value;
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
## Method: getProbs
##	
setMethodS3(
	"getMaxLength", 
	class="ContinousInsertor", 
	function(
		this,
		...
	){

		this$.max.length;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProbs
##	
setMethodS3(
	"setMaxLength", 
	class="ContinousInsertor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if (length(value) != 1 ) {
			throw("Value vector size should be 1!\n");
		}
		else if (!is.numeric(value)) {
			throw("Value vector size should be numeric!\n");
		}
		else if( round(value) != value ) {
			throw("maxLength must be integer!\n");
		} else {
			this$.max.length<-value;
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: plot
##
setMethodS3(
  "plot",
  class="ContinousInsertor",
  function(
    this,
		sample.size=NA,
    ...
  ){

		
		if( !is.numeric(this$maxLength) | !is.expression(this$dist) ){
				warning("Insertion length distribution is not defined properly! Nothing to plot here!\n");
				return();
		}
		size<-(this$maxLength * 5);
		if(!missing(sample.size)){
				if(!is.numeric(sample.size) | ( length(sample.size)) !=1 ) {
					throw("Sample size paramter must be a numeric vector of size 1!\n");
				} else {
					size<-round(sample.size);
				}
		}

			sample<-apply(as.array(0:size),1,this$.propose.by);
      plot.default(
				density(sample,from=0,to=this$maxLength),
        main=paste("Estimated insertion size density for:",this$id),
				sub=paste("Sample size:", size),
				type='l',
        xlab="Size",
        ylab="Density",
				col="red",
				lwd=1.5,
				xaxt="n"
      );
			axis(side=1, at=(0:this$maxLength), labels=(0:this$maxLength));

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
setMethodS3(
  "summary",
  class="ContinousInsertor",
  function(
    this,
    value,
    ...
  ){

    .addSummaryNameId(this);

    this$.summary$"Length sampling expression"<-deparse(this$dist);
    this$.summary$"Maximum insertion length"<-this$maxLength;
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: DiscreteDeletor.R,v 1.7 2009-04-27 17:38:09 sbotond Exp $
##
##	Class: DsiscreteDeletor
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
  "DiscreteDeletor",
  function( 
		name="Anonymous",
		rate=NA,
		sizes=NA,
		probs=NA,
		... 
		)	{

		this<-GeneralDeletor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			...
		);
    this<-extend(
      this,
      "DiscreteDeletor",
			.sizes=NA,
			.probs=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(sizes)) {
			this$sizes<-sizes;
			STATIC<-FALSE;
		}
		
		if(!missing(probs)) {
			this$probs<-probs;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(this=NA,...){
			if( !is.numeric(this$.sizes) | !is.numeric(this$.probs) ){
				throw("Cannot propose deletion length because the length distribution is not defined properly!\n");
			}
			if(length(this$.sizes) == 1){
				return(this$.sizes[[1]]);
			} else {
				return(sample(x=this$.sizes,size=1,replace=FALSE,prob=this$.probs));
			}
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
  class="DiscreteDeletor",
  function(
    this,
    length,
    ...
  ){

          wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
				
        if (is.numeric(this$sizes)) {
          this$sizes<-this$sizes;
        } 
				else if (!is.na(this$sizes)){
					throw("Deletion size vector is invalid!\n");
				}

        if (is.numeric(this$probs)) {
          this$probs<-this$probs;
        }
				else if (!is.na(this$probs)){
					throw("Deletion size probability vector is invalid!\n");
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
## Method: getSizes
##	
setMethodS3(
	"getSizes", 
	class="DiscreteDeletor", 
	function(
		this,
		...
	){

		this$.sizes;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSizes
##	
setMethodS3(
	"setSizes", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} else if (!is.numeric(value)) {
			throw("The new value must be numeric vector!\n");
		} else {
			if(length(value) == 0 ) {
				warning("Deletion size vector has zero length!\n");
			}
			this$.sizes<-round(value);	
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProbs
##	
setMethodS3(
	"getProbs", 
	class="DiscreteDeletor", 
	function(
		this,
		...
	){

		this$.probs;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProbs
##	
setMethodS3(
	"setProbs", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if(!is.numeric(this$.sizes)) {
			throw("Cannot set probabilities because indel size vector is not defined!\n");
		}
		else if (!is.numeric(value)) {
			throw("The new value must be a numeric vector!\n");
		}
		else if(length(value) != length(this$.sizes)) {
			throw("The length of the probabilities vector must be the same as the length of the deletion sizes vector");	
		} 
		else if( length(value[value < 0 ]) != 0 ) {
			throw("The elements of the probability vector must not be negative!\n");
		}
		else {
  		if(!isTRUE(all.equal(sum(value),1.0))){
        value<-value/sum(value);
        warning("The provided values were rescaled in order to sum to one!\n");
    	}
			this$.probs<-value;
		}
		
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
setMethodS3(
	"summary", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){

		.addSummaryNameId(this);

		expected.length<-NA;	
		sd<-NA;
		if( is.numeric(this$sizes) & is.numeric(this$probs)) {
		expected.length<-weighted.mean(this$sizes, this$probs);		
		# FIXME - check back on this
		sd<-sqrt(sum( (this$sizes - expected.length)^2 * this$probs ));
		}

		this$.summary$"Expected deletion length"<-expected.length;
		this$.summary$"Standard deviation of deletion length"<-format(sd);
		NextMethod();
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
setMethodS3(
	"plot", 
	class="DiscreteDeletor", 
	function(
		this,
		value,
		...
	){

			if( !is.numeric(this$sizes) | !is.numeric(this$probs) ){
				warning("Deletion length distribution is not defined properly! Nothing to plot here!\n");
				return();
			}
			plot.default(	
				x=this$sizes,
				y=this$probs,
				col=c("blue"),
				lwd=2,
				type="h",
				main=paste("Deletion size distribution for:",this$id), 
				xlab="Size",
				ylab="Probability",
				ylim=c(0,1),
				xaxt="n"
			);
			axis(side=1, at=this$sizes, labels=this$sizes);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: DiscreteInsertor.R,v 1.4 2009-04-27 17:38:09 sbotond Exp $
##
##	Class: DsiscreteInsertor
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
  "DiscreteInsertor",
  function( 
		name="Anonymous",
		rate=NA,
		sizes=NA,
		probs=NA,
		... 
		)	{

		this<-GeneralInsertor(
			 name=NA,
			 rate=rate,
			 propose.by=NA,
    	 accept.by=NA,
			 generate.by=NA,
			 ...
		);
    this<-extend(
      this,
      "DiscreteInsertor",
			.sizes=NA,
			.probs=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		STATIC<-TRUE;
		if(!missing(sizes)) {
			this$sizes<-sizes;
			STATIC<-FALSE;
		}
		
		if(!missing(probs)) {
			this$probs<-probs;
			STATIC<-FALSE;
		}

		this$proposeBy<-function(this=NA,...){
			 if( !is.numeric(this$.sizes) | !is.numeric(this$.probs) ){
        throw("Cannot propose insert length because the length distribution is not defined properly!\n");
      }
			if(length(this$.sizes) == 1){
				return(this$.sizes[[1]]);
			} else {
				return(sample(x=this$.sizes,size=1,replace=FALSE,prob=this$.probs));
			}
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
  class="DiscreteInsertor",
  function(
    this,
    length,
    ...
  ){

          wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (is.numeric(this$sizes)) {
          this$sizes<-this$sizes;
        }
        else if (!is.na(this$sizes)){
          throw("Insertion size vector is invalid!\n");
        }

        if (is.numeric(this$probs)) {
          this$probs<-this$probs;
        }
        else if (!is.na(this$probs)){
          throw("Insertion size probability vector is invalid!\n");
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
## Method: getSizes
##	
setMethodS3(
	"getSizes", 
	class="DiscreteInsertor", 
	function(
		this,
		...
	){

		this$.sizes;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSizes
##	
setMethodS3(
	"setSizes", 
	class="DiscreteInsertor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} else if (!is.numeric(value)) {
			throw("The new value must be numeric vector!\n");
		} else {
			if(length(value) == 0 ) {
				warning("Deletion size vector has zero length!\n");
			}
			this$.sizes<-round(value);	
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProbs
##	
setMethodS3(
	"getProbs", 
	class="DiscreteInsertor", 
	function(
		this,
		...
	){

		this$.probs;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProbs
##	
setMethodS3(
	"setProbs", 
	class="DiscreteInsertor", 
	function(
		this,
		value,
		...
	){
	
		.checkWriteProtection(this);
		if (missing(value)) {
			throw("No new value provided!\n");
		} 
		else if(!is.numeric(this$.sizes)) {
			throw("Cannot set probabilities because insert sizes vector is not defined!\n");
		}
		else if (!is.numeric(value)) {
			throw("The new value must be a numeric vector!\n");
		}
		else if(length(value) != length(this$.sizes)) {
			throw("The length of the probabilities vector must be the same as the length of the insertion sizes vector");	
		} 
		else if( length(value[value < 0 ]) != 0 ) {
			throw("The elements of the probability vector must not be negative!\n");
		}
		else {
			if(!isTRUE(all.equal(sum(value),1.0))){
				value<-(value/sum(value));	
				warning("The provided values were rescaled in order to sum to one!\n");
		}
			this$.probs<-value;
		}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
setMethodS3(
	"plot", 
	class="DiscreteInsertor", 
	function(
		this,
		value,
		...
	){

			if( !is.numeric(this$sizes) | !is.numeric(this$probs) ){
				warning("Insertion length distribution is not defined properly! Nothing to plot here!\n");
				return();
			}
			plot.default(	
				x=this$sizes,
				y=this$probs,
				col=c("red"),
				lwd=2,
				type="h",
				main=paste("Insertion size distribution for:",this$id), 
				xlab="Size",
				ylab="Probability",
				ylim=c(0,1),
				xaxt="n"
			);
			axis(side=1, at=this$sizes, labels=this$sizes);
		
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
setMethodS3(
  "summary",
  class="DiscreteInsertor",
  function(
    this,
    value,
    ...
  ){

    .addSummaryNameId(this);

    expected.length<-NA;
    sd<-NA;
    if( is.numeric(this$sizes) & is.numeric(this$probs)) {
    expected.length<-weighted.mean(this$sizes, this$probs);
    # FIXME - check back on this
    sd<-sqrt(sum( (this$sizes - expected.length)^2 * this$probs ));
    }

    this$.summary$"Expected insertion length"<-expected.length;
    this$.summary$"Standard deviation of insertion length"<-format(sd);
    NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: Event.R,v 1.25 2009-04-29 13:28:25 sbotond Exp $
##
##	Class: Event
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
	"Event",
	function( 
		name=NA,
		rate=NA,
		site=NA,
		position=NA,
		process=NA,
		handler=NA,
		target.state=NA,
		... 
		){

		this<-extend(
			PSRoot(),
			"Event",
			.name="Anonymous",
			.rate=NA,
			.process=NA,
			.handler=NA,
			.site=NA,
			.position=NA,
			.target.state=NA,
			.write.protected=FALSE,
			.is.event=TRUE
		);

		STATIC<-TRUE;
						
		if(!missing(name)) {
			this$name<-name;
			STATIC<-FALSE;
		}
		
		if(!missing(rate)) {
			this$rate<-rate;
			STATIC<-FALSE;
		}	

		if(!missing(process)) {
			this$process<-process;
			STATIC<-FALSE;
		}	
		
		if(!missing(site)) {
			this$site<-site;
			STATIC<-FALSE;
		}	
		
		if(!missing(position)) {
			this$position<-position;
			STATIC<-FALSE;
		}	
		
		if (!missing(target.state) & missing(site)) {
			throw("Target state is specified, but no site object given!\n");
		}
    else if (!missing(target.state)) {
			this$targetState<-target.state;
		}
		
		# The site object was passed through a getField method,
		# which disabled the virtual fields, so we have to enable them:
		if (!is.na(this$.site)){
				this$.site<-enableVirtual(this$.site);
		}
		this;
	},
	enforceRCC=TRUE
);

##
## Method: is.Event
##
setMethodS3(
  "is.Event",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.event)){return(TRUE)}
    if ( inherits(this, "Event")) {
      this$.is.event<-TRUE;
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
## Method: getName
##	
setMethodS3(
	"getName", 
	class="Event", 
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
	class="Event", 
	function(
		this,
		new.name,
		...
	){
		.checkWriteProtection(this);	
		if(missing(new.name)){throw("No new name provided!\n")}
		new.name<-as.character(new.name);	
		if (new.name == "") {throw("Cannot set empty name!\n")}
		else { this$.name<-new.name}
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRate
##	
setMethodS3(
	"getRate", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.rate;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){
	
		.checkWriteProtection(this);	
		if(missing(new.rate)){throw("No new rate provided!\n")}
		else if (!is.numeric(new.rate)){throw("The rate must be numeric!\n")}
		else { this$.rate<-new.rate }
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcess
##	
setMethodS3(
	"getProcess", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.process;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProcess
##	
setMethodS3(
	"setProcess", 
	class="Event", 
	function(
		this,
		new.proc,
		...
	){
		
		.checkWriteProtection(this);	
		if(missing(new.proc)){throw("No new rate provided!\n")}
		else if (!is.Process(new.proc)){throw("Process object invalid!\n")}
		else { this$.process<-new.proc}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getHandler
##	
setMethodS3(
	"getHandler", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.handler;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setHandler
##	
setMethodS3(
	"setHandler", 
	class="Event", 
	function(
		this,
		new.rate,
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
## Method: getSite
##	
setMethodS3(
	"getSite", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.site;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getPosition
##	
setMethodS3(
	"getPosition", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.position;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setPosition
##	
setMethodS3(
	"setPosition", 
	class="Event", 
	function(
		this,
		value,
		...
	){
	
		if(value > this$.site$.sequence$.length | value < 1) {
			throw("Invalid position!\n");
		}
		this$.position<-value;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTargetState
##	
setMethodS3(
	"getTargetState", 
	class="Event", 
	function(
		this,
		...
	){
		
		this$.target.state;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTargetState
##	
setMethodS3(
	"setTargetState", 
	class="Event", 
	function(
		this,
		new.state,
		...
	){
		
		.checkWriteProtection(this);	
		if (is.na(this$.site)) {
			throw("The event has no target site, so target alphabet is unknown. Refusing to set targetState!\n");
		}	
		else if (!is.Site(this$.site)) {
			throw("Target site is invalid!\n");
		}	
		else if (is.na(new.state)) {
			return(this$.target.state<-new.state);
		}
		else if (!hasSymbols(this$.site$.alphabet,new.state)) { 
			throw("The target state must be in the target site alphabet!\n");	
		}
		else {
			if(this$.site$state != new.state) {
					warning("The proposed new target state is not equal with the current state of the associated site. This is strange, but proceeding anyway.\n");
			}
			this$.target.state<-new.state;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSite
##	
setMethodS3(
	"setSite", 
	class="Event", 
	function(
		this,
		new.site,
		...
	){
			
		.checkWriteProtection(this);	
		if (!is.Site(new.site)) {throw("Site object invalid!\n")}
		new.site<-enableVirtual(new.site);
		if(missing(new.site)) {throw("No site given")}

		else if (!is.na(this$process)){
				if (this$.process$.alphabet != new.site$.alphabet){
					throw("The site and process alphabets are incompatible!\n");
			}
		}
		
		this$.site<-new.site;
		invisible(this$.site)
		
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setHandler
##	
setMethodS3(
	"Perform", 
	class="Event", 
	function(
		this,
		...
	){
	
		if(!is.function(this$.handler)){throw("Event handler is not a function!\n")}		
		else if (!is.Site(this$.site)){throw("The site associated with the event is not valid!\n")}
		else if(is.null(this$.position)){throw("The target site position is unknown!Refusing to perform event!\n")}
		else if (!is.Process(this$.process)) {
			throw("The event has no generator process. Refusing to perform!\n");
		}
		else if (is.na(this$.rate)) {
			throw("The event has no rate. Refusing to perform!\n");
		}
		else if (!is.na(this$.target.state) & (this$.target.state != this$site$state) ) {
			warning(paste("The event acts on state ",this$.target.state," but the target site has the state ",this$site$state,"! Doing nothing!\n",sep=""));
			return(invisible(FALSE));
		}
		else {

			# Better not perform anything on a dirty object!
			# FIXME -  the checking of this flag can be redundant!
			if(this$.site$.sequence$.cumulative.rate.flag) {
				.recalculateCumulativeRates(this$.site$.sequence);
			}
			# Do NOT flag cumulative rate! The vent should take care of that!

			# Flag site if we deal with substitutions:
			if( is.GeneralSubstitution( getProcess(this) ) ) {
				this$.site$.sequence$.flagged.sites<-c(this$.site$.sequence$.flagged.sites, this$.position);
			}

			# Call the event handler to perform event:
			this$.handler(this);

			# Event will self-destruct to prevent trouble:
			.setHandler(this, function(event=this) { throw("You can perform an event only once!\n") } );

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
## Method: .setHandler
##	
setMethodS3(
	".setHandler", 
	class="Event", 
	function(
		this,
		new.handler,
		...
	){
		
		if(missing(new.handler)){throw("No new handler provided!\n")}
		else if (!is.function(new.handler)){throw("The handler must be a function!\n")}
		else { this$.handler<-new.handler}

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
  class="Event",
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
  class="Event",
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
  private=FALSE,
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
  class="Event",
  function(
    this,
    value,
    ...
  ){

    if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

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
setMethodS3(
  "checkConsistency",
  class="Event",
  function(
    this,
    ...
  ){
		
		wp<-this$writeProtected;
		if(wp) {
			this$writeProtected<-FALSE;
		}

		may.fail<-function(this) {

			if (is.null(this$.name)){
				throw("Event name is NULL!\n");
			}
			else if (!is.na(this$.name))	{
				this$name<-this$name;
			}

			if (is.null(this$.rate)){
				throw("Event rate is NULL!\n");
			}
			else if (!is.na(this$.rate))	{
				this$rate<-this$rate;
			}


			if (is.null(this$.process)){
				throw("Event rate is NULL!\n");
			}
			else if (!is.na(this$.process))	{
				this$process<-this$process;
			}

			if (is.null(this$.site)){
				throw("Event site is NULL!\n");
			}
			else if (!is.na(this$.site))	{
				this$site<-this$site;
			}


			if (is.null(this$.target.state)){
				throw("Event target state is NULL!\n");
			}
			else if (!is.na(this$.target.state))	{
				this$targetState<-this$targetState;
			}
		}
		tryCatch(may.fail(this), finally=this$writeProtected<-wp);

		return(invisible(TRUE))	

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: as.character.Event
##	
setMethodS3(
	"as.character", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){

		procid<-NA;
		if(!is.na(this$.process)){
			procid<-this$.process$id;
		}	
		paste(this$.name," (",this$.rate,")"," <-- ",procid,sep="");	
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Event
##	
setMethodS3(
	"summary", 
	class="Event", 
	function(
		this,
		new.rate,
		...
	){
			
		this$.summary$"Name"<-this$.name;	
		this$.summary$"Rate"<-this$.rate;	

		procid<-NA;
		if(!is.na(this$.process)){
			procid<-this$.process$id;
		}	
		this$.summary$"Generator process"<-procid;
		
		site.state<-NA;
		if(!is.na(this$.site)){
			site.state<-getState(this$.site);
		}	
		
		this$.summary$"Target state"<-this$.target.state;
		this$.summary$"Target site state"<-site.state;
		if(this$writeProtected) {
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



##	$Id: ContinousDeletor.R,v 1.4 2009-04-24 10:12:27 sbotond Exp $
##
##	Class: FieldDeletor
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
  "FieldDeletor",
  function( 
		name="Anonymous",
		type="exponential",
		... 
		)	{

		this<-GeneralDeletor(
			...
		);
    this<-extend(
      this,
      "FieldDeletor",
			.type=type,
			.q.max=NA
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		if(type == "exponential"){

			this$proposeBy<-function(seq,pos){

					# Find out the maximum deletion tolerance parameter:					
					# FIXME - this is very slow!
					q<-c();

					for(site in seq$.sites){
							if(isAttached(site, this)){
									tmp<-getParameterAtSite(this, site, id="deletion.tolerance")$value;
									tmp<-exp(-tmp);
									q<-c(q,tmp);
							}
					} # for site
					q.max<-max(q);
					
					express<-expression(rgeom(1,(1-q.max))+1);
					tmp<-round(eval(express));
					this$.q.max<-q.max;
					cat("Proposed",tmp,"\n");
					return(tmp);

			} # if exponetial
		
			this$acceptBy<-function(sequence,range){

				q.max<-this$.q.max;
				this$.q.max<-NA;

				q<-c();
				# FIXME - missing process conceptual issue!
				for(site in seq$.sites[range]){
							if(isAttached(site, this)){
									tmp<-getParameterAtSite(this, site, id="deletion.tolerance")$value;
									q<-c(q,tmp);
							}
					} # for site
				
				K<-length(q);
				q<-prod(exp(-q));

				accept.prob<-(q/(q.max^K));
        # Accept/reject:
        tmp<-( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
				return(tmp);
	
			}
		
		
		} # /exponential


    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
setMethodS3(
  "checkConsistency",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

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
## Method: proposeLength
##	
setMethodS3(
  "proposeLength",
  class="FieldDeletor",
  function(
    this,
    ...
  ){

			throw("Disabled for FieldDeletion processes!\n");

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
  class="FieldDeletor",
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
  class="FieldDeletor",
  function(
    this,
    ...
  ){

		throw("The type of the FieldDeletor process cannot be modified. Please set it by the constructor argument.");

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);




##	$Id: GeneralIndel.R,v 1.9 2009-05-01 08:48:21 sbotond Exp $
##
##	Class: GenarlIn and GeneralDel
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
  "GeneralInDel",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		any.alphabet<-Alphabet(type="*ANY*");
		any.alphabet$.any.flag<-TRUE;
		this<-Process(
			alphabet=any.alphabet
		);
    this<-extend(
      this,
      "GeneralInDel",
			.rate=rate,
			.propose.by=NA,
			.accept.by=NA,
			.is.general.indel=TRUE
    );

		# Using virtual field to clear Id cache:
		this$name<-name;

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
setMethodS3(
	"checkConsistency", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$rate)) {
          this$rate<-this$rate;
        }

				if(!is.function(this$proposeBy)){
					if(!is.na(this$proposeBy)){
						throw("proposeBy is invalid!\n");
					}
				}
				
				if(!is.function(this$acceptBy)){
					if(!is.na(this$acceptBy)){
						throw("acceptBy is invalid!\n");
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
## Method: getRate
##	
setMethodS3(
	"getRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.rate;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasUndefinedRate
##	
setMethodS3(
	"hasUndefinedRate", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		return(is.na(this$.rate));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
			throw("No new value provided!\n");}
		else if(!is.numeric(value)) {
			throw("Rate must be numeric!\n");
		} else {
			this$.rate<-value;
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProposeBy
##	
setMethodS3(
	"getProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.propose.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRate
##	
setMethodS3(
	"setProposeBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of proposeBy must be a function.!\n");	
		} else {
			this$.propose.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptBy
##	
setMethodS3(
	"getAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		...
	){

		this$.accept.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptBy
##	
setMethodS3(
	"setAcceptBy", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of acceptBy must be a function.!\n");	
		} else {
			this$.accept.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: proposeLength
##	
setMethodS3(
	"proposeLength", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){

		return( this$.propose.by());

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: is.GeneralIndel
##	
setMethodS3(
	"is.GeneralInDel", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.indel)){return(TRUE)}
    if ( inherits(this, "GeneralInDel")) {
      this$.is.general.indel<-TRUE;
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
## Method: summary
##	
setMethodS3(
	"summary", 
	class="GeneralInDel", 
	function(
		this,
		value,
		...
	){
	
		.addSummaryNameId(this);
		this$.summary$"General rate"<-this$rate;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

###########################################################################
# Class:GeneralInsertor
setConstructorS3(
  "GeneralInsertor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		template.seq=NA,
		insert.hook=NA,
		accept.win=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralInsertor",
			.generate.by=NA,
			.handler.template=NA,
			.template.seq=NA,
			.insert.hook=NA,
			.accept.win=1,
			.is.general.insertor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="insertion.tolerance",
      name="Insertion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );
	
		if(!missing(template.seq)){
			this$templateSeq<-template.seq;
		}

		this$acceptBy<-function(sequence,range){
			
				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Discard the site if the process is not attached to it:
						if(!isAttached(site, this)){
							next();
						}
						else {
							accept.prob<-c(accept.prob, getParameterAtSite(this, site, "insertion.tolerance")$value);
						}
				}
				accept.prob<-prod(as.numeric(accept.prob));


			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}

	###	

	 this$generateBy<-function(this,length=NA){

			if(is.na(length) | (length(length) == 0) | length == 0){
				throw("Invalid insert length!\n");
			}	
			else if(is.na(this$.template.seq)){
				throw("Cannot generate insert without template sequence!\n");
			}

			times<-( ceiling( length/this$.template.seq$.length) );
			to.delete<-( ( (this$.template.seq$.length) * times) - length);

			tmp<-clone(this$.template.seq);
		
			if( (times-1) > 0){
				for(i in 1:(times-1)){
					insertSequence(tmp,this$.template.seq,tmp$length);
				}
			}

			if(to.delete > 0){
				deleteSubSequence(tmp,(tmp$length - to.delete + 1):tmp$length);
			}
			
			return(tmp);
				
	 }

	if(!missing(insert.hook)){
		this$insertHook<-insert.hook;
	}

	###	
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 WINDOW.SIZE<-this$.accept.win;
					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 sequence<-getSequence(getSite(event));

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Set insertion tolerance window:
					 # FIXME - more general handling
					 window<-integer();
					 insert.pos<-position;
					 if(direction == "LEFT") {
							window<-(position-WINDOW.SIZE):position;
					 		insert.pos<-(position-1);
					 }
					 else if (direction == "RIGHT"){
							window<-position:(position+WINDOW.SIZE);
					 }
					 else {
						throw("You should never see this message!\n");
					}

					# Discard illegal positions:
					window<-window[ window > 0 & window <= sequence$.length];
				  if(this$.accept.by(sequence,window)){
							insert<-generateInsert(this);

							# Call the insert hook:
							if(is.function(this$.insert.hook)){
								insert<-this$.insert.hook(insert);
							}
							insertSequence(sequence,insert, insert.pos,process=this);
					}
					
				}
		 }
		###

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralInsertor
##	
setMethodS3(
	"is.GeneralInsertor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.insertor)){return(TRUE)}
    if ( inherits(this, "GeneralInsertor")) {
      this$.is.general.insertor<-TRUE;
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
## Method: checkConsistency
##	
setMethodS3(
	"checkConsistency", 
	class="GeneralInsertor", 
	function(
		this,
		length,
		...
	){

		      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {

        if (!is.na(this$templateSeq)) {
          this$templateSeq<-this$templateSeq;
        }

        if(!is.function(this$generateBy)){
          if(!is.na(this$generateBy)){
            throw("generateBy is invalid!\n");
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
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="GeneralInsertor", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		if(missing(target.site)) {
			throw("No target site provided!\n");
		} else if (!sloppy) {
			if(!is.Site(target.site)) {
				throw("Target site invalid!\n");
			}
			else if(!is.function(this$.propose.by)) {
				throw("proposeBy is not set, cannot propose insertion!\n");
			} 
			else if (!is.function(this$.accept.by)){
				throw("acceptBy is not set, cannot generate insertion event!\n");
			}
		} #/!sloppy

		# Check if process is attached?

		# Just return an empty list if the rate is undefined or zero:
		if( is.na(this$.rate) | this$.rate == 0) {
			return(list());
		}

		 # Clone the event template object:
		 insertion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 insertion.event$.position<-target.site$.position;
		 # Set the target site:
		 insertion.event$site<-target.site;
		 # Set the target state (good for consistency):
		 insertion.event$targetState<-getState(target.site);
		 # Set event name:
		 insertion.event$name<-"Insertion";
		 # Set the generator process:
		 insertion.event$process<-this;
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
		 rate.multiplier<-getParameterAtSite(this,target.site,"rate.multiplier")$value;
		 if(rate.multiplier == 0 ) {
			return(list());
		 }
		 insertion.event$rate<-(this$rate * rate.multiplier );

		 # Set the handler for the deletion event:
		 .setHandler(insertion.event, this$.handler.template);

		# Write protect the event object:	
		insertion.event$writeProtected<-TRUE;	

		# Return the event object in a list:
		list(insertion.event);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: generateInsert
##	
setMethodS3(
	"generateInsert", 
	class="GeneralInsertor", 
	function(
		this,
		length=NA,
		...
	){

		if(missing(length)){
			length<-this$.propose.by(this=this);
		}
		insert<-this$.generate.by(this,length);
		sampleStates(insert);	
		return(insert);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getGenerateBy
##	
setMethodS3(
	"getGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.generate.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setGenerateBy
##	
setMethodS3(
	"setGenerateBy", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The value of generateBy must be a function.!\n");	
		} else {
			this$.generate.by<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTemplateSeq
##	
setMethodS3(
	"getTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.template.seq;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setGenerateBy
##	
setMethodS3(
	"setTemplateSeq", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)) {
				throw("No new template sequence provided!\n");	
		}
		else if(!is.Sequence(value)){
			 throw("Sequence object is invalid!\n");	
		}
		else if(value$length == 0) {
			throw("Cannot set template sequence of length zero!\n");
		}
		else {
			this$.template.seq<-clone(value);
			for (site in this$.template.seq$.sites){
				site$.ancestral<-this;
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
## Method: getGenerateBy
##	
setMethodS3(
	"getAcceptBy", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.accept.by;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAcceptWin
##	
setMethodS3(
	"getAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.accept.win;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAcceptWin
##	
setMethodS3(
	"setAcceptWin", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		if(missing(value)){
			throw("No new value provided");
		}
		else if(!all(is.numeric(value)) | (length(value) != 1)){
			throw("The new value must be a numeric vector of length one.");
		}
		else{
			this$.accept.win<-value;
		}
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
setMethodS3(
	"summary", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
		this$.summary$"Accept window size"<-this$.accept.win;
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
###########################################################################
# Class:GeneralDeletor
setConstructorS3(
  "GeneralDeletor",
  function( 
		name="Anonymous", 
		rate=NA,
		propose.by=NA,
		accept.by=NA,
		... 
		)	{

		this<-GeneralInDel(
			rate=rate,
			propose.by=propose.by,
			accept.by=accept.by
		);

    this<-extend(
      this,
      "GeneralDeletor",
			.handler.template=NA,
			.is.general.deletor=TRUE
    );
		# Using virtual field to clear Id cache:
		this$name<-name;

		# Adding insertion tolerance parameter.
    .addSiteSpecificParameter(
      this,
      id="deletion.tolerance",
      name="Deletion tolerance parameter",
      value=as.double(1), # Accept all by default
      type="numeric"
    );

		this$acceptBy<-function(sequence,range){

				accept.prob<-c();
				for(site in sequence$.sites[range]){
						# Reject if the range contains a site which is not attached to 
						# the process:
						if(!isAttached(site, this)){
							return(FALSE);
						}
						accept.prob<-c(accept.prob, getParameterAtSite(this, site, "deletion.tolerance")$value);
				}

				# Calculate the product of the per-site 
				# acceptance probabilities.
				accept.prob<-prod(as.numeric(accept.prob));

			  # Accept/reject:
				return( sample(c(TRUE,FALSE),replace=FALSE,prob=c(accept.prob,(1-accept.prob)),size=1) );
		}
		
	 this$.handler.template<-function(event=NA) {

				if(!is.na(event)){

					 # Using temporary varibales for clarity:
					 position<-event$.position;
					 sequence<-getSequence(getSite(event));

					 # Propose a sequence length:
					 length<-this$proposeBy(this=this,seq=sequence, pos=position);

					 # Propose the direction:
					 direction<-sample(c("LEFT","RIGHT"),replace=FALSE,size=1);

					 # Calculate the sites to delete:
					 range<-numeric();	
					 if(direction == "RIGHT") {
							range<-position:(position+length-1);
					 } else if(direction == "LEFT") {
						  range<-(position-length+1):position;
					 } else {
							throw("You should never see this message!\n");
					 }

					 # Discard potential negative values and values larger than the sequence length:
					 range<-range[ range > 0 & range <= sequence$.length];
					 
					 # Perform the deletion if it is accepted:
					 if (this$.accept.by(sequence=sequence,range=range) == TRUE) {
					 	deleteSubSequence(sequence,range);
					}
				}
		 }

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: is.GeneralDeletor
##	
setMethodS3(
	"is.GeneralDeletor", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.deletor)){return(TRUE)}
    if ( inherits(this, "GeneralDeletor")) {
      this$.is.general.deletor<-TRUE;
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
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="GeneralDeletor",
  function(
    this,
    length,
    ...
  ){
      NextMethod();

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="GeneralDeletor", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		if(missing(target.site)) {
			throw("No target site provided!\n");
		} else if (!sloppy) {
			if(!is.Site(target.site)) {
				throw("Target site invalid!\n");
			}
			else if(!is.function(this$.propose.by)) {
				throw("proposeBy is not set, cannot propose deletion!\n");
			} 
			else if (!is.function(this$.accept.by)){
				throw("acceptBy is not set, cannot generate deletion event deletion!\n");
			}
		} #/!sloppy

		 # Complain if sequence has a zero length:
		 if(target.site$.sequence$.length == 0) {
			 throw("Sequence has zero length so there is nothing to delete! How did you get here anyway?\n");
		 }

		 # Clone the event template object:
		 deletion.event<-clone(this$.event.template);
		 # Set the target position passed in a temporary field:
		 deletion.event$.position<-target.site$.position;
		 # Set the target site:
		 deletion.event$site<-target.site;
		 # Set the target state (good for consistency):
		 deletion.event$targetState<-getState(target.site);
		 # Set event name:
		 deletion.event$name<-"Deletion";
		 # Set the genrator process:
		 deletion.event$process<-this;
		
		
		 # Event rate is the product of the general rate and the 
		 # site specific rate multiplier:
		 deletion.event$rate<-(this$rate * (getParameterAtSite(this,target.site,"rate.multiplier")$value) );

		 # Set the handler for the deletion event:
		 .setHandler(deletion.event, this$.handler.template);

		# Write protect the event object:	
		deletion.event$writeProtected<-TRUE;	

		# Return the event object in a list:
		list(deletion.event);

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
setMethodS3(
	"summary", 
	class="GeneralDeletor", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertHook
##	
setMethodS3(
	"getInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		...
	){

		this$.insert.hook;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertHook
##	
setMethodS3(
	"setInsertHook", 
	class="GeneralInsertor", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.Sequence(this$.template.seq)){
			throw("Cannot set insert hook because the template sequence is not defined!\n");
		}
		if(missing(value)) {
				throw("No new value provided!\n");	
		}
		else if(!is.function(value)){
			 throw("The insert hook must be a function.!\n");	
		}
		else if( length(intersect(names(formals(value)), "seq")) == 0 ){
      throw("The insert hook function must have a an argument named \"seq\"");
		}
		else if(!is.Sequence(value(generateInsert(this,length=1)))){
			throw("The insert hook function must return a Sequence object!\n");	
		} else {
			this$.insert.hook<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);




##	$Id: GeneralSubstitution.R,v 1.24 2009-05-01 16:06:13 sbotond Exp $
##
##	Class: GenarlIn and GeneralDel
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
  "GeneralSubstitution",
  function( 
		name="Anonymous", 
		alphabet=NA,
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

	
		# Set an empty alphabet by default
		# to satisfy the static instance:
		if(missing(alphabet)){
			alphabet<-Alphabet(name="Undefined");
		}

		this<-Process(
			name=name,
			alphabet=alphabet
		);
    this<-extend(
      this,
      "GeneralSubstitution",
			.q.matrix=NA,
			.equ.dist=NA,
			.handler.template=NA,
			.is.general.substitution=TRUE
    );

		# Initialize with NA-s equDist:
		if (missing(equ.dist)){
			.initEquDist(this);
		} else {
			# or set if we have one:
			this$equDist<-equ.dist;
		}

		# Create the QMatrix object:
		qm<-QMatrix(name=name, alphabet=alphabet);
	
		# Set the rates:	
		if(!missing(rate.list)){
			qm$rateList<-rate.list;
		}
		
		# Attach the QMatrix to the process:
		this$.q.matrix<-qm;
		this$.q.matrix$process<-this;

		# Try to guess the equlibrium distribution:
		if (missing(equ.dist) & !missing(rate.list)){
			if(.setEquDistFromGuess(this)){
				# and perfrom rescaling if suceeded:
				rescaleQMatrix(this);
			}
		}

		# Using virtual field to clear Id cache:
		this$name<-name;

		# Set the template for handling substitution events:
		this$.handler.template<-function(event=NA){
				# Just set the new state base on the event name:
				setState(getSite(event), strsplit(event$name,split="->")[[1]][[2]]);
				# The name *should* be valid and correct, so no more checking is needed.
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
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }

      may.fail<-function(this) {
			
			# The process must have a valid alphabet object:	
			if(!is.Alphabet(this$.alphabet)){
				throw("Alphabet object is invalid!\n");
			}
		
			# Name:
			if(!is.na(this$name)){
				this$name<-this$name;
			}
		
			# EquDist:
			if(!any(is.na(this$.equ.dist))){
				this$equDist<-this$equDist;
			}
			
			# QMatrix should never be NA!
			this$QMatrix<-this$QMatrix;
		
			# Further checks if survived the one above:
			checkConsistency(this$.q.matrix,check.process=FALSE);

			if(is.Process(this$.q.matrix$.process)){
              # Check for alphabet compatibility:
              if(this$.alphabet != this$.q.matrix$.process$alphabet){
                throw("Process/QMatrix alphabet mismatch!\n");
              }
              # Check if the parent process QMatrix is this object:
              if(!equals(this$.q.matrix$.process, this) ){
                throw("QMatrix process is not identical with self!\n");
              }
			} else if(!is.na(this$.q.matrix$.process)){
					throw("QMatrix process entry is invalid!\n");
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
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="GeneralSubstitution", 
	function(
		this,
		target.site,
		sloppy=FALSE,
		...
	){

		# The main method of this class,
		# generating a list of event objects given the 
		# state of the target site.

	 if(missing(target.site)) {
      throw("No target site provided!\n");
    } else if (!sloppy) {
			# Additional checks. They can be
			# disabled by sloppy=TRUE			

      if(!is.Site(target.site)) {
        throw("Target site invalid!\n");
      }
	 		else if(!is.QMatrix(this$.q.matrix)){
				throw("Cannot provide event objects because the rate matrix is not set!\n");	
			}
			else if(!is.numeric(this$.equ.dist)){
				throw("Cannot provide event objects because the equilibrium frequencies are not defined!\n");	
			} 

		} 

			state<-getState(target.site);
		# Just return an empty list if the state is NA:
			if(is.na(state)){
				return(list());
			}

			symbols<-this$alphabet$symbols;
			rest<-symbols[ which(symbols != state) ];
			# Generate the names of the possible events:
			event.names<-paste(state,rest,sep="->");

			# Create the event objects:
			events<-list();
			for(name in event.names){

		 		# Clone the event template object:
     		event<-clone(this$.event.template);
     		# Set event name:
     		event$name<-name;
     		# Set the generator process:
     		event$process<-this;
     		# Set the target position passed in a temporary field,
				# Event objects are not aware of their posiitions in general!
     		event$.position<-target.site$.position;
     		# Set the target site:
     		event$site<-target.site;
     		# Set the target state object (good for consistency):
     		event$targetState<-state;

			  # The rate of the event is the product of the general rate and the
     		# site specific rate multiplier:
     		rate.multiplier<-getParameterAtSite(this,target.site,"rate.multiplier")$value;
				
				# Return empty list if the rate multiplier is zero.
     		if(rate.multiplier == 0 ) {
      		return(list());
     		}	
			
				# Set the event rate:	
				event$rate<-(rate.multiplier * getEventRate(this$.q.matrix, name ));	
				# Set the handler for the substitution event:
     		.setHandler(event, this$.handler.template);
   			 # Write protect the event object:
    		event$writeProtected<-TRUE;
				# Add to events list:	
				events<-c(events, list(event));

			}

			return(events);

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
  class="GeneralSubstitution",
  function(
    this,
    value,
		force=FALSE,
		silent=FALSE,
    ...
  ){

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
    else if(!PSRoot$all.equal(sum(value), 1.0)) {
				value<-(value/sum(value));
				if (silent == FALSE){
					warning("The provided probabilities were rescaled in order to sum to one!\n");
				}
    }

		# Check if the provided equlibrium distribution is
		# compatible with the rate matrix:
		 if( !.checkEquMatCompat(this, rbind(value)) & force==FALSE){
				throw("The provided equlibrium distribution: ",paste(value,collapse=" ")," is not compatible with the rate matrix! Use force=TRUE to set it anyway!\n");
			}
			
			# Set the value:
      this$.equ.dist<-rbind(value);
			# Set dimnames:
      colnames(this$.equ.dist)<-(this$alphabet$symbols);
      rownames(this$.equ.dist)<-c("Prob:");
			return(invisible(this));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: .setEquDistFromGuess
##
setMethodS3(
  ".setEquDistFromGuess",
  class="GeneralSubstitution",
  function(
    this,
    ...
  ){
			
			# Try to guess equlibrium distribution:
			tmp<-.guessEquDist(this);
			# Take care with the condition here!
			# We can get in trouble with any()
			# if the first value is zero!
			if( length(tmp) == 1 & all(tmp == FALSE) ){
				warning("The equlibrium distribution of the substitution process could not be determined based on the rate matrix!\n You have to set yourself the proper distribution in order to use the process!");
				return(FALSE);
			}
			else {
				this$equDist<-tmp;
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
## Method: .checkEquMatCompat
##
setMethodS3(
  ".checkEquMatCompat",
  class="GeneralSubstitution",
  function(
    this,
    value,
    ...
  ){

    if(missing(value)) {
      throw("No equlibrium distribution provided!\n")
		}
		else if ( length(value) != dim(this$.q.matrix$.orig.matrix)[[2]] ){
				throw("Value vector length should be",dim(this$.q.matrix$.orig.matrix)[[2]],"!\n");	
		}
		else {
			# The following matrix product of the equlibrium distribution
			# and the rate matrix should give the zero vector:
			tmp<-(rbind(value) %*% as.matrix(this$.q.matrix$.orig.matrix));
			if(PSRoot$all.equal(tmp, rep(0.0, times=length(tmp))) ){
				return(invisible(TRUE));
			} else {
				return(FALSE);
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
## Method: .guessEquDist
##	
setMethodS3(
	".guessEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot guess equilibrium distribution because the Q matrix is not set!\n");
		}
		
		# Refuse to guess if the rate matrix has zero entries:
		if(length(which(this$.q.matrix$.orig.matrix == 0)) != 0 ){
			warning("Rate matrix has zero entries!\n");
			return(FALSE);
		}

		# Get the left eigenvalues and eigenvectors of the rate matrix:
		eigen<-eigen(t(this$.q.matrix$.orig.matrix));
		dist<-numeric(0);

		if( length(intersect(is.complex(eigen$values),TRUE)) == 0 ) {
			# if all  eigenvalues are real:
			# Choose the largest eigenvalue (which should be zero):
			index<-which( eigen$values == max(eigen$values));
			# Choose the correspondign eigenvector:
			dist<-rbind(eigen$vectors[ ,index]);
		}
		else {
			# If we have complex eigenvalues:
			# Choose the eigenvalue (l) with maximum |e^(l)|  
			tmp<-abs(exp(eigen$values));
			index<-which(tmp == max(tmp));
			# ... and the corresponding eigenvector:
			dist<-as.real(eigen$vectors[,index]);
		}	

		# Normalize the eigenvector:
		return(dist/sum(dist));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
				

##	
## Method: .initEquDist
##	
setMethodS3(
	".initEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		dummy=NA, # to satisfy method classification
		...
	){

		if(!isEmpty(this$.alphabet)){
			# Fill in with NA-s
			this$.equ.dist<-rbind(rep(NA,times=this$.alphabet$size));
			# Set the dimnames:
			colnames(this$.equ.dist)<-this$.alphabet$symbols;
			rownames(this$.equ.dist)<-c("Prob:");
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEquDist
##	
setMethodS3(
	"getEquDist", 
	class="GeneralSubstitution", 
	function(
		this,
		dummy=NA, # to satisfy method classification
		...
	){

		this$.equ.dist;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: sampleState;
##	
setMethodS3(
	"sampleState", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		if(any(is.na(this$.equ.dist))){
			throw("Cannot sample state because the equlibrium distribution is not defined!\n");
		}
		else if (!is.Alphabet(this$.alphabet)){
			throw("Cannot sample state because the alphabet is not valid! That is strange as equlibrium distribution is defined!\n");	
		}
		else {
				if(this$.alphabet$size == 0){
					throw("The process alphabet is empty, nothing to sample here!\n");
				}
				if(this$.alphabet$size == 1){
					# Special case: single letter in the alphabet:
					return(this$.alphabet$symbols[[1]]);
				}
				else {
					# Sample from the equlibrium distribution:
					sample(x=this$.alphabet$.symbols, size=1, replace=FALSE, prob=this$.equ.dist);
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
## Method: getQMatrix
##	
setMethodS3(
	"getQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		this$.q.matrix;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setQMatrix
##	
setMethodS3(
	"setQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if(!is.QMatrix(value)){
			throw("The provided object is not a QMatrix!\n");
		}
		else if (!is.Alphabet(getAlphabet(this))){
			throw("Cannot set QMatrix because process alphabet is not defined!\n");
		}
		else if(!is.Alphabet(value$alphabet)){
			throw("Cannot set QMatrix because the alphabet of the provided QMatrix object is not set!\n");	
		}
		else if(getAlphabet(this) != value$alphabet){
			throw("Alphabet mismatch! Cannot set QMatrix!\n");	
		}
		else {
			this$.q.matrix<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
setMethodS3(
	"setAlphabet", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if (!is.Alphabet(value)){
			throw("Alphabet object is invalid!\n");
		} else {
			# Set the QMatrix alphabet
			if(is.QMatrix(this$.q.matrix)){
				setAlphabet(this$.q.matrix, value);
			}
			this$.alphabet<-value;
		}	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
setMethodS3(
	"getAlphabet", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		# Just to satisfy method classification:
		this$.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: hasUndefinedRate
##	
setMethodS3(
	"hasUndefinedRate", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		if( any(is.na(this$.q.matrix$.orig.matrix)) | any(is.na(this$.q.matrix$.rate.matrix))){
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


##	
## Method: getEventRate
##	
setMethodS3(
	"getEventRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){

		# For getting the scaled event rate:
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
		}
		else if(!missing(name) & missing(from) & missing(to)){
			return(getEventRate(this$.q.matrix, name=name));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(getEventRate(this$.q.matrix, from=from, to=to));
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
		
##
## Method: getEventRateAtSite
##
setMethodS3(
  "getEventRateAtSite",
  class="GeneralSubstitution",
  function(
    this,
    site,
    name=NA,
    from=NA,
    to=NA,
    ...
  ){

      if(missing(site)){
        throw("No site provided");
      }
      else if (!isAttached(site, process=this)){
        throw("The process is not attached to the specified site!\n");
      }

      glbal.rate<-numeric();
			
			# Event specified by name:
      if(!missing(name) & missing(from) & missing(to)){
          global.rate<-getEventRate(this$.q.matrix, name=name);
      }
			# Event specified by from= and to=
      else if(missing(name) & !missing(from) & !missing(to)){
          global.rate<-getEventRate(this$.q.matrix, from=from, to=to);
      }
      else {
        throw("The substitution should be specified by name or by the \"from\" and \"to\" arguments!\n");
      }

      return(global.rate * getParameterAtSite(this, site, "rate.multiplier")$value );

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
	

##	
## Method: [
##	
setMethodS3(
	"[", 
	class="GeneralSubstitution", 
	function(
		this,
		from,
		to
	){

		if( missing(from) | missing(to) ){
			throw("You must specify two indices!\n");
		}
		# Getting unscaled rate:
		getRate(this$.q.matrix, from=from, to=to);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRate
##	
setMethodS3(
	"getRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){
		
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate as the rate matrix is undefined!\n");
		}
		else if(!missing(name) & missing(from) & missing(to)){
			return(getRate(this$.q.matrix, name=name, value=value));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(getRate(this$.q.matrix, from=from, to=to, value=value));
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="GeneralSubstitution", 
	function(
		this,
		name=NA,
		value,
		from=NA,
		to=NA,
		...
	){
		
		.checkWriteProtection(this);
		# Setting unscaled rate:
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot set rate as the rate matrix is undefined!\n");
		}
		else if(!missing(name) & missing(from) & missing(to)){
			return(setRate(this$.q.matrix, name=name, value=value));
		}
		else if (missing(name) & !missing(from) & !missing(to)){
			return(setRate(this$.q.matrix, from=from, to=to, value=value));
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateList
##	
setMethodS3(
	"getRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){
		
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
		else {
			return(getRateList(this$.q.matrix));
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateList
##	
setMethodS3(
	"setRateList", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot get rate list as the rate matrix is undefined!\n");
		} 
		else if(missing(value)){
			throw("No new rate list specified!\n");
		}
		else {
			return(setRateList(this$.q.matrix, value) );
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-
##	
setMethodS3(
	"[<-", 
	class="GeneralSubstitution", 
	function(
		this,
		from,
		to,
		value
	){

		if( missing(from) | missing(to) ){
			throw("You must specify two indices!\n");
		}
		setRate(this, from=from, to=to,value=value);
		return(invisible(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: rescaleQMatrix
##	
setMethodS3(
	"rescaleQMatrix", 
	class="GeneralSubstitution", 
	function(
		this,
		...
	){

		if(is.na(this$.q.matrix)){
			return(invisible(FALSE));
		}
		else if(!is.QMatrix(this$.q.matrix)){
			throw("Cannot rescale rate matrix because it is invalid!\n");
		}
		else if (any(is.na(this$.q.matrix))){
			throw("Cannot rescale rate matrix because not all rates are specified!\n");
		}
		else if(any(is.na(this$.equ.dist))){
			throw("Cannot rescale rate matrix because the equlibrium distribution is not defined properly!\n");
		}
		else {
			
			# Set rescaling constant to zero:
			K <- 0; 
			# Check for alphabet mismatch:
			if(this$alphabet != this$.q.matrix$.alphabet){
				throw("The process alphabet and the QMatrix alphabet is not the same! Refusing to rescale!\n");
			}
			# get the symbols:
			symbols<-this$alphabet$symbols;
			
			# For every symbol:
			for (i in symbols) {
		  # Get the equlibrium probability:
				i.equ<-this$.equ.dist[[ which(colnames(this$.equ.dist) == i) ]];
				for(j in symbols){
					if(i == j){next}
					# For every other symbol - update the constant:
					K <- K + (i.equ * getRate(this$.q.matrix, from=i, to=j ) );
				}
			}
	
    Scale(this$.q.matrix,constant=(1/K));
		# After rescaling the expected rate of substitutions per site
		# at equlibrium is 1.
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
## Method: summary
##	
setMethodS3(
	"is.GeneralSubstitution", 
	class="default", 
	function(
		this,
		...
	){

	if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.general.substitution)){return(TRUE)}
    if ( inherits(this, "GeneralSubstitution")) {
      this$.is.general.substitution<-TRUE;
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
## Method: print
##	
setMethodS3(
	"as.character",
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){

		this$id;

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
setMethodS3(
	"summary", 
	class="GeneralSubstitution", 
	function(
		this,
		value,
		...
	){
	
		.addSummaryNameId(this);
		.addSummaryAlphabet(this);
		
		if(is.null(this$.summary$"Unscaled rate matrix")){
			this$.summary$"Unscaled rate matrix"<-paste( "\n\t",paste(capture.output(print(this$.q.matrix)),collapse="\n\t"),"\n",sep="");
		}
		this$.summary$"Equilibrium distribution"<-paste( "\n\t",paste(capture.output(print(this$.equ.dist)),collapse="\n\t"),"\n",sep="");
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: clone
##
setMethodS3(
  "clone",
  class="GeneralSubstitution",
  function(
    this,
    value,
    ...
  ){

			# Clone the process object:
      that<-clone.Object(this);
			# Disable write protection:
      if(that$writeProtected){
          that$writeProtected<-FALSE;
      }

			# Clone Q matrix object:
			that$.q.matrix<-clone(this$.q.matrix);
			that$.q.matrix$.process<-that;

      # Reassingning name to force Id update:
      that$name<-that$name;
      return(that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);




##	$Id: NucleotideModels.R,v 1.1 2009-05-01 16:06:14 sbotond Exp $
##
##	Class: NucleotideModels*
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
  "UNREST",
  function( 
		name="Anonymous", 
		rate.list=NA,	
		equ.dist=NA,
		... 
		)	{

		got.rate.list<-!missing(rate.list);
		got.equ.dist<-!missing(equ.dist);
		
			this<-NA;
			# Got rate list and equlibrium distribution:
			if(got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					rate.list=rate.list,
					equ.dist=equ.dist
				);	
				this<-extend(this, "UNREST");
			}
		
			# Got rate list	
			else if(got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					rate.list=rate.list
				);	
				this<-extend(this, "UNREST");
			}
			
			# Got equlibrium distribution,
			# we set it, but it will be owerwritten anyway.
			else if(!got.rate.list & got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet(),
					equ.dist=equ.dist
				);	
				this<-extend(this, "UNREST");
			}

			# Got nothing:
			else if(!got.rate.list & !got.equ.dist){
				this<-GeneralSubstitution(
					name=name,
					alphabet=NucleotideAlphabet()
				);	
				this<-extend(this, "UNREST");
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
setMethodS3(
	"checkConsistency", 
	class="UNREST", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
	
				if(!inherits(this$alphabet, "NucleotideAlphabet")){
					throw("This object must have as alphabet a NucleotideAlphabet object!\n");
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
## Constructor: JC69
##	
setConstructorS3(
  "JC69",
  function( 
		name="Anonymous",
		... 
		)	{
		
		this<-UNREST(rate.list=list(
  		"A->T"=1,
  		"A->C"=1,
  		"A->G"=1,
  		"T->A"=1,
  		"T->C"=1,
  		"T->G"=1,
  		"C->A"=1,
  		"C->T"=1,
  		"C->G"=1,
  		"G->A"=1,
  		"G->T"=1,
  		"G->C"=1
		));
		
		this<-extend(this,"JC69");
		this$name<-name;
		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: checkConsistency
##	
setMethodS3(
	"checkConsistency", 
	class="JC69", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# FIXME - what's to do here?	
		
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

#
# Constructor: GTR
#
setConstructorS3(
  "GTR",
  function( 
		name="Anonymous", 
		rate.params=list(
				"a"=1,
				"b"=1,
				"c"=1,
				"d"=1,
				"e"=1,
				"f"=1
		),	
		base.freqs=rep(0.25,times=4),
		... 
		)	{

		this<-UNREST();

		this<-extend(
			this,
			"GTR",
			.gtr.params=list(
					"a"=NA,
					"b"=NA,
					"c"=NA,
					"d"=NA,
					"e"=NA,
					"f"=NA
				)
		);
		this$name<-name;
		setEquDist(this,value=base.freqs,force=TRUE)	
		if(!missing(rate.params)){
			setRateParamList(this,value=rate.params);
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
	class="GTR", 
	function(
		this,
		...
	){

      wp<-this$writeProtected;
      if (wp) {
        this$writeProtected<-FALSE;
      }
			
		  may.fail<-function(this) {
				
				# FIXME - what's to do here?	
		
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
## Method: getRateParam
##	
setMethodS3(
	"getRateParam", 
	class="GTR", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else if(length(intersect(name,names(this$.gtr.params))) == 0){
			throw("The specified rate parameter name is not valid!\n");
		}
		else {
			return(this$.gtr.params[[name]]);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParam
##	
setMethodS3(
	"setRateParam", 
	class="GTR", 
	function(
		this,
		name,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(name)){
			throw("No rate parameter name specified!\n");
		}
		else if(length(intersect(name,names(this$.gtr.params))) == 0){
			throw("The specified rate parameter name is not valid!\n");
		}
		else if(missing(value)){
			throw("No new value given!\n")
		}
		else if(length(value) != 1|any(!is.numeric(value))){
			throw("The new value must be a numeric vector of length 1!\n");	
		}
		else if(any(is.na(this$.equ.dist))){
			throw("Cannot set rate parameter because the nucleotide frequencies are not defined properly!\n");
		}
		else {
			this$.gtr.params[[name]]<-value;

			# The parmeters are named as in 
			# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.

			this$rateParamList<-this$.gtr.params;
			# FIXME - explain this!
										
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateParamList
##	
setMethodS3(
	"getRateParamList", 
	class="GTR", 
	function(
		this,
		...
	){

		this$.gtr.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateParamList
##	
setMethodS3(
	"setRateParamList", 
	class="GTR", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.gtr.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {
				# Set the rate parameters:
				# The parmeters are named as in 
				# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.

				rate.list=list(

                "T->C"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "C->T"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "T->A"=(value[["b"]] * this$.equ.dist[1,"A"] ),
                "A->T"=(value[["b"]] * this$.equ.dist[1,"T"] ),
                "T->G"=(value[["c"]] * this$.equ.dist[1,"G"] ),
                "G->T"=(value[["c"]] * this$.equ.dist[1,"C"] ),
                "C->A"=(value[["d"]] * this$.equ.dist[1,"A"] ),
                "A->C"=(value[["d"]] * this$.equ.dist[1,"C"] ),
                "C->G"=(value[["e"]] * this$.equ.dist[1,"G"] ),
                "G->C"=(value[["e"]] * this$.equ.dist[1,"C"] ),
                "A->G"=(value[["f"]] * this$.equ.dist[1,"G"] ),
                "G->A"=(value[["f"]] * this$.equ.dist[1,"A"] )

                );
			this$.gtr.params<-value;
			setRateList(this,rate.list);
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
## Method: getBaseFreqs
##	
setMethodS3(
	"getBaseFreqs", 
	class="GTR", 
	function(
		this,
		...
	){

		this$.equ.dist;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBaseFreqs
##	
setMethodS3(
	"setBaseFreqs", 
	class="GTR", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		# FIXME - explain this + more chekings
		setEquDist(this,value,force=TRUE);
		setRateParamList(this,value=this$.gtr.params);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.GTR
##	
setMethodS3(
	"summary", 
	class="GTR", 
	function(
		this,
		...
	){

		.addSummaryNameId(this);
    .addSummaryAlphabet(this);
		if (class(this)[[1]] == "GTR") {
		this$.summary$"Rate parameters"<-paste(names(this$.gtr.params),this$.gtr.params,sep=" = ",collapse=", ");
		}

		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of GTR methods ############

##	
## Constructor: TN93
##	
setConstructorS3(
  "TN93",
  function( 
		name="Anonymous",
		rate.params=list(
				"Alpha1"  =1,
      	"Alpha2"  =1,
      	"Beta"    =1
			),
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(
			this,
			"TN93",
			.tn93.params=list(
					"Alpha1"	=NA,
					"Alpha2"	=NA,
					"Beta"		=NA
				)
			);

		this$name<-name;

		return(this);
	
  },
  enforceRCC=TRUE
);

##	
## Method: getRateParamList
##	
setMethodS3(
	"getRateParamList", 
	class="TN93", 
	function(
		this,
		...
	){

		this$.tn93.params;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .checkRateParamList
##	
setMethodS3(
	".checkRateParamList", 
	class="GTR", 
	function(
		this,
		names,
		value.names,
		...
	){

		# Check for illegal rate parameter names:
		if(length((illegal<-setdiff(value.names, names))) != 0){
			throw("The following rate parameter names are illegal: ",paste(illegal, collapse=", ")," !\n");
		}
		else {
			missing<-setdiff(names, value.names);
			if(length(missing) > 0) {
				throw("Cannot build the model because the following rate parameters are missing: ",paste(missing,coll=", ")," \n");	
		} else {
					return(TRUE);
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
## Method: setRateParamList
##	
setMethodS3(
	"setRateParamList", 
	class="TN93", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.list(value)){
		throw("The provided value must be a list!\n");
	}
	else if(any((as.numeric(value)) < 0)){
 		throw("Cannot set negative rate parameter!\n");
	}
	else {

		# Get the rate parameter names:
		names<-names(this$.tn93.params);
		value.names<-names(value);

		if(.checkRateParamList(this,names,value.names)) {

				# Set the rate parameters:
				# The parmeters are named as in 
				# "Ziheng Yang: Computational Molecular Evolution, Oxford university Press, Oxford, 2006", pp. 34.
			
				this$.tn93.params<-value;
				# Setting the GTR rate parameters:
				rate.list=list(

                "T->C"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "C->T"=(value[["a"]] * this$.equ.dist[1,"T"] ),
                "T->A"=(value[["b"]] * this$.equ.dist[1,"A"] ),
                "A->T"=(value[["b"]] * this$.equ.dist[1,"T"] ),
                "T->G"=(value[["c"]] * this$.equ.dist[1,"G"] ),
                "G->T"=(value[["c"]] * this$.equ.dist[1,"C"] ),
                "C->A"=(value[["d"]] * this$.equ.dist[1,"A"] ),
                "A->C"=(value[["d"]] * this$.equ.dist[1,"C"] ),
                "C->G"=(value[["e"]] * this$.equ.dist[1,"G"] ),
                "G->C"=(value[["e"]] * this$.equ.dist[1,"C"] ),
                "A->G"=(value[["f"]] * this$.equ.dist[1,"G"] ),
                "G->A"=(value[["f"]] * this$.equ.dist[1,"A"] )

                );
			setRateList(this,rate.list);
			}

		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

######### end of TN93 methods ############

##	
## Constructor: HKY
##	
setConstructorS3(
  "HKY",
  function( 
		name="Anonymous",
		... 
		)	{
		
		this<-GTR();
		
		this<-extend(this,"HKY");

		this$name<-name;

		return(this);
	
  },
  enforceRCC=TRUE
);

######### end of HKY methods ############
##	$Id: PSRoot.R,v 1.24 2009-04-30 09:51:11 sbotond Exp $
##
##	Class: 
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
setConstructorS3(
  "PSRoot",
  function(...){
  extend(Object(), "PSRoot",
		.comments=character(0),
		.summary=list()
  );
  },
  ###
  enforceRCC=TRUE
);


##	
## Method: virtualAssignmentForbidden
##	
setMethodS3(
	"virtualAssignmentForbidden", 
	class="PSRoot", 
	###
	function(
		this,
		...
	){
		throw("You cannot set the value of this virtual field directly!");
	},
	###
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: enableVirtual
##	
setMethodS3(
	"enableVirtual", 
	class="PSRoot", 
	###
	function(
		this,
		...
	){
			attr(this,"disableGetMethods")<-NULL;
			attr(this,"disableSetMethods")<-NULL;
			this;
	},
	###
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);
##	
## Method: stringLength
##	
setMethodS3(
	"stringLength", 
	class="default", 
	function(
		this,
		...
	){
		
		this<-as.character(this);	
		if (length(this) != 1){throw("This function can handle only vectors of length 1!")};

		return(length(strsplit(this,split="")[[1]]));	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: stringLengthVector
##	
setMethodS3(
	"stringLengthVector", 
	class="character", 
	function(
		this,
		...
	){
	
		as.numeric(apply(as.array(this),1,stringLength));
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: getMethodsList
##
setMethodS3(
  "getMethodsList",
  class="PSRoot",
  function(
    this,
    ...
  ){

			class<-class(this)[[1]];
			mlist<-getMethods.Class(this);

			# If the class has no methods, do not 
			# consider the methods from the parent class.
			if(names(mlist)[[1]] == class){	
      	as.character(names(mlist[[1]]));
			}
			else {
				return(character(0));
			}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: listMethods
##
setMethodS3(
  "setMethodsList",
  class="PSRoot",
  function(
    this,
		value,
    ...
  ){

		virtualAssignmentForbidden(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: listMethods
##
setMethodS3(
  "ll",
  class="PSRoot",
  function(
    this,
		quiet=FALSE,
    ...
  ){
		
		class<-class(this);
		parents<-class[-1];
		class<-class[[1]]
		methods<-getMethodsList(this);
		fields<-getFields(this);
		text<-character(0);	

		pretty.print<-function(vec,text){

				tmp<-"";
				if(length(vec) > 0 ){
					tmp<-paste(tmp,"  ",vec,sep="",collapse="\n");
				  paste(text,tmp,"\n",sep="");
				} else {
					return(text);
				}
		}

	
		text<-paste(text,"\nClass: ",class,"\n",sep="");
		text<-paste(text,"Inherits from: ",paste(parents,collapse=" "),"\n",sep="");
		text<-paste(text,"Fields (",length(fields),"):\n",sep="");
		text<-pretty.print(fields,text);	

		# Discriminate between the methods implementing 
		# virtual fileds and the rest:
	
		vfields<-character(0);
		methods.not.virtual<-character(0);

		num.args<-function(fun){
			length(formals(fun))
		}

		method.to.field<-function(method){

			 method<-sub('^(get|set)(.*)','\\2',method);
			 tmp<-as.array(strsplit(method,""))[[1]];
       tmp[1]<-tolower(tmp[1]);
       paste(tmp,collapse="");			

		}

		classify.method<-function(method,limit) {

				if( num.args( paste(method,".",class(this)[[1]],sep="") ) == limit){
                vfields<<-c(vfields,method.to.field(method));
            } else {
              methods.not.virtual<<-c(methods.not.virtual,method);
            }

		}

		for(method in methods){
			
				# Get methods for virtual fields have 2 aguments: "this" and "...".
				if(length(grep("^get",method,perl=TRUE)) == 1) {
					classify.method (method,limit=2)
				}
				# Set methods for virtual fields have 3 aguments: "this", "..." and "value".
				else if (length(grep("^set",method,perl=TRUE)) == 1) {
					classify.method (method,limit=3)
				} else {
					methods.not.virtual<-c(methods.not.virtual,method);
				}
		
		}
		vfields<-sort(unique(vfields));	

		lapply(methods.not.virtual,
			function(name) {
				tmp<-method.to.field(name);
				if (length(intersect(tmp,vfields)) > 0 ) {
					print(intersect(tmp,vfields));
					throw("Method classification inconsistency! Blaming ",paste(intersect(tmp,vfields),collapse=" "),". \n");
				}
			}
		);
		
		text<-paste(text,"Virtual fields (",length(vfields),"):\n",sep="");
		text<-pretty.print(vfields,text);
		text<-paste(text,"Methods implemented in ",class," (",length(methods.not.virtual),"):\n",sep="");
		text<-pretty.print(sort(methods.not.virtual),text);
		text<-paste(text,"\n",sep="");
		
		if(!quiet){ cat(text) }	

		invisible(text);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: getComments
##
setMethodS3(
  "getComments",
  class="PSRoot",
  function(
    this,
    ...
  ){
			this$.comments;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: setComments
##
setMethodS3(
  "setComments",
  class="PSRoot",
  function(
    this,
		new_value,
    ...
  ){
			this$.comments<-new_value;
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: PSRoot$all.equal
##
setMethodS3(
  "all.equal",
  class="PSRoot",
  function(
    this,
		one,
		two,
    ...
  ){

		TOLERANCE<-.Machine$double.eps ^ 0.5;
		if(missing(one) | missing (two)){
			throw("Two objects are needed for comparison!\n");
		}
		else {
			one<-as.double(one);
			two<-as.double(two);
			return(isTRUE(all.equal(one,two, tolerance=TOLERANCE)));
		}
		

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: summary.PSRoot
##
setMethodS3(
  "summary",
  class="PSRoot",
  function(
    this,
		verbose=FALSE,
    ...
  ){
		
		# Adding the Comments field:
		if(length(this$.comments) > 0 ) {
		this$.summary$Comments<-paste(this$.comments, collapse=", ");
		}
		# Adding ll() output in verbose mode:
		if(verbose == TRUE ) {

			tmp<-ll(this,quiet=TRUE);
			tmp<-strsplit(tmp,split="\n",extended=TRUE)[[1]];
			tmp<-paste(tmp, collapse="\n  ");
		  this$.summary$"\nObject information"<-tmp;

		}
	
		obj<-PSRootSummary(summary=this$.summary);
		this$.summary<-list();
		# Return a summary object:
		return(obj);

	},
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);


##
## Constructor: summary.PSRoot
##
setConstructorS3(
  "PSRootSummary",
  function(summary=list(),...){
			
			# Stepping out of the R.oo framework to provide 
			# the expected behaviour.
			class(summary)<-c("PSRootSummary");
			summary;
  },
  ###
  enforceRCC=FALSE
);

##
## Method: print.summary.PSRoot
##
setMethodS3(
  "print",
  class="PSRootSummary",
  function(
    this,
    ...
  ){
	
		cat("\n");
		for (i in names(this)){
        cat(paste(i,": ",this[[i]],"\n",sep=""));
   	}
		cat("\n");
		invisible(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: is.na.PSRoot
##
setMethodS3(
  "is.na",
  class="PSRoot",
  function(
    this,
    ...
  ){
		
		# We don't want our objects to be NA-s!	
		return(FALSE);
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: is.PSRoot.default
##
setMethodS3(
  "is.PSRoot",
  class="default",
  function(
    this,
    ...
  ){

		# FIXME - some safe speedup here!	
		if(!is.object(this)) {return(FALSE)}
		inherits(this,"PSRoot");
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
  conflict="warning"
);

##
## Method: checkConsistency;
##
setMethodS3(
  "checkConsistency",
  class="PSRoot",
  function(
		this,
    ...
  ){
		
		warning("Consistency check is not implemented in class ",class(this)[[1]],"!\n");	
		return(invisible(TRUE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
	static=TRUE,
  conflict="warning"
);

##
## Method: globalConsistencyCheck;
##
setMethodS3(
  "globalConsistencyCheck",
  class="PSRoot",
  function(
    ...
  ){
		
		for(name in ls(envir=.GlobalEnv)) {
				obj<-get(name,envir=.GlobalEnv);
				if (is.PSRoot(obj)) {
					cat("Checking ",name," ... ");	
					if( checkConsistency((obj)) ) {
							cat("OK\n");
					}
				}
		}
		return(invisible(TRUE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
	static=TRUE,
  conflict="warning"
);

##
## Method: plot.PSRoot
##
setMethodS3(
  "plot",
  class="PSRoot",
  function(
    ...
  ){
	
		cat("No plot method defined for this object!\n");	
		return(invisible(FALSE));
		
  },
  private=FALSE,
  protected=FALSE,
  overwrite=TRUE,
	static=TRUE,
  conflict="warning"
);
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

##	$Id: PopularAlphabets.R,v 1.2 2009-05-01 16:06:14 sbotond Exp $
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
## BinaryAlphabet
##
setConstructorS3(
  "BinaryAlphabet",
  function(... ){

		this<-Alphabet(type="Binary",symbols=c("0","1"));
		extend(this,"BinaryAlphabet");

  },
  enforceRCC=TRUE
);

##
## NucleotideAlphabet
##
setConstructorS3(
  "NucleotideAlphabet",
  function(... ){

		this<-Alphabet(type="Nucleotide",symbols=c("A","C","G","T"));
		extend(this,"NucleotideAlphabet");

  },
  enforceRCC=TRUE
);

##
## AminoAcidAlphabet
##
setConstructorS3(
  "AminoAcidAlphabet",
  function(... ){

		this<-Alphabet(
				type="Amino acid",
				symbols=c(
					"A",
					"R",
					"N",
					"D", 
					"C",
					"Q",
					"E",
					"G", 
					"H",
					"I",
					"L",
					"K",
					"M",
					"F",
					"P",
					"S", 
					"T",
					"W",
					"Y",
					"V"
				)
		 );
		extend(this,"AminoAcidAlphabet");

  },
  enforceRCC=TRUE
);


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


##	$Id: Process.R,v 1.32 2009-05-04 14:34:10 sb Exp $
##
##	Class: Process
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
	"Process",
	function( 
		name=NA,
		alphabet=NA,
		... 
		){

		this<-extend(
			PSRoot(),
			"Process",
			.name=NA,
			.id=NA,
			.alphabet=NA,
			.site.specific.param.list=list(),
			.event.template=NA,
			.write.protected=FALSE,
			.is.process=TRUE
		);
		.addSiteSpecificParameter(
			this,
			id="rate.multiplier",
			name="Rate multiplier",
			value=as.double(1),
			type="numeric"
		);
		
		STATIC<-TRUE;	

		if(!missing(alphabet)){
			this$alphabet<-alphabet;
			STATIC<-FALSE;
		}

		if (!missing(name)){
			this$name<-name;
			STATIC<-FALSE;
		} else {
			this$name<-"Anonymous";
		}

		if(!STATIC) {
			this$.event.template<-Event(process=this);
		}

		this;
	},
	enforceRCC=TRUE
);

##
## Method: is.Process
##
setMethodS3(
  "is.Process",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.process)){return(TRUE)}
    if ( inherits(this, "Process")) {
      this$.is.process<-TRUE;
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
## Method: checkConsistency
##
setMethodS3(
  "checkConsistency",
  class="Process",
  function(
    this,
    ...
  ){
		
			if(is.null(this$.alphabet)) {
				throw("Process alphabet is NULL!\n");
			}
			else if(is.null(this$.name)) {
				throw("Process name is NULL!\n");
			}
			if(is.null(this$.site.specific.param.list)) {
				throw("Site specific parameter list is NULL!\n");
			}

			wp<-this$writeProtected;		
			if (wp) {
				this$writeProtected<-FALSE;
			}
			
			may.fail<-function(this) {
				# Do not reset alphabet for substirution processes
				# as that would wipe out the rates:
				if (!is.na(this$alphabet) & !is.GeneralSubstitution(this)) {
					this$alphabet<-this$alphabet;
				}			

			}
			tryCatch(may.fail(this),finally=this$writeProtected<-wp);
			
			.checkSiteSpecificParamList(this,plist=this$.site.specific.param.list);			
	
			return(invisible(TRUE));

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkSiteSpecificParamList
##
setMethodS3(
  ".checkSiteSpecificParamList",
  class="Process",
  function(
    this,
		plist,
    ...
  ){
		
			if (missing(plist)) {
					throw("No list given!\n");
			}			

			if(!is.list(plist)) {
				throw("Site specific parameter list is invalid!\n");
			} else {
				for (p in plist) {
						if (!setequal(names(p),c("name","value","type"))) {
								throw("Process-site specific parameter list inconsistent!\n");
						}
						else {
								if (length(p$name) == 0 | !is.character(p$name)) {
									throw("Site specific process parameter name invalid!\n");
								}
								else if (length(p$type) == 0 | !is.character(p$type)) {
									throw("Site specific process parameter type invalid!\n");
								}
								else if (length(intersect(class(p$value),p$type)) == 0 ) {
									throw(paste("The site specific parameter \"",p$name,"\" supposed to be \"",p$type,"\", but it is something else!\n",sep=""));
							}
						}
				}

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
## Method: ==.Process
##
setMethodS3(
  "==",
  class="Process",
  function(
    this,
		that,
    ...
  ){
		
		equals(this, that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: !=.Process
##
setMethodS3(
  "!=",
  class="Process",
  function(
    this,
		that,
    ...
  ){
		
		!equals(this, that);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .addSiteSpecificParameter
##
setMethodS3(
  ".addSiteSpecificParameter",
  class="Process",
  function(
    this,
		id,
		name,
		value,
		type,
    ...
  ){
		
		if (missing(id)) {throw("No id given!\n")}
		else if (missing(name)) {throw("No name given!\n")}
		else if (missing(value)) {throw("No value given!\n")}
		else if (length( intersect(class(value),type) ) == 0 ) {
			throw("The specified default value is not of the correct type!\n");
		}
		
		id<-as.character(id);
		this$.site.specific.param.list[[id]]<-list(
            "name"=name,
            "value"=value,
						"type"=type
        );

  },
  private=TRUE,
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
	class="Process", 
	function(
		this,
		...
	){

		this$.id;

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
	".setId", 
	class="Process", 
	function(
		this,
		...
	){
	
	this.class<-class(this)[1];
	this$.id<-paste(this.class,this$.name,hashCode(this),sep=":");

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
	class="Process", 
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
## Method: getName
##	
setMethodS3(
	"getName", 
	class="Process", 
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
	class="Process", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
		.setId(this);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSiteSpecificParamList
##	
setMethodS3(
	"getSiteSpecificParamList", 
	class="Process", 
	function(
		this,
		...	
	){
		this$.site.specific.param.list;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSiteSpecificParamList
##	
setMethodS3(
	"setSiteSpecificParamList", 
	class="Process", 
	function(
		this,
	  value,	
		...	
	){
		throw("You should not set the siteSpecificParamList directly!\n");
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getSiteSpecificParamIds
##	
setMethodS3(
	"getSiteSpecificParamIds", 
	class="Process", 
	function(
		this,
		...	
	){
		names(this$.site.specific.param.list);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSiteSpecificParamIds
##	
setMethodS3(
	"setSiteSpecificParamIds", 
	class="Process", 
	function(
		this,
	  value,	
		...	
	){
		throw("You should not set the siteSpecificParamIds directly!\n");
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
setMethodS3(
	"getAlphabet", 
	class="Process", 
	function(
		this,
		...	
	){
			this$.alphabet;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
setMethodS3(
	"setAlphabet", 
	class="Process", 
	function(
		this,
		new.alphabet,
		...	
	){
		
			.checkWriteProtection(this);	
			if (!is.Alphabet(new.alphabet)) {throw("The alphabet object is invalid!\n")}
			else {
				this$.alphabet<-new.alphabet;				
			}
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventsAtSite
##	
setMethodS3(
	"getEventsAtSite", 
	class="Process", 
	function(
		this,
		site,
		position,
		...	
	){
		
		# Returns a list of event objects;	
		#e1<-Event(name="A->T",rate=0.2,process=this);	
		#e2<-clone(e1);
		#e1$name<-"Insertion";
		#list(e1,e2);
		list();
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
  class="Process",
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
  class="Process",
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
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: hasUndefinedRate
##
setMethodS3(
  "hasUndefinedRate",
  class="Process",
  function(
    this,
    value,
    ...
  ){

		return(FALSE);

  },
  private=FALSE,
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
  class="Process",
  function(
    this,
    value,
    ...
  ){

    if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##
## Method: clone
##
setMethodS3(
  "clone",
  class="Process",
  function(
    this,
    value,
    ...
  ){

			tmp<-clone.Object(this);
			if(tmp$writeProtected){
					tmp$writeProtected<-FALSE;
			}
		
			# Reassingning name to
			# force Id update.
			tmp$name<-tmp$name;
			tmp;

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: as.character.Process
##	
setMethodS3(
	"as.character", 
	class="Process", 
	function(
		this,
		...	
	){
			this$id;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .addSummaryAlphabet
##	
setMethodS3(
	".addSummaryAlphabet", 
	class="Process", 
	function(
		this,
		...	
	){

		
			if(!is.na(this$alphabet)) {
     	 	alphabet_symbols<-paste(this$alphabet$symbols,collapse=" ");
      	this$.summary$"Alphabet"<-paste("\n","  Type: ",this$alphabet$type,"\n","  Symbols: ", alphabet_symbols,sep="");
    	} else {
       	 this$.summary$"Alphabet"<-NA
    	}
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .addSummaryNameId
##	
setMethodS3(
	".addSummaryNameId", 
	class="Process", 
	function(
		this,
		...	
	){

		if(is.null(this$.summary$"Name")){
			this$.summary$"Name"<-this$name;
		}
		if(is.null(this$.summary$"Id")){
			this$.summary$"Id"<-this$id;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Process
##	
setMethodS3(
	"summary", 
	class="Process", 
	function(
		this,
		...	
	){

		.addSummaryNameId(this);

		# Skip the alphabet for InDel processes
		if(!is.GeneralInDel(this)){
			.addSummaryAlphabet(this);
		}
			
			tmp<-character();
			param_list<-this$siteSpecificParamList;
			counter<-0;		
	
			for (id in names(param_list)) {
					param<-param_list[[id]];	
					tmp<-paste(tmp,
												"\n  Id: ",id,
												"\n  Name: ",param$name,
												"\n  Type: ",param$type,
												"\n  Default value: ",param$value,
												sep=""
										);
										counter<-counter+1;
										if ( counter < length(param_list) ){
											tmp<-paste(tmp,"\n");
										}
										
			}	
		
			header<-paste("Site specific parameters (",length(param_list),")",sep="");
			this$.summary[[header]]<-tmp;
			if(getWriteProtected(this)) {
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


##	$Id: ProcessesList.R,v 1.2 2009-04-29 08:35:19 sbotond Exp $
##
##	Class: ProcessesList
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
	"ProcessesList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"ProcessesList",
			.seq=NA
		);

		if(!missing(seq)) {
			if(!is.Sequence(seq)) {
				throw("Sequence object not valid!\n");
			} else {
				this$.seq=seq;
			}
		}

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.ProcessesList
##	
setMethodS3(
	"is.ProcessesList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "ProcessesList");

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
setMethodS3(
	"checkConsistency", 
	class="ProcessesList", 
	function(
		this,
		...
	){

		if(!is.Sequence(this$.seq)){
			throw("Process list sequence reference is invalid!\n");
		}
		return(TRUE);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [.ProcessesList
##	
setMethodS3(
	"[", 
	class="ProcessesList", 
	function(
		this,
		index
	){

		getProcesses(this$.seq,index);	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.ProcessesList
##	
setMethodS3(
	"[<-", 
	class="ProcessesList", 
	function(
		this,
		index,
		value
	){

		setProcesses(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.ProcessesList
##	
setMethodS3(
	"[[", 
	class="ProcessesList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getProcesses(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.ProcessesList
##	
setMethodS3(
	"[[<-", 
	class="ProcessesList", 
	function(
		this,
		index,
		value
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		} else if (length(value) > 1) {
			warning("Value vector longer than one!\n");
		}
		setProcesses(this$.seq,value,index);

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
	class="ProcessesList", 
	function(
		this,
		...
	){
		
		this[];	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	$Id: QMatrix.R,v 1.12 2009-04-30 17:10:22 sbotond Exp $
##
##	Class: QMatrix
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
  "QMatrix",
  function( 
		name="Anonymous",
		alphabet=NA,
		rate.list=NA,
		process=NA,
		... 
		)	{

		this<-PSRoot();
    this<-extend(
      this,
      "QMatrix",
			.name=NA,
			.alphabet=NA,
			.rate.matrix=NA,
			.orig.matrix=NA,
		  .norm.const=NA,
			.process=NA,
			.is.q.matrix=TRUE
    );
		
		this$name<-name;

		if(!missing(process)){
			this$process<-process;
		}

		if(!missing(alphabet)){
			this$alphabet<-alphabet;
		}

		if(!missing(rate.list)){
			if(missing(alphabet)){
				throw("Cannot set rates because the alphabet is not specified!\n")
			}
			this$rateList<-rate.list;
		}

    return(this);
  },
  enforceRCC=TRUE
);

##	
## Method: .buildRateMatrix
##	
setMethodS3(
	".buildRateMatrix", 
	class="QMatrix", 
	function(
		this,
		...
	){

		size<-this$alphabet$size;
		symbols<-this$alphabet$symbols;

		# Setting the dimension names
		# for the original rates matrix:
		if(!isEmpty(this$.alphabet)){
			this$.orig.matrix<-matrix(nrow=size,ncol=size);
			colnames(this$.orig.matrix)<-symbols;
			rownames(this$.orig.matrix)<-symbols;
		}
		# Copy to the scaled matrix:
		this$.rate.matrix<-this$.orig.matrix;
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
setMethodS3(
	"checkConsistency", 
	class="QMatrix", 
	function(
		this,
		check.process=TRUE,
		...
	){

			if(is.Process(this$.process)){

      		wp<-this$.process$.write.protected;
      		if (wp) {
					# Cannot use virtual field here because of 
					# some obscure errror:
        	this$.process$.write.protected<-FALSE;
      	}
			}
      may.fail<-function(this) {
					
				# Reassing name:
				this$name<-this$name;
				# Do not reassign alphabet as that will wipe out the rates!
				if(!is.na(this$.alphabet) & !is.Alphabet(this$.alphabet)){
					throw("QMatrix alphabet is invalid!\n");
				}
		
				# Check if the original rate matrix is a matrix or NA.
				if(!is.matrix(this$.orig.matrix) & !all(is.na(this$.orig.matrix))){
						throw("The original rates matrix is invalid!\n");
				}	
				# Check original rates matrix size:
				else if(!all(dim(this$.orig.matix) != c(this$.alphabet$.size, this$.alphabet$.size))) {
					throw("The original rates matrix is of wrong size!");
				}
				
				# Check if the rescaled rate matrix is a matrix or NA.
				if(!is.matrix(this$.rate.matrix) & !all(is.na(this$.rate.matrix))){
						throw("The rescaled rates matrix is invalid!\n");
				}	
				# Check rescaled rates matrix size:
				else if(!all(dim(this$.rates.matix) != c(this$.alphabet$.size, this$.alphabet$.size))) {
					throw("The original rates matrix is of wrong size!");
				}

				# Flag for not having NA-as in the matrices.
				COMPLETE<-TRUE;

			
				if(is.matrix(this$.orig.matrix) ){
					if(any(is.na(this$.orig.matrix))){
						warning("Some rates are undefined!\n");
						COMPLETE<-FALSE;
					}
					else if (!all(is.numeric(this$.orig.matrix))){
						throw("The original rates matrix has non-numeric elements!\n");
					}
				}
				
				if(is.matrix(this$.orig.matrix) ){
					if( any(is.na(this$.orig.matrix)) & COMPLETE ){
						COMPLETE<-FALSE;
						throw("The original rates matrix is complete, but the rescaled matrix has undefined elements!\n");
					}
					else if (!all(is.numeric(this$.orig.matrix))){
						throw("The rescaled rates matrix has non-numeric elements!\n");
					}
				}

				# Check the normalizing constant:
				if( length(this$.norm.const) != 1 | (!is.na(this$.norm.const) & !is.numeric(this$.norm.const)) ){
					 throw("Normalizing constant is invalid!\n");
				}

				# Check the normalization:
				if(is.matrix(this$.orig.matrix) & is.matrix(this$.rate.matrix) & COMPLETE ){
						if(!PSRoot$all.equal(this$.rate.matrix, (this$.norm.const * this$.orig.matrix)) ){
							throw("The scaled matrix is inconsistent with the original matrix and the normalizing constant!\n");
						}
				}

				# Check the process:
				if(check.process==TRUE){
					 if(is.Process(this$.process)){
							# Check for alphabet compatibility:
							if(this$.alphabet != this$.process$alphabet){
								throw("Process/QMatrix alphabet mismatch!\n");
							}
							# Check if the parent process QMatrix is this object:
							if(!equals(this$.process$.q.matrix, this) ){
								throw("Parent process QMatrix is not identical with self!\n");
							}
					 }
					 else if(!is.na(this$.process)){
							throw("Process entry is invalid!\n");
					 }
				}
				
      
			}
      tryCatch(may.fail(this),finally={if(is.Process(this$.process)){this$.process$.write.protected<-wp}});
			return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .callRateRescaling
##	
setMethodS3(
	".callRateRescaling", 
	class="QMatrix", 
	function(
		this,
		guess.equ=TRUE,
		...
	){

		# Usually called when the rate matrix chenges.
		# If the Q matrix has a parent process with a valid equilibrium distribution:
		if(is.Process(this$.process) & !any(is.na(as.vector(this$.orig.matrix))) ){

		if(guess.equ){
			# Try to guess the new equlibrium distribution:
			if(!.setEquDistFromGuess(this$.process)){
						# Fill with NA-s if failed with guessing:
      	    .initEquDist(this$.process);
      	}
		}
			# Rescale if the equilibrium distribution was succesfully set:
			if(all(!is.na(this$.process$equDist))){
				rescaleQMatrix(this$.process);
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
## Method: getName
##	
setMethodS3(
	"getName", 
	class="QMatrix", 
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
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else{
		value<-as.character(value);
		if(stringLength(value) == 0){
			throw("Cannot set empty name!");
		} else {
			this$.name<-value;
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
## Method: getProcess
##	
setMethodS3(
	"getProcess", 
	class="QMatrix", 
	function(
		this,
		...
	){

		this$.process;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setProcess
##	
setMethodS3(
	"setProcess", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(missing(value)){
			throw("No new value provided!\n");
		}
		else if (!is.Process(value)){
			throw("Process object invalid!\n");
		} 
		else {
			this$.process<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
setMethodS3(
	"getAlphabet", 
	class="QMatrix", 
	function(
		this,
		...
	){

		this$.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
setMethodS3(
	"setAlphabet", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

	.checkWriteProtection(this);
	if(missing(value)){
		throw("No new value provided!\n");
	}
	else if(!is.Alphabet(value)) {
		throw("Alphabet object invalid!\n");
	}
	else if(is.Process(this$.process)){
		if(value != this$.process$alphabet){
		throw("The new alphabet should match with the one from the subsitution process!\n");
		}
	}
	if(is.matrix(this$.rate.matrix)){
			warning("Be aware that setting a new alphabet wipes out completely the rate matrix!\n");
	}
		this$.alphabet<-value;
		.buildRateMatrix(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .nameToDim
##	
setMethodS3(
	".nameToDim", 
	class="QMatrix", 
	function(
		this,
		name,
		...
	){

		if(missing(name)){
			throw("No name provided!\n");
		}

		# split the name
		substitution<-rbind(strsplit(name,split="->")[[1]]);
		if(length(substitution) != 2 ) {
			throw("Substitution event name was invalid!");
		}

		# Check if symbols are valid:
		if(!hasSymbols(this$.alphabet, substitution)){
				throw("All symbols must be in the alphabet!\n");
		}

		# Return a vector with the DIMNAMES:
		colnames(substitution)<-c("from","to");
		rownames(substitution)<-c("Substitution");
		substitution;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .dimToName
##	
setMethodS3(
	".dimToName", 
	class="QMatrix", 
	function(
		this,
		dim,
		...
	){

		if(missing(dim)){
			throw("No event vector provided!\n");
		}
		else if(length(dim) != 2) {
			throw("Event vector is invalid!\n");
		}
		# FIXME alphabet check
		paste(dim,collapse="->");


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventRate
##	
setMethodS3(
	"getEventRate", 
	class="QMatrix", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){

			if (isEmpty(this$.alphabet)){
				throw("Alphabet is valid but empty, so no rates are defined!\n");
			}
			# Event specified by name:
			else if(!missing(name) & missing(from) & missing(to)){
				# convert to dimnames
				tmp<-.nameToDim(this, name);
				# Return the rate from the rescaled matrix: 
				return(this$.rate.matrix[tmp[1],tmp[2]]);
			}
			# Event specified by from= and to=
			else if(missing(name) & !missing(from) & !missing(to)){
				# Check symbols:
				if(!hasSymbols(this$.alphabet, c(from,to))){
					throw("All symbols must be in the alphabet!\n")
				}
				else{
					# Get the rate from the rescaled matrix:
					return(this$.rate.matrix[as.character(from),as.character(to)]);
				}
			}
			else {
				throw("The substitution should be specified by name or by the \"from\" and \"to\" arguments!\n");
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRate
##	
setMethodS3(
	"getRate", 
	class="QMatrix", 
	function(
		this,
		name=NA,
		from=NA,
		to=NA,
		...
	){

			if (isEmpty(this$.alphabet)){
				throw("Alphabet is valid but empty, so no rates are defined!\n");
			}
			# Event specified by name:
			else if(!missing(name) & missing(from) & missing(to)){
				# Convert to dimnames:
				tmp<-.nameToDim(this, name);
				# return unscaled rate:
				return(this$.orig.matrix[tmp[1],tmp[2]]);
			}
			# Event specified by from= and to=:
			else if(missing(name) & !missing(from) & !missing(to)){
				# check symbols:
				if(!hasSymbols(this$.alphabet, c(from,to))){
					throw("All symbols must be in the alphabet!\n")
				}
				else{
				# return unscaled rate:
					return(this$.orig.matrix[as.character(from),as.character(to)]);
				}
			}
			else {
				throw("The substitution should be specified by name or by the \"from\" and \"to\" arguments!\n");
			}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: setRate
##	
setMethodS3(
	"setRate", 
	class="QMatrix", 
	function(
		this,
		name=NA,
		value,
		from=NA,
		to=NA,
		scale=TRUE,
		diag=TRUE,
		...
	){

		.checkWriteProtection(this);
		if (isEmpty(this$.alphabet)){
				throw("Alphabet is valid but empty, so no rates are defined!\n");
		}
		else if(missing(value)) {
			throw("No new value provided!\n");}
		else if(!is.numeric(value)) {
			throw("Rate must be numeric!\n");
		}
		else if (value < 0){
			throw("Cannot set negative rate!\n");
		} else {
			
			.from<-character();		
			.to<-character();		
		
			# Event specified by name:		
			if(!missing(name) & missing(from) & missing(to)){
				# convert to dimnames:
				tmp<-.nameToDim(this, name);
				.from<-tmp[1];
				.to<-tmp[2];
				
			}
			# Event specified by from= and to=:
			else if(missing(name) & !missing(from) & !missing(to)){
				# check the symbols
				if(!hasSymbols(this$.alphabet, c(from,to))){
					throw("All symbols must be in the alphabet!\n")
				}
				else{
				.from<-as.character(from);
				.to<-as.character(to);
				}
			}
			else {
				throw("The substitution should be specified by name or by the \"from\" and \"to\" arguments!\n");
			}
				
				# Complain if tried to modify a diagonal element:
				if(.from == .to){
					throw("Modification of diagonal elements is not allowed!\n");
				}
				else {
				
					# Set the rate in the original rates matrix:	
			 		this$.orig.matrix[.from, .to]<-value;
			 		# Set the new diagonal element in the original rates matrix:
					if (diag == TRUE) {
			 			this$.orig.matrix[.from, .from]<-.calculateDiagonal(this, symbol=.from);
					}
					
					# Call rate rescaling, this will set the new values
					# in the rescaled rates matrix:
			 		if(scale==TRUE){
						.callRateRescaling(this);
			 		}
			}
		 return(invisible(value));

		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: .getRateList
##	
setMethodS3(
	".getRateList", 
	class="QMatrix", 
	function(
		this,
		type="ORIGINAL",
		...
	){

		# Be gentle and return an ampty list if the
		# alphabet is empty:
		if( isEmpty(this$.alphabet) ){
				return(list());	
		}
		else {
				# Fill in the rates list:
				rates<-list();
				for(i in this$.alphabet$symbols){
					for(j in this$.alphabet$symbols){
						if(i != j){
							name<-paste(i,j,sep="->");
							# with the original rates:
							if(type == "ORIGINAL"){
							# or with the rescaled ones:
								rate<-getRate(this, from=i, to=j);
							} 
							else {
								rate<-getEventRate(this, from=i, to=j);
							}
							rates[[name]]<-rate;
						}
					}
				}
				return(rates);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateList
##	
setMethodS3(
	"getRateList", 
	class="QMatrix", 
	function(
		this,
		type=NA,
		...
	){
		
		.getRateList(this, type="ORIGINIAL");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setRateList
##	
setMethodS3(
	"setRateList", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);
		if(!is.Alphabet(this$.alphabet)){
			throw("Cannot set rates because alphabet is undefined!\n");
		}
		if(missing(value)) {
			throw("No new value provided!\n");}
		else if(!is.list(value)) {
			throw("The new value must be a list!\n");
		} else {

			# Check if all of the rates are specified!
			expected<-.genExpected(this);
	
			# Check for missing rates:		
			if(length(tmp<-setdiff(expected,names(value))) != 0 ){
					throw("The rate matrix is not specified correctly, the following rates are missing: ",paste(tmp,collapse=" "),"!\n");
			}

			# Warn the user about the superfluous rates:
			if(length(tmp<-setdiff(names(value),expected)) != 0 ){
					warning("The following rates were not expected by this process: ",paste(tmp,collapse=" "),", so they were ignored!\n");
					# Getting rid of unexpected rates:
					value[tmp]<-NULL;
			}

				# set the rate matrix if all is OK!
				for (name in names(value)){
					setRate(this,name=name,value=(value[[name]]),scale=FALSE,diag=FALSE);
				}

				# Set diagonal elements:
				for (sym in this$alphabet$symbols){
					this$.orig.matrix[sym, sym]<-.calculateDiagonal(this, sym);
				}
	
				# Call the parent process object to guess the new equlibrium distribution and rescale
				# rates:
				.callRateRescaling(this);

			

		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .genExpected
##	
setMethodS3(
	".genExpected", 
	class="QMatrix", 
	function(
		this,
		...
	){

      expected<-list();
      sym<-this$alphabet$symbols;

      # Cretae the list of expected rates:
      for(i in sym){
        for(j in sym){
          if(i != j){
          expected<-c(expected,paste(i,j,sep="->"));
          }
        }
      }
			return(expected);
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventRateList
##	
setMethodS3(
	"getEventRateList", 
	class="QMatrix", 
	function(
		this,
		type=NA,
		...
	){
		
		.getRateList(this, type="SCALED");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setEventRateList
##	
setMethodS3(
	"setEventRateList", 
	class="QMatrix", 
	function(
		this,
		type=NA,
		...
	){
		
		.virtualAssignmentForbidden(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .calculateDiagonal
##	
setMethodS3(
	".calculateDiagonal", 
	class="QMatrix", 
	function(
		this,
		symbol=NA,
		...
	){

		if(!missing(symbol)){
			# convert diname to dim:
			index<-.symToIndex(this, symbol=symbol);
		}
		else {
			throw("Symbol not specified!\n");
		}
			# Return -1 * sum of the off-diagonal elements
			# from the row specified by the index:
			return(-sum((this$.orig.matrix[symbol,])[-index] ));
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .symToIndex
##	
setMethodS3(
	".symToIndex", 
	class="QMatrix", 
	function(
		this,
		symbol=NA,
		...
	){

		if(missing(symbol)){
			throw("No symbol specified");
		} else {
			index<-which(rownames(this$.orig.matrix) == symbol);
			if(length(index) == 0){
				print(symbol);
				throw("Symbol not in rate matrix!\n");
			}
			else if (length(index) > 1){
				throw("Rate matrix is inconsistent!\n");
			}
			return(index);
		}


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getMatrix
##	
setMethodS3(
	"getMatrix", 
	class="QMatrix", 
	function(
		this,
		...
	){

		this$.orig.matrix;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setMatrix
##	
setMethodS3(
	"setMatrix", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		.virtualAssignmentForbidden(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getScaledMatrix
##	
setMethodS3(
	"getScaledMatrix", 
	class="QMatrix", 
	function(
		this,
		...
	){

		this$.rate.matrix;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setMatrix
##	
setMethodS3(
	"setScaledMatrix", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		.virtualAssignmentForbidden(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: Scale
##	
setMethodS3(
	"Scale", 
	class="QMatrix", 
	function(
		this,
		constant=NA,
		...
	){

		if(missing(constant)){
			throw("No scaling constant specified!\n");
		if(!is.numeric(constant)){
			throw("Scaling constant must be numeric!\n");
		}
		} else {
			 
			 # Set the rescaled matrix to the original matrix
			 # multiplied by the given constant:
			 this$.rate.matrix<-(this$.orig.matrix * constant);
			 # store the current rescaling constant:
			 this$.norm.const<-constant;
			 return(invisible(this));
		}

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
setMethodS3(
	"as.character", 
	class="QMatrix", 
	function(
		this,
		...
	){
		
		this$id

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: print
##	
setMethodS3(
	"print", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		print.default(this$.orig.matrix);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: print
##	
setMethodS3(
	"summary", 
	class="QMatrix", 
	function(
		this,
		value,
		...
	){

		this$.summary$"Name"<-this$name;
		this$.summary$"Id"<-this$id;
		this$.summary$"Attached to process"<-this$process;
	  this$.summary$"Unscaled rate matrix"<-paste( "\n\n\t",paste(capture.output(print(this$.orig.matrix)),collapse="\n\t"),"\n",sep="");
		this$.summary$"Scaling factor"<-this$.norm.const;
	  this$.summary$"Scaled rate matrix"<-paste( "\n\n\t",paste(capture.output(print(this$.rate.matrix)),collapse="\n\t"),"\n",sep="");
	
		
		NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## setId
##
setMethodS3(
  "setId",
  class="QMatrix",
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
## Method: getId
##
setMethodS3(
  "getId",
  class="QMatrix",
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
## Method: getwWriteProtected
##
setMethodS3(
  "getWriteProtected",
  class="QMatrix",
  function(
    this,
    ...
  ){

	# return false if no process is attached:
	if(!is.Process(this$.process)) {
			return(FALSE);
	}
	else {
			# The flag from the parent process is used!
			return(getWriteProtected(this$.process));
	}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: is.QMatrix
##
setMethodS3(
  "is.QMatrix",
  class="default",
  function(
    this,
    ...
  ){

    if(!is.PSRoot(this)) {return(FALSE)}
    if(!is.null(this$.is.process)){return(TRUE)}
    if ( inherits(this, "QMatrix")) {
      this$.is.q.matrix<-TRUE;
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
## Method: setWriteProtected
##
setMethodS3(
  "setWriteProtected",
  class="QMatrix",
  function(
    this,
		value,
    ...
  ){

		throw("The QMatrix objects use the write protection flags of the enclosing substitution process, modify that (if exists)!\n");

  },
  private=FALSE,
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
  class="QMatrix",
  function(
    this,
    ...
  ){

    if(this$writeProtected) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	$Id: Sequence.R,v 1.44 2009-05-01 08:48:21 sbotond Exp $
##
##	Class: Sequence
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
## Method: plusGamma
##	
setMethodS3(
	"plusGamma", 
	class="default", 
	function(
		this,
		process,
		shape,	
		index,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		if(!is.GeneralSubstitution(process)){
			throw("The sepcified process is not a substitution process!\n");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		else {
		
			if(missing(index)){
				index<-seq(along=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}

			setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(length(index),shape=shape,rate=shape),index=index);
	
		}
		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plusInvGamma
##	
setMethodS3(
	"plusInvGamma", 
	class="default", 
	function(
		this,
		process,
		pinv,
		shape,	
		index,
		...
	){

		if(missing(process)){
			throw("No process specified!\n");
		}
		if(!is.GeneralSubstitution(process)){
			throw("The sepcified process is not a substitution process!\n");
		}
		else if(missing(pinv)){
			throw("No proportion of invariant sites given!\n");
		}
		else if(!all(is.numeric(pinv)) | length(pinv) != 1){
			throw("The pinv parameter must be a numeric vector of lenght 1!\n");	
		}
		else if(pinv > 1){
			throw("Tpe proportion of invariant sites cannot be larger than 1.!");
		}
		else if(missing(shape)){
			throw("No shape parameter specified!\n");
		}
		else if(!all(is.numeric(shape)) | length(shape) != 1){
			throw("The shape parameter must be a numeric vector of lenght 1!\n");	
		}
		else {
		
			if(missing(index)){
				index<-seq(along=this$.sites);
			}
			else {
				index<-.checkIndexSanity(this, index);
			}

			# Iterating over the sites specified by the index vector:
			
			for(site in index){

				# Choose between invariant and gamma:
				type<-sample(c("INV","GAMMA"),size=1, replace=FALSE, prob=c( pinv, (1-pinv) ) );

				if(type == "INV"){
					setParameterAtSites(this, process=process, id="rate.multiplier",value=0,index=c(site));
				}
				else {
					setParameterAtSites(this, process=process, id="rate.multiplier",value=rgamma(1,shape=shape,rate=shape),index=c(site));
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
##	$Id: Sequence.R,v 1.44 2009-05-01 08:48:21 sbotond Exp $
##
##	Class: Sequence
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
	"Sequence",
	function(
		name=NA,
		string=NA,
		length=NA,
		alphabets=NA,
		processes=NA,
		ancestral.obj=NA,
		...
	){

		# Marking the instance as static by default:	
		STATIC<-TRUE;	

		# Extending the PSRoot class:
		this<-extend(
			PSRoot(),
			"Sequence",
			.name="Anonymous",
			.length=NA,
			.sites=list(),
			.ancestral.obj=NA,
			.cumulative.rates=NA,
			.total.rates=NA,
			.cumulative.rate.flag=TRUE,
			.flagged.sites=integer(0),
			.write.protected=FALSE,
			.is.sequence=TRUE,
			.root.ins=NA
		);

		# Initializing the variables for length and states:
		len<-0;
		str<-list();

		# Optional argument: name
		if(!missing(name)) {
			this$name<-name;
			STATIC<-FALSE;
		}
		
		# The user can specify a sequence
		# or the sequence length, but not both.		
		if (!missing(string) & !missing(length)) {
			throw("You can specify the sequence, or the sequence length, but not both!\n");}
		else if (!missing(string)){
			STATIC<-FALSE;
			# An alphabet list must be specified along the sequence! 
			if(missing(alphabets)) {throw("A list of valid alphabets must be specified when a string is given!\n");}
		}

		# Deal with the string or length argument:
		if (!missing(length)) {
			STATIC<-FALSE;
			len<-length;
		}
		else if( !missing(string) ) {
					str<-strsplit(string,split="")[[1]];
					len<-length(str);
		}
		this$.length<-len;

		root.ins<-NA;
		if (!is.Process(Sequence$.root.ins)) {
			# Dummy proces used as ancestral object for sites.
			this$.root.ins<-Process(name="Root insertion process");
			this$.root.ins$comments<-"This is just a dummy process object serving as ancestral for newly created site and sequence objects.";
			root.ins<-this$.root.ins;
		} else {
			root.ins<-Sequence$.root.ins
		}

		# Site template object:
			 site.template<-Site(
            ancestral=root.ins,
            sequence=this
          );	
	

		# Clone and store the site objects:
		if(!STATIC) {
			if ( len > 0 ) {
				for(position in 1:len) {
					 this$.sites[[position]]<-clone(site.template);
				}
			}

		}	
		
		# Optional argument: ancestral object
		if (!missing(ancestral.obj)) {
			STATIC<-FALSE;
			this$ancestral<-ancestral.obj;
		} else {
			this$ancestral<-root.ins;
		}

		# Setting the alphabets:
		if(!missing(alphabets)) {
			STATIC<-FALSE;
			# setAlphabets will check the arguments
			setAlphabets(this, alphabets);
		}
	
		# Setting the processes:
		if (!missing(processes)) {
			STATIC<-FALSE;
			# setProcesses will take care about the arguments
			# and deal with alphabet mismatch.
			setProcesses(this,processes)	
		}

			# Initializing these vectors properly is
			# importtant for the insertion method!
			this$.total.rates<-double(len);
			this$.cumulative.rates<-double(len);

		if(!STATIC){

			# Now we are prepared to set the states:	
			if (!missing(string)) {
				setStates(this, str);
			}

			# Calculate cumulative rates for the first time, but only if
			# states are defined. This is expensive, as total rates are calculated.
			if (!missing(string) & (length(str) > 0) ) {
				.recalculateCumulativeRates(this);
			}

		}

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.Site
##	
setMethodS3(
	"is.Sequence", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
   	if(!is.null(this$.is.sequence)){return(TRUE)}
    if ( inherits(this, "Sequence")) {
      this$.is.sequence<-TRUE;
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
## Method: checkConsistency
##	
setMethodS3(
	"checkConsistency", 
	class="Sequence", 
	function(
		this,
		ommit.sites=FALSE,
		...
	){

		if(this$length != length(this$.sites)) {
			throw("Sequence length inconsistency detected");
		} else if (length(this$.cumulative.rates) != this$.length) {
			throw("Cumulative rates vector length mismatch!\n");
		} else if (length(this$.total.rates) != this$.length) {
			throw("Total rates vector length mismatch!\n");
		} else if (!identical(this$.cumulative.rates, cumsum(this$.total.rates))) {
			throw("Cumulative rates vector is not consistent with total rates vector!\n");
		}

		if(!is.numeric(this$.flagged.sites)) {
			throw("Flagged sites vector is not numeric!\n");
		} else if (length(this$.flagged.sites) > 0) {
			if ( (min(this$.flagged.sites) < 1) | ( max(this$.flagged.sites) > this$.length) ) {
				throw("Inconsistency in the flagged sites vector!\n");
			}	
		}

		if(!is.character(this$.name)) {
			throw("Sequence name is invalid!\n");
		} else if(stringLength(this$.name) == 0) {
			throw("Sequence name is of length zero!\n");
		}

		if(!is.Sequence(this$.ancestral.obj) & !is.Process(this$.ancestral.obj)) {
			throw("The ancestral object is invalid!\n");
		}
		if(!is.logical(this$.cumulative.rate.flag)) {
			throw("Cumulative rate flag is not logical!\n");
		}

		# Calling consistency check on sites.
		# This will be painfully slow!
		if(!ommit.sites){
			for(site in this$.sites){
				checkConsistency(site);
			}
		}
		return(TRUE);

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
  class="Sequence",
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
	class="Sequence", 
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
## Method: getName
##	
setMethodS3(
	"getName", 
	class="Sequence", 
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
	class="Sequence", 
	function(
		this,
		new.name,
		...
	){
		
		.checkWriteProtection(this);	
		this$.name<-as.character(new.name);
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getString
##	
setMethodS3(
	"getString", 
	class="Sequence", 
	function(
		this,
		...
	){
			
			paste(as.vector(lapply(this$.sites,
				function(site){
					if(is.na(site$state)){return("?")}
					else {return(site$.state)}
				}
			)),collapse="");

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setString
##	
setMethodS3(
	"setString", 
	class="Sequence", 
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
## Method: getLength
##	
setMethodS3(
	"getLength", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$.length;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setLength
##	
setMethodS3(
	"setLength", 
	class="Sequence", 
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
## Method: getSites
##	
setMethodS3(
	"getSites", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		this$.sites;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getStates
##	
setMethodS3(
	"getStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		 if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$.state;
      }
    );

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setStates
##	
setMethodS3(
	"setStates", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
	  else if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
		# Recycling value vector by using rep().
    if (length(value) < length(this$.sites) ) {
        value<-rep(as.character(value),length.out=length(index))
    }
		
    for (i in 1:length(index)) {
        this$.sites[[ index[[i]] ]]$state<-value[i];
    }
		# Flagging the changed sites:
		.flagCumulativeRates(this);
		this$.flagged.sites<-c(this$.flagged.sites, index);
		.recalculateCumulativeRates(this);

    invisible(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getStatesList
##	
setMethodS3(
	"getStatesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		StatesList(seq=this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabets
##	
setMethodS3(
	"getAlphabets", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          site$alphabet;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabets
##	
setMethodS3(
	"setAlphabets", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		sloppy=FALSE,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}
		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
				for(a in value) {
					if(!is.Alphabet(a)) {
						throw("The value parameter must be a list of valid alphabet objects!\n");
					}
				}
		}

	  if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!

    value.counter<-1;
    for (i in index)  {
        if(value.counter > length(value)) {
          value.counter<-1;
        }
				# Arguments were verified before, using the sloppy method:
				if(sloppy) {
					.setAlphabetSloppy(this$.sites[[i]], value[[value.counter]]);
				} else {
					setAlphabet(this$.sites[[i]], value[[value.counter]]);
				}

        value.counter<-(value.counter + 1);
    }
    invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabetsList
##	
setMethodS3(
	"getAlphabetsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		this$alphabetsList<-NULL;
		AlphabetsList(seq=this);	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueAlphabets
##	
setMethodS3(
	"getUniqueAlphabets",
	class="Sequence", 
	function(
		this,
		...
	){
	
		tmp<-list();			
    lapply(
      this$.sites,
      function(site) {
				tmp<<-c(tmp,list(site$.alphabet))
      }
		);
		return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueAlphabets
##	
setMethodS3(
	"setUniqueAlphabets", 
	class="Sequence", 
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
## Method: attachProcess
##	
setMethodS3(
	"attachProcess", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

    lapply(
      this$.sites[index],
      function(site) {
          attachProcess(site,process);
      }
    );	
		return(invisible(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	
## Method: detachProcess
##	
setMethodS3(
	"detachProcess", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		.checkWriteProtection(this);	
		if(!is.Process(process)){
			throw("Process object invalid!\n");
		}
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}
    lapply(
      this$.sites[index],
      function(site) {
          detachProcess(site,process);
      }
    );	
		return(invisible(this));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcesses
##	
setMethodS3(
	"getProcesses", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
	
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);
		}

    lapply(
      this$.sites[index],
      function(site) {
          site$processes;
      }
    );	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getUniqueProcesses
##	
setMethodS3(
	"getUniqueProcesses", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		tmp<-list();			
    lapply(
      this$.sites,
      function(site) {
				tmp<<-c(tmp,site$processes)
      }
		);
		return(unique(tmp));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setUniqueProcesses
##	
setMethodS3(
	"setUniqueProcesses", 
	class="Sequence", 
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
## Method: setProcesses
##	
setMethodS3(
	"setProcesses", 
	class="Sequence", 
	function(
		this,
		value,
		index,
		sloppy=FALSE,
		...
	){

		.checkWriteProtection(this);	
		if(missing(value)) {
			throw("No new values specified!\n");
		}

		if(!is.list(value)) {
			throw("The value parameter must be a list!\n");
		} else {
			lapply(
					value,
					function(element) {
						if(!is.list(element)){
							throw("The value parameter must be a list of lists containing process objects!\n");
						}
					}
			);
		}

	  if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
			index<-.checkIndexSanity(this, index);	
		}

    value.counter<-1;

		# Recycling value vector. rep() cannot be used here,
		# because we loose the object references!
    for (i in index)  {
        if(value.counter > length(value)) {
          value.counter<-1;
        }
				if (sloppy == FALSE) {
						setProcesses(this$.sites[[i]], value[[value.counter]]);
				} else {
					.setProcessesSloppy(this$.sites[[i]], value[[value.counter]]);
				}
        value.counter<-(value.counter + 1);
    }
    invisible(this);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setParameterAtSites
##	
setMethodS3(
	"setParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		value,
		index,
		...
	){

		.checkWriteProtection(this);
		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		} else if (missing(value)){
			throw("No new value given!\n");
		}

		if (missing(index)) {
			index<-seq(along=this$.sites);
		} else {
			index<-.checkIndexSanity(this, index);
		}
	
		if(length(value) == 1) {
			lapply(
				this$.sites[index],
				function(site){
					setParameterAtSite(process,site,id,value);
				}
			);
		} else {
			
			counter<-1;
			lapply(
				this$.sites[index],
				function(site){
					if( counter > length(value) ){
						counter<<-1;
					}	
					setParameterAtSite(process,site,id,value[[counter]]);
					counter<<-(counter+1);

				}
			);
				
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
## Method: setRateMultipliers
##	
setMethodS3(
	"setRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if(missing(value)){
			throw("No value provided!\n");
		}
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
		}
		setParameterAtSites(this=this,process=process,id="rate.multiplier",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getRateMultipliers
##	
setMethodS3(
	"getRateMultipliers", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if(!is.GeneralSubstitution(process)){
			throw("The specified process is not a substitution process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="rate.multiplier",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);



##	
## Method: getParameterAtSites
##	
setMethodS3(
	"getParameterAtSites", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index,
		...
	){

		if(missing(process)) {
			throw("No process given!\n");
		}
		else if(!is.Process(process)){
			throw("Process object invalid!\n");}
		else if (missing(id)) {
			throw("No site-process specific parameter id given!\n");
		} else if (!is.character(id)) {
			throw("Parameter id must be character!\n");
		}

		if (missing(index)) {
			index<-seq(along=this$.sites);
		} else {
				index<-.checkIndexSanity(this, index);
		}	
		
		lapply(
			this$.sites[index],
			function(site){
				getParameterAtSite(process,site,id);
			}
		);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getProcessesList
##	
setMethodS3(
	"getProcessesList", 
	class="Sequence", 
	function(
		this,
		...
	){

		ProcessesList(seq=this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEvents
##	
setMethodS3(
	"getEvents", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){
		
 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

		tmp<-list();
    for (i in index){
					# Setting the .positions filed for then Events.
					this$.sites[[i]]$.position<-i;
          tmp<-c(tmp, getEvents(this$.sites[[i]]));
					# Deleting the .position field;
					this$.sites[[i]]$.position<-NULL;
    }
		tmp;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEventsList
##	
setMethodS3(
	"getEventsList", 
	class="Sequence", 
	function(
		this,
		...
	){

		getEvents(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setEventsList
##	
setMethodS3(
	"setEventsList", 
	class="Sequence", 
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
## Method: getTotalRates
##	
setMethodS3(
	"getTotalRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){


 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

	
		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.total.rates[index];

 		#if (missing(index)) {
    #  index<-seq(along=this$.sites);
    #}
		#lapply(
		#		this$.sites[index],
		#		function(site){
		#				getTotalRate(site);
		#			}
		#		);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRates
##	
setMethodS3(
	"getTotalRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getTotalRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTotalRatesList
##	
setMethodS3(
	"setTotalRates", 
	class="Sequence", 
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
## Method: getAncestralSequence
##	
setMethodS3(
	"getAncestral", 
	class="Sequence", 
	function(
		this,
		...
	){

	this$.ancestral.obj;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRates
##	
setMethodS3(
	"getCumulativeRatesFromRange", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

 		if (missing(index)) {
      index<-seq(along=this$.sites);
    } else {
        index<-.checkIndexSanity(this, index);
    }

	
		if (this$.cumulative.rate.flag){
			.recalculateCumulativeRates(this);
		}

		this$.cumulative.rates[index];


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .recalculateCumulativeRates
##	
setMethodS3(
	".recalculateCumulativeRates", 
	class="Sequence", 
	function(
		this,
		target.site,
		...
	){
		
		# No flagged sites, we have a fresh start:	

		if( length(this$.flagged.sites) == 0 ) {
			if( this$.length > 0 ) {
				# Calculate total rates:
				for(i in 1:this$.length) {
					this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
				}
				this$.cumulative.rates<-cumsum(this$.total.rates);	
				this$.flagged.sites<-integer(0);

			}
	
		} else {
			if( this$.length > 0 ) {

				# We have some flagged sites, recalculate just their total rates:
        for(i in this$.flagged.sites) {
          this$.total.rates[[i]]<-this$.sites[[i]]$totalRate;
        }

				# The site before the first flagged site: 
				min.index<-(min(this$.flagged.sites) - 1);
				
				if(min.index > 1){
					
					# Do the cumsum only on the dirty part:
					new.cumrates<-this$.cumulative.rates[1:(min.index-1)];
					new.cumrates<-c(new.cumrates, cumsum( c(this$.cumulative.rates[min.index], this$.total.rates[(min.index+1):length(this$.total.rates) ] ) ) );
					this$.cumulative.rates<-new.cumrates;

				} else {
        	this$.cumulative.rates<-cumsum(this$.total.rates);
				}
				
				# Cleaning out flagged sites.
				this$.flagged.sites<-integer(0);
				
      } #/else
	
		}
	
		this$.cumulative.rate.flag<-FALSE;
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .flagCumulativeRates
##	
setMethodS3(
	".flagCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){
		
		this$.cumulative.rate.flag<-TRUE;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getCumulativeRates
##	
setMethodS3(
	"getCumulativeRates", 
	class="Sequence", 
	function(
		this,
		...
	){

		getCumulativeRatesFromRange(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setCumulativeRates
##	
setMethodS3(
	"setCumulativeRates", 
	class="Sequence", 
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
## Method: getBigRate
##	
setMethodS3(
	"getBigRate", 
	class="Sequence", 
	function(
		this,
		...
	){
	
		if (length(this$.sites) > 0) {
			if(this$.cumulative.rate.flag)	{
				.recalculateCumulativeRates(this);	
			}
			getCumulativeRatesFromRange(this, this$.length);
		} else {
			return(NA);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setBigRate
##	
setMethodS3(
	"setBigRate", 
	class="Sequence", 
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
## Method: setAncestralSeq
##	
setMethodS3(
	"setAncestral", 
	class="Sequence", 
	function(
		this,
		value,
		...
	){

		.checkWriteProtection(this);	
		if (!is.Sequence(value) & ! is.Process(value)) {
			throw("Ancestral object must be a sequence or a process!\n");
		}	else {
			this$.ancestral.obj<-value;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: clone.Sequence
##	

setMethodS3(
	"clone", 
	class="Sequence", 
	function(
		this,
		...
	){

		# Cloning the whole sequence object:
		that<-clone.Object(this);

		# Disabling write protection:

		if(that$writeProtected) {
			that$writeProtected<-FALSE;
		}

		# Setting the ancestral sequence:
		that$.ancestral.obj<-this;

		# Setting the name:

		# Resetting comments:
		that$.comments<-list();

		# Cloning sites;
		if(this$length > 0) {
					for (i in 1:this$.length) {
						that$.sites[[i]]<-clone(this$.sites[[i]]);

						# Calling these assignments is actually expensive!
						that$.sites[[i]]$.ancestral<-this$.sites[[i]];
						that$.sites[[i]]$.sequence<-that;
			}
		}

		that$name<-paste("clone of",this$name);
	
		return(that);

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
  class="Sequence",
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
## Method: .checkIndexSanity
##
setMethodS3(
  ".checkIndexSanity",
  class="Sequence",
  function(
    this,
    index,
    ...
  ){

		if (length(index) == 0 ) {
				return(c());
		}
	  if( length(index) == 1 ) {
			if(is.na(index)) {
				warning("Index vector is NA! Coercing to empty vector!\n");
				return(c());
			} 
			if (is.nan(index)) {
				warning("Index vector is NaN! Coercing to empty vector!\n");
				return(c());
			}

		}

		if(min(index) < 1 ) {
			throw("Index vector element ",min(index)," too small!\n");
		}
		if( max(index) > this$.length ) {
			throw("Index vector element ",max(index)," too big!\n");
		}
		return(index);

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
  class="Sequence",
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
  private=FALSE,
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
  class="Sequence",
  function(
    this,
    value,
    ...
  ){

    if(getWriteProtected(this)) {throw("Cannot set value because the object is write protected!\n")}
    else {return(FALSE)}

  },
  private=TRUE,
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
	class="Sequence", 
	function(
		this,
		...
	){

		 this$.summary$"Name"<-this$name;
		 this$.summary$"Id"<-this$id;
		 this$.summary$"Length"<-this$length;
		 this$.summary$"Big rate"<-this$bigRate;
		 this$.summary$"Ancestral object"<-this$ancestral$id;

		 if(this$.cumulative.rate.flag) {
		 	this$.summary$"Cumulative rate flag"<-TRUE;
		 }
		 if(length(this$.flagged.sites) > 0 ) {
		 	this$.summary$"Flagged sites"<-paste(this$.flagged.sites,collapse=" ");
		 }
		 if(this$writeProtected) {
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
## Method: as.character.Sequence
##	
setMethodS3(
	"as.character", 
	class="Sequence", 
	function(
		this,
		...
	){

		getString(this);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plot
##	
setMethodS3(
	"plot", 
	class="Sequence", 
	function(
		this,
		index=NA,
		...
	){


    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
		if( length(unique(this$totalRates)) == 1 & any(is.na(unique(this$totalRates))) ){
      warning("The total rates are undefined, nothing to plot here!\n");
      return(invisible(FALSE));
		}
    else {
      if(missing(index)) {
        index<-seq(along=1:this$length,by=1);
      }
      if(this$.cumulative.rate.flag){
        .recalculateCumulativeRates(this);
      }
      what<-this$.total.rates[c(index)]

      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Total rate plot for sequence", this$id),
        xlab="Position",
        ylab="Total rate",
				ylim=c(0,max(what)),
        xlim=c(min(index),max(index)),
				xaxt="n"
      );
			axis(side=1, at=index, labels=index);
    }


	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: plotParameterAtSites
##	
setMethodS3(
	"plotParametersAtSite", 
	class="Sequence", 
	function(
		this,
		process,
		id,
		index=NA,
		...
	){


    if(this$length == 0) {
      warning("The sequence leght is zero, nothing to plot here!\n");
      return(invisible(FALSE));
    }
    if(missing(index)) {
      index<-seq(along=3:this$length,by=1);
    }
		what<-apply(as.array(index),1,
			function(pos){
				tmp<-getParameterAtSites(this,process,id,pos)[[1]]$value;
				if(!is.numeric(tmp)){
					throw("Plot method failed becuase encountered non-numeric parameter value!\n");
				}
				return(tmp);
			}
		);
      plot(
        x=index,
        y=what,
        type="h",
        lwd=1,
        col="blue",
        main=paste("Plot of parameter",id,"for process",process$id),
        xlab="Position",
        ylab="Value",
        xlim=c(min(index),max(index)),
				ylim=c(0,max(what)),
				xaxt="n"
      );
			axis(side=1, at=index, labels=index);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setDeletionTolerance
##	
setMethodS3(
	"setDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
		setParameterAtSites(this=this,process=process,id="deletion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getDeletionTolerance
##	
setMethodS3(
	"getDeletionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralDeletor")){
			throw("The specified process is not an insertion process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="deletion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setInsertionTolerance
##	
setMethodS3(
	"setInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		value,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		else if (missing(value)){
			throw("No value provided!\n");
		}
		else if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
		setParameterAtSites(this=this,process=process,id="insertion.tolerance",value=value,index=index);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getInsertionTolerance
##	
setMethodS3(
	"getInsertionTolerance", 
	class="Sequence", 
	function(
		this,
		process,
		index,
		...
	){

		if(missing(process)){
			throw("No process given!\n");
		}
		if(!inherits(process,"GeneralInsertor")){
			throw("The specified process is not an insertion process!\n");
		}
		rm<-getParameterAtSites(this=this,process=process,id="insertion.tolerance",index=index);
		return(as.numeric(lapply(rm,function(param){param$value})));
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: sampleStates
##	
setMethodS3(
	"sampleStates", 
	class="Sequence", 
	function(
		this,
		index,
		...
	){

		if(!missing(index)){
			index<-.checkIndexSanity(this, index);
		}
		else {
			index<-seq(along=this$.sites);
		}
	
    for(site in this$.sites[index]){
    # Sample states from the equlibrium distributions if
    # the state is NA:
      if(is.na(site$state)){
        # Assemble the list of substitution processes:
        subst.proc<-list();
        for (proc in site$processes){
          if(is.GeneralSubstitution(proc)){
              subst.proc<-c(subst.proc, list(proc));
            }
        }

        # Complain if we have no substitution processes to sample from:
        if(length(subst.proc) == 0){
          throw("Site state is NA and no substitution processes are attached. Cannot sample state!\n");
        }

        site.rates<-as.numeric(lapply(
            subst.proc,
            function(proc){
              return(getParameterAtSite(proc, site,"rate.multiplier")$value);
            }
        ));

        # Normalizing site rates:
        site.rates<-site.rates/sum(site.rates);

        # Single subst process:
        if(length(subst.proc) == 1){
          site$state<-sampleState(subst.proc[[1]]);
        }
        else {
          # Sample a substitution process according to the rate multipliers:
          nproc<-sample(x=c(1:length(subst.proc)),size=1, replace=FALSE, prob=site.rates);
          # Sample the state from the winner process:
          site$state<-sampleState(subst.proc[[nproc]]);
        }
 		} # if is.na...

    }

		return(invisible(this));
	
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);


##	$Id: SequenceIndel.R,v 1.11 2009-05-01 08:48:21 sbotond Exp $
##
##	Class: Sequence - indel related methods
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
## Method: insertSequence
##
setMethodS3(
  "insertSequence",
  class="Sequence",
  function(
    this,
		insert,
		position,
		process=NA,
		sloppy=FALSE,
		paranoid=FALSE,
    ...
  ){

		.checkWriteProtection(this);
		if(missing(insert)) {
			throw("Insert sequence object is missing!\n");
		}
		else if (missing(position)) {
			throw("Insertion position is missing!\n");
		}

		if ( sloppy == FALSE ) {
			if(!is.Sequence(insert)) {
				throw("Insert object not valid!\n");
			}
			else if (this$length == 0 & position != 0 ) {
				throw("Acceptor sequence length is zero! The only valid insertion position is 0!\n");	
			}
			else if ( !( position >= 0 & position <=(this$.length + 1))) {
				throw("Insertion position ",position," is invalid!\n");
			}
	 }

	 # Just return if insert has zero length:	
	 if(insert$length == 0){
			warning("The length of the sequence to be inserted is zero! Nothing to do here!\n");
			return(invisible(FALSE));	
	 } 
	 # Clone insert object:
	 insert<-clone(insert);

	 # Set the generator process:
	 if(!missing(process)) {
			if( (length(process) == 0) | !is.Process(process)){
				throw("Process object invalid!\n");
			}
	 } else {
			process<-Sequence$.root.ins;
	 }
	 for(site in insert$.sites) {
				site$.ancestral<-process;
				site$sequence<-this;
	 }

	 # Recalculate cumulative rates if the flag is on:
	 if(this$.cumulative.rate.flag) {
			.recalculateCumulativeRates(this);
	 }

	 # Flagging cumulative rates:
		.flagCumulativeRates(this);

		# Inserting new site objects:

		if ( position == this$.length) {
			# Insertion at the end of the sequence;
			this$.sites<-c(this$.sites,insert$.sites);
			this$.total.rates<-c(this$.total.rates,rep(c(NA),times=insert$.length) );
			this$.cumulative.rates<-c(this$.cumulative.rates,rep(NA,times=insert$.length) );

		} else if (position == 0) {
			# Insertion in the sequence
			this$.sites<-c(insert$.sites, this$.sites);
			this$.total.rates<-c(rep(NA,times=insert$.length),this$.total.rates);
      this$.cumulative.rates<-c(rep(NA,times=insert$.length),this$.cumulative.rates);

		} else {
			# Insertion at position 0
			this$.sites<-c(this$.sites[1:position],insert$.sites,this$.sites[(position+1):this$.length]);
			this$.total.rates<-c(this$.total.rates[1:position],rep(NA,times=insert$.length),this$.total.rates[(position+1):this$.length]);
			this$.cumulative.rates<-c(this$.cumulative.rates[1:position],rep(NA,times=insert$.length),this$.cumulative.rates[(position+1):this$.length]);

		}

	 # Checking if lengths are consistent:
		
		if(length(this$.sites) != (this$.length + insert$.length)) {
			throw("Length inconsistency after insertion!\n");
		} else {
			this$.length<-(this$.length + insert$.length);
		}

	# Flagging the inserted sites:
		this$.flagged.sites<-c(this$.flagged.sites,(position+1):(position+insert$.length));

	if( sloppy == FALSE ) {
		if(length(this$.total.rates) != this$.length) {
			throw("Total rates vector inconsistency after insertion!\n");
		}
		if(length(this$.cumulative.rates) != this$.length) {
			throw("Cumulative rates vector inconsistency after insertion!\n");
		}
	}

	# Recalculating cumulative rates:
		.recalculateCumulativeRates(this);

	# Paranoid check of total rates:

	if(paranoid) {	
		for (i in 1:this$.length) {
				if(this$.sites[[i]]$totalRate != this$.total.rates[[i]]) {
					throw("Object total rates inconsistent with total rates vector!\n");
				}
		}
	}

	# Deleting the insert:
	rm(insert);

	return(invisible(this));


  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: deleteSubSequence
##
setMethodS3(
  "deleteSubSequence",
  class="Sequence",
  function(
    this,
		index,
		sloppy=FALSE,
    ...
  ){

		.checkWriteProtection(this);
		if(missing(index)) {
			 throw("No index vector specified!\n");
		} else if (sloppy != FALSE) {
			index<-.checkIndexSanity(this, index);
		}
		if(length(index) == 0) {
			return(FALSE);
		} else {

			# Avoid deletion on dirty sequence as
			# that will cause havoc.
			if(this$.cumulative.rate.flag){
			 .recalculateCumulativeRates(this);	
			}
			# Flagging cumulative rates:	
			.flagCumulativeRates(this);
			min.index<-min(index);
			# Deleting site objects:	
			this$.sites[index]<-NULL;
			# Updating rate vectors:
			this$.total.rates<-this$.total.rates[-index];
			this$.cumulative.rates<-this$.cumulative.rates[-index];
			
			# Flag the site before the deletion to
			# to force cumulative rate recalculation:
			# FIXME: this will call site$totalRate
			if (min.index > 2 ) {
				this$.flagged.sites<-c(this$.flagged.sites,(min.index - 1));
			}

			if( length(this$.sites) != (this$.length - length(index) ) ) {
				throw("Inconsistency after deleting sites!\n");
			} else{
				this$.length<-length(this$.sites);
			}
			.recalculateCumulativeRates(this);	
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
## Method: copySubSequence
##
setMethodS3(
  "copySubSequence",
  class="Sequence",
  function(
    this,
		index,
		process=NA,
		sloppy=FALSE,
    ...
  ){

		if(missing(index)) {
				index<-seq(along=this$.sites);
		}
		else {
			index<-.checkIndexSanity(this, index);
		}
		if(!is.na(process) & !is.Process(process)) {
			throw("Process object invalid!\n");		
		} else {

			# Avoid copying from dirty sequence:
			if(this$.cumulative.rate.flag){
			 .recalculateCumulativeRates(this);	
			}

			length<-length(index);

			# Create an empty sequence object:
			copy<-Sequence();

			# Flag copy cumulative rates:
			copy$.cumulative.rate.flag<-TRUE;

			if(is.na(process)){
				# Getting the root insertion process:
				process<-Sequence$.root.ins;
			}
			# Setting the ancestral to sequence:
			copy$.ancestral.obj<-process;
			
			# Setting copy name:
			copy$name<-paste("Copied from",this$name);

			# Setting length:
			copy$.length<-length;

			# Clone the sites:
			copy$.sites<-lapply(this$.sites[index],
				function(site){
					site.copy<-clone(site);
					site.copy$.ancestral<-process;
					return(site.copy);
				}
			);

			# Copy total rates:	
			copy$.total.rates<-this$.total.rates[index];

			# Create cumulative rates vector:
			copy$.cumulative.rates<-cumsum(copy$.total.rates);

			copy$.cumulative.rate.flag<-FALSE;

			if(length(copy$.sites) != length){
				throw("Sites list length mismatch!\n")
			}
			else if(length(copy$.total.rates) != length){
				throw("Total rates vector length mismatch!\n")
			}
			else if(length(copy$.cumulative.rates) != length){
				throw("Cumulative rates vector length mismatch!\n")
			}

			return(copy);
		
		}

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);
##	$Id: Site-Process.R,v 1.17 2009-05-01 08:48:21 sbotond Exp $
##
##	Methods for Site/Process interactions
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
## Method: hasSiteSpecificParameter
##
setMethodS3(
  "hasSiteSpecificParameter",
  class="Process",
  function(
    this,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			else if ( length (intersect((as.vector(this$siteSpecificParamIds) == id),TRUE) ) == 0 ) {
					return(FALSE);
			} else {
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
## Method: getParameterAtSite
##
setMethodS3(
  "getParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
    ...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")}
			
			if (.checkTriplett(this,site,id)){
				id<-as.character(id);
				list (
					id=id,
					name=site$.processes[[getId(this)]]$site.params[[id]]$name,
					value=site$.processes[[getId(this)]]$site.params[[id]]$value,
					type=site$.processes[[getId(this)]]$site.params[[id]]$type
				);
			}
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: setParameterAtSite
##
setMethodS3(
  "setParameterAtSite",
  class="Process",
  function(
    this,
    site,
    id,
		value,
		...
  ){
			if (missing(id)) {throw("Parameter identifier is missing!\n")};
      id<-as.character(id);
			
			if (.checkTriplett(this,site,id)){

				type<-site$.processes[[this$id]]$site.params[[id]]$type;
				if (length(intersect(class(value),type)) == 0 ) {throw("The new value is of wrong type!\n")}
				site$.processes[[this$id]]$site.params[[id]]$value<-value;
	
			}
			flagTotalRate(site);
		 .flagSeqCumulativeRates(site);
			invisible(this);
				
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: .checkTriplett
##
setMethodS3(
  ".checkTriplett",
  class="Process",
  function(
    this,
    site,
		id,
		...
  ){
					
			if (!is.Site(site)) {throw ("Site object not valid!\n")}
			else if (!hasSiteSpecificParameter(this,id)) {
				throw(paste("The process",this$id,"has no site specific paramter with id:",id,"!\n",sep=" "));
			}
			else if (!isAttached(site,this)) {throw("Process is not attached to site!\n")} else {
				return(TRUE);
			}
  },
  private=TRUE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
);

##
## Method: isAttached
##
setMethodS3(
  "isAttached",
  class="Site",
  function(
    this,
    process,
    ...
  ){
			
			if (!is.Process(process)) {throw("Process object invalid!\n")}	
			attached_processes<-getProcesses(this);
			if (length(attached_processes) == 0 ){ return(FALSE)}
			
			tmp<-lapply(
					attached_processes,
					function(proc) { equals(proc, process)}
			);	
			tmp<-unique(tmp);
			
			if(length(tmp) == 1 ) {
					# If we have only one process attached,
					# than simply return the result of the equals() function.
					return(tmp[[1]]);
			} else {
					
					# Additional check to make sure that the .process entry is here.	
				#	if (length (intersect(class(this$.processes[[getId(process)]]),"list")) == 0) {
				#		throw("Something evil is happening! The process is attached, but the .process entry is invalid!\n");
				#	}
					# If length(tmp) > 1, than one of its elements must me TRUE,
					# so returning TRUE.
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
## Method: attachProcess
##
setMethodS3(
  "attachProcess",
  class="Site",
  function(
    this,
    process,
    ...
  ){
	
		if(!is.Process(process)) {
			throw("Process object is not valid!\n"); }
		else if( is.na(process$alphabet) ){
				throw("The process has no alphabet attached!\n"); }
		else if( is.na(this$alphabet) ){
				throw("The site has no alphabet attached!\n"); }
		else if (this$alphabet != process$alphabet) {
				throw("The site and process alphabets are incompatible!\n"); }
		else if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); 
		}
		# FIXME - checking for template sequence 
		else if( hasUndefinedRate(process) ){
				warning("The process",process$id," has undefined rates!\n");
		}
		else {
			this$.processes[[process$id]]<-list (
				object 				= 	process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params		=		process$siteSpecificParamList	
			);
		}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .attachProcessSloppy
##
setMethodS3(
  ".attachProcessSloppy",
  class="Site",
  function(
    this,
    process,
    ...
  ){
	
		if(isAttached(this,process)) {
				warning("Process already attached, doing nothing!\n");
				return(invisible(this)); }
		else {
			this$.processes[[process$id]]<-list (
				object 				= 	process,
				# We copy the default site-process specific parameters
				# from the process object.
				site.params		=		process$siteSpecificParamList	
			);
		}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		
		# The user should not modify the process
		# after is attached to a site!
		process$writeProtected<-TRUE;	
		invisible(this);

  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: detachProcess
##
setMethodS3(
  "detachProcess",
  class="Site",
  function(
    this,
    process,
    ...
  ){
			
		if(!is.Process(process)) {
			throw("Process object is not valid!\n");
		}
		else if (!isAttached(this,process)) {
				warning("Process is not attached, doing nothing!\n");
		}
		
		# Setting the list entry to NULL,
		# so it will wanish from the list.
		this$.processes[[process$id]]<-NULL;
	
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: getProcesses
##
setMethodS3(
  "getProcesses",
  class="Site",
  function(
    this,
    ...
  ){
			
		 	lapply(names(this$.processes),function(id){this$.processes[[id]][["object"]]});
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: setProcesses
##
setMethodS3(
  "setProcesses",
  class="Site",
  function(
    this,
		value,
    ...
  ){

		if(missing(value)) {throw("No new value given!\n")}
		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		for(i in value) {
			if(!is.Process(i)){
					throw("The accepted argument is a list of processes!\nVectors and lists are not euivalent, take care!\n");
			}
		}
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcess(this,process)});
		lapply(to.attach, function(process) {attachProcess(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );

##
## Method: .setProcessesSloppy
##
setMethodS3(
  ".setProcessesSloppy",
  class="Site",
  function(
    this,
		value,
    ...
  ){

		value<-as.list(value);
	
		# All the top-level elements must be Process instances!	
		attached<-getProcesses(this);
		
		# Sadly we cannot use set operations directly here
		# beacuse we lose the object references.
		to.attach<-list();
		to.detach<-list();
		the.rest<-list();
		
		for (i in value) {
				if (!isAttached(this,i)) {
					to.attach<-c(to.attach,list(i));
				} else {
					the.rest<-c(the.rest,list(i));
				}
		}		
		
		for (i in attached) {
				in.the.rest<-FALSE;
				for (j in the.rest) {
					if (i == j)	{
						in.the.rest<-TRUE;
						break;
					}
				} # /for j
				if(!in.the.rest) {
					to.detach<-c(to.detach,list(i));	
				}
		} # /for i				
	
	
		lapply(to.detach, function(process) {detachProcessSloppy(this,process)});
		lapply(to.attach, function(process) {.attachProcessSloppy(this,process)});
	
		invisible(this);
  },
  private=FALSE,
  protected=FALSE,
  overwrite=FALSE,
  conflict="warning",
  validators=getOption("R.methodsS3:validators:setMethodS3")
  );
##	$Id: Site.R,v 1.46 2009-04-30 13:27:43 sbotond Exp $
##
##	Class: Site
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
	"Site",
	function(
		state=NA,	#
		alphabet=NA,
		ancestral=NA,
		processes=NA,
		sequence=NA,
		...
	){
	

		# Extend the PSRoot Class:
		this<-extend(
			PSRoot(),
			"Site",
			.state=NA,
			.ancestral=NA,
			.alphabet=NA,
			.processes=list(),
			.total.rate=NA,
			.sequence=NA,
			.is.site=TRUE
		);
		
		# The instance is static by default:
		STATIC<-TRUE;

		# Set alphabet if present:	
		if(!missing(alphabet)){
			this$alphabet<-alphabet;
			STATIC<-FALSE;
		}

		# Alphabet is mandatory if 
		# ancestral is present:

		if (!missing(ancestral) & missing(alphabet) & !is.Process(ancestral)) {
				throw("Ancestral object sepcified, but no alphabet is given!\n");
		}

		# Set ancestral pointer if present:	
		if(!missing(ancestral)){
			# The ancestral is a site or a process:
			if( !is.Process(ancestral) & !is.Site(ancestral)) {
					throw("The ancestral object must be a site or a process!\n");	
			} else {
				this$.ancestral<-ancestral;
				STATIC<-FALSE;
			}
		}

		# Set state if present,
		# complain if no alphabet is specified:
		if (!missing(state)) {
				STATIC<-FALSE;
				if(!missing(alphabet)){
					this$state<-state;
				} else { throw("The state is specified, but no alphabet is given!\n"); }
		}
	
		# Set the processes:		
		if(!missing(processes)){	
			this$processes<-processes;
		}
		

		# Set the parent sequence if present:		
		if(!missing(sequence)){	
			this$sequence<-sequence;
		}

		# Calculate total rate given the state
		# and the processes:
		if(!STATIC){ 
			if(!is.na(this$.state)) {
				.recalculateTotalRate(this);
			}
			.checkConsistency(this);
		}

		# Return the Site object:
		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.Site
##	
setMethodS3(
	"is.Site", 
	class="default", 
	function(
		this,
		...
	){

    if(!is.PSRoot(this)) {return(FALSE)}
		if(!is.null(this$.is.site)){return(TRUE)}
    if ( inherits(this, "Site")) {
			this$.is.site<-TRUE;
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
## Method: .checkConsistency
##	
setMethodS3(
	".checkConsistency", 
	class="Site", 
	function(
		this,
		...
	){
			message<-"Site state is inconsistent! ";		

			# If the ancestral object is a site:	
			if (is.Site(this$.ancestral)) {
				
					#Check if the alphabets match:		
					# Warning: using the '!='.Alphabet here!
					if( this$alphabet != this$ancestral$alphabet ) {
						throw(message, "The ancestral alphabet and the site alphabet is different!\n");
					}
			} else if (is.Process(this$.ancestral)) { 
					# Hook for checking the process object;
					# print(this$.ancestral)			
			} else if (!is.na(this$.ancestral)){
					throw("Ancestral object is invalid!\n");
			}
			
			# Check if the total rate is numeric or NA:
			if(is.null(this$.total.rate)) {
				throw("The total rate is NULL!\n");
			}
			if (!is.numeric(this$.total.rate) && !is.na(this$.total.rate)) {throw(message,"The total rate is not numeric!\n")}
				
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
setMethodS3(
	"checkConsistency", 
	class="Site", 
	function(
		this,
		...
	){

			#cat("Checking site consistency ...\n");
			# Reassigning the values by virtual fields.
			# The methods should complain if something is wrong.
			# Slow but maybe elegant.

			if(is.null(this$.alphabet)) {
				throw("Site alphabet is NULL!\n");
			}
			else if(!is.na(this$.alphabet)) {
				this$.alphabet<-this$.alphabet;
			}

			
			if(is.null(this$.ancestral)) {
				throw("Ancestral object is NULL!\n");
			}
			
			if(is.null(this$.processes)) {
				throw("Process list is NULL!\n");
			}
			else {
				this$processes<-this$processes;
			}
			
			.checkConsistency(this);
			
			lapply(
				this$.processes,
				function(p) {
						# Even more paranoid check is possible here!
						.checkSiteSpecificParamList(p$object,plist=p$site.params);
				}
			);
			return(invisible(TRUE));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);



##	
## Method: getState
##	
setMethodS3(
	"getState", 
	class="Site", 
	function(
		this,
		...
	){
	
		this$.state;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setState
##	
setMethodS3(
	"setState", 
	class="Site", 
	function(
		this,
		new.state,
		...
	){
		
		new.state<-as.character(new.state);	
		# Check if new.state is scalar:
		if (length(new.state) != 1 ){throw("The state must be a vector of length 1!\n")}
		# Check if the site has an alphabet attached:
		else if(is.na(this$alphabet)) {throw("Cannot set state because the site has no alphabet attached!\n")}
		# Check if symbol is in the site alphabet:	
		else if( !hasSymbols(this$.alphabet,new.state)) {throw("Symbol not in site alphabet!\n")}
		flagTotalRate(this);
		.flagSeqCumulativeRates(this);
		this$.state<-new.state;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAlphabet
##	
setMethodS3(
	"getAlphabet", 
	class="Site", 
	function(
		this,
		...
	){
	
		this$.alphabet;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAlphabet
##	
setMethodS3(
	"setAlphabet", 
	class="Site", 
	function(
		this,
		new.alphabet,
		...
	){
		
		if(!is.Alphabet(new.alphabet)){
				throw("The supplied alphabet object is not valid!\n");
		} else if (is.Site(this$.ancestral)) {
				if (this$.ancestral$alphabet != new.alphabet) {
							throw("The alphabet is not equivalent with the ancestral alphabet!\n");
					}
		} 
		else if(!is.na(this$.state) & !hasSymbols(new.alphabet, this$.state)){
			throw("The current state is not part of the new alphabet!\n");
		}
		else{
			flagTotalRate(this);
		 .flagSeqCumulativeRates(this);
			this$.alphabet<-new.alphabet;
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .setAlphabetSloppy
##	
setMethodS3(
	".setAlphabetSloppy", 
	class="Site", 
	function(
		this,
		new.alphabet,
		...
	){
	
		this$.alphabet<-new.alphabet;
		return(invisible(new.alphabet));

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getAncestral
##	
setMethodS3(
	"getAncestral", 
	class="Site", 
	function(
		this,
		...
	){
		
		this$.ancestral;
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setAncestral
##	
setMethodS3(
	"setAncestral", 
	class="Site", 
	function(
		this,
		value,
		...
	){
		
		throw("You should never try to modify directly the ancestral attribute!\n");		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: recalculateTotalRate
##	
setMethodS3(
	".recalculateTotalRate", 
	class="Site", 
	function(
		this,
		...
	){

		if(!is.na(getState(this))){
			this<-enableVirtual(this);	
			total.rate<-0;	
			for(e in this$events) {
				total.rate<-(total.rate + getRate(e));
			}
			this$.total.rate<-total.rate	
		}
	},
	private=TRUE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getTotalRate
##	
setMethodS3(
	"getTotalRate", 
	class="Site", 
	function(
		this,
		...
	){
		
		if(is.na(this$.total.rate)) {
			.recalculateTotalRate(this);
		}

			return(this$.total.rate);
		
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setTotalRate
##	
setMethodS3(
	"setTotalRate", 
	class="Site", 
	function(
		this,
	  value,	
		...
	){
		
		throw("You should never try to set the totalRate directly!\n");	

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: flagTotalRate
##	
setMethodS3(
	"flagTotalRate", 
	class="Site", 
	function(
		this,
		...
	){
		
		# Setting .total.rate to NA,
		# this will force recalculation
		# when next accessed.	
		this$.total.rate<-NA;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: .flagSeqCumulativeRates
##	
setMethodS3(
	".flagSeqCumulativeRates", 
	class="Site", 
	function(
		this,
		...
	){
		
		if(is.Sequence(this$.sequence)) {
			.flagCumulativeRates(this$.sequence);
		}

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: getEvents
##	
setMethodS3(
	"getEvents", 
	class="Site", 
	function(
		this,
		...
	){

		# Virtual fields disabled in getField methods!
		procs<-getProcesses(this);

		tmp<-list();
		for (p in procs) {
				tmp<-c(tmp,getEventsAtSite(p, this));
		}
		return(tmp);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setEvents
##	
setMethodS3(
	"setEvents", 
	class="Site", 
	function(
		this,
		new.rate,
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
## Method: getSequence
##	
setMethodS3(
	"getSequence", 
	class="Site", 
	function(
		this,
		...
	){

		this$.sequence;

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: setSequence
##	
setMethodS3(
	"setSequence", 
	class="Site", 
	function(
		this,
		new.seq,
		...
	){

		if(!is.Sequence(new.seq)) {
			throw("Sequence object invalid!\n");
		} else {
			this$.sequence<-new.seq;
		}

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
	class="Site", 
	function(
		this,
		...
	){

		this$state;		

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: summary.Site
##	
setMethodS3(
	"summary", 
	class="Site", 
	function(
		this,
		verbose=FALSE,
		...
	){

			this$.summary$"State"=this$state;
			if(!is.na(this$alphabet)) {
			alphabet_symbols = paste(this$alphabet$symbols,collapse=" ");
			this$.summary$"Alphabet"=paste("\n","  Type: ",this$alphabet$type,"\n","  Symbols: ", alphabet_symbols,sep="");
			} else {
				this$.summary$"Alphabet"=NA
			}
			
			attached_processes<-this$processes;
			header<-paste("Attached processes (",length(attached_processes),")",sep="");
			tmp<-character(0);

			for (p in attached_processes) {
				tmp<-paste(tmp,"\n ",p$id)
			}		

			flagTotalRate(this);
		 .flagSeqCumulativeRates(this);

			this$.summary[[header]]<-tmp;
			
			tmp<-character(0);
			for (e in this$events) {
				tmp<-paste(tmp,"\n ");
				tmp<-paste(tmp,"Name:",e$name);
				tmp<-paste(tmp," Rate:",e$rate);
				tmp<-paste(tmp," Process:",e$process$id);
			}
			this$.summary$"Active events"<-tmp;

			this$.summary$"Total rate"<-getTotalRate(this);
	
			if(!is.na(this$sequence)){
				this$.summary$"Part of sequence"<-this$sequence$id;
			}

			if(is.Process(this$ancestral)) {
					this$.summary$"Directly inserted by"<-this$ancestral$id;
			} else if (is.Site(this$ancestral)) {
					this$.summary$"Ancestral state"<-this$ancestral$state;
			} else if (!is.na(this$ancestral)){
					throw("summary.Site detected inconsistent state!\n");

			}

			NextMethod();

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
	);

##	$Id: StatesList.R,v 1.6 2009-04-29 08:35:19 sbotond Exp $
##
##	Class: StatesList
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
	"StatesList",
	function(
		...,
		seq=NA
	){
	
		this<-extend(
			PSRoot(),
			"StatesList",
			.seq=NA
		);

		if(!missing(seq)) {
			if(!is.Sequence(seq)) {
				throw("Sequence object not valid!\n");
			} else {
				this$.seq=seq;
			}
		}

		return(this);
	},
	enforceRCC=TRUE
);

##	
## Method: is.StatesList
##	
setMethodS3(
	"is.StatesList", 
	class="default", 
	function(
		this,
		...
	){

		if(!is.PSRoot(this)) {return(FALSE)}
		inherits(this, "StatesList");

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
setMethodS3(
	"checkConsistency", 
	class="StatesList", 
	function(
		this,
		...
	){

   if(!is.Sequence(this$.seq)){
      throw("States list sequence reference is invalid!\n");
    }
    return(TRUE);

	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [.StatesList
##	
setMethodS3(
	"[", 
	class="StatesList", 
	function(
		this,
		index
	){

		getStates(this$.seq,index);	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [<-.StatesList
##	
setMethodS3(
	"[<-", 
	class="StatesList", 
	function(
		this,
		index,
		value
	){

		setStates(this$.seq,value,index);
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[.StatesList
##	
setMethodS3(
	"[[", 
	class="StatesList", 
	function(
		this,
		index
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		}
		getStates(this$.seq,index)[[1]];
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

##	
## Method: [[<-.StatesList
##	
setMethodS3(
	"[[<-", 
	class="StatesList", 
	function(
		this,
		index,
		value
	){
		
		if (length(index) > 1 ) {
			throw("Attempted to select more than one element!\n");	
		} else if (length(value) > 1) {
			warning("Value vector longer than one!\n");
		}
		setStates(this$.seq,value,index);

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
	class="StatesList", 
	function(
		this,
		...
	){
		
		this[];	
	
	},
	private=FALSE,
	protected=FALSE,
	overwrite=FALSE,
	conflict="warning",
	validators=getOption("R.methodsS3:validators:setMethodS3")
);

