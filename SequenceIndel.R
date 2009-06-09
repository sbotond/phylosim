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
