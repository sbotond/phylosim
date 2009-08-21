##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
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

		# Use the package data directory if loaded:
		if(length(intersect(search(),c("package:phylosim"))) == 1){
			RDATDIR<-paste(.path.package("phylosim"),"/data/",sep="");
			PAMLDIR<-RDATDIR;
		}
		
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






