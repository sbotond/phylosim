##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## cpREV
##
setConstructorS3(
  "cpREV",
  function(
		equ.dist=NA,
    ...
  ){

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
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

		this<-AminoAcidSubst$newAAMatrix(
			name="WAG",
			paml.file="wag.dat",
			equ.dist=equ.dist
		);
		return(this);

  },
  enforceRCC=FALSE
);






