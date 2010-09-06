##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	

##
## cpREV
##
##########################################################################/** 
#
# @RdocClass cpREV
# 
# @title "The cpREV empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Adachi, J., P. J. Waddell, W. Martin, and M. Hasegawa (2000) Plastid
# genome phylogeny and a model of amino acid substitution for proteins
# encoded by chloroplast DNA - Journal of Molecular Evolution 50:348 358 \url{http://bit.ly/bnBVLm}
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{equ.dist}{Equilibrium distribution.}
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create substitution model object 
#	p<-cpREV()
#	# get object summary	
#	summary(p)
#
#       # The following code demostrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the range 1:5 invariable
#       setRateMultipliers(s,p,0,1:5)
#       # get rate multipliers
#       getRateMultipliers(s,p)
#       # create a simulation object
#       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
#       # run simulation
#       Simulate(sim)
#       # print alignment
#       sim$alignment
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






