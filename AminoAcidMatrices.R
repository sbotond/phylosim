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
# encoded by chloroplast DNA - Journal of Molecular Evolution 50:348--358 
# DOI: 10.1007/s002399910038 \url{http://bit.ly/bnBVLm}
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
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
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
##########################################################################/** 
#
# @RdocClass PAM
# 
# @title "The PAM empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Dayhoff, M. O.; Schwartz, R. M.; Orcutt, B. C. (1978). "A model of evolutionary change in proteins" - 
# Atlas of Protein Sequence and Structure 5 (3):345-352
#
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
#	p<-PAM()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass PAM.dcmut
# 
# @title "The PAM.dcmut empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Kosiol, C, and Goldman, N (2005) Different versions of the Dayhoff rate matrix -
# Molecular Biology and Evolution 22:193-199 \url{http://dx.doi.org/10.1093/molbev/msi005}
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
#	p<-PAM.dcmut()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass JTT
# 
# @title "The JTT empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Jones, D. T., W. R. Taylor, and J. M. Thornton (1992) The rapid generation of mutation data matrices 
# from protein sequences. CABIOS 8:275-282 \url{http://dx.doi.org/10.1093/bioinformatics/8.3.275}
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
#	p<-JTT()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass JTT.dcmut
# 
# @title "The JTT.dcmut empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Kosiol, C, and Goldman, N (2005) Different versions of the Dayhoff rate matrix -
# Molecular Biology and Evolution 22:193-199 \url{http://dx.doi.org/10.1093/molbev/msi005}
#
# Jones, D. T., W. R. Taylor, and J. M. Thornton (1992) The rapid generation of mutation data matrices 
# from protein sequences. CABIOS 8:275-282 \url{http://dx.doi.org/10.1093/bioinformatics/8.3.275}
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
#	p<-JTT.dcmut()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first threee positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass LG
# 
# @title "The LG empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Le, S. Q., and O. Gascuel (2008) An improved general amino acid replacement matrix - 
# Mol. Biol. Evol. 25:1307-1320 \url{http://dx.doi.org/10.1093/molbev/msn067}
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
#	p<-LG()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass mtArt
# 
# @title "The mtArt empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Abascal, F., D. Posada, and R. Zardoya (2007) MtArt: A new Model of
# amino acid replacement for Arthropoda - Mol. Biol. Evol. 24:1-5 \url{http://dx.doi.org/10.1093/molbev/msl136}
#
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
#	p<-mtArt()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass mtMam
# 
# @title "The mtMam empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Yang, Z., R. Nielsen, and M. Hasegawa (1998) Models of amino acid
# substitution and applications to Mitochondrial protein evolution,
# Molecular Biology and Evolution 15:1600-1611 \url{http://bit.ly/by4NMb}
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
#	p<-mtMam()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass mtREV24
# 
# @title "The mtREV24 empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Adachi, J. and Hasegawa, M. (1996) MOLPHY version 2.3: programs for
# molecular phylogenetics based on maximum likelihood.  Computer Science
# Monographs of Institute of Statistical Mathematics 28:1-150
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
#	p<-mtREV24()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass MtZoa
# 
# @title "The MtZoa empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Rota-Stabelli, O., Z. Yang, and M. Telford. (2009) MtZoa: a general mitochondrial amino acid 
# substitutions model for animal evolutionary studies. Mol. Phyl. Evol 52(1):268-72 \url{http://bit.ly/bjZfKi}
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
#	p<-MtZoa()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass WAG
# 
# @title "The WAG empirical amino acid substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Whelan, S. and N. Goldman (2001)  A general empirical model of
# protein evolution derived from multiple protein families using a maximum likelihood 
# approach - Molecular Biology and Evolution 18:691-699 \url{http://bit.ly/dpTKAd}
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
#	p<-WAG()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
#	# sample states
#	sampleStates(s)
#       # make the first three positions invariable
#       setRateMultipliers(s,p,0,1:3)
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
# 	AminoAcidSubst GeneralSubstitution UNREST
# }
# 
#*/###########################################################################
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






