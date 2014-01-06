##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	

##
## ECMrest
##
##########################################################################/** 
#
# @RdocClass ECMrest
# 
# @title "The ECMrest empirical codon substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Kosiol C., Holmes I., Goldman, N. (2007) An empirical codon model for protein sequence evolution -
# Mol Biol Evol. 24(7):1464-79 DOI: 10.1093/molbev/msm064 \url{http://bit.ly/1ia8gWm}
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
#	p<-ECMrest()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-CodonSequence(length=10,processes=list(list(p)) )
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
# 	CodonSubst GeneralSubstitution CodonUNREST
# }
# 
#*/###########################################################################
setConstructorS3(
  "ECMrest",
  function(
		equ.dist=NA,
    		...
  ){

		this<-CodonSubst$newMatrix(
			name="ECMrest",
			paml.file="ECMrest.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);

##
## ECMunrest
##
##########################################################################/** 
#
# @RdocClass ECMunrest
# 
# @title "The ECMunrest empirical codon substitution model"
# 
# \description{ 
#
#
#	@classhierarchy
# }
#
# \references{
# Kosiol C., Holmes I., Goldman, N. (2007) An empirical codon model for protein sequence evolution -
# Mol Biol Evol. 24(7):1464-79 DOI: 10.1093/molbev/msm064 \url{http://bit.ly/1ia8gWm}
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
#	p<-ECMunrest()
#	# get object summary	
#	summary(p)
#	# display a bubble plot
#	plot(p)
#
#       # The following code demonstrates how to use 
#       # the process in a simulation.
#	
#	# create a sequence, attach process p
#	s<-CodonSequence(length=10,processes=list(list(p)) )
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
# 	CodonSubst GeneralSubstitution CodonUNREST
# }
# 
#*/###########################################################################
setConstructorS3(
  "ECMunrest",
  function(
		equ.dist=NA,
    		...
  ){

		this<-CodonSubst$newMatrix(
			name="ECMunrest",
			paml.file="ECMunrest.dat",
			equ.dist=equ.dist
		);

		return(this);

  },
  enforceRCC=FALSE
);



