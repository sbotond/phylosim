%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getPhylo.PhyloSim}
\alias{getPhylo.PhyloSim}
\alias{PhyloSim.getPhylo}
\alias{getPhylo,PhyloSim-method}

 \title{Get the phylo object aggregated in a PhyloSim object}

 \description{
	Get the phylo object aggregated in a PhyloSim object.

 }

 \usage{
\method{getPhylo}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A phylo object or NA.
 }

 \examples{
	#create a PhyloSim object
	sim<-PhyloSim();
	# creat a phylo object
	tree<-rcoal(3);
	# get/set phylo object
	setPhylo(sim,tree);
	getPhylo(sim,tree);
	# get/set phylo object via virtual field
	sim$tree<-rcoal(5);
	sim$tree;
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	The PhyloSim class, the \pkg{ape} package.
 }
