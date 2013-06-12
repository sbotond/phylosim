%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setPhylo.PhyloSim}
\alias{setPhylo.PhyloSim}
\alias{PhyloSim.setPhylo}
\alias{setPhylo,PhyloSim-method}

 \title{Set the phylo object for a PhyloSim object}

 \description{
	Set the phylo object for a PhyloSim object.

	The internal structure of the provided phylo object is reordered in a cladeweise fashion.
 }

 \usage{
\method{setPhylo}{PhyloSim}(this, value, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{value}{A phylo object created by the \pkg{ape} package.}
 	\item{...}{Not used.}
 }

 \value{
 	A phylo object or FALSE.
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
