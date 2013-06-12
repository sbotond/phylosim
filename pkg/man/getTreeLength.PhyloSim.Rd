%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTreeLength.PhyloSim}
\alias{getTreeLength.PhyloSim}
\alias{PhyloSim.getTreeLength}
\alias{getTreeLength,PhyloSim-method}

 \title{Get the tree length from a PhyloSim object}

 \description{
	Get the tree length from a PhyloSim object.

	This method retruns the sum of the edge lengths stored in the aggregated phylo object.
 }

 \usage{
\method{getTreeLength}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the tree length
	getTreeLength(sim)
	# get tree length via virtual field
	sim$treeLength
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
