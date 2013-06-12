%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{scaleTree.PhyloSim}
\alias{scaleTree.PhyloSim}
\alias{PhyloSim.scaleTree}
\alias{scaleTree,PhyloSim-method}

 \title{Scale the branch lengths of a phylo object aggragted by a PhyloSim object}

 \description{
	Scale the branch lengths of a phylo object aggragted by a PhyloSim object.
	This method multiples all the edge lengths by the specified factor.
 }

 \usage{
\method{scaleTree}{PhyloSim}(this, factor, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{factor}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The PhyloSim object (invisible).
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the tree length
	sim$treeLength
	# scale tree
	scaleTree(sim,10)
	# get the scaled tree length
	sim$treeLength
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
