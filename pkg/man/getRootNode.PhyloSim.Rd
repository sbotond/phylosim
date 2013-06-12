%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRootNode.PhyloSim}
\alias{getRootNode.PhyloSim}
\alias{PhyloSim.getRootNode}
\alias{getRootNode,PhyloSim-method}

 \title{Get the identifier of the root node from a PhyloSim object}

 \description{
	Get the identifier of the root node from a PhyloSim object.
 }

 \usage{
\method{getRootNode}{PhyloSim}(this, ...)
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
	# get the root node ID
	getRootNode(sim)
	# get the root node ID via virtual field
	sim$rootNode
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
