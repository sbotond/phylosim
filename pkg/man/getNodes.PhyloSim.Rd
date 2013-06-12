%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getNodes.PhyloSim}
\alias{getNodes.PhyloSim}
\alias{PhyloSim.getNodes}
\alias{getNodes,PhyloSim-method}

 \title{Get the node identifiers from a PhyloSim object}

 \description{
	Get the node identifiers from a PhyloSim object.
 }

 \usage{
\method{getNodes}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the node IDs
	getNodes(sim)
	# get the node IDs via virtual field
	sim$nodes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
