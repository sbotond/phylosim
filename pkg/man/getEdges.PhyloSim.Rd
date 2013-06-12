%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEdges.PhyloSim}
\alias{getEdges.PhyloSim}
\alias{PhyloSim.getEdges}
\alias{getEdges,PhyloSim-method}

 \title{Get the edge matrix from a phylo object aggregated by a PhyloSim object}

 \description{
	Get the edge matrix from a phylo object aggregated by a PhyloSim object.

	The rows of the edge matrix contain the nodes connected by the edge and the edge length.
 }

 \usage{
\method{getEdges}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the edge matrix
	getEdges(sim)
	# get the edge matrix via virtual field
	sim$edges
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
