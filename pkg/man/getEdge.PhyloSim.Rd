%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEdge.PhyloSim}
\alias{getEdge.PhyloSim}
\alias{PhyloSim.getEdge}
\alias{getEdge,PhyloSim-method}

 \title{Get and edge from the edge matrix}

 \description{
	Get and edge from the edge matrix.
 }

 \usage{
\method{getEdge}{PhyloSim}(this, number=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{number}{The edge number.}
 	\item{...}{Not used.}
 }

 \value{
 	The edge as a matrix with a single row.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get edge number 3
	getEdge(sim,3)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
