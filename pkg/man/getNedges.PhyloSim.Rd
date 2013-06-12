%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getNedges.PhyloSim}
\alias{getNedges.PhyloSim}
\alias{PhyloSim.getNedges}
\alias{getNedges,PhyloSim-method}

 \title{Get the number of edges from phylo object aggregated by a PhyloSim object}

 \description{
	Get the number of edges from phylo object aggregated by a PhyloSim object.
 }

 \usage{
\method{getNedges}{PhyloSim}(this, ...)
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
	# get the number of the edges
	getNedges(sim)
	# get the number of the edges via virtual field
	sim$nedges
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
