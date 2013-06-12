%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTips.PhyloSim}
\alias{getTips.PhyloSim}
\alias{PhyloSim.getTips}
\alias{getTips,PhyloSim-method}

 \title{Get the node identifiers of the tip nodes from a PhyloSim object}

 \description{
	Get the node identifiers of the tip nodes from a PhyloSim object.
 }

 \usage{
\method{getTips}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the tip IDs
	getTips(sim)
	# get the tip IDs via virtual field
	sim$tips
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
