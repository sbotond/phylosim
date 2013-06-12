%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getNtips.PhyloSim}
\alias{getNtips.PhyloSim}
\alias{PhyloSim.getNtips}
\alias{getNtips,PhyloSim-method}

 \title{Get the number of the tips form a phylo object aggregated by a PhyloSim object}

 \description{
	Get the number of the tips form a phylo object aggregated by a PhyloSim object.
 }

 \usage{
\method{getNtips}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the number of tips
	getNtips(sim)
	# get the number of tips via virtual field
	sim$ntips
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
