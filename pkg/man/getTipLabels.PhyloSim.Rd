%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTipLabels.PhyloSim}
\alias{getTipLabels.PhyloSim}
\alias{PhyloSim.getTipLabels}
\alias{getTipLabels,PhyloSim-method}

 \title{Get the tip labels from a phylo object aggregated by a PhyloSim object}

 \description{
	Get the tip labels from a phylo object aggregated by a PhyloSim object.
 }

 \usage{
\method{getTipLabels}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the tip labels.
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# get the tip labels
	getTipLabels(sim)
	# get the tip lables via virtual field
	sim$tipLabels
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
