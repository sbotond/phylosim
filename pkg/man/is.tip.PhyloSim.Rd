%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.tip.PhyloSim}
\alias{is.tip.PhyloSim}
\alias{PhyloSim.is.tip}
\alias{is.tip,PhyloSim-method}

 \title{Check if a node is a tip}

 \description{
	Check if a node is a tip.
 }

 \usage{
\method{is.tip}{PhyloSim}(this, node=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{node}{A node identifier (integer vector of length one).}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{
	# create a PhyloSim object
	sim<-PhyloSim(phylo=rcoal(5));
	# check if node 4 is a tip
	is.tip(sim,4)
	# check if node 6 is a tip
	is.tip(sim,6)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
