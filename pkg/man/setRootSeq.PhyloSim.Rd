%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRootSeq.PhyloSim}
\alias{setRootSeq.PhyloSim}
\alias{PhyloSim.setRootSeq}
\alias{setRootSeq,PhyloSim-method}

 \title{Set the root sequence for a PhyloSim object}

 \description{
	Set the root sequence for a PhyloSim object.

	The root sequence will be used as a starting point for the simulation. The phylo object must be set before
	trying to set the root sequence object.
 }

 \usage{
\method{setRootSeq}{PhyloSim}(this, value, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{value}{A valid Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	The root Sequence object if succesfull, FALSE otherwise.
 }

 \examples{
	# create some objects
	sim<-PhyloSim(phylo=rcoal(3));
	seq<-NucleotideSequence(string="ATGCC");
	# set/get root sequence
	setRootSeq(sim, seq);
	getRootSeq(sim, seq);
	# set/get root sequence via virtual field
	sim$rootSeq<-BinarySequence(string="111000111000");
	sim$rootSeq;

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}. Sequence Process
 }
