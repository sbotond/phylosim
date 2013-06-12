%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRootSeq.PhyloSim}
\alias{getRootSeq.PhyloSim}
\alias{PhyloSim.getRootSeq}
\alias{getRootSeq,PhyloSim-method}

 \title{Get the root sequence aggregated by a PhyloSim object}

 \description{
	Get the root sequence aggregated by a PhyloSim object.

 }

 \usage{
\method{getRootSeq}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	The root Sequence object or NA.
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
