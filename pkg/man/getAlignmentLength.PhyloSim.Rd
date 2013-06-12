%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlignmentLength.PhyloSim}
\alias{getAlignmentLength.PhyloSim}
\alias{PhyloSim.getAlignmentLength}
\alias{getAlignmentLength,PhyloSim-method}

 \title{Get the alignment length from a PhyloSim object}

 \description{
	Get the alignment length from a PhyloSim object.

	This method retruns the number of columns in the alignment stored in the PhyloSim object.
 }

 \usage{
\method{getAlignmentLength}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a PhyloSim object and run a simulation:
	sim<-Simulate(
                     PhyloSim(phy=rcoal(3),
                     root=NucleotideSequence(string="ATGC", proc=list(list(JC69())) ) )
             )
	# get the alignment length
	getAlignmentLength(sim)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
