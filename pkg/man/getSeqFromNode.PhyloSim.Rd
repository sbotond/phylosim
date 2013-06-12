%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSeqFromNode.PhyloSim}
\alias{getSeqFromNode.PhyloSim}
\alias{PhyloSim.getSeqFromNode}
\alias{getSeqFromNode,PhyloSim-method}

 \title{Get the Sequence object associated with a given node of a phylo object aggregated by a PhyloSim object}

 \description{
	Get the Sequence object associated with a given node of a phylo object aggregated by a PhyloSim object.
 }

 \usage{
\method{getSeqFromNode}{PhyloSim}(this, node=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{node}{Node identifier.}
 	\item{...}{Not used.}
 }

 \value{
 	A Sequence object.
 }

 \examples{
	# Create a PhyloSim object.
	# Provide the phylo object
	# and the root sequence.
	sim<-PhyloSim(
		name="TinySim",
		phylo=rcoal(3),
		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
	);
	# get the sequence associated with node 5
	getSeqFromNode(sim,5)	# Should be NA
	# Run the simulation
	Simulate(sim)
	# try again
	getSeqFromNode(sim,5)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
