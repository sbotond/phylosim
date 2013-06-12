%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSequences.PhyloSim}
\alias{getSequences.PhyloSim}
\alias{PhyloSim.getSequences}
\alias{getSequences,PhyloSim-method}

 \title{Gets all the Sequence objects associated with the nodes of a phylo object aggregated by a PhyloSim object}

 \description{
	Gets all the Sequence objects associated with the nodes of a phylo object aggregated by a PhyloSim object.

	The order of the Sequence objects in the returned list reflects the identifiers of the associated nodes.
 }

 \usage{
\method{getSequences}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of sequence objects.
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
	# run the simulation
	Simulate(sim)
	# get all the associated sequence objects
	getSequences(sim)
	# get the sequence associated with node 3
	# via virtual field
	sim$sequences[[3]]
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
