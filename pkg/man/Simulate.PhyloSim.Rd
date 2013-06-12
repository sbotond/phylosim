%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Simulate.PhyloSim}
\alias{Simulate.PhyloSim}
\alias{PhyloSim.Simulate}
\alias{Simulate,PhyloSim-method}

 \title{Run a simulation according to a PhyloSim object}

 \description{
	Run a simulation according to a PhyloSim object.

	The phylo object and the root sequence must be set before attempting to run a simulation.
	Also the bigRate of the root sequence must not be NA or zero, so at least one sane
	Process object must be attached to the root sequence object.
 }

 \usage{
\method{Simulate}{PhyloSim}(this, quiet=FALSE, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{quiet}{TRUE or FALSE (default).}
 	\item{...}{Not used.}
 }

 \value{
 	The PhyloSim object (invisible).
 }

 \examples{
	# Create a PhyloSim object.
	# Provide the phylo object
	# and the root sequence.
	sim<-PhyloSim(
		name="TinySim",
		phylo=rcoal(3),
		root.seq=NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	);
	# Run the simulation
	Simulate(sim);
	# Print the resulting sequences
	sim$sequences
	# Print the resulting alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
