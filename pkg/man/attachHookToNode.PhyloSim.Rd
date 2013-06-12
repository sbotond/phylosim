%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{attachHookToNode.PhyloSim}
\alias{attachHookToNode.PhyloSim}
\alias{PhyloSim.attachHookToNode}
\alias{attachHookToNode,PhyloSim-method}

 \title{Attach a callback function to a given node of a phylo object aggregated by a PhyloSim object}

 \description{
	Attach a callback function to a given node of a phylo object aggregated by a PhyloSim object.

	A "node hook" is a function which accepts a Sequence object through the named argument "seq" and returns a
	Sequence object. The node hook function must accept any object which inherits from the \code{Sequence} class!

	After simulating the branch leading to the node, the resulting Sequence object is passed
	to the node hook and the returned object is used to simulate the downstream branches.

	By using node hooks the attached processes can be replaced during simulation, hence enabling the simulation of
	non-homogeneous sequence evolution.
 }

 \usage{
\method{attachHookToNode}{PhyloSim}(this, node=NA, fun=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{node}{Node identifier.}
 	\item{fun}{A function (see above).}
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
	# create a node hook function
	hook<-function(seq=NA){
		# replace the substitution process with F84
		if(inherits(seq,"NucleotideSequence")){
			cat("Replacing JC69 with F84.\n");
			seq$processes<-list(list(F84(rate.params=list("Kappa" = 2))));
		}
		return(seq);
	}
	# attach hook function to node 5
	attachHookToNode(sim,5,hook);
	# Run the simulation
	Simulate(sim);
	# Check if the processes have been truly replaced
	lapply(sim$sequences, getUniqueProcesses.Sequence)
	# Print the resulting alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
