%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{detachHookFromNode.PhyloSim}
\alias{detachHookFromNode.PhyloSim}
\alias{PhyloSim.detachHookFromNode}
\alias{detachHookFromNode,PhyloSim-method}

 \title{Detach a node hook function from a given node of a phylo object aggregated by a PhyloSim object}

 \description{
	Detach a node hook function from a given node of a phylo object aggregated by a PhyloSim object.

 }

 \usage{
\method{detachHookFromNode}{PhyloSim}(this, node=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{node}{Node identifier.}
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
	# detach hook from node 5
	detachHookFromNode(sim,5);
	# Run the simulation again
	Simulate(sim);	# You should not see the message printed out by the "hook" function.

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	attachHookToNode PhyloSim Simulate.PhyloSim
 }
