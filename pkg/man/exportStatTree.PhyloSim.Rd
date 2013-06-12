%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{exportStatTree.PhyloSim}
\alias{exportStatTree.PhyloSim}
\alias{PhyloSim.exportStatTree}
\alias{exportStatTree,PhyloSim-method}

 \title{Export the per-branch counts of an event as a phylo object}

 \description{
	Export the per-branch counts of an event as a phylo object.

	During simulation the number of events performed on every branch is recorded. The recorded events can be "basic"
	events, like "insertion", "deletion" and "A->T" or events which are sums of basic events, like "substituion". The
	\code{getBranchEvents} method returns a character vector with the names of the events having per-branch
	statistics recorded. The method should be called after the simulation is finished.

	The per-branch statistics can be exported as phylo objects by using the \code{exportStatTree} method.
	The branch lengths of the exported phylo objects are set to the value of the respective per-branch event count.
 }

 \usage{
\method{exportStatTree}{PhyloSim}(this, event, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{event}{The name of the event as returned by the \code{getBranchEvents} method.}
 	\item{...}{Not used.}
 }

 \value{
 	A phylo object.
 }

 \examples{
	# Create a PhyloSim object.
	# Provide the phylo object
	# and the root sequence.

	# NOTE: this will be a little bit slow
	sim<-PhyloSim(
		phylo=rcoal(3),
		root.seq=CodonSequence(
                               string="ATGATTATT",
                               processes=list(list(GY94(kappa=2,omega.default=0.5)))
                               )
	);
	# make the tree longer to have more events
	scaleTree(sim,5)
	# plot the tree
	plot(sim)
	# run simulation
	Simulate(sim)
	# get the list of recorded per-branch event counts
	getBranchEvents(sim)
	# export the number of substitutions as a phylo object
	subst<-exportStatTree(sim,"substitution")
	# plot the exported phylo object
	plot(subst)
	#export the number of synonymous substitutions as a phylo object
	subst<-exportStatTree(sim,"nr.syn.subst")
	# plot the exported phylo object
	plot(subst)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
