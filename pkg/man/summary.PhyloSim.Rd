%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{summary.PhyloSim}
\alias{summary.PhyloSim}
\alias{PhyloSim.summary}
\alias{summary,PhyloSim-method}

 \title{Summarize the properties of an object}

 \description{
       Summarize the properties of an object.
 }

 \usage{
\method{summary}{PhyloSim}(object, ...)
}

 \arguments{
       \item{object}{An object}
       \item{...}{Not used.}
 }

 \value{
  Returns a PSRootSummary object.
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
       # get a summary
       summary(sim)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{PhyloSim}}.
 }
