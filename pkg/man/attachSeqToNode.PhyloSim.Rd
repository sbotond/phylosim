%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{attachSeqToNode.PhyloSim}
\alias{attachSeqToNode.PhyloSim}
\alias{PhyloSim.attachSeqToNode}
\alias{attachSeqToNode,PhyloSim-method}

 \title{Assotiate a Sequence object with a given node of a phylo object aggregated by a PhyloSim object}

 \description{
	Assotiate a Sequence object with a given node of a phylo object aggregated by a PhyloSim object.

	This method is mainly used internally.

 }

 \usage{
\method{attachSeqToNode}{PhyloSim}(this, node=NA, seq=NA, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{node}{Node identifier.}
	\item{seq}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	The PhyloSim object (invisible).
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
