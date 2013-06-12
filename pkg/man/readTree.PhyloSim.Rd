%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{readTree.PhyloSim}
\alias{readTree.PhyloSim}
\alias{PhyloSim.readTree}
\alias{readTree,PhyloSim-method}

 \title{Read tree from file}

 \description{
	Read tree from file.

	This method reads a tree by using the \code{read.tree} function from the \code{\link{ape}}
	package and stores in the \code{PhyloSim} object. If an alignment is already attached
       to the \code{PhyloSim} object, it must contain all sequences corresponding to tip nodes.
 }

 \usage{
\method{readTree}{PhyloSim}(this, file, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{file}{A file name specified by either a variable of mode character, or a double-quoted string.}
 	\item{...}{Not used.}
 }

 \value{
	The PhyloSim object (invisible).
 }

 \examples{
	# get a safe file name
	fname<-paste("PhyloSim_dummy_fas_",Sys.getpid(),sep="")
	# write out a fasta alignment
	cat("(a,(b,c));",file=fname);
	# construct a PhyloSim object:
	sim<-PhyloSim()
	# read the alignment
	readTree(sim,fname)
	# remove alignment file
	unlink(fname)
	# plot the tree
	plot(sim)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
