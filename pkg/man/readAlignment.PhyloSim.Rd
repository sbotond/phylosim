%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{readAlignment.PhyloSim}
\alias{readAlignment.PhyloSim}
\alias{PhyloSim.readAlignment}
\alias{readAlignment,PhyloSim-method}

 \title{Read alignment from file}

 \description{
	Read alignment from file.

	This method reads an alignment by using the \code{read.dna} function from the \code{\link{ape}}
	package and stores in the \code{PhyloSim} object. If a tree is already attached to the \code{PhyloSim}
       object, the alignment must at least contain the sequences corresponding to tip nodes (but it
       may also contain additional ancestral sequences).
 }

 \usage{
\method{readAlignment}{PhyloSim}(this, file, format="fasta", ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{file}{A file name specified by either a variable of mode character, or a double-quoted string.}
	\item{format}{a character string specifying the format of the DNA sequences. Four choices are possible: "interleaved", "sequential", "clustal", or "fasta", or any unambiguous abbreviation of these.}
 	\item{...}{Not used.}
 }

 \value{
	The PhyloSim object (invisible).
 }

 \examples{
	# get a safe file name
	fname<-paste("PhyloSim_dummy_fas_",Sys.getpid(),sep="")
	# write out a fasta alignment
	cat("> t3\nGTCTTT-CG-\n",file=fname);
	cat("> t4\nG--TC-TCGG\n",file=fname,append=TRUE);
	cat("> t2\nG--TC-TCGG\n",file=fname,append=TRUE);
	cat("> t1\nGTC-G-TCGG",file=fname,append=TRUE);
	# construct a PhyloSim object,
	# set the phylo object
	sim<-PhyloSim(phylo=rcoal(4))
	# read the alignment
	readAlignment(sim,fname)
	# remove alignment file
	unlink(fname)
	# plot the tree & alignment
	plot(sim)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
