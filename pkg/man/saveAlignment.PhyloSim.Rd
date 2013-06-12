%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{saveAlignment.PhyloSim}
\alias{saveAlignment.PhyloSim}
\alias{PhyloSim.saveAlignment}
\alias{saveAlignment,PhyloSim-method}

 \title{Save the alignment stored in a PhyloSim object in a Fasta file}

 \description{
	Save the alignment stored in a PhyloSim object in a Fasta file.
 }

 \usage{
\method{saveAlignment}{PhyloSim}(this, file="phylosim.fas", skip.internal=FALSE, paranoid=FALSE, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{file}{The name of the output file.}
 	\item{skip.internal}{Do not save sequences corresponding to internal nodes.}
 	\item{paranoid}{Check the consistency of the alignment.}
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
		root.seq=NucleotideSequence(string="ATG",processes=list(list(JC69())))
	);
	# run the simulation
	Simulate(sim)
	# save the alignment
	file<-paste("PhyloSim_dummy_fasta_",Sys.getpid(),".fas",sep="");
	saveAlignment(sim,file=file,paranoid=TRUE);
	# print out the Fasta file
	cat(paste(scan(file=file,what=character(),sep="\n"),collapse="\n"));cat("\n");
	# delete Fasta file
	unlink(file);
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
