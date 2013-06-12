%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProcesses.Sequence}
\alias{getProcesses.Sequence}
\alias{Sequence.getProcesses}
\alias{getProcesses,Sequence-method}

 \title{Get the Process objects attached to the Site objects aggregated by a Sequence object}

 \description{
	Get the Process objects attached to the Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getProcesses}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of lists of Process objects.
 }

 \examples{
	# create a sequence object with some processes attached
	s<-Sequence(
               length=4,
               alphabets=list(NucleotideAlphabet()),
               processes=list(list(JC69(),K80()),list(GTR()))
               )
	# get the list of lists of attached processes from positions 1 and 3
	getProcesses(s,c(1,3))
	# get processes via virtual field
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
