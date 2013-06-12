%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{clearStates.Sequence}
\alias{clearStates.Sequence}
\alias{Sequence.clearStates}
\alias{clearStates,Sequence-method}

 \title{Set the states of a collection of Site objects aggregated by a Sequence object to undefined (NA)}

 \description{
	Set the states of a collection of Site objects aggregated by a Sequence object to undefined (NA).
 }

 \usage{
\method{clearStates}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a nucleotide sequence
	s<-NucleotideSequence(string="ATGC")
	s
	# set states to NA in the range 2:3
	clearStates(s,2:3)
	s
	# set all states to NA
	clearStates(s)
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
