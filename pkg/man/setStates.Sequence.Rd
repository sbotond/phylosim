%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setStates.Sequence}
\alias{setStates.Sequence}
\alias{Sequence.setStates}
\alias{setStates,Sequence-method}

 \title{Set the states for a set of Site objects aggregated by a Sequence object}

 \description{
	Set the states for a set of Site objects aggregated by a Sequence object.
	The value vector is recycled, which is useful when creating repeated patterns.
 }

 \usage{
\method{setStates}{Sequence}(this, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{value}{A character vector containg the states (recycled if shorter than the index vector). The states must be compatible with the corresponding Alphabet object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a sequence object of length 10
	s<-Sequence(alphabets=list(NucleotideAlphabet()),length=10)
	# set the states in some ranges
	setStates(s,c("A","T","A"),index=1:5)
	setStates(s,c("G","C"),index=6:10)
	# display sequence
	s
	# set the states for the whole Sequence object
	setStates(s,c("A","T","T","A"))
	s
	# set states via virtual field
	s$states<-c("A","T")
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
