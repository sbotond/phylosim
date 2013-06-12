%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getStates.Sequence}
\alias{getStates.Sequence}
\alias{Sequence.getStates}
\alias{getStates,Sequence-method}

 \title{Get the states of a set of Site objects aggregated by a Sequence object}

 \description{
	Get the states of a set of Site objects aggregated by a Sequence object.

 }

 \usage{
\method{getStates}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{index}{An integer vector specifying a set of positions.
	It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector.
 }

 \examples{
	# create a sequence object
	s<-Sequence(alphabets=list(NucleotideAlphabet()),string="AATTGCCCCCTTGG")
	# get all Site states
	getStates(s)
	# get the states for a collection of sites
	getStates(s,c(1:3,5,8))
	# get states via virtual field
	s$states
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
